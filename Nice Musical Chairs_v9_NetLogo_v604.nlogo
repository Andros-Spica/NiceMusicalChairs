;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  The Nice Musical Chairs model - A model of the interaction between farming and herding, introducing explicit groups and group mechanisms
;;  Copyright (C) 2016 Andreas Angourakis (andros.spica@gmail.com)
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

extensions [ vid ]

;;;;;;;;;;;;;;;;;
;;;;; BREEDS ;;;;
;;;;;;;;;;;;;;;;;

breed [ groups group ]

breed [ pointers pointer ]

breed [ labelpositions labelposition ]

;;;;;;;;;;;;;;;;;
;;; VARIABLES ;;;
;;;;;;;;;;;;;;;;;

globals
[
  totalPatches

  ;;; modified parameters
  initH initF
  baseIntGrowth maxExtGrowth
  initGroups
  effectivenessGr
  maxGroupChangeRate
  opt optimalGrowthIncrease

  group_management group_pasture_tenure pairing

  ;;; variables used in resolve_conflict
  defender contender

  ;;; counters and final measures
  countLandUseF countLandUseH
  numberGroups
  FFcompetitions HHcompetitions HFcompetitions FHcompetitions landUseChangeEvents managementEvents
  farmingDemand farmingGrowth farmingDeterrence farmingBalance
  herdingDemand herdingGrowth herdingDeterrence herdingBalance
  meanGroupSize bigGroupSize
  meanGroupEffectiveness bigGroupEffectiveness
  bigTargetFarmingRatio meanTargetFarmingRatio
  meanFarmingIntegration meanHerdingIntegration meanMixedIntegration
]

groups-own
[
  groupSize groupEffectiveness
  intGrowthF intGrowthH
  farmingRatio targetFarmingRatio
  ;;; helpers
  groupSizeF groupSizeH
  groupDemandF groupDemandH
  groupDemandRemain
]

patches-own [ landUse myGroup contendersF contendersH withinIntegration betweenIntegration ]

pointers-own [ value ]

labelpositions-own [ name ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

  ;;; This procedure initializes the model

  clear-all

  set totalPatches count patches

  parameters-check-1

  ;;; setup parameters depending on the type of experiment
  if (typeOfExperiment = "random")
  [
    ; set random seed
    let aSeed new-seed
    random-seed aSeed
    set seed aSeed

    let listOfScenarios (list "Ao - open access, simple group dynamics" "Bo - open access, pairing" "Co - open access, management" "Do - open access, pairing and management" "Ar - restricted access, simple group dynamics" "Br - restricted access, pairing" "Cr - restricted access, management" "Dr - restricted access, pairing and management")
    let randomIndex random 8
    set scenario item randomIndex listOfScenarios ;;; randomly choose one scenario
    set baseIntGrowth 0.01 + random-float base_intrinsic_growth_rate
    set maxExtGrowth 0.001 + random-float max_extrinsic_growth_rate
    set opt random-float optimum
    set optimalGrowthIncrease random-float optimal_growth_increase
    set initGroups 1 + random initial_number_of_groups
    set maxGroupChangeRate random-float max_group_change_rate
    set effectivenessGr random-float totalPatches
    set initH random round ((init_herding / 100) * totalPatches)
    set initF random round ((init_farming / 100) * totalPatches)
  ]
  if (typeOfExperiment = "defined by GUI")
  [
    ; set random seed
    random-seed seed

    set baseIntGrowth base_intrinsic_growth_rate
    set maxExtGrowth max_extrinsic_growth_rate
    set opt optimum
    set optimalGrowthIncrease optimal_growth_increase
    set initGroups initial_number_of_groups
    set maxGroupChangeRate max_group_change_rate
    set effectivenessGr effectiveness_gradient
    set initH round ((init_herding / 100) * totalPatches)
    set initF round ((init_farming / 100) * totalPatches)
  ]
  if (typeOfExperiment = "defined by expNumber")
  [
    ; set random seed
    let aSeed new-seed
    random-seed aSeed
    set seed aSeed

    load-experiment
  ]

  parameters-check-2

  set pairing true
  if (scenario = "Ao - open access, simple group dynamics" OR scenario = "Co - open access, management" OR scenario = "Ar - restricted access, simple group dynamics" OR scenario = "Cr - restricted access, management")
  [
    set optimalGrowthIncrease 0
    set pairing false
  ]

  set group_pasture_tenure false
  if (scenario = "Ar - restricted access, simple group dynamics" OR scenario = "Br - restricted access, pairing" OR scenario = "Cr - restricted access, management" OR scenario = "Dr - restricted access, pairing and management")
  [
    set group_pasture_tenure true
  ]

  set group_management false
  if (scenario = "Co - open access, management" OR scenario = "Do - open access, pairing and management" OR scenario = "Cr - restricted access, management" OR scenario = "Dr - restricted access, pairing and management")
  [
    set group_management true
  ]

  ask patch 0 0
  [
    sprout-groups initGroups
  ]

  ;;; set land use according to the parameter setting (position is arbitrary and has no consequence)
  ask patches [ set landUse "N" set myGroup nobody set contendersF (turtle-set) set contendersH (turtle-set) ]
  ask n-of initF patches
  [
    set landUse "F"
  ]
  ask n-of initH patches with [landUse = "N"]
  [
    set landUse "H"
  ]
  initialize-patches-and-groups

  ;;; initialize visualization

  ask patch (min-pxcor + round ((max-pxcor - min-pxcor) * 0.97) ) (min-pycor + round ((max-pycor - min-pycor) * 0.97) )
  [
    sprout-labelpositions 1 [ set name "scenario" set label scenario set shape "invisible" ]
  ]
  if (display_details = true)
  [
    ask patch (min-pxcor + round ((max-pxcor - min-pxcor) * 0.97) ) (min-pycor + round ((max-pycor - min-pycor) * 0.03) )
    [
      sprout-labelpositions 1 [ set name "time" set label "time: 0" set shape "invisible" ]
    ]
    ask patch (min-pxcor + round ((max-pxcor - min-pxcor) * 0.3) ) (min-pycor + round ((max-pycor - min-pycor) * 0.03) )
    [
      sprout-labelpositions 1 [ set name "farming" set shape "invisible" ]
    ]
    ask patch (min-pxcor + round ((max-pxcor - min-pxcor) * 0.72) ) (min-pycor + round ((max-pycor - min-pycor) * 0.03) )
    [
      sprout-labelpositions 1 [ set name "bigGroupSize" set shape "invisible" ]
    ]

  ]

  update-visualization

  reset-ticks

end

to parameters-check-1

  ;;; check if values were reset to 0
  ;;; and set default values
  if (end-simulation-in-tick = 0)               [ set end-simulation-in-tick             1000 ]
  if (base_intrinsic_growth_rate = 0)           [ set base_intrinsic_growth_rate            0.03 ]
  if (initial_number_of_groups = 0)             [ set initial_number_of_groups             25 ]
  if (initH = 0 and init_farming = 0)    [ set init_herding                         20
                                                  set init_farming                         20 ]

  ;;; initial parameter check (avoiding division per zero error
  check-par-is-positive "end-simulation-in-tick" end-simulation-in-tick
  check-par-is-positive "initial_number_of_groups" initial_number_of_groups

end

to check-par-is-positive [ parName parValue ]

  if (parValue <= 0)
  [
    print (word "ERROR: " parName " must be greater than zero")
    stop
  ]

end

to parameters-check-2

  ;;; check number of initial centers and land use units
  if (initH + initF >= count patches) [print "ERROR: initial number of land use units (initF + initH) is too high compare to the number of patches" stop ]
  if (initGroups >= count patches) [print "ERROR: initial number of groups (initGroups) is too high compare to the number of patches" stop ]

  if (initH + initF < 3 * initGroups)
  [print "Warning: initial number of groups (initGroups) is too high compare to the initial number of land use units (initF + initH). Some groups may be initialised empty" ]

end

to initialize-patches-and-groups

  ;;; This procedure initializes patch and group variables

  ask patches
  [
      if (landUse != "N") [ set myGroup (one-of groups) ]
  ]

  ask groups
  [
    set hidden? true
    move-to one-of patches with [any? groups-here = false and (pxcor > 2) and (pycor > 2) and (pxcor < max-pxcor - 2) and (pycor < max-pycor - 2)]
    set targetFarmingRatio random-float 1
    ;set targetFarmingRatio FarmingRatio ;;; alternative initialization of "targetFarmingRatio"
    update-group
  ]

  ask patches
  [
    update-landUnits
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CYCLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  ;;; This procedure is the cycle of the model (what happens during one "tick").

  reset-counters

  growth

  landUse-expansion

  check-competitions

  change-groups

  if (group_management = true) [ group-management ]

  update-visualization

  tick
  if (display_details = true) [ ask labelpositions with [ name = "time" ] [ set label (word "time: " ticks) ] ]
  if ticks > end-simulation-in-tick [stop]

end

to reset-counters

  ;;; This procedure reset all counters which are used either during the cycle or summarized at the "update-visualization" procedure.

  set farmingGrowth 0
  set farmingDeterrence 0
  set herdingGrowth 0
  set herdingDeterrence 0
  set FFcompetitions 0
  set HHcompetitions 0
  set HFcompetitions 0
  set FHcompetitions 0
  set landUseChangeEvents 0
  set managementEvents 0

  ask groups [ set groupDemandF 0 set groupDemandH 0 ]

end

to growth

  ;;; This procedure calculates the groups demands for each land use class, based on both the intrinsic and extrinsic growth rates of each of them.
  ;;; Note that growth rates are dependent on parameters, but also on the context, and may vary from one "tick" to another.

  ask groups [ set groupDemandF 0 set groupDemandH 0 ]
  ;;; FARMING
  ;;; Intrinsic Demand
  ask groups with [groupSize > 0]
  [
    let myLand count patches with [landUse = "F" and myGroup = myself]
    repeat myLand
    [
      if ( random-float 1 <= intGrowthF )
      [
        set groupDemandF groupDemandF + 1
      ]
    ]
  ]
  ;;; Extrinsic Demand
  let extF (round (maxExtGrowth * ( totalPatches - countLandUseF ) ) )
  repeat extF
  [
    ask one-of groups
    [
      set groupDemandF groupDemandF + 1
    ]
  ]
  ;;; HERDING
  ;;; Intrinsic Growth
  ask groups with [groupSize > 0]
  [
    let myLand count patches with [landUse = "H" and myGroup = myself]
    repeat myLand
    [
      if ( random-float 1 <= intGrowthH )
      [
        set groupDemandH groupDemandH + 1
      ]
    ]
  ]
  ;;; Extrinsic Growth
  let extH (round (maxExtGrowth * ( totalPatches - countLandUseH ) ) )
  repeat extH
  [
    ask one-of groups
    [
      set groupDemandH groupDemandH + 1
    ]
  ]

end

to landUse-expansion

  ;;; This procedure calls for the expansion procedures of farming and herding, intentionally in this order.

  farming-expansion
  herding-expansion

end

to farming-expansion

  ;;; In this procedure, groups attempt to assign patches to their new farming units.
  ;;; If there is no patch freely available, groups will randomly choose a patch, and
  ;;; if this patch belongs to another group (density-dependent growth), the group will generate a competitive situation and be accounted within "contendersF".
  ;;; In the case that the patch is used for "herding" and "group_pasture_tenure = false", the group will automatically occupy it and change its land use to farming.

  let growingGroupsF groups with [ groupDemandF > 0 ]
  ask growingGroupsF [ set groupDemandRemain groupDemandF ]

  repeat sum [groupDemandRemain] of growingGroupsF
  [
    ask one-of growingGroupsF with [groupDemandRemain > 0]
    [
      let me self
      ifelse (any? patches with [myGroup = nobody])
      [
        ;;; if the land is not saturated
        ask one-of patches with [myGroup = nobody]
        [
          if (landUse = "N") [ set landUseChangeEvents landUseChangeEvents + 1 ]
          set myGroup me set landUse "F"
        ]
      ]
      [
        ;;; if the territory is saturated
        ;;; Choose a random patch
        ask one-of patches
        [
          ;;; if the patch is used by another group
          if (myGroup != me)
          [
            ifelse (landUse = "F") [
              if ( allow_within-class_competition = true )
              [
                ;;; If the patch is used for farming, F-F competition will be called later
                set contendersF (turtle-set contendersF me)
                set FFcompetitions (FFcompetitions + 1)
              ]
            ]
            [
              ifelse (group_pasture_tenure = true)
              [
                ;;; F-H competition will be called later
                set contendersF (turtle-set contendersF me)
                set FHcompetitions (FHcompetitions + 1)
              ]
              [
                ;;; farming will start using a former pasture
                set myGroup myself
                set landUSe "F"
                set landUseChangeEvents landUseChangeEvents + 1
                set farmingGrowth farmingGrowth + 1
                set herdingDeterrence herdingDeterrence + 1
              ]
            ]
          ]
        ]
      ]
      set groupDemandRemain groupDemandRemain - 1
    ]
  ]

end

to herding-expansion

  ;;; In this procedure, groups attempt to assign patches to all their herding units (if "group_pasture_tenure = false") or to their new herding units (group_pasture_tenure = true).
  ;;; If there is no patch freely available, groups will randomly choose a patch, and
  ;;; if this patch belongs to another group (density-dependent growth), the group will generate a competitive situation and be accounted within "contendersH"

  let groupsH nobody
  let herds 0
  ifelse (group_pasture_tenure = true)
  [
    set groupsH groups with [ groupDemandH > 0 ]
    ask groupsH [ set groupDemandRemain groupDemandH ]
    set herds sum [groupDemandRemain] of groupsH
  ]
  [
    set groupsH groups with [ farmingRatio < 1 ]
    ask groupsH
    [
      let me self
      set groupDemandRemain groupSizeH + groupDemandH
    ]
    set herds sum [ groupDemandRemain ] of groups

    ;;; reset herding positions (herds go back not necessarily to the same patch)
    ask patches with [ landUse = "H"] [ set myGroup nobody ]
  ]

  repeat herds
  [
    ask one-of groupsH with [ groupDemandRemain > 0 ]
    [
      let me self
      ifelse (any? patches with [myGroup = nobody])
      [
        ;;; if the land is not saturated
        ask one-of patches with [myGroup = nobody]
        [
          if (landUse != "H")
          [
            set landUse "H"
            set landUseChangeEvents landUseChangeEvents + 1
            set herdingGrowth herdingGrowth + 1
          ]
          set myGroup me
        ]
      ]
      [
        ;;; if the territory is saturated
        ;;; Choose a random patch
        ask one-of patches
        [
          ;;; Fit-to-maximum exclusion, Density-dependent exclusion
          if (myGroup != me)
          [
            ifelse (landUse = "F")
            [
              ;;; a H-F competition will be called later
              set contendersH (turtle-set contendersH me)
              set HFcompetitions (HFcompetitions + 1)
            ]
            [
              if (allow_within-class_competition = true)
              [
                ;;; a H-H competition will be called later
                set contendersH (turtle-set contendersH me)
                set HHcompetitions (HHcompetitions + 1)
              ]
            ]
          ]
        ]
      ]
      set groupDemandRemain groupDemandRemain - 1
    ]
  ]

  ;;; rangelands not claimed will be considered free land (no land use)
  if (any? patches with [landUse = "H" and myGroup = nobody] ) [ ask patches with [landUse = "H" and myGroup = nobody] [ set landUse "N" ] ]

end

to check-competitions

  ;;; This procedure calls, in a particular sequence, for the resolution of all competitive situations generated by farming and herding expansions.

  ;;; Due to their sedentary condition, farming contenders will act first (F-F and F-H -> H-H and H-F)

  ;;; Farming stakeholders prefer to acquire other groups' farmlands (F-F),
  ;;; rather than investing in new infraestructures (F-H)

  check-FFcompetitions
  check-FHcompetitions

  ;;; Herding stakeholders prefer to acquire other groups' pastures (H-H),
  ;;; rather than converting farmlands by  violence or negotiation (H-F)

  check-HHcompetitions
  check-HFcompetitions

  ask groups [ update-group ]

end

to check-FFcompetitions

  ;;; farming-farming competition

  ask patches with [ landUse = "F" and any? contendersF ]
  [
;    print "F-F"
    ;;; the center assigned is the one that is effectively using the land
    set defender myGroup
    repeat count contendersF
    [
      set contender one-of contendersF
      ;;; remove contender from the respective contenders agent-set
      set contendersF contendersF with [self != contender]
      ;print (word "contendersF after: " contendersF)
      resolve-competition "FF"
    ]
  ]

end

to check-FHcompetitions

  ;;; farming-herding competition

  ask patches with [ landUse = "H" and any? contendersF ]
  [
;    print "F-H"
    set defender myGroup
    repeat count contendersF
    [
      set contender one-of contendersF
      ;;; remove contender from the respective contenders agent-set
      set contendersF contendersF with [self != contender]
;      print (word "contendersH after: " contendersH)
      resolve-competition "FH"
    ]
  ]

end

to check-HHcompetitions

  ;;; herding-herding competition

  ask patches with [ landUse = "H" and any? contendersH ]
  [
;    print "H-H"
    ;;; Since their schedule may vary, a herding center is assigned randomly among the contenders to be the one arriving first (defender)
    set defender myGroup
    repeat count contendersH
    [
      set contender one-of contendersH
      ;;; remove contender from the respective contenders agent-set
      set contendersH contendersH with [self != contender]
;      print (word "contendersH after: " contendersH)
      ;;; check if any of contenders still exists. If so, then resolve competition
      if ([groupSize] of contender > 0)
      [ resolve-competition "HH"]
    ]
  ]

end

to check-HFcompetitions

  ;;; farming-herding competition

  ask patches with [ landUse = "F" and any? contendersH ]
  [
;    print "H-F"
    set defender myGroup
    repeat count contendersH
    [
      set contender one-of contendersH
      ;;; remove contender from the respective contenders agent-set
      set contendersH contendersH with [self != contender]
;      print (word "contendersH after: " contendersH)
      resolve-competition "HF"
    ]
  ]

end

to resolve-competition [ typeOfComp ]

  ;;; This procedure resolves the current competitive situation,
  ;;; and calculate the consequences of contenders success according to "typeOfComp" ("FF"=farming-farming, "FH"=farming-herding, "HH"=herding-herding, "HF"=herding-farming).

  ;;; set competition conditions
  ; define intensities
  let supportDef get-group-influence defender
  let supportCon get-group-influence contender
;  print (word defender " vs " contender ")
;  print (word "supportDef: " supportDef " ; supportCon: " supportCon)

  ;;; the contender is discarted if both defender and contender have zero strength at this patch
  if (supportCon + supportDef > 0)
  [
    ;;; a contender is the one attempting to expand, thus it is the one to make a informed decision
    let ratio_of_intensities  (supportCon /(supportCon + supportDef))

    ;;; Does the competitive situation evolves into land use change event?
    if ( random-float 1 < ratio_of_intensities)
    [
      ;;; extending whichever land use is encouraged
      set myGroup contender

      ;;; update landUse
      if (typeOfComp = "HF")
      [
  ;      print "herding wins"
        set landUse "H"
        ;;; Hence, there is land use change
        set landUseChangeEvents landUseChangeEvents + 1
        set herdingGrowth herdingGrowth + 1
        set farmingDeterrence farmingDeterrence + 1
      ]
      if (typeOfComp = "FH")
      [
  ;      print "farming wins"
        set landUse "F"
        ;;; Hence, there is land use change
        set landUseChangeEvents landUseChangeEvents + 1
        set farmingGrowth farmingGrowth + 1
        set herdingDeterrence herdingDeterrence + 1
      ]
    ]
  ]


end

to change-groups

  ;;; In this procedure, every patch of every group test their particular probability of changing to another group,
  ;;; which may be an existing group or a new one collecting all the defecting patches of a group (fission).
  ;;; The criterium to leave and choose a group is the competitive strength or influence that groups have in the patch at hand (size * effectiveness)

  ask groups
  [
    ;;; each patch of a group will assess their will (maxGroupChangeRate)
    ;;; and their freedom, which is inversely related to the group effectiveness (1 - ([groupEffectiveness] of myGroup) ),
    ;;; to change groups, possibly forming a new group.
    let me self
    let myLand patches with [myGroup = me]
    let defectingPatches (patch-set nobody)
    ask myLand
    [
      if ( random-float 1 < maxGroupChangeRate * (1 - ([groupEffectiveness] of myGroup) ) )
      [
        set defectingPatches (patch-set defectingPatches self)
      ]
    ]
    if (any? defectingPatches)
    [
      ;;; if there are any patches defecting from this group...
      ;;; the viability of the possible new group is calculated for each patch and compared to the most influent group
      let newGroup nobody
      let influenceNewGroup (count defectingPatches) * e ^ ( - (count defectingPatches) / (effectivenessGr * totalPatches) )
      let mostInfluentGroup max-one-of groups [groupSize * groupEffectiveness]
      let influenceOtherGroup get-group-influence mostInfluentGroup
      ifelse (influenceOtherGroup > influenceNewGroup)
      [
        ask defectingPatches [ set myGroup mostInfluentGroup ]
      ]
      [
        if (newGroup = nobody)
        [
          ifelse (any? groups with [groupSize = 0])
          [
            ask one-of groups with [groupSize = 0]
            [
              set targetFarmingRatio ([targetFarmingRatio] of me)
              set newGroup self
            ]
          ]
          [
            hatch-groups 1
            [
            ;;; new groups inherit the traits of the original group *** or modify them given a mutation parameter
            set hidden? true
            move-to one-of patches with [any? groups-here = false and (pxcor > 2) and (pycor > 2) and (pxcor < max-pxcor - 2) and (pycor < max-pycor - 2)]
            ;;; random mutation
            ;set targetFarmingRatio min(list 1 max(list 0 (([targetFarmingRatio] of me) + (0.1 - random-float 0.2)) ) )
            ;;; following the optimal
            ;set targetFarmingRatio ([targetFarmingRatio] of me) + 0.1 * (opt - targetFarmingRatio)
            ;;; following tradition
            set targetFarmingRatio ([targetFarmingRatio] of me)
            set newGroup self
;            print (word " Group fission: " me " (groupSize=" count myLand ") splits into " me " (groupSize=" (count myLand - count defectingPatches) ") and " newGroup " (groupSize=" count defectingPatches ")")
            ]
          ]
        ]
        ask defectingPatches [ set myGroup newGroup ]
      ]
    ]
    update-group
  ]

end

to-report get-group-influence [ theGroup ]

  report [groupSize * groupEffectiveness] of theGroup

end

to group-management

  ;;; In this procedure, groups with more than one member calculate the difference between their "farmingRatio" and their "targetFarmingRatio",
  ;;; and attempt to change the land use of the respective number of patches (note that "floor" is used),
  ;;; with a success proportional to their "groupEffectiveness".

  ask groups
  [
    if (groupSize > 1)
      [
        let dif ((farmingRatio - targetFarmingRatio) * groupSize)
        let num floor (abs dif * groupEffectiveness)
;        print (word self " -> farmingRatio: " precision farmingRatio 4 " ¦¦ targetFarmingRatio: " precision targetFarmingRatio 4 " ¦¦ groupSize: " groupSize " ¦¦ groupEffectiveness: " precision groupEffectiveness 4 " ¦¦ dif: " dif " ¦¦ num: " num)
        if (num > 0)
        [
          ;;; if it is greater than target
          ifelse ( dif > 0 )
          [
            ask n-of num patches with [landUse ="F" and myGroup = myself]
            [
              set landUse "H"
              ;;; Hence, there is land use change
              set landUseChangeEvents landUseChangeEvents + 1
              set herdingGrowth herdingGrowth + 1
              set farmingDeterrence farmingDeterrence + 1
              set managementEvents managementEvents + 1
            ]
          ]
          [
            ;;; if it is smaller than target
            if ( dif < 0 )
            [
              ask n-of num patches with [landUse ="H" and myGroup = myself]
              [
                set landUse "F"
                ;;; Hence, there is land use change
                set landUseChangeEvents landUseChangeEvents + 1
                set farmingGrowth farmingGrowth + 1
                set herdingDeterrence herdingDeterrence + 1
                set managementEvents managementEvents + 1
              ]
            ]
          ]
        ]
      ]
      update-group
  ]

end

to update-group

  ;;; This procedure updates group variables (groupSize, groupEffectiveness, farmingRatio, intGrowthF, intGrowthH).

  set farmingRatio 0
  set groupSize count patches with [myGroup = myself]
  set groupSizeF count patches with [myGroup = myself and landUse = "F"]
  set groupSizeH count patches with [myGroup = myself and landUse = "H"]
  if (groupSize > 0)
  [
    set farmingRatio ( count patches with [landUse = "F" and myGroup = myself] / groupSize )
  ]
  set groupEffectiveness e ^ ( - groupSize / (effectivenessGr * totalPatches) )

  ;;; calculate modified growth out of the group distance from the optimal
  let d 0
  ifelse (farmingRatio < opt)
  [
    set d (farmingRatio / opt)
    set intGrowthF baseIntGrowth * (1 + (optimalGrowthIncrease / 100))
    set intGrowthH baseIntGrowth * (1 + (optimalGrowthIncrease / 100) * d)
  ]
  [
    ifelse (opt = 1)
    [ set d 1 ]
    [ set d 1 - ((farmingRatio - opt) / (1 - opt)) ]
    set intGrowthH baseIntGrowth * (1 + (optimalGrowthIncrease / 100))
    set intGrowthF baseIntGrowth * (1 + (optimalGrowthIncrease / 100) * d)
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VISUALIZATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-landUnits

  ;;; This procedure updates the patches' "withinIntegration" and "betweenIntegration".

  ask patches with [myGroup != nobody]
  [
    ifelse (landUse = "F")
    [
      set withinIntegration [farmingRatio] of myGroup
      set betweenIntegration 1 - [farmingRatio] of myGroup
    ]
    [
      set withinIntegration 1 - [farmingRatio] of myGroup
      set betweenIntegration [farmingRatio] of myGroup
    ]
  ]

end

to update-visualization

  ;;; this procedure updates the display and all global output variables.

  if (display_mode = "land use proportion") [ update-patches if (display_details = true) [ ask labelpositions [set label-color black] ] ]
  if (display_mode = "groups") [ update-network if (display_details = true) [ ask labelpositions [set label-color white] ] ]

  set numberGroups count groups with [groupSize > 0]
  set countLandUseF count patches with [ landUse = "F" ]
  set countLandUseH count patches with [ landUse = "H" ]

  set farmingBalance (farmingGrowth - farmingDeterrence)
  set herdingBalance (herdingGrowth - herdingDeterrence)

  set meanGroupSize mean [[groupSize] of myGroup] of patches with [ landUse != "N" ]
  set bigGroupSize [groupSize] of max-one-of groups [groupSize]
  set meanGroupEffectiveness mean [[groupEffectiveness] of myGroup] of patches with [ landUse != "N" ]
  set bigGroupEffectiveness [groupEffectiveness] of max-one-of groups [groupSize]

  set meanTargetFarmingRatio mean [[targetFarmingRatio] of myGroup] of patches with [ landUse != "N" ]
  set bigTargetFarmingRatio [targetFarmingRatio] of max-one-of groups [groupSize]

  update-landUnits

  ifelse (any? patches with [landUse = "F"]) [ set meanFarmingIntegration mean [withinIntegration] of patches with [landUse = "F"] ] [ set meanFarmingIntegration -0.01 ]
  ifelse (any? patches with [landUse = "H"]) [ set meanHerdingIntegration mean [withinIntegration] of patches with [landUse = "H"] ] [ set meanHerdingIntegration -0.01 ]

  set meanMixedIntegration mean [betweenIntegration] of patches with [ landUse != "N" ]

  ifelse (display_details = true)
  [
    ask labelpositions with [ name = "farming" ] [ set label (word "farming(%): " (precision (100 * countLandUseF / totalPatches) 2) ) ]
    ask labelpositions with [ name = "bigGroupSize" ] [ set label (word "biGroupSize(%): " (precision (100 * bigGroupSize / totalPatches) 2) ) ]
  ]
  [
    ask labelpositions with [ name = "time" ] [ set label "" ]
    ask labelpositions with [ name = "farming" ] [ set label "" ]
    ask labelpositions with [ name = "bigGroupSize" ] [ set label "" ]
  ]

end

to update-patches

  ;;; this procedure updates the "land use proportion" display mode.

  ask pointers [die]
  ask groups [set hidden? true]
  ask patches
  [
    set pcolor brown
    if (landUse = "F")
    [ set pcolor green ]
    if (landUse = "H")
    [ set pcolor yellow ]
  ]

end

to update-network

  ;;; this procedure updates the "groups" display mode.

  ask pointers [die]
  ask groups
  [
    ifelse (groupSize > 0) [set hidden? false set color red set shape "circle" set size 0.5] [ set hidden? true ]
    create-links-with other groups [ set color black]
  ]

  layout-spring groups links 0.18 9 1.2
  ask links [die]
  ask patches
  [
    set pcolor black
    let thisPatch self
    if (landUse != "N" )
    [
      sprout-pointers 1 [
        set shape "circle" set size 0.2
        ifelse ([landUse] of patch-here = "F") [ set color green ] [ set color yellow ]
        create-link-with myGroup [ set color grey]
        move-to myGroup
      ]
    ]
  ]
  ask groups
  [
    let num groupSize
    repeat groupSize [
      ask one-of link-neighbors [
        rt 360 * who
        fd 0.1 * num * e ^ (- num / 60)
      ]
      set num num - 1
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Parametrization from file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to load-experiment

  ;;; this procedure loads the values of each (explored) parameter from a csv file.
  ;;; Note that the setup will use the value set by the user for any other parameter (e.g. scenario).

  let FilePath "SensAnalysis//exp//" ;;; create folders in the model's directory before trying to load experiments
  let filename (word FilePath "exp_" expNumber ".csv") ;;; the parameter setting of experiments must be saved as ".csv" files named "exp_<NUMBER>.csv"
  file-open filename
  while [not file-at-end?]
    [
      ;;; the values of the file must follow this same order

      set initH round ((file-read / 100) * totalPatches)
      set initF round ((file-read / 100) * totalPatches)
      set baseIntGrowth file-read
      set maxExtGrowth file-read
      set initGroups file-read
      set effectivenessGr file-read
      set maxGroupChangeRate file-read
      set opt file-read
      set optimalGrowthIncrease file-read

      set end-simulation-in-tick file-read ;- 1500 ;; use this to cut down the time of simulation (e.g. if the file reads 2000)
    ]
  file-close

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; movie generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to generate-animation

  ;;; this procedure generates a video sequencing the displays of a simulation (using the current parameter configuration).

  setup
  vid:start-recorder
  repeat end-simulation-in-tick [ go vid:record-view ]
  vid:save-recording  (word "run_" behaviorspace-run-number ".mov") ; you can add more information in the name of the file (here, only scenario is used)
  vid:reset-recorder

end
@#$#@#$#@
GRAPHICS-WINDOW
468
78
851
387
-1
-1
15.0
1
14
1
1
1
0
0
0
1
0
24
0
19
1
1
1
ticks
30.0

BUTTON
550
37
613
70
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
624
37
687
70
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
697
38
760
71
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1129
29
1311
62
base_intrinsic_growth_rate
base_intrinsic_growth_rate
0
0.1
0.01
0.01
1
NIL
HORIZONTAL

PLOT
3
164
459
312
land use
ticks
patches
0.0
10.0
0.0
10.0
true
true
"set-plot-y-range -0.01 (count patches + 0.01)" ""
PENS
"herding" 1.0 0 -1184463 true "plot count patches with [landUSe = \"H\"]" "plot count patches with [landUSe = \"H\"]"
"farming" 1.0 0 -13840069 true "plot count patches with [landUSe = \"F\"]" "plot count patches with [landUSe = \"F\"]"
"optimum" 1.0 0 -4539718 true "plot optimum * (count patches)" "plot optimum * (count patches)"
"bigGroupTarget" 1.0 0 -2674135 true "plot bigTargetFarmingRatio * (count patches)" "plot bigTargetFarmingRatio * (count patches)"

PLOT
1136
190
1356
333
herding dependence on farming
NIL
frequency
0.0
200.0
0.0
200.0
false
false
"set-plot-x-range -0.01 1.01" "set-plot-y-range -0.01 (count patches with [landUSe = \"H\"])"
PENS
"dependence" 1.0 1 -16777216 true "histogram [betweenIntegration] of patches with [landUSe = \"H\"]\nset-histogram-num-bars 10" "histogram [betweenIntegration] of patches with [landUSe = \"H\"]"

PLOT
3
310
460
485
events
ticks
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"land use change events" 1.0 0 -5825686 true "plot landUseChangeEvents" "plot landUseChangeEvents"
"managementEvents" 1.0 0 -7500403 true "" "plot managementEvents"
"FF" 1.0 0 -2064490 true "" "plot FFcompetitions"
"HH" 1.0 0 -8630108 true "" "plot HHcompetitions"
"FH" 1.0 0 -13345367 true "" "plot FHcompetitions"
"HF" 1.0 0 -2674135 true "" "plot HFcompetitions"

SLIDER
1129
64
1311
97
max_extrinsic_growth_rate
max_extrinsic_growth_rate
0
1
0.004
0.001
1
NIL
HORIZONTAL

PLOT
1134
333
1357
478
farming dependence on herding
NIL
frequency
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range -0.01 1.01" "set-plot-y-range -0.01 (count patches with [landUSe = \"F\"])"
PENS
"dependence" 1.0 1 -16777216 true "histogram [betweenIntegration] of patches with [landUSe = \"F\"]\nset-histogram-num-bars 10" "histogram [betweenIntegration] of patches with [landUSe = \"F\"]"

PLOT
856
189
1137
333
herding in groups
NIL
frequency
0.0
10.0
0.0
10.0
false
false
"set-plot-x-range -0.01 (max [who] of groups + 0.01)\nset-histogram-num-bars 1 + max [who] of groups" "set-plot-x-range -0.01 (max [who] of groups + 0.01)\nset-histogram-num-bars 1 + max [who] of groups\nset-plot-y-range -0.01 (max [groupSize] of groups)"
PENS
"default" 1.0 1 -2674135 true "histogram [[who] of myGroup] of patches with [landUse = \"H\"]" "histogram [[who] of myGroup] of patches with [landUse = \"H\"]"

PLOT
856
333
1136
477
farming in groups
NIL
frequency
0.0
10.0
0.0
10.0
false
false
"set-plot-x-range -0.01 (initGroups + 0.01)\nset-histogram-num-bars 1 + max [who] of groups" "set-plot-x-range -0.01 (max [who] of groups + 0.01)\nset-histogram-num-bars 1 + max [who] of groups\nset-plot-y-range -0.01 (max [groupSize] of groups)"
PENS
"default" 1.0 1 -955883 true "histogram [[who] of myGroup] of patches with [landUse = \"F\"]" "histogram [[who] of myGroup] of patches with [landUse = \"F\"]"

PLOT
3
489
361
609
growth
ticks
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"farming agents" 1.0 0 -13791810 true "" "plot farmingBalance"
"herding agents" 1.0 0 -10873583 true "" "plot herdingBalance"
"0" 1.0 0 -16777216 true "" "plot 0"

MONITOR
70
758
153
795
mean intGrowthF
mean [intGrowthF] of groups
4
1
9

MONITOR
1311
64
1394
101
NIL
maxExtGrowth
4
1
9

MONITOR
402
247
459
284
patches
count patches
0
1
9

MONITOR
361
497
446
534
NIL
farmingGrowth
2
1
9

MONITOR
361
534
445
571
NIL
farmingDeterrence
2
1
9

MONITOR
445
497
530
534
NIL
herdingGrowth
2
1
9

MONITOR
445
534
530
571
NIL
herdingDeterrence
2
1
9

MONITOR
354
247
404
284
farming
count patches with [landUSe = \"F\"]
0
1
9

MONITOR
411
458
461
495
HF
HFcompetitions
4
1
9

MONITOR
359
357
467
394
land use change events
landUseChangeEvents
4
1
9

MONITOR
361
570
446
607
NIL
farmingBalance
2
1
9

MONITOR
445
571
530
608
NIL
herdingBalance
2
1
9

PLOT
1356
190
1516
333
herding integration
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range -0.01 1.01" "set-plot-y-range -0.01 (count patches with [landUSe = \"H\"])"
PENS
"default" 1.0 1 -13345367 false "histogram [withinIntegration] of patches with [landUSe = \"H\"]\nset-histogram-num-bars 10" "histogram [withinIntegration] of patches with [landUSe = \"H\"]"

PLOT
1357
332
1517
478
farming integration
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range -0.01 1.01" "set-plot-y-range -0.01 (count patches with [landUSe = \"F\"])"
PENS
"default" 1.0 1 -13345367 false "histogram [withinIntegration] of patches with [landUSe = \"F\"]\nset-histogram-num-bars 10" "histogram [withinIntegration] of patches with [landUSe = \"F\"]"

PLOT
404
608
686
756
group size
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range 0.01 (max [groupSize] of groups + 1)\nset-histogram-num-bars 20" "set-plot-x-range 0.01 (max [groupSize] of groups + 1)\nset-histogram-num-bars 20"
PENS
"groups" 1.0 1 -16777216 true "histogram [groupSize] of groups" "histogram [groupSize] of groups"

PLOT
1136
478
1517
598
Connectivity
ticks
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-y-range -0.01 1.01" ""
PENS
"f-f" 1.0 0 -13840069 true "plot meanFarmingIntegration" "plot meanFarmingIntegration"
"f-h" 1.0 0 -955883 true "plot meanMixedIntegration" "plot meanMixedIntegration"
"h-h" 1.0 0 -1184463 true "plot meanHerdingIntegration" "plot meanHerdingIntegration"

INPUTBOX
856
101
988
161
initial_number_of_groups
25.0
1
0
Number

PLOT
530
486
856
606
Number of groups
ticks
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-y-range -0.01 (initGroups + 0.01)" ""
PENS
"default" 1.0 0 -16777216 true "plot numberGroups" "plot numberGroups"

SLIDER
1410
104
1582
137
optimal_growth_increase
optimal_growth_increase
0
100
8.0
1
1
NIL
HORIZONTAL

SLIDER
1410
71
1582
104
optimum
optimum
0
1
0.45
0.01
1
NIL
HORIZONTAL

SLIDER
1121
149
1306
182
max_group_change_rate
max_group_change_rate
0
1
0.1
0.001
1
NIL
HORIZONTAL

SLIDER
1121
116
1306
149
effectiveness_gradient
effectiveness_gradient
0
10
1.235
0.001
1
NIL
HORIZONTAL

SWITCH
-2
30
190
63
allow_within-class_competition
allow_within-class_competition
0
1
-1000

MONITOR
361
422
411
459
FF
FFcompetitions
0
1
9

MONITOR
411
422
461
459
HH
HHcompetitions
0
1
9

MONITOR
361
458
411
495
FH
FHcompetitions
0
1
9

PLOT
687
608
954
755
group effectiveness
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range -0.01 1.01\nset-histogram-num-bars 20" "set-plot-x-range -0.01 1.01\nset-histogram-num-bars 20"
PENS
"default" 1.0 1 -16777216 true "histogram [[groupEffectiveness] of myGroup] of patches with [landUse != \"N\"]" "histogram [[groupEffectiveness] of myGroup] of patches with [landUse != \"N\"]"

MONITOR
1306
112
1379
149
NIL
effectivenessGr
0
1
9

MONITOR
1306
149
1407
186
NIL
maxGroupChangeRate
4
1
9

MONITOR
786
486
855
523
NIL
numberGroups
0
1
9

MONITOR
1582
104
1688
141
NIL
optimalGrowthIncrease
2
1
9

MONITOR
1582
67
1639
104
optimum
opt
4
1
9

PLOT
5
606
205
756
farming intrinsic growth rate
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range -0.01 (0.01 + baseIntGrowth * (1 + optimalGrowthIncrease / 100))" "set-plot-y-range -0.01 (count patches with [landUSe = \"F\"])"
PENS
"default" 1.0 1 -16777216 true "histogram [[intGrowthF] of myGroup] of patches with [landUSe = \"F\"]\nset-histogram-num-bars 10" "histogram [[intGrowthF] of myGroup] of patches with [landUSe = \"F\"]"

PLOT
205
606
405
756
herding intrinsic growth rate
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range -0.01 (0.01 + baseIntGrowth * (1 + optimalGrowthIncrease / 100))" "set-plot-y-range -0.01 (count patches with [landUSe = \"H\"])"
PENS
"default" 1.0 1 -16777216 true "histogram [[intGrowthH] of myGroup] of patches with [landUSe = \"H\"]\nset-histogram-num-bars 10" "histogram [[intGrowthH] of myGroup] of patches with [landUSe = \"H\"]"

MONITOR
269
758
353
795
mean intGrowthH
mean [intGrowthH] of groups
4
1
9

PLOT
856
476
1137
596
group farming ratio target
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range -0.01 1.01\nset-histogram-num-bars 20" "set-plot-x-range -0.01 1.01\nset-histogram-num-bars 20"
PENS
"farmingRatio" 1.0 1 -16777216 true "histogram [[farmingRatio] of myGroup] of patches with [landUse != \"N\"]" "histogram [[farmingRatio] of myGroup] of patches with [landUse != \"N\"]"
"targetFarmingRatio" 1.0 0 -7500403 true "histogram [[targetFarmingRatio] of myGroup] of patches with [landUse != \"N\"]" "histogram [[targetFarmingRatio] of myGroup] of patches with [landUse != \"N\"]"

CHOOSER
683
435
839
480
display_mode
display_mode
"land use proportion" "groups"
1

INPUTBOX
189
14
258
74
expNumber
0.0
1
0
Number

CHOOSER
318
120
462
165
TypeOfexperiment
TypeOfexperiment
"defined by GUI" "random" "defined by expNumber"
0

INPUTBOX
259
14
376
74
end-simulation-in-tick
2000.0
1
0
Number

CHOOSER
-3
119
317
164
Scenario
Scenario
"Ao - open access, simple group dynamics" "Bo - open access, pairing" "Co - open access, management" "Do - open access, pairing and management" "Ar - restricted access, simple group dynamics" "Br - restricted access, pairing" "Cr - restricted access, management" "Dr - restricted access, pairing and management"
2

MONITOR
20
75
140
120
NIL
group_pasture_tenure
0
1
11

MONITOR
140
75
247
120
NIL
group_management
0
1
11

MONITOR
1310
26
1395
63
NIL
baseIntGrowth
4
1
9

MONITOR
247
75
304
120
NIL
pairing
0
1
11

BUTTON
480
442
552
475
refresh
update-visualization
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SWITCH
555
442
675
475
display_details
display_details
0
1
-1000

MONITOR
365
389
455
426
NIL
managementEvents
0
1
9

INPUTBOX
374
13
446
73
seed
1.336919135E9
1
0
Number

TEXTBOX
11
6
189
40
Experiment configuration
14
0.0
1

TEXTBOX
628
10
778
28
Controls
14
0.0
1

TEXTBOX
622
415
719
433
Display settings
14
0.0
1

TEXTBOX
1198
10
1280
28
Growth rates
14
0.0
1

MONITOR
1034
27
1113
64
initF (% patches)
initF / totalPatches
2
1
9

MONITOR
1034
64
1113
101
initH (% patches)
initH / totalPatches
2
1
9

MONITOR
988
113
1052
150
NIL
initGroups
0
1
9

SLIDER
856
31
1034
64
init_farming
init_farming
0
100
20.0
1
1
% patches
HORIZONTAL

SLIDER
856
63
1034
96
init_herding
init_herding
0
100
20.0
1
1
% patches
HORIZONTAL

TEXTBOX
935
10
1037
28
Initial conditions
14
0.0
1

TEXTBOX
1192
97
1342
115
Group dynamics
14
0.0
1

TEXTBOX
1471
51
1579
69
Pairing conditions
14
0.0
1

MONITOR
460
756
548
793
NIL
meanGroupSize
2
1
9

MONITOR
548
756
623
793
NIL
bigGroupSize
0
1
9

MONITOR
716
755
828
792
NIL
meanGroupEffectiveness
2
1
9

MONITOR
828
755
927
792
NIL
bigGroupEffectiveness
2
1
9

@#$#@#$#@
## WHAT IS IT?

The Nice Musical Chairs (NMC) model represent the competition for space between groups of stakeholders of farming and herding activities in the arid Afro-Eurasia. This is a follow-up of a simpler model, the Musical Chairs model, which depict the same general idea, disregarding any explicit representation of groups and the different rules of behaviour presented here (i.e. scenarios).

## HOW IT WORKS

The NMC model links land use and their stakeholders (patches) and the groups around which they aggregate (agents or "turtles"). It represents a context where farming (sedentary land use) and herding (seasonal mobile) land use compete seasonally for a limited space, but also groups compete for territorial influence. Although patches DO represent spatial units, spatial relationships (neighborhood, distance) are not relevant in this model.

The cycle begins with the calculation of the demand for land for both farming and herding in each group (`growth` procedure). The demand may be generated by intrinsic (_density dependent_) or extrinsic (_density independent_) factors. If `pairing` is activated, the overall demand of a group will increase (up to a maximum defined by `optimal_growth_increase`) the closer the internal farming ratio is to the optimum (`optimum_farming_ratio`).

Aiming to satisfy such demand, groups will attempt to expand, first over the land not used, and them over the land used by other groups (expansion procedures). Bigger groups will be here penalised by the lack of opportunities to expand. Whenever a group presses against the land use of another group, the patch at hand will register the former as a "contender" (saved in a list to be filled with other possible contenders).  If access to pastures is open, farming stakeholders may settle automatically and change the land use; if there is a restricted access regime, the group will be registered as a contender.

The procedure `check-competitions` iterate over all four modalities of competitive situations (farming pressing farming, farming pressing herding, herding pressing herding and herding pressing farming; particularly in this order), over all patches, and resolve them (`resolve-competition`). The resolution is a stochastic process comparing the contender and the defender strengths, which here depend on `groupSize` (number of patches belonging to the group) and the `groupEffectiveness` (a function of `effectiveness_gradient` and `groupSize`).

After all competitive situations are resolved, individual stakeholders may have the opportunity to translate themselves and their land to another (stronger) group (`group-change`). The frequency of this opportunity within a group will vary up to `max_group_change_rate`, depending on its `groupEffectiveness`. Among the group options considered by each stakeholder is the possible group that can be formed with all defective members of the same group (i.e. group fission).

Last, whenever `management` is enabled, groups can modify the land use of their members, adjusting their `farmingRatio` to their `targetFarmingRatio` (which is fixed to a random number upon the creation of the group). This process depends also on the `groupEffectiveness`.

## HOW TO USE IT

First, you should select the desired type of experiment (`typeOfExperiment`):

* "defined by GUI": all values introduced by the user in sliders, boxes and choosers (except `expNumber`) will be applied.

* "random": randomly selects values for all the parameters (except `expNumber`, `endSimulation`, and `allow_within-class_competition`), choosing also randomly among the eight possible scenarios.

* "defined by expNumber": the setup procedure will call for the `load-experiment` procedure, which set the parameters using the values of a "exp_<expNumber>.csv" file, if available. The parameters `allow_within-class_competition` and `scenario` must be manually set by the user.

GUI elements:

* `allow_within-class_competition`: if set to false, farming-farming and herding-herding competitive situations will not be allowed to occur.

* `scenario`: allows to choose among the eight possible scenarios (combinations of open/restricted access to pastures, management and pairing).

* `endSimulation`: the number of cycles to simulate.

* `seed`: set the seed for the random number generator. The same integer number will always generate the same simulation, given the same parameter configuration.

* `display_mode`: choose between visualizing (a) the proportion between farming and herding patches (green=farming, yellow=herding), and (b) the distribution of land units (colored by land use) among groups (red circles).Click `refresh` to apply changes when the simulation is paused.

* `display_details`: if enabled, tick number and the two main state variables will be displayed in the view canvas.

* `init_farming`: the initial percentage of patches used for farming.

* `init_herding`: the initial percentage of patches used for herding.

* `initial_number_of_groups`: the initial number of groups.

* `base_intrinsic_growth_rate`: the base value of the intrinsic growth for land use per patch, for both land use classes (0.05 = 5%).

* `max_extrinsic_growth_rate`: the maximum value of the extrinsic growth for land use, for both land use classes (0.1 = 10%).

* `effectiveness_gradient`: the number of patches in a group with the maximum competitive strength possible. Greater penalization on group size is denoted by smaller values.

* `max_group_change_rate`: the maximum rate in which patches can change groups.

* `optimum`: the percentage of farming within a group that allows patches to generate the maximum demand for land use.

* `optimal_growth_increase`: the maximum increase of growth for land use per patch, in terms of percentage of the base intrinsic growth due to the benefits of land use pairing.


## THINGS TO NOTICE

The NMC model allows exploring how land use competition may be constrained by social structure and dynamics, according to which individual stakeholders compete and cooperate depending on adscription to social groups.

The NMC model also enables to test the effects of two particular modalities of cooperative mechanisms: land use pairing, the awarding, in terms of productivity, of any direct collaboration between farming and herding within a group; and group management, the prerogative of a group leadership to manage individual stakeholders in order to pursue a particular proportion between farming and herding.

Lastly, the model allows assessing the effects of these mechanisms under either open or restricted access to pasture regimes.

## THINGS TO TRY

* increase the `max_group_change_rate` and decrease `effectiveness_gradient`: there will be a lot of group diversity and constant change of their influence. However, does the land use pattern change significantly?

* try out the scenario "Ar" with several combinations of initial farming and herding (be aware not to sum up more than the number of patches available): Are the end values sensitive to the initial conditions?

* try out several values of `optimal_growth_increase`, fixing the `optimum` at 0.5, throughout all scenarios with pairing: Does this parameter have any effect?

## RELATED MODELS

### Musical Chairs
Angourakis, Andreas (2016, February 3). "Musical Chairs" (Version 1). CoMSES Computational Model Library. Retrieved from: https://www.openabm.org/model/4880/version/1
Available also in NetLogo Model Library (http://ccl.northwestern.edu/netlogo/models/) and Netlogo Modeling Commons (http://modelingcommons.org/).

## CREDITS AND REFERENCES

Angourakis, A., Salpeteur, M., Martínez, V., and Gurt, J.M. (2017). The Nice Musical Chairs model. Exploring the role of competition and cooperation between farming and herding in the formation of land use patterns in arid Afro-Eurasia. Journal of Archaeological Method and Theory, 21: 405-425. http://dx.doi.org/10.1007/s10816-016-9309-8.

Angourakis, A. (2017, January 9). "Nice Musical Chairs" (Version 5). CoMSES Computational Model Library. Retrieved from: https://www.openabm.org/model/4885/version/5.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

invisible
true
0

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment_Ao" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total_patches</metric>
    <metric>seed</metric>
    <metric>initH</metric>
    <metric>initF</metric>
    <metric>baseIntGrowth</metric>
    <metric>maxExtGrowth</metric>
    <metric>initGroups</metric>
    <metric>effectivenessGr</metric>
    <metric>maxGroupChangeRate</metric>
    <metric>opt</metric>
    <metric>optimalGrowthIncrease</metric>
    <metric>countLandUseF</metric>
    <metric>countLandUseH</metric>
    <metric>numberGroups</metric>
    <metric>FFcompetitions</metric>
    <metric>HHcompetitions</metric>
    <metric>HFcompetitions</metric>
    <metric>FHcompetitions</metric>
    <metric>landUseChangeEvents</metric>
    <metric>bigGroupSize</metric>
    <metric>bigTargetFarmingRatio</metric>
    <enumeratedValueSet variable="TypeOfexperiment">
      <value value="&quot;defined by expNumber&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="expNumber" first="1" step="1" last="1000"/>
    <enumeratedValueSet variable="allow_intraclass_competition">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Ao - open access, simple group dynamics&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Bo" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total_patches</metric>
    <metric>seed</metric>
    <metric>initH</metric>
    <metric>initF</metric>
    <metric>baseIntGrowth</metric>
    <metric>maxExtGrowth</metric>
    <metric>initGroups</metric>
    <metric>effectivenessGr</metric>
    <metric>maxGroupChangeRate</metric>
    <metric>opt</metric>
    <metric>optimalGrowthIncrease</metric>
    <metric>countLandUseF</metric>
    <metric>countLandUseH</metric>
    <metric>numberGroups</metric>
    <metric>FFcompetitions</metric>
    <metric>HHcompetitions</metric>
    <metric>HFcompetitions</metric>
    <metric>FHcompetitions</metric>
    <metric>landUseChangeEvents</metric>
    <metric>bigGroupSize</metric>
    <metric>bigTargetFarmingRatio</metric>
    <enumeratedValueSet variable="TypeOfexperiment">
      <value value="&quot;defined by expNumber&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="expNumber" first="1" step="1" last="1000"/>
    <enumeratedValueSet variable="allow_intraclass_competition">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Bo - open access, pairing&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Co" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total_patches</metric>
    <metric>seed</metric>
    <metric>initH</metric>
    <metric>initF</metric>
    <metric>baseIntGrowth</metric>
    <metric>maxExtGrowth</metric>
    <metric>initGroups</metric>
    <metric>effectivenessGr</metric>
    <metric>maxGroupChangeRate</metric>
    <metric>opt</metric>
    <metric>optimalGrowthIncrease</metric>
    <metric>countLandUseF</metric>
    <metric>countLandUseH</metric>
    <metric>numberGroups</metric>
    <metric>FFcompetitions</metric>
    <metric>HHcompetitions</metric>
    <metric>HFcompetitions</metric>
    <metric>FHcompetitions</metric>
    <metric>landUseChangeEvents</metric>
    <metric>bigGroupSize</metric>
    <metric>bigTargetFarmingRatio</metric>
    <enumeratedValueSet variable="TypeOfexperiment">
      <value value="&quot;defined by expNumber&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="expNumber" first="1" step="1" last="1000"/>
    <enumeratedValueSet variable="allow_intraclass_competition">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Co - open access, management&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Do" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total_patches</metric>
    <metric>seed</metric>
    <metric>initH</metric>
    <metric>initF</metric>
    <metric>baseIntGrowth</metric>
    <metric>maxExtGrowth</metric>
    <metric>initGroups</metric>
    <metric>effectivenessGr</metric>
    <metric>maxGroupChangeRate</metric>
    <metric>opt</metric>
    <metric>optimalGrowthIncrease</metric>
    <metric>countLandUseF</metric>
    <metric>countLandUseH</metric>
    <metric>numberGroups</metric>
    <metric>FFcompetitions</metric>
    <metric>HHcompetitions</metric>
    <metric>HFcompetitions</metric>
    <metric>FHcompetitions</metric>
    <metric>landUseChangeEvents</metric>
    <metric>bigGroupSize</metric>
    <metric>bigTargetFarmingRatio</metric>
    <enumeratedValueSet variable="TypeOfexperiment">
      <value value="&quot;defined by expNumber&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="expNumber" first="1" step="1" last="1000"/>
    <enumeratedValueSet variable="allow_intraclass_competition">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Do - open access, pairing and management&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Ar" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total_patches</metric>
    <metric>seed</metric>
    <metric>initH</metric>
    <metric>initF</metric>
    <metric>baseIntGrowth</metric>
    <metric>maxExtGrowth</metric>
    <metric>initGroups</metric>
    <metric>effectivenessGr</metric>
    <metric>maxGroupChangeRate</metric>
    <metric>opt</metric>
    <metric>optimalGrowthIncrease</metric>
    <metric>countLandUseF</metric>
    <metric>countLandUseH</metric>
    <metric>numberGroups</metric>
    <metric>FFcompetitions</metric>
    <metric>HHcompetitions</metric>
    <metric>HFcompetitions</metric>
    <metric>FHcompetitions</metric>
    <metric>landUseChangeEvents</metric>
    <metric>bigGroupSize</metric>
    <metric>bigTargetFarmingRatio</metric>
    <enumeratedValueSet variable="TypeOfexperiment">
      <value value="&quot;defined by expNumber&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="expNumber" first="1" step="1" last="1000"/>
    <enumeratedValueSet variable="allow_intraclass_competition">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Ar - restricted access, simple group dynamics&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Br" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total_patches</metric>
    <metric>seed</metric>
    <metric>initH</metric>
    <metric>initF</metric>
    <metric>baseIntGrowth</metric>
    <metric>maxExtGrowth</metric>
    <metric>initGroups</metric>
    <metric>effectivenessGr</metric>
    <metric>maxGroupChangeRate</metric>
    <metric>opt</metric>
    <metric>optimalGrowthIncrease</metric>
    <metric>countLandUseF</metric>
    <metric>countLandUseH</metric>
    <metric>numberGroups</metric>
    <metric>FFcompetitions</metric>
    <metric>HHcompetitions</metric>
    <metric>HFcompetitions</metric>
    <metric>FHcompetitions</metric>
    <metric>landUseChangeEvents</metric>
    <metric>bigGroupSize</metric>
    <metric>bigTargetFarmingRatio</metric>
    <enumeratedValueSet variable="TypeOfexperiment">
      <value value="&quot;defined by expNumber&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="expNumber" first="1" step="1" last="1000"/>
    <enumeratedValueSet variable="allow_intraclass_competition">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Br - restricted access, pairing&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Cr" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total_patches</metric>
    <metric>seed</metric>
    <metric>initH</metric>
    <metric>initF</metric>
    <metric>baseIntGrowth</metric>
    <metric>maxExtGrowth</metric>
    <metric>initGroups</metric>
    <metric>effectivenessGr</metric>
    <metric>maxGroupChangeRate</metric>
    <metric>opt</metric>
    <metric>optimalGrowthIncrease</metric>
    <metric>countLandUseF</metric>
    <metric>countLandUseH</metric>
    <metric>numberGroups</metric>
    <metric>FFcompetitions</metric>
    <metric>HHcompetitions</metric>
    <metric>HFcompetitions</metric>
    <metric>FHcompetitions</metric>
    <metric>landUseChangeEvents</metric>
    <metric>bigGroupSize</metric>
    <metric>bigTargetFarmingRatio</metric>
    <enumeratedValueSet variable="TypeOfexperiment">
      <value value="&quot;defined by expNumber&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="expNumber" first="1" step="1" last="1000"/>
    <enumeratedValueSet variable="allow_intraclass_competition">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Cr - restricted access, management&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Dr" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total_patches</metric>
    <metric>seed</metric>
    <metric>initH</metric>
    <metric>initF</metric>
    <metric>baseIntGrowth</metric>
    <metric>maxExtGrowth</metric>
    <metric>initGroups</metric>
    <metric>effectivenessGr</metric>
    <metric>maxGroupChangeRate</metric>
    <metric>opt</metric>
    <metric>optimalGrowthIncrease</metric>
    <metric>countLandUseF</metric>
    <metric>countLandUseH</metric>
    <metric>numberGroups</metric>
    <metric>FFcompetitions</metric>
    <metric>HHcompetitions</metric>
    <metric>HFcompetitions</metric>
    <metric>FHcompetitions</metric>
    <metric>landUseChangeEvents</metric>
    <metric>bigGroupSize</metric>
    <metric>bigTargetFarmingRatio</metric>
    <enumeratedValueSet variable="TypeOfexperiment">
      <value value="&quot;defined by expNumber&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="expNumber" first="1" step="1" last="1000"/>
    <enumeratedValueSet variable="allow_intraclass_competition">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Dr - restricted access, pairing and management&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_All_noIntra" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total_patches</metric>
    <metric>seed</metric>
    <metric>initH</metric>
    <metric>initF</metric>
    <metric>baseIntGrowth</metric>
    <metric>maxExtGrowth</metric>
    <metric>initGroups</metric>
    <metric>effectivenessGr</metric>
    <metric>maxGroupChangeRate</metric>
    <metric>opt</metric>
    <metric>optimalGrowthIncrease</metric>
    <metric>countLandUseF</metric>
    <metric>countLandUseH</metric>
    <metric>numberGroups</metric>
    <metric>FFcompetitions</metric>
    <metric>HHcompetitions</metric>
    <metric>HFcompetitions</metric>
    <metric>FHcompetitions</metric>
    <metric>landUseChangeEvents</metric>
    <metric>bigGroupSize</metric>
    <metric>bigTargetFarmingRatio</metric>
    <enumeratedValueSet variable="TypeOfexperiment">
      <value value="&quot;defined by expNumber&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="expNumber" first="1" step="1" last="1000"/>
    <enumeratedValueSet variable="allow_intraclass_competition">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Ao - open access, simple group dynamics&quot;"/>
      <value value="&quot;Bo - open access, pairing&quot;"/>
      <value value="&quot;Co - open access, management&quot;"/>
      <value value="&quot;Do - open access, pairing and management&quot;"/>
      <value value="&quot;Ar - restricted access, simple group dynamics&quot;"/>
      <value value="&quot;Br - restricted access, pairing&quot;"/>
      <value value="&quot;Cr - restricted access, management&quot;"/>
      <value value="&quot;Dr - restricted access, pairing and management&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
