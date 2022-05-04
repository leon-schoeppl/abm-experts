breed[experts expert] ;(E)
breed[laypeople layperson] ;(L)

;------------------------------------------------------------------------------------------------------
;The properties of the expert are objective values used in Duijf's model.
experts-own[
  competency ;(e) how likely is the expert to be correct about phi?
  interestAlignment ;(alpha) to what degree does the experts interest align with that of the layperson?
  assesment ;what does the expert think about phi
  testimony ;What does the expert suggest the layperson do?
  myLaypersonNumber ;used to reference their layperson
  objectiveTrustworthiness ;should this E be trusted by their L according to Duijf's model?

]

;------------------------------------------------------------------------------------------------------
;While the layperson's competency is an objective value, some of the other ones are used in Hartmann's (subjective) Bayesian model.
laypeople-own[
  competency ;(l) how likely is the expert to be correct about phi?
  psiGivenPhi ;what's the layperson's interest given phi is the case?
  psi ;Boolean normative question that the layperson has an interest in
  myExpertNumber ;Used to reference their expert
  assesment ;What does L conclude about phi?
  trustCondition1 ;proper value and adjustment of HYP
  trustCondition2 ;increase of REL
  didTrust? ;did this L end up trusting their E?

  ;************************************************************************************************************************************************
  ;Subjective values
  priorE ;used to initialize rel
  priorAlpha ;used to initialize rel
  priorCorrectExpertAdvice ;used to initialize rel and beta
  hyp ;node in the Bayes-Net
  rel ;node in the Bayes-Net
  rep ;node in the Bayes-Net
  beta ;used to define the rep node
  charlatan? ;Does L flip their expert's report in case of updating on bad experts?

  ]

globals[
  ;numberOfPairs ;How many experts and laypeople are there?
  phi ;Boolean factual proposition
  modelMatchCounter ;How often did didTrust? match objectiveTrustworthiness?
]

to setup ;called at the start of each simulation
  clear-all
  reset-ticks


  setupWorld
  beginSetupLaypeople
  setupExperts
  finishSetupLaypeople

  printSetup
 end


to go ;called once per tick
  tick

  goExperts
  goLaypeople

  print (word "So far this run, in " modelMatchCounter " out of " (ticks * numberOfPairs) " interactions the objective prediction matched the subjective decision.")
end

to setupWorld
  set phi random 2
  set modelMatchCounter 0
  resize-world -5 5 -5 5
end

to beginSetupLaypeople
  create-Laypeople numberOfPairs[
    set color white
    ;Each layperson should stand opposite their respective expert.
    set xcor -2
    set ycor 0.5 - numberOfPairs / 2 + who

    set myExpertNumber who + numberOfPairs



    ;L competency is a random float within the specified interval.
    set competency ((random-float (maxLaypersonCompetency - minLaypersonCompetency)) + minLaypersonCompetency)

    ;L's actual interest on the question psi is determined based on the actual truth about phi.
    ifelse phi = 1 [set psi psiGivenPhi]
    [set psi (1 - psiGivenPhi)]

    ;L's subjective prior for the hypothesis that |psi|=true is given by their competency
    ifelse psi = 1 [set hyp competency]
    [set hyp 1 - competency]
  ]

end

to finishSetupLaypeople

  ask laypeople[
    ;************************************************************************************************************************************************
    ;L takes an educated guess at e and alpha of their expert, based on the "laypersonAstutness" setting.

    let i (random-float laypersonAstuteness * 2) - (laypersonAstuteness)
    let j (random-float laypersonAstuteness * 2) - (laypersonAstuteness)

    set priorE [competency] of turtle myExpertNumber + i
    set priorAlpha [interestAlignment] of turtle myExpertNumber + j

    if priorE < 0 [set priorE 0]
    if priorE > 1 [set priorE 1]
    if priorAlpha < 0 [set priorAlpha 0]
    if priorAlpha > 1 [set priorAlpha 1]

    ;************************************************************************************************************************************************
    ;L's subjective prior's in the experts reliability arise from their priors in e and alpha.
    set priorCorrectExpertAdvice (1 - (priorAlpha * (1 - priorE) + (1 - priorAlpha) * priorE))

    ;************************************************************************************************************************************************
    ;Based on the just determined prior and the UI setting "updateOnReliablyBadExperts", REL is initialized

    ifelse updateOnReliablyBadExperts [

      ;An expert with a 50 % chance of giving correct advice is reliable with probability 0
      ;An expert with a 0 or 100 % chance of giving correct advice is reliable with a probability 1
      ;An expert with a 25 or 75 % chance of giving correct advice is reliable with a probability 0.5
      ;And so on...
      ;Here, beta is just 0.5

      if priorCorrectExpertAdvice < 0.5 [
        ;Experts that L thinks to be worse than chance are titled charlatans.
        set charlatan? true
        print "This layperson thinks their vis-à-vis to be a charlatan."
        set priorCorrectExpertAdvice 0.5 + (0.5 - priorCorrectExpertAdvice)
      ]

      set rel (priorCorrectExpertAdvice - 0.5) * 2



    ][
      ;An expert with a 50 % chance of giving correct advice is reliable with probability 0
      ;An expert with a 100 % chance of giving correct advice is reliable with a probability 1
      ;An expert with a 75 % chance of giving correct advice is reliable with a probability 0.5
      ;An expert with a sub 50 % chance of giving correct advice is unreliable

      if priorCorrectExpertAdvice < 0.5 [
        ;Experts that L thinks to be worse than chance are titled charlatans.
        set charlatan? true
        set priorCorrectExpertAdvice 0.5
        print "This layperson thinks their vis-à-vis to be a charlatan."
      ]

      set rel (priorCorrectExpertAdvice - 0.5) * 2
    ]
  ]

end

to setupExperts
  create-experts numberOfPairs[
    ;color is initially neutral
    set color white

    ;Each expert should stand opposite their respective layperson.
    set xcor 2
    set ycor 0.5 - numberOfPairs / 2 + (who - numberOfPairs)

    ;1:1 linking of exerts and laypeople
    set myLaypersonNumber who - numberOfPairs
    create-link-with turtle myLaypersonNumber

    ;E competency is a random float within the specified interval.
    let myMinCompetency minExpertCompetency
    if e>l [set myMinCompetency max (list minExpertCompetency [competency] of turtle myLaypersonNumber)]
    set competency ((random-float (maxExpertCompetency - myMinCompetency)) + myMinCompetency)

    ;alpha is a random float within the specified interval
    set interestAlignment ((random-float (maxInterestAlignment - minInterestAlignment)) + minInterestAlignment)
  ]

end

to printSetup
  print "---------------------------------------------------------------------------------------------------------"
   print "---------------------------------------------------------------------------------------------------------"
  print (word "The value of phi in this run is " phi ".")
  print "---------------------------------------------------------------------------------------------------------"
  let i 0
  loop [
    if i = numberOfPairs [
      print "---------------------------------------------------------------------------------------------------------"

      stop]
    print (word "In pair #" (i + 1) " the expert has a competency of "
      precision [competency] of turtle (i + numberOfPairs) 2 " and the layperson has a competency of " precision [competency] of turtle i 2
      ". Their interests align to a degree of " precision [interestAlignment] of turtle (i + numberOfPairs) 2 ".")
    set i i + 1
  ]

end


to goLaypeople
   ask laypeople [
    ;************************************************************************************************************************************************
    ;determine L's assesment of phi (and thereby of psi) based on l.
    let i random-float 1
    ifelse i < competency [set assesment phi][set assesment 1 - phi]
    ifelse assesment = phi [set shape "face happy"][set shape "x"] ;laypeople that figured out phi get a smiley, others a X

     ifelse updateOnReliablyBadExperts = true [
      set beta 0.5
    ][
      ;Here, beta is instead given by L's subjective expectation that E claims that psi.
      set beta (priorCorrectExpertAdvice * assesment + (1 - priorCorrectExpertAdvice) * (1 - assesment))
    ]

    ;************************************************************************************************************************************************
    let rep? [testimony] of turtle myExpertNumber
    ;Reliably bad expert testimony is computed invertedly, as higher order evidence
    if charlatan? = true and updateOnReliablyBadExperts = true [set rep? 1 - rep?]


    ;************************************************************************************************************************************************
    ;Update HYP based on whether REP or ¬REP.
    let previousHyp hyp

    ifelse rep? = 1 [
      set hyp ((rel + beta - rel * beta) * hyp)/(hyp * rel + beta - rel * beta)
    ][
      set hyp ((1 - (rel + beta - rel * beta))* hyp) / (1 - (rel * hyp + beta - rel * beta))
    ]



    ;************************************************************************************************************************************************
    ;Determine the first condition for trust (proper updating and belief in HYP).


    ifelse previousHyp < hyp [

      print (word "Layperson #" who " increased their estimate of psi.")


      ifelse rep? = 1 and hyp >= 0.5[
       ;If HYP increased, is above 0.5 and E gave REP, the condition is met.
        if whatDoesTrustMean? = "Adjustment of HYP according to testimony" [
          set color green
          set didTrust? true
        ]
        set trustcondition1 true
      ][
        ;If HYP increased, but is below 0.5 or E gave ¬REP, the condition is NOT met.
        if whatDoesTrustMean? = "Adjustment of HYP according to testimony" [
          set color red
          set didTrust? false

        ]
        set trustCondition1 false
      ]
    ][
      print (word "Layperson #" who " did NOT increase their estimate of psi.")

      ifelse rep? = 0 and hyp <= 0.5 [
        ;If HYP decreased, is below 0.5 and E gave ¬REP, the condition is met.
        set trustcondition1 true
        if whatDoesTrustMean? = "Adjustment of HYP according to testimony" [
          set color green
          set didTrust? true

        ]
      ][
        ;If HYP decreased, but is above 0.5 or E gave REP, the condition is NOT met.
        set trustcondition1 false
        if whatDoesTrustMean? = "Adjustment of HYP according to testimony" [
          set color red
          set didTrust? false
        ]
      ]
    ]

    ;************************************************************************************************************************************************
    ;Update REL based on REP or ¬REP.

    let previousRel rel
    ifelse rep? = 1[
      set rel (previousHyp * rel)/(previousHyp * rel + beta - rel * beta)
    ][
      set rel ((1 - previousHyp) * rel)/(1 - (previousHyp * rel + beta - rel * beta))
    ]

    ;************************************************************************************************************************************************
    ;Determine the second condition for trust (increase of REL).

    ifelse previousRel < rel [
      print (word "Layperson #" who " increased their estimate of their expert's reliability.")
      set trustcondition2  true

      if whatDoesTrustMean? = "Increase of REL" [
        set color green
        set didTrust? true
      ]
    ][
      print (word "Layperson #" who " did NOT increase their estimate of their expert's reliability.")
      set trustcondition2  false
      if whatDoesTrustMean? = "Increase of REL" [
        set color red ;Decreasing REL is taken as evidence of distrusting E
        set didTrust? false
      ]
    ]

    ;************************************************************************************************************************************************
    ;Determine trust based on the two conditions.

    if whatDoesTrustMean? = "both" [

      ifelse trustcondition1 = true and trustcondition2 = true[
        set color green
        print "This layperson did trust their expert."
        set didTrust? true
      ][
        set color red
        print "This layperson did NOT trust their expert."
        set didTrust? false
      ]
    ]

    ;************************************************************************************************************************************************
    ;If L trusted (or distrusted) in accordance to the objective trustworthiness of E, count that as a model match.

    if didTrust? = [objectiveTrustworthiness] of turtle myExpertNumber [
      set modelMatchCounter modelMatchcounter + 1
    ]
  ]
end

to goExperts

  ask experts[
    ;************************************************************************************************************************************************
    ;determine E's assesment of phi based on e.
    let i random-float 1
    ifelse i < competency [set assesment phi][set assesment 1 - phi]
    print (word "Expert " (who - numberOfPairs) " assessed the value of phi to be " assesment ".")

    ;************************************************************************************************************************************************
    ;determine E's assesment of psi based on their assesment of phi and their alpha.
    ifelse assesment = phi [
      let j random-float 1
      ifelse j < interestalignment [
        set testimony [psi] of turtle mylaypersonnumber

      ][set testimony 1 - [psi] of turtle mylaypersonnumber]

    ][
      let j random-float 1
      ifelse j > interestalignment [
        set testimony [psi] of turtle mylaypersonnumber
      ][
        set testimony 1 - [psi] of turtle mylaypersonnumber
      ]
    ]

    print (word "Their testimony is " testimony ", while it would lay in their layperson's interest to " [psi] of turtle mylaypersonnumber ".")
    ifelse assesment = phi [set shape "face happy"][set shape "x"] ;experts that figured out phi get a smiley, others a X
    ifelse testimony = [psi] of turtle mylaypersonnumber [set color green][ set color red] ;experts that gave correct testimony are green, others red

    ;************************************************************************************************************************************************
    ;determine whether deferral is optimal according to Duijf's model and color the links accordingly
    let chanceOfCorrectTestimony (interestAlignment * competency + (1 - interestAlignment) * (1 - competency))

    ifelse  ([competency] of turtle mylaypersonnumber) < chanceOfCorrectTestimony [
      print (word "According to Duijf's model, the layperson #" (who - numberofpairs) " should trust this expert.")
      ask my-links [set color green]
      set objectiveTrustworthiness true
    ][
      print (word "According to Duijf's model, the layperson #" (who - numberofpairs) " should NOT trust this expert.")
      ask my-links [set color red]
      set objectiveTrustworthiness false
    ]
    print "--------------------------------------------------------------------------------------------------------------"

  ]
end
