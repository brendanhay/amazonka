{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRun
  ( AssessmentRun (..),

    -- * Smart constructor
    mkAssessmentRun,

    -- * Lenses
    arDataCollected,
    arState,
    arArn,
    arCreatedAt,
    arFindingCounts,
    arUserAttributesForFindings,
    arRulesPackageARNs,
    arStartedAt,
    arName,
    arDurationInSeconds,
    arStateChanges,
    arCompletedAt,
    arStateChangedAt,
    arAssessmentTemplateARN,
    arNotifications,
  )
where

import Network.AWS.Inspector.Types.AssessmentRunNotification
import Network.AWS.Inspector.Types.AssessmentRunState
import Network.AWS.Inspector.Types.AssessmentRunStateChange
import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.Severity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A snapshot of an Amazon Inspector assessment run that contains the findings of the assessment run .
--
-- Used as the response element in the 'DescribeAssessmentRuns' action.
--
-- /See:/ 'mkAssessmentRun' smart constructor.
data AssessmentRun = AssessmentRun'
  { -- | A Boolean value (true or false) that specifies whether the process of collecting data from the agents is completed.
    dataCollected :: Lude.Bool,
    -- | The state of the assessment run.
    state :: AssessmentRunState,
    -- | The ARN of the assessment run.
    arn :: Lude.Text,
    -- | The time when 'StartAssessmentRun' was called.
    createdAt :: Lude.Timestamp,
    -- | Provides a total count of generated findings per severity.
    findingCounts :: Lude.HashMap Severity (Lude.Int),
    -- | The user-defined attributes that are assigned to every generated finding.
    userAttributesForFindings :: [Attribute],
    -- | The rules packages selected for the assessment run.
    rulesPackageARNs :: Lude.NonEmpty Lude.Text,
    -- | The time when 'StartAssessmentRun' was called.
    startedAt :: Lude.Maybe Lude.Timestamp,
    -- | The auto-generated name for the assessment run.
    name :: Lude.Text,
    -- | The duration of the assessment run.
    durationInSeconds :: Lude.Natural,
    -- | A list of the assessment run state changes.
    stateChanges :: [AssessmentRunStateChange],
    -- | The assessment run completion time that corresponds to the rules packages evaluation completion time or failure.
    completedAt :: Lude.Maybe Lude.Timestamp,
    -- | The last time when the assessment run's state changed.
    stateChangedAt :: Lude.Timestamp,
    -- | The ARN of the assessment template that is associated with the assessment run.
    assessmentTemplateARN :: Lude.Text,
    -- | A list of notifications for the event subscriptions. A notification about a particular generated finding is added to this list only once.
    notifications :: [AssessmentRunNotification]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssessmentRun' with the minimum fields required to make a request.
--
-- * 'dataCollected' - A Boolean value (true or false) that specifies whether the process of collecting data from the agents is completed.
-- * 'state' - The state of the assessment run.
-- * 'arn' - The ARN of the assessment run.
-- * 'createdAt' - The time when 'StartAssessmentRun' was called.
-- * 'findingCounts' - Provides a total count of generated findings per severity.
-- * 'userAttributesForFindings' - The user-defined attributes that are assigned to every generated finding.
-- * 'rulesPackageARNs' - The rules packages selected for the assessment run.
-- * 'startedAt' - The time when 'StartAssessmentRun' was called.
-- * 'name' - The auto-generated name for the assessment run.
-- * 'durationInSeconds' - The duration of the assessment run.
-- * 'stateChanges' - A list of the assessment run state changes.
-- * 'completedAt' - The assessment run completion time that corresponds to the rules packages evaluation completion time or failure.
-- * 'stateChangedAt' - The last time when the assessment run's state changed.
-- * 'assessmentTemplateARN' - The ARN of the assessment template that is associated with the assessment run.
-- * 'notifications' - A list of notifications for the event subscriptions. A notification about a particular generated finding is added to this list only once.
mkAssessmentRun ::
  -- | 'dataCollected'
  Lude.Bool ->
  -- | 'state'
  AssessmentRunState ->
  -- | 'arn'
  Lude.Text ->
  -- | 'createdAt'
  Lude.Timestamp ->
  -- | 'rulesPackageARNs'
  Lude.NonEmpty Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'durationInSeconds'
  Lude.Natural ->
  -- | 'stateChangedAt'
  Lude.Timestamp ->
  -- | 'assessmentTemplateARN'
  Lude.Text ->
  AssessmentRun
mkAssessmentRun
  pDataCollected_
  pState_
  pArn_
  pCreatedAt_
  pRulesPackageARNs_
  pName_
  pDurationInSeconds_
  pStateChangedAt_
  pAssessmentTemplateARN_ =
    AssessmentRun'
      { dataCollected = pDataCollected_,
        state = pState_,
        arn = pArn_,
        createdAt = pCreatedAt_,
        findingCounts = Lude.mempty,
        userAttributesForFindings = Lude.mempty,
        rulesPackageARNs = pRulesPackageARNs_,
        startedAt = Lude.Nothing,
        name = pName_,
        durationInSeconds = pDurationInSeconds_,
        stateChanges = Lude.mempty,
        completedAt = Lude.Nothing,
        stateChangedAt = pStateChangedAt_,
        assessmentTemplateARN = pAssessmentTemplateARN_,
        notifications = Lude.mempty
      }

-- | A Boolean value (true or false) that specifies whether the process of collecting data from the agents is completed.
--
-- /Note:/ Consider using 'dataCollected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDataCollected :: Lens.Lens' AssessmentRun Lude.Bool
arDataCollected = Lens.lens (dataCollected :: AssessmentRun -> Lude.Bool) (\s a -> s {dataCollected = a} :: AssessmentRun)
{-# DEPRECATED arDataCollected "Use generic-lens or generic-optics with 'dataCollected' instead." #-}

-- | The state of the assessment run.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arState :: Lens.Lens' AssessmentRun AssessmentRunState
arState = Lens.lens (state :: AssessmentRun -> AssessmentRunState) (\s a -> s {state = a} :: AssessmentRun)
{-# DEPRECATED arState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ARN of the assessment run.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arArn :: Lens.Lens' AssessmentRun Lude.Text
arArn = Lens.lens (arn :: AssessmentRun -> Lude.Text) (\s a -> s {arn = a} :: AssessmentRun)
{-# DEPRECATED arArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time when 'StartAssessmentRun' was called.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arCreatedAt :: Lens.Lens' AssessmentRun Lude.Timestamp
arCreatedAt = Lens.lens (createdAt :: AssessmentRun -> Lude.Timestamp) (\s a -> s {createdAt = a} :: AssessmentRun)
{-# DEPRECATED arCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Provides a total count of generated findings per severity.
--
-- /Note:/ Consider using 'findingCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arFindingCounts :: Lens.Lens' AssessmentRun (Lude.HashMap Severity (Lude.Int))
arFindingCounts = Lens.lens (findingCounts :: AssessmentRun -> Lude.HashMap Severity (Lude.Int)) (\s a -> s {findingCounts = a} :: AssessmentRun)
{-# DEPRECATED arFindingCounts "Use generic-lens or generic-optics with 'findingCounts' instead." #-}

-- | The user-defined attributes that are assigned to every generated finding.
--
-- /Note:/ Consider using 'userAttributesForFindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arUserAttributesForFindings :: Lens.Lens' AssessmentRun [Attribute]
arUserAttributesForFindings = Lens.lens (userAttributesForFindings :: AssessmentRun -> [Attribute]) (\s a -> s {userAttributesForFindings = a} :: AssessmentRun)
{-# DEPRECATED arUserAttributesForFindings "Use generic-lens or generic-optics with 'userAttributesForFindings' instead." #-}

-- | The rules packages selected for the assessment run.
--
-- /Note:/ Consider using 'rulesPackageARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRulesPackageARNs :: Lens.Lens' AssessmentRun (Lude.NonEmpty Lude.Text)
arRulesPackageARNs = Lens.lens (rulesPackageARNs :: AssessmentRun -> Lude.NonEmpty Lude.Text) (\s a -> s {rulesPackageARNs = a} :: AssessmentRun)
{-# DEPRECATED arRulesPackageARNs "Use generic-lens or generic-optics with 'rulesPackageARNs' instead." #-}

-- | The time when 'StartAssessmentRun' was called.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arStartedAt :: Lens.Lens' AssessmentRun (Lude.Maybe Lude.Timestamp)
arStartedAt = Lens.lens (startedAt :: AssessmentRun -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedAt = a} :: AssessmentRun)
{-# DEPRECATED arStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | The auto-generated name for the assessment run.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arName :: Lens.Lens' AssessmentRun Lude.Text
arName = Lens.lens (name :: AssessmentRun -> Lude.Text) (\s a -> s {name = a} :: AssessmentRun)
{-# DEPRECATED arName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The duration of the assessment run.
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDurationInSeconds :: Lens.Lens' AssessmentRun Lude.Natural
arDurationInSeconds = Lens.lens (durationInSeconds :: AssessmentRun -> Lude.Natural) (\s a -> s {durationInSeconds = a} :: AssessmentRun)
{-# DEPRECATED arDurationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead." #-}

-- | A list of the assessment run state changes.
--
-- /Note:/ Consider using 'stateChanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arStateChanges :: Lens.Lens' AssessmentRun [AssessmentRunStateChange]
arStateChanges = Lens.lens (stateChanges :: AssessmentRun -> [AssessmentRunStateChange]) (\s a -> s {stateChanges = a} :: AssessmentRun)
{-# DEPRECATED arStateChanges "Use generic-lens or generic-optics with 'stateChanges' instead." #-}

-- | The assessment run completion time that corresponds to the rules packages evaluation completion time or failure.
--
-- /Note:/ Consider using 'completedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arCompletedAt :: Lens.Lens' AssessmentRun (Lude.Maybe Lude.Timestamp)
arCompletedAt = Lens.lens (completedAt :: AssessmentRun -> Lude.Maybe Lude.Timestamp) (\s a -> s {completedAt = a} :: AssessmentRun)
{-# DEPRECATED arCompletedAt "Use generic-lens or generic-optics with 'completedAt' instead." #-}

-- | The last time when the assessment run's state changed.
--
-- /Note:/ Consider using 'stateChangedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arStateChangedAt :: Lens.Lens' AssessmentRun Lude.Timestamp
arStateChangedAt = Lens.lens (stateChangedAt :: AssessmentRun -> Lude.Timestamp) (\s a -> s {stateChangedAt = a} :: AssessmentRun)
{-# DEPRECATED arStateChangedAt "Use generic-lens or generic-optics with 'stateChangedAt' instead." #-}

-- | The ARN of the assessment template that is associated with the assessment run.
--
-- /Note:/ Consider using 'assessmentTemplateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAssessmentTemplateARN :: Lens.Lens' AssessmentRun Lude.Text
arAssessmentTemplateARN = Lens.lens (assessmentTemplateARN :: AssessmentRun -> Lude.Text) (\s a -> s {assessmentTemplateARN = a} :: AssessmentRun)
{-# DEPRECATED arAssessmentTemplateARN "Use generic-lens or generic-optics with 'assessmentTemplateARN' instead." #-}

-- | A list of notifications for the event subscriptions. A notification about a particular generated finding is added to this list only once.
--
-- /Note:/ Consider using 'notifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arNotifications :: Lens.Lens' AssessmentRun [AssessmentRunNotification]
arNotifications = Lens.lens (notifications :: AssessmentRun -> [AssessmentRunNotification]) (\s a -> s {notifications = a} :: AssessmentRun)
{-# DEPRECATED arNotifications "Use generic-lens or generic-optics with 'notifications' instead." #-}

instance Lude.FromJSON AssessmentRun where
  parseJSON =
    Lude.withObject
      "AssessmentRun"
      ( \x ->
          AssessmentRun'
            Lude.<$> (x Lude..: "dataCollected")
            Lude.<*> (x Lude..: "state")
            Lude.<*> (x Lude..: "arn")
            Lude.<*> (x Lude..: "createdAt")
            Lude.<*> (x Lude..:? "findingCounts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "userAttributesForFindings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "rulesPackageArns")
            Lude.<*> (x Lude..:? "startedAt")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "durationInSeconds")
            Lude.<*> (x Lude..:? "stateChanges" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "completedAt")
            Lude.<*> (x Lude..: "stateChangedAt")
            Lude.<*> (x Lude..: "assessmentTemplateArn")
            Lude.<*> (x Lude..:? "notifications" Lude..!= Lude.mempty)
      )
