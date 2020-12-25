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
    arArn,
    arName,
    arAssessmentTemplateArn,
    arState,
    arDurationInSeconds,
    arRulesPackageArns,
    arUserAttributesForFindings,
    arCreatedAt,
    arStateChangedAt,
    arDataCollected,
    arStateChanges,
    arNotifications,
    arFindingCounts,
    arCompletedAt,
    arStartedAt,
  )
where

import qualified Network.AWS.Inspector.Types.Arn as Types
import qualified Network.AWS.Inspector.Types.AssessmentRunNotification as Types
import qualified Network.AWS.Inspector.Types.AssessmentRunState as Types
import qualified Network.AWS.Inspector.Types.AssessmentRunStateChange as Types
import qualified Network.AWS.Inspector.Types.AssessmentTemplateArn as Types
import qualified Network.AWS.Inspector.Types.Attribute as Types
import qualified Network.AWS.Inspector.Types.Name as Types
import qualified Network.AWS.Inspector.Types.Severity as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A snapshot of an Amazon Inspector assessment run that contains the findings of the assessment run .
--
-- Used as the response element in the 'DescribeAssessmentRuns' action.
--
-- /See:/ 'mkAssessmentRun' smart constructor.
data AssessmentRun = AssessmentRun'
  { -- | The ARN of the assessment run.
    arn :: Types.Arn,
    -- | The auto-generated name for the assessment run.
    name :: Types.Name,
    -- | The ARN of the assessment template that is associated with the assessment run.
    assessmentTemplateArn :: Types.AssessmentTemplateArn,
    -- | The state of the assessment run.
    state :: Types.AssessmentRunState,
    -- | The duration of the assessment run.
    durationInSeconds :: Core.Natural,
    -- | The rules packages selected for the assessment run.
    rulesPackageArns :: Core.NonEmpty Types.Arn,
    -- | The user-defined attributes that are assigned to every generated finding.
    userAttributesForFindings :: [Types.Attribute],
    -- | The time when 'StartAssessmentRun' was called.
    createdAt :: Core.NominalDiffTime,
    -- | The last time when the assessment run's state changed.
    stateChangedAt :: Core.NominalDiffTime,
    -- | A Boolean value (true or false) that specifies whether the process of collecting data from the agents is completed.
    dataCollected :: Core.Bool,
    -- | A list of the assessment run state changes.
    stateChanges :: [Types.AssessmentRunStateChange],
    -- | A list of notifications for the event subscriptions. A notification about a particular generated finding is added to this list only once.
    notifications :: [Types.AssessmentRunNotification],
    -- | Provides a total count of generated findings per severity.
    findingCounts :: Core.HashMap Types.Severity Core.Int,
    -- | The assessment run completion time that corresponds to the rules packages evaluation completion time or failure.
    completedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The time when 'StartAssessmentRun' was called.
    startedAt :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AssessmentRun' value with any optional fields omitted.
mkAssessmentRun ::
  -- | 'arn'
  Types.Arn ->
  -- | 'name'
  Types.Name ->
  -- | 'assessmentTemplateArn'
  Types.AssessmentTemplateArn ->
  -- | 'state'
  Types.AssessmentRunState ->
  -- | 'durationInSeconds'
  Core.Natural ->
  -- | 'rulesPackageArns'
  Core.NonEmpty Types.Arn ->
  -- | 'createdAt'
  Core.NominalDiffTime ->
  -- | 'stateChangedAt'
  Core.NominalDiffTime ->
  -- | 'dataCollected'
  Core.Bool ->
  AssessmentRun
mkAssessmentRun
  arn
  name
  assessmentTemplateArn
  state
  durationInSeconds
  rulesPackageArns
  createdAt
  stateChangedAt
  dataCollected =
    AssessmentRun'
      { arn,
        name,
        assessmentTemplateArn,
        state,
        durationInSeconds,
        rulesPackageArns,
        userAttributesForFindings = Core.mempty,
        createdAt,
        stateChangedAt,
        dataCollected,
        stateChanges = Core.mempty,
        notifications = Core.mempty,
        findingCounts = Core.mempty,
        completedAt = Core.Nothing,
        startedAt = Core.Nothing
      }

-- | The ARN of the assessment run.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arArn :: Lens.Lens' AssessmentRun Types.Arn
arArn = Lens.field @"arn"
{-# DEPRECATED arArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The auto-generated name for the assessment run.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arName :: Lens.Lens' AssessmentRun Types.Name
arName = Lens.field @"name"
{-# DEPRECATED arName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the assessment template that is associated with the assessment run.
--
-- /Note:/ Consider using 'assessmentTemplateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAssessmentTemplateArn :: Lens.Lens' AssessmentRun Types.AssessmentTemplateArn
arAssessmentTemplateArn = Lens.field @"assessmentTemplateArn"
{-# DEPRECATED arAssessmentTemplateArn "Use generic-lens or generic-optics with 'assessmentTemplateArn' instead." #-}

-- | The state of the assessment run.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arState :: Lens.Lens' AssessmentRun Types.AssessmentRunState
arState = Lens.field @"state"
{-# DEPRECATED arState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The duration of the assessment run.
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDurationInSeconds :: Lens.Lens' AssessmentRun Core.Natural
arDurationInSeconds = Lens.field @"durationInSeconds"
{-# DEPRECATED arDurationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead." #-}

-- | The rules packages selected for the assessment run.
--
-- /Note:/ Consider using 'rulesPackageArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRulesPackageArns :: Lens.Lens' AssessmentRun (Core.NonEmpty Types.Arn)
arRulesPackageArns = Lens.field @"rulesPackageArns"
{-# DEPRECATED arRulesPackageArns "Use generic-lens or generic-optics with 'rulesPackageArns' instead." #-}

-- | The user-defined attributes that are assigned to every generated finding.
--
-- /Note:/ Consider using 'userAttributesForFindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arUserAttributesForFindings :: Lens.Lens' AssessmentRun [Types.Attribute]
arUserAttributesForFindings = Lens.field @"userAttributesForFindings"
{-# DEPRECATED arUserAttributesForFindings "Use generic-lens or generic-optics with 'userAttributesForFindings' instead." #-}

-- | The time when 'StartAssessmentRun' was called.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arCreatedAt :: Lens.Lens' AssessmentRun Core.NominalDiffTime
arCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED arCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The last time when the assessment run's state changed.
--
-- /Note:/ Consider using 'stateChangedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arStateChangedAt :: Lens.Lens' AssessmentRun Core.NominalDiffTime
arStateChangedAt = Lens.field @"stateChangedAt"
{-# DEPRECATED arStateChangedAt "Use generic-lens or generic-optics with 'stateChangedAt' instead." #-}

-- | A Boolean value (true or false) that specifies whether the process of collecting data from the agents is completed.
--
-- /Note:/ Consider using 'dataCollected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDataCollected :: Lens.Lens' AssessmentRun Core.Bool
arDataCollected = Lens.field @"dataCollected"
{-# DEPRECATED arDataCollected "Use generic-lens or generic-optics with 'dataCollected' instead." #-}

-- | A list of the assessment run state changes.
--
-- /Note:/ Consider using 'stateChanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arStateChanges :: Lens.Lens' AssessmentRun [Types.AssessmentRunStateChange]
arStateChanges = Lens.field @"stateChanges"
{-# DEPRECATED arStateChanges "Use generic-lens or generic-optics with 'stateChanges' instead." #-}

-- | A list of notifications for the event subscriptions. A notification about a particular generated finding is added to this list only once.
--
-- /Note:/ Consider using 'notifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arNotifications :: Lens.Lens' AssessmentRun [Types.AssessmentRunNotification]
arNotifications = Lens.field @"notifications"
{-# DEPRECATED arNotifications "Use generic-lens or generic-optics with 'notifications' instead." #-}

-- | Provides a total count of generated findings per severity.
--
-- /Note:/ Consider using 'findingCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arFindingCounts :: Lens.Lens' AssessmentRun (Core.HashMap Types.Severity Core.Int)
arFindingCounts = Lens.field @"findingCounts"
{-# DEPRECATED arFindingCounts "Use generic-lens or generic-optics with 'findingCounts' instead." #-}

-- | The assessment run completion time that corresponds to the rules packages evaluation completion time or failure.
--
-- /Note:/ Consider using 'completedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arCompletedAt :: Lens.Lens' AssessmentRun (Core.Maybe Core.NominalDiffTime)
arCompletedAt = Lens.field @"completedAt"
{-# DEPRECATED arCompletedAt "Use generic-lens or generic-optics with 'completedAt' instead." #-}

-- | The time when 'StartAssessmentRun' was called.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arStartedAt :: Lens.Lens' AssessmentRun (Core.Maybe Core.NominalDiffTime)
arStartedAt = Lens.field @"startedAt"
{-# DEPRECATED arStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

instance Core.FromJSON AssessmentRun where
  parseJSON =
    Core.withObject "AssessmentRun" Core.$
      \x ->
        AssessmentRun'
          Core.<$> (x Core..: "arn")
          Core.<*> (x Core..: "name")
          Core.<*> (x Core..: "assessmentTemplateArn")
          Core.<*> (x Core..: "state")
          Core.<*> (x Core..: "durationInSeconds")
          Core.<*> (x Core..: "rulesPackageArns")
          Core.<*> (x Core..:? "userAttributesForFindings" Core..!= Core.mempty)
          Core.<*> (x Core..: "createdAt")
          Core.<*> (x Core..: "stateChangedAt")
          Core.<*> (x Core..: "dataCollected")
          Core.<*> (x Core..:? "stateChanges" Core..!= Core.mempty)
          Core.<*> (x Core..:? "notifications" Core..!= Core.mempty)
          Core.<*> (x Core..:? "findingCounts" Core..!= Core.mempty)
          Core.<*> (x Core..:? "completedAt")
          Core.<*> (x Core..:? "startedAt")
