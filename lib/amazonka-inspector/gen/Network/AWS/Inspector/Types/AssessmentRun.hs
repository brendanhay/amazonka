{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.AssessmentRun
  ( AssessmentRun (..)
  -- * Smart constructor
  , mkAssessmentRun
  -- * Lenses
  , arArn
  , arName
  , arAssessmentTemplateArn
  , arState
  , arDurationInSeconds
  , arRulesPackageArns
  , arUserAttributesForFindings
  , arCreatedAt
  , arStateChangedAt
  , arDataCollected
  , arStateChanges
  , arNotifications
  , arFindingCounts
  , arCompletedAt
  , arStartedAt
  ) where

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
  { arn :: Types.Arn
    -- ^ The ARN of the assessment run.
  , name :: Types.Name
    -- ^ The auto-generated name for the assessment run.
  , assessmentTemplateArn :: Types.AssessmentTemplateArn
    -- ^ The ARN of the assessment template that is associated with the assessment run.
  , state :: Types.AssessmentRunState
    -- ^ The state of the assessment run.
  , durationInSeconds :: Core.Natural
    -- ^ The duration of the assessment run.
  , rulesPackageArns :: Core.NonEmpty Types.Arn
    -- ^ The rules packages selected for the assessment run.
  , userAttributesForFindings :: [Types.Attribute]
    -- ^ The user-defined attributes that are assigned to every generated finding.
  , createdAt :: Core.NominalDiffTime
    -- ^ The time when 'StartAssessmentRun' was called.
  , stateChangedAt :: Core.NominalDiffTime
    -- ^ The last time when the assessment run's state changed.
  , dataCollected :: Core.Bool
    -- ^ A Boolean value (true or false) that specifies whether the process of collecting data from the agents is completed.
  , stateChanges :: [Types.AssessmentRunStateChange]
    -- ^ A list of the assessment run state changes.
  , notifications :: [Types.AssessmentRunNotification]
    -- ^ A list of notifications for the event subscriptions. A notification about a particular generated finding is added to this list only once.
  , findingCounts :: Core.HashMap Types.Severity Core.Int
    -- ^ Provides a total count of generated findings per severity.
  , completedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The assessment run completion time that corresponds to the rules packages evaluation completion time or failure.
  , startedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when 'StartAssessmentRun' was called.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AssessmentRun' value with any optional fields omitted.
mkAssessmentRun
    :: Types.Arn -- ^ 'arn'
    -> Types.Name -- ^ 'name'
    -> Types.AssessmentTemplateArn -- ^ 'assessmentTemplateArn'
    -> Types.AssessmentRunState -- ^ 'state'
    -> Core.Natural -- ^ 'durationInSeconds'
    -> Core.NonEmpty Types.Arn -- ^ 'rulesPackageArns'
    -> Core.NominalDiffTime -- ^ 'createdAt'
    -> Core.NominalDiffTime -- ^ 'stateChangedAt'
    -> Core.Bool -- ^ 'dataCollected'
    -> AssessmentRun
mkAssessmentRun arn name assessmentTemplateArn state
  durationInSeconds rulesPackageArns createdAt stateChangedAt
  dataCollected
  = AssessmentRun'{arn, name, assessmentTemplateArn, state,
                   durationInSeconds, rulesPackageArns,
                   userAttributesForFindings = Core.mempty, createdAt, stateChangedAt,
                   dataCollected, stateChanges = Core.mempty,
                   notifications = Core.mempty, findingCounts = Core.mempty,
                   completedAt = Core.Nothing, startedAt = Core.Nothing}

-- | The ARN of the assessment run.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arArn :: Lens.Lens' AssessmentRun Types.Arn
arArn = Lens.field @"arn"
{-# INLINEABLE arArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The auto-generated name for the assessment run.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arName :: Lens.Lens' AssessmentRun Types.Name
arName = Lens.field @"name"
{-# INLINEABLE arName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ARN of the assessment template that is associated with the assessment run.
--
-- /Note:/ Consider using 'assessmentTemplateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAssessmentTemplateArn :: Lens.Lens' AssessmentRun Types.AssessmentTemplateArn
arAssessmentTemplateArn = Lens.field @"assessmentTemplateArn"
{-# INLINEABLE arAssessmentTemplateArn #-}
{-# DEPRECATED assessmentTemplateArn "Use generic-lens or generic-optics with 'assessmentTemplateArn' instead"  #-}

-- | The state of the assessment run.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arState :: Lens.Lens' AssessmentRun Types.AssessmentRunState
arState = Lens.field @"state"
{-# INLINEABLE arState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The duration of the assessment run.
--
-- /Note:/ Consider using 'durationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDurationInSeconds :: Lens.Lens' AssessmentRun Core.Natural
arDurationInSeconds = Lens.field @"durationInSeconds"
{-# INLINEABLE arDurationInSeconds #-}
{-# DEPRECATED durationInSeconds "Use generic-lens or generic-optics with 'durationInSeconds' instead"  #-}

-- | The rules packages selected for the assessment run.
--
-- /Note:/ Consider using 'rulesPackageArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRulesPackageArns :: Lens.Lens' AssessmentRun (Core.NonEmpty Types.Arn)
arRulesPackageArns = Lens.field @"rulesPackageArns"
{-# INLINEABLE arRulesPackageArns #-}
{-# DEPRECATED rulesPackageArns "Use generic-lens or generic-optics with 'rulesPackageArns' instead"  #-}

-- | The user-defined attributes that are assigned to every generated finding.
--
-- /Note:/ Consider using 'userAttributesForFindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arUserAttributesForFindings :: Lens.Lens' AssessmentRun [Types.Attribute]
arUserAttributesForFindings = Lens.field @"userAttributesForFindings"
{-# INLINEABLE arUserAttributesForFindings #-}
{-# DEPRECATED userAttributesForFindings "Use generic-lens or generic-optics with 'userAttributesForFindings' instead"  #-}

-- | The time when 'StartAssessmentRun' was called.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arCreatedAt :: Lens.Lens' AssessmentRun Core.NominalDiffTime
arCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE arCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The last time when the assessment run's state changed.
--
-- /Note:/ Consider using 'stateChangedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arStateChangedAt :: Lens.Lens' AssessmentRun Core.NominalDiffTime
arStateChangedAt = Lens.field @"stateChangedAt"
{-# INLINEABLE arStateChangedAt #-}
{-# DEPRECATED stateChangedAt "Use generic-lens or generic-optics with 'stateChangedAt' instead"  #-}

-- | A Boolean value (true or false) that specifies whether the process of collecting data from the agents is completed.
--
-- /Note:/ Consider using 'dataCollected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDataCollected :: Lens.Lens' AssessmentRun Core.Bool
arDataCollected = Lens.field @"dataCollected"
{-# INLINEABLE arDataCollected #-}
{-# DEPRECATED dataCollected "Use generic-lens or generic-optics with 'dataCollected' instead"  #-}

-- | A list of the assessment run state changes.
--
-- /Note:/ Consider using 'stateChanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arStateChanges :: Lens.Lens' AssessmentRun [Types.AssessmentRunStateChange]
arStateChanges = Lens.field @"stateChanges"
{-# INLINEABLE arStateChanges #-}
{-# DEPRECATED stateChanges "Use generic-lens or generic-optics with 'stateChanges' instead"  #-}

-- | A list of notifications for the event subscriptions. A notification about a particular generated finding is added to this list only once.
--
-- /Note:/ Consider using 'notifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arNotifications :: Lens.Lens' AssessmentRun [Types.AssessmentRunNotification]
arNotifications = Lens.field @"notifications"
{-# INLINEABLE arNotifications #-}
{-# DEPRECATED notifications "Use generic-lens or generic-optics with 'notifications' instead"  #-}

-- | Provides a total count of generated findings per severity.
--
-- /Note:/ Consider using 'findingCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arFindingCounts :: Lens.Lens' AssessmentRun (Core.HashMap Types.Severity Core.Int)
arFindingCounts = Lens.field @"findingCounts"
{-# INLINEABLE arFindingCounts #-}
{-# DEPRECATED findingCounts "Use generic-lens or generic-optics with 'findingCounts' instead"  #-}

-- | The assessment run completion time that corresponds to the rules packages evaluation completion time or failure.
--
-- /Note:/ Consider using 'completedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arCompletedAt :: Lens.Lens' AssessmentRun (Core.Maybe Core.NominalDiffTime)
arCompletedAt = Lens.field @"completedAt"
{-# INLINEABLE arCompletedAt #-}
{-# DEPRECATED completedAt "Use generic-lens or generic-optics with 'completedAt' instead"  #-}

-- | The time when 'StartAssessmentRun' was called.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arStartedAt :: Lens.Lens' AssessmentRun (Core.Maybe Core.NominalDiffTime)
arStartedAt = Lens.field @"startedAt"
{-# INLINEABLE arStartedAt #-}
{-# DEPRECATED startedAt "Use generic-lens or generic-optics with 'startedAt' instead"  #-}

instance Core.FromJSON AssessmentRun where
        parseJSON
          = Core.withObject "AssessmentRun" Core.$
              \ x ->
                AssessmentRun' Core.<$>
                  (x Core..: "arn") Core.<*> x Core..: "name" Core.<*>
                    x Core..: "assessmentTemplateArn"
                    Core.<*> x Core..: "state"
                    Core.<*> x Core..: "durationInSeconds"
                    Core.<*> x Core..: "rulesPackageArns"
                    Core.<*>
                    x Core..:? "userAttributesForFindings" Core..!= Core.mempty
                    Core.<*> x Core..: "createdAt"
                    Core.<*> x Core..: "stateChangedAt"
                    Core.<*> x Core..: "dataCollected"
                    Core.<*> x Core..:? "stateChanges" Core..!= Core.mempty
                    Core.<*> x Core..:? "notifications" Core..!= Core.mempty
                    Core.<*> x Core..:? "findingCounts" Core..!= Core.mempty
                    Core.<*> x Core..:? "completedAt"
                    Core.<*> x Core..:? "startedAt"
