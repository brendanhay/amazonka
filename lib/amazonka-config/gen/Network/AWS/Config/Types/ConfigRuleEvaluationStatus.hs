{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRuleEvaluationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ConfigRuleEvaluationStatus
  ( ConfigRuleEvaluationStatus (..)
  -- * Smart constructor
  , mkConfigRuleEvaluationStatus
  -- * Lenses
  , cresConfigRuleArn
  , cresConfigRuleId
  , cresConfigRuleName
  , cresFirstActivatedTime
  , cresFirstEvaluationStarted
  , cresLastDeactivatedTime
  , cresLastErrorCode
  , cresLastErrorMessage
  , cresLastFailedEvaluationTime
  , cresLastFailedInvocationTime
  , cresLastSuccessfulEvaluationTime
  , cresLastSuccessfulInvocationTime
  ) where

import qualified Network.AWS.Config.Types.ConfigRuleName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Status information for your AWS managed Config rules. The status includes information such as the last time the rule ran, the last time it failed, and the related error for the last failure.
--
-- This action does not return status information about custom AWS Config rules.
--
-- /See:/ 'mkConfigRuleEvaluationStatus' smart constructor.
data ConfigRuleEvaluationStatus = ConfigRuleEvaluationStatus'
  { configRuleArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the AWS Config rule.
  , configRuleId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS Config rule.
  , configRuleName :: Core.Maybe Types.ConfigRuleName
    -- ^ The name of the AWS Config rule.
  , firstActivatedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that you first activated the AWS Config rule.
  , firstEvaluationStarted :: Core.Maybe Core.Bool
    -- ^ Indicates whether AWS Config has evaluated your resources against the rule at least once.
--
--
--     * @true@ - AWS Config has evaluated your AWS resources against the rule at least once.
--
--
--     * @false@ - AWS Config has not once finished evaluating your AWS resources against the rule.
--
--
  , lastDeactivatedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that you last turned off the AWS Config rule.
  , lastErrorCode :: Core.Maybe Core.Text
    -- ^ The error code that AWS Config returned when the rule last failed.
  , lastErrorMessage :: Core.Maybe Core.Text
    -- ^ The error message that AWS Config returned when the rule last failed.
  , lastFailedEvaluationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that AWS Config last failed to evaluate your AWS resources against the rule.
  , lastFailedInvocationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that AWS Config last failed to invoke the AWS Config rule to evaluate your AWS resources.
  , lastSuccessfulEvaluationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that AWS Config last successfully evaluated your AWS resources against the rule.
  , lastSuccessfulInvocationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that AWS Config last successfully invoked the AWS Config rule to evaluate your AWS resources.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ConfigRuleEvaluationStatus' value with any optional fields omitted.
mkConfigRuleEvaluationStatus
    :: ConfigRuleEvaluationStatus
mkConfigRuleEvaluationStatus
  = ConfigRuleEvaluationStatus'{configRuleArn = Core.Nothing,
                                configRuleId = Core.Nothing, configRuleName = Core.Nothing,
                                firstActivatedTime = Core.Nothing,
                                firstEvaluationStarted = Core.Nothing,
                                lastDeactivatedTime = Core.Nothing, lastErrorCode = Core.Nothing,
                                lastErrorMessage = Core.Nothing,
                                lastFailedEvaluationTime = Core.Nothing,
                                lastFailedInvocationTime = Core.Nothing,
                                lastSuccessfulEvaluationTime = Core.Nothing,
                                lastSuccessfulInvocationTime = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresConfigRuleArn :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.Text)
cresConfigRuleArn = Lens.field @"configRuleArn"
{-# INLINEABLE cresConfigRuleArn #-}
{-# DEPRECATED configRuleArn "Use generic-lens or generic-optics with 'configRuleArn' instead"  #-}

-- | The ID of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresConfigRuleId :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.Text)
cresConfigRuleId = Lens.field @"configRuleId"
{-# INLINEABLE cresConfigRuleId #-}
{-# DEPRECATED configRuleId "Use generic-lens or generic-optics with 'configRuleId' instead"  #-}

-- | The name of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresConfigRuleName :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Types.ConfigRuleName)
cresConfigRuleName = Lens.field @"configRuleName"
{-# INLINEABLE cresConfigRuleName #-}
{-# DEPRECATED configRuleName "Use generic-lens or generic-optics with 'configRuleName' instead"  #-}

-- | The time that you first activated the AWS Config rule.
--
-- /Note:/ Consider using 'firstActivatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresFirstActivatedTime :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.NominalDiffTime)
cresFirstActivatedTime = Lens.field @"firstActivatedTime"
{-# INLINEABLE cresFirstActivatedTime #-}
{-# DEPRECATED firstActivatedTime "Use generic-lens or generic-optics with 'firstActivatedTime' instead"  #-}

-- | Indicates whether AWS Config has evaluated your resources against the rule at least once.
--
--
--     * @true@ - AWS Config has evaluated your AWS resources against the rule at least once.
--
--
--     * @false@ - AWS Config has not once finished evaluating your AWS resources against the rule.
--
--
--
-- /Note:/ Consider using 'firstEvaluationStarted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresFirstEvaluationStarted :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.Bool)
cresFirstEvaluationStarted = Lens.field @"firstEvaluationStarted"
{-# INLINEABLE cresFirstEvaluationStarted #-}
{-# DEPRECATED firstEvaluationStarted "Use generic-lens or generic-optics with 'firstEvaluationStarted' instead"  #-}

-- | The time that you last turned off the AWS Config rule.
--
-- /Note:/ Consider using 'lastDeactivatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresLastDeactivatedTime :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.NominalDiffTime)
cresLastDeactivatedTime = Lens.field @"lastDeactivatedTime"
{-# INLINEABLE cresLastDeactivatedTime #-}
{-# DEPRECATED lastDeactivatedTime "Use generic-lens or generic-optics with 'lastDeactivatedTime' instead"  #-}

-- | The error code that AWS Config returned when the rule last failed.
--
-- /Note:/ Consider using 'lastErrorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresLastErrorCode :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.Text)
cresLastErrorCode = Lens.field @"lastErrorCode"
{-# INLINEABLE cresLastErrorCode #-}
{-# DEPRECATED lastErrorCode "Use generic-lens or generic-optics with 'lastErrorCode' instead"  #-}

-- | The error message that AWS Config returned when the rule last failed.
--
-- /Note:/ Consider using 'lastErrorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresLastErrorMessage :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.Text)
cresLastErrorMessage = Lens.field @"lastErrorMessage"
{-# INLINEABLE cresLastErrorMessage #-}
{-# DEPRECATED lastErrorMessage "Use generic-lens or generic-optics with 'lastErrorMessage' instead"  #-}

-- | The time that AWS Config last failed to evaluate your AWS resources against the rule.
--
-- /Note:/ Consider using 'lastFailedEvaluationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresLastFailedEvaluationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.NominalDiffTime)
cresLastFailedEvaluationTime = Lens.field @"lastFailedEvaluationTime"
{-# INLINEABLE cresLastFailedEvaluationTime #-}
{-# DEPRECATED lastFailedEvaluationTime "Use generic-lens or generic-optics with 'lastFailedEvaluationTime' instead"  #-}

-- | The time that AWS Config last failed to invoke the AWS Config rule to evaluate your AWS resources.
--
-- /Note:/ Consider using 'lastFailedInvocationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresLastFailedInvocationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.NominalDiffTime)
cresLastFailedInvocationTime = Lens.field @"lastFailedInvocationTime"
{-# INLINEABLE cresLastFailedInvocationTime #-}
{-# DEPRECATED lastFailedInvocationTime "Use generic-lens or generic-optics with 'lastFailedInvocationTime' instead"  #-}

-- | The time that AWS Config last successfully evaluated your AWS resources against the rule.
--
-- /Note:/ Consider using 'lastSuccessfulEvaluationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresLastSuccessfulEvaluationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.NominalDiffTime)
cresLastSuccessfulEvaluationTime = Lens.field @"lastSuccessfulEvaluationTime"
{-# INLINEABLE cresLastSuccessfulEvaluationTime #-}
{-# DEPRECATED lastSuccessfulEvaluationTime "Use generic-lens or generic-optics with 'lastSuccessfulEvaluationTime' instead"  #-}

-- | The time that AWS Config last successfully invoked the AWS Config rule to evaluate your AWS resources.
--
-- /Note:/ Consider using 'lastSuccessfulInvocationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresLastSuccessfulInvocationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.NominalDiffTime)
cresLastSuccessfulInvocationTime = Lens.field @"lastSuccessfulInvocationTime"
{-# INLINEABLE cresLastSuccessfulInvocationTime #-}
{-# DEPRECATED lastSuccessfulInvocationTime "Use generic-lens or generic-optics with 'lastSuccessfulInvocationTime' instead"  #-}

instance Core.FromJSON ConfigRuleEvaluationStatus where
        parseJSON
          = Core.withObject "ConfigRuleEvaluationStatus" Core.$
              \ x ->
                ConfigRuleEvaluationStatus' Core.<$>
                  (x Core..:? "ConfigRuleArn") Core.<*> x Core..:? "ConfigRuleId"
                    Core.<*> x Core..:? "ConfigRuleName"
                    Core.<*> x Core..:? "FirstActivatedTime"
                    Core.<*> x Core..:? "FirstEvaluationStarted"
                    Core.<*> x Core..:? "LastDeactivatedTime"
                    Core.<*> x Core..:? "LastErrorCode"
                    Core.<*> x Core..:? "LastErrorMessage"
                    Core.<*> x Core..:? "LastFailedEvaluationTime"
                    Core.<*> x Core..:? "LastFailedInvocationTime"
                    Core.<*> x Core..:? "LastSuccessfulEvaluationTime"
                    Core.<*> x Core..:? "LastSuccessfulInvocationTime"
