{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DebugRuleEvaluationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DebugRuleEvaluationStatus
  ( DebugRuleEvaluationStatus (..),

    -- * Smart constructor
    mkDebugRuleEvaluationStatus,

    -- * Lenses
    dresLastModifiedTime,
    dresRuleConfigurationName,
    dresRuleEvaluationJobArn,
    dresRuleEvaluationStatus,
    dresStatusDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.RuleConfigurationName as Types
import qualified Network.AWS.SageMaker.Types.RuleEvaluationJobArn as Types
import qualified Network.AWS.SageMaker.Types.RuleEvaluationStatus as Types
import qualified Network.AWS.SageMaker.Types.StatusDetails as Types

-- | Information about the status of the rule evaluation.
--
-- /See:/ 'mkDebugRuleEvaluationStatus' smart constructor.
data DebugRuleEvaluationStatus = DebugRuleEvaluationStatus'
  { -- | Timestamp when the rule evaluation status was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the rule configuration
    ruleConfigurationName :: Core.Maybe Types.RuleConfigurationName,
    -- | The Amazon Resource Name (ARN) of the rule evaluation job.
    ruleEvaluationJobArn :: Core.Maybe Types.RuleEvaluationJobArn,
    -- | Status of the rule evaluation.
    ruleEvaluationStatus :: Core.Maybe Types.RuleEvaluationStatus,
    -- | Details from the rule evaluation.
    statusDetails :: Core.Maybe Types.StatusDetails
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DebugRuleEvaluationStatus' value with any optional fields omitted.
mkDebugRuleEvaluationStatus ::
  DebugRuleEvaluationStatus
mkDebugRuleEvaluationStatus =
  DebugRuleEvaluationStatus'
    { lastModifiedTime = Core.Nothing,
      ruleConfigurationName = Core.Nothing,
      ruleEvaluationJobArn = Core.Nothing,
      ruleEvaluationStatus = Core.Nothing,
      statusDetails = Core.Nothing
    }

-- | Timestamp when the rule evaluation status was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresLastModifiedTime :: Lens.Lens' DebugRuleEvaluationStatus (Core.Maybe Core.NominalDiffTime)
dresLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED dresLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the rule configuration
--
-- /Note:/ Consider using 'ruleConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresRuleConfigurationName :: Lens.Lens' DebugRuleEvaluationStatus (Core.Maybe Types.RuleConfigurationName)
dresRuleConfigurationName = Lens.field @"ruleConfigurationName"
{-# DEPRECATED dresRuleConfigurationName "Use generic-lens or generic-optics with 'ruleConfigurationName' instead." #-}

-- | The Amazon Resource Name (ARN) of the rule evaluation job.
--
-- /Note:/ Consider using 'ruleEvaluationJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresRuleEvaluationJobArn :: Lens.Lens' DebugRuleEvaluationStatus (Core.Maybe Types.RuleEvaluationJobArn)
dresRuleEvaluationJobArn = Lens.field @"ruleEvaluationJobArn"
{-# DEPRECATED dresRuleEvaluationJobArn "Use generic-lens or generic-optics with 'ruleEvaluationJobArn' instead." #-}

-- | Status of the rule evaluation.
--
-- /Note:/ Consider using 'ruleEvaluationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresRuleEvaluationStatus :: Lens.Lens' DebugRuleEvaluationStatus (Core.Maybe Types.RuleEvaluationStatus)
dresRuleEvaluationStatus = Lens.field @"ruleEvaluationStatus"
{-# DEPRECATED dresRuleEvaluationStatus "Use generic-lens or generic-optics with 'ruleEvaluationStatus' instead." #-}

-- | Details from the rule evaluation.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresStatusDetails :: Lens.Lens' DebugRuleEvaluationStatus (Core.Maybe Types.StatusDetails)
dresStatusDetails = Lens.field @"statusDetails"
{-# DEPRECATED dresStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

instance Core.FromJSON DebugRuleEvaluationStatus where
  parseJSON =
    Core.withObject "DebugRuleEvaluationStatus" Core.$
      \x ->
        DebugRuleEvaluationStatus'
          Core.<$> (x Core..:? "LastModifiedTime")
          Core.<*> (x Core..:? "RuleConfigurationName")
          Core.<*> (x Core..:? "RuleEvaluationJobArn")
          Core.<*> (x Core..:? "RuleEvaluationStatus")
          Core.<*> (x Core..:? "StatusDetails")
