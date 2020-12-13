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
    dresStatusDetails,
    dresRuleEvaluationStatus,
    dresRuleEvaluationJobARN,
    dresRuleConfigurationName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.RuleEvaluationStatus

-- | Information about the status of the rule evaluation.
--
-- /See:/ 'mkDebugRuleEvaluationStatus' smart constructor.
data DebugRuleEvaluationStatus = DebugRuleEvaluationStatus'
  { -- | Timestamp when the rule evaluation status was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | Details from the rule evaluation.
    statusDetails :: Lude.Maybe Lude.Text,
    -- | Status of the rule evaluation.
    ruleEvaluationStatus :: Lude.Maybe RuleEvaluationStatus,
    -- | The Amazon Resource Name (ARN) of the rule evaluation job.
    ruleEvaluationJobARN :: Lude.Maybe Lude.Text,
    -- | The name of the rule configuration
    ruleConfigurationName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DebugRuleEvaluationStatus' with the minimum fields required to make a request.
--
-- * 'lastModifiedTime' - Timestamp when the rule evaluation status was last modified.
-- * 'statusDetails' - Details from the rule evaluation.
-- * 'ruleEvaluationStatus' - Status of the rule evaluation.
-- * 'ruleEvaluationJobARN' - The Amazon Resource Name (ARN) of the rule evaluation job.
-- * 'ruleConfigurationName' - The name of the rule configuration
mkDebugRuleEvaluationStatus ::
  DebugRuleEvaluationStatus
mkDebugRuleEvaluationStatus =
  DebugRuleEvaluationStatus'
    { lastModifiedTime = Lude.Nothing,
      statusDetails = Lude.Nothing,
      ruleEvaluationStatus = Lude.Nothing,
      ruleEvaluationJobARN = Lude.Nothing,
      ruleConfigurationName = Lude.Nothing
    }

-- | Timestamp when the rule evaluation status was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresLastModifiedTime :: Lens.Lens' DebugRuleEvaluationStatus (Lude.Maybe Lude.Timestamp)
dresLastModifiedTime = Lens.lens (lastModifiedTime :: DebugRuleEvaluationStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DebugRuleEvaluationStatus)
{-# DEPRECATED dresLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | Details from the rule evaluation.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresStatusDetails :: Lens.Lens' DebugRuleEvaluationStatus (Lude.Maybe Lude.Text)
dresStatusDetails = Lens.lens (statusDetails :: DebugRuleEvaluationStatus -> Lude.Maybe Lude.Text) (\s a -> s {statusDetails = a} :: DebugRuleEvaluationStatus)
{-# DEPRECATED dresStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | Status of the rule evaluation.
--
-- /Note:/ Consider using 'ruleEvaluationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresRuleEvaluationStatus :: Lens.Lens' DebugRuleEvaluationStatus (Lude.Maybe RuleEvaluationStatus)
dresRuleEvaluationStatus = Lens.lens (ruleEvaluationStatus :: DebugRuleEvaluationStatus -> Lude.Maybe RuleEvaluationStatus) (\s a -> s {ruleEvaluationStatus = a} :: DebugRuleEvaluationStatus)
{-# DEPRECATED dresRuleEvaluationStatus "Use generic-lens or generic-optics with 'ruleEvaluationStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the rule evaluation job.
--
-- /Note:/ Consider using 'ruleEvaluationJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresRuleEvaluationJobARN :: Lens.Lens' DebugRuleEvaluationStatus (Lude.Maybe Lude.Text)
dresRuleEvaluationJobARN = Lens.lens (ruleEvaluationJobARN :: DebugRuleEvaluationStatus -> Lude.Maybe Lude.Text) (\s a -> s {ruleEvaluationJobARN = a} :: DebugRuleEvaluationStatus)
{-# DEPRECATED dresRuleEvaluationJobARN "Use generic-lens or generic-optics with 'ruleEvaluationJobARN' instead." #-}

-- | The name of the rule configuration
--
-- /Note:/ Consider using 'ruleConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresRuleConfigurationName :: Lens.Lens' DebugRuleEvaluationStatus (Lude.Maybe Lude.Text)
dresRuleConfigurationName = Lens.lens (ruleConfigurationName :: DebugRuleEvaluationStatus -> Lude.Maybe Lude.Text) (\s a -> s {ruleConfigurationName = a} :: DebugRuleEvaluationStatus)
{-# DEPRECATED dresRuleConfigurationName "Use generic-lens or generic-optics with 'ruleConfigurationName' instead." #-}

instance Lude.FromJSON DebugRuleEvaluationStatus where
  parseJSON =
    Lude.withObject
      "DebugRuleEvaluationStatus"
      ( \x ->
          DebugRuleEvaluationStatus'
            Lude.<$> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "StatusDetails")
            Lude.<*> (x Lude..:? "RuleEvaluationStatus")
            Lude.<*> (x Lude..:? "RuleEvaluationJobArn")
            Lude.<*> (x Lude..:? "RuleConfigurationName")
      )
