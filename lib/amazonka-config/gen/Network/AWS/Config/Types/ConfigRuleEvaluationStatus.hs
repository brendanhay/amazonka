{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRuleEvaluationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigRuleEvaluationStatus
  ( ConfigRuleEvaluationStatus (..),

    -- * Smart constructor
    mkConfigRuleEvaluationStatus,

    -- * Lenses
    cresLastErrorCode,
    cresLastFailedEvaluationTime,
    cresFirstActivatedTime,
    cresLastSuccessfulEvaluationTime,
    cresLastDeactivatedTime,
    cresConfigRuleName,
    cresLastErrorMessage,
    cresConfigRuleId,
    cresLastFailedInvocationTime,
    cresFirstEvaluationStarted,
    cresLastSuccessfulInvocationTime,
    cresConfigRuleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Status information for your AWS managed Config rules. The status includes information such as the last time the rule ran, the last time it failed, and the related error for the last failure.
--
-- This action does not return status information about custom AWS Config rules.
--
-- /See:/ 'mkConfigRuleEvaluationStatus' smart constructor.
data ConfigRuleEvaluationStatus = ConfigRuleEvaluationStatus'
  { lastErrorCode ::
      Lude.Maybe Lude.Text,
    lastFailedEvaluationTime ::
      Lude.Maybe Lude.Timestamp,
    firstActivatedTime ::
      Lude.Maybe Lude.Timestamp,
    lastSuccessfulEvaluationTime ::
      Lude.Maybe Lude.Timestamp,
    lastDeactivatedTime ::
      Lude.Maybe Lude.Timestamp,
    configRuleName ::
      Lude.Maybe Lude.Text,
    lastErrorMessage ::
      Lude.Maybe Lude.Text,
    configRuleId :: Lude.Maybe Lude.Text,
    lastFailedInvocationTime ::
      Lude.Maybe Lude.Timestamp,
    firstEvaluationStarted ::
      Lude.Maybe Lude.Bool,
    lastSuccessfulInvocationTime ::
      Lude.Maybe Lude.Timestamp,
    configRuleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigRuleEvaluationStatus' with the minimum fields required to make a request.
--
-- * 'configRuleARN' - The Amazon Resource Name (ARN) of the AWS Config rule.
-- * 'configRuleId' - The ID of the AWS Config rule.
-- * 'configRuleName' - The name of the AWS Config rule.
-- * 'firstActivatedTime' - The time that you first activated the AWS Config rule.
-- * 'firstEvaluationStarted' - Indicates whether AWS Config has evaluated your resources against the rule at least once.
--
--
--     * @true@ - AWS Config has evaluated your AWS resources against the rule at least once.
--
--
--     * @false@ - AWS Config has not once finished evaluating your AWS resources against the rule.
--
--
-- * 'lastDeactivatedTime' - The time that you last turned off the AWS Config rule.
-- * 'lastErrorCode' - The error code that AWS Config returned when the rule last failed.
-- * 'lastErrorMessage' - The error message that AWS Config returned when the rule last failed.
-- * 'lastFailedEvaluationTime' - The time that AWS Config last failed to evaluate your AWS resources against the rule.
-- * 'lastFailedInvocationTime' - The time that AWS Config last failed to invoke the AWS Config rule to evaluate your AWS resources.
-- * 'lastSuccessfulEvaluationTime' - The time that AWS Config last successfully evaluated your AWS resources against the rule.
-- * 'lastSuccessfulInvocationTime' - The time that AWS Config last successfully invoked the AWS Config rule to evaluate your AWS resources.
mkConfigRuleEvaluationStatus ::
  ConfigRuleEvaluationStatus
mkConfigRuleEvaluationStatus =
  ConfigRuleEvaluationStatus'
    { lastErrorCode = Lude.Nothing,
      lastFailedEvaluationTime = Lude.Nothing,
      firstActivatedTime = Lude.Nothing,
      lastSuccessfulEvaluationTime = Lude.Nothing,
      lastDeactivatedTime = Lude.Nothing,
      configRuleName = Lude.Nothing,
      lastErrorMessage = Lude.Nothing,
      configRuleId = Lude.Nothing,
      lastFailedInvocationTime = Lude.Nothing,
      firstEvaluationStarted = Lude.Nothing,
      lastSuccessfulInvocationTime = Lude.Nothing,
      configRuleARN = Lude.Nothing
    }

-- | The error code that AWS Config returned when the rule last failed.
--
-- /Note:/ Consider using 'lastErrorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresLastErrorCode :: Lens.Lens' ConfigRuleEvaluationStatus (Lude.Maybe Lude.Text)
cresLastErrorCode = Lens.lens (lastErrorCode :: ConfigRuleEvaluationStatus -> Lude.Maybe Lude.Text) (\s a -> s {lastErrorCode = a} :: ConfigRuleEvaluationStatus)
{-# DEPRECATED cresLastErrorCode "Use generic-lens or generic-optics with 'lastErrorCode' instead." #-}

-- | The time that AWS Config last failed to evaluate your AWS resources against the rule.
--
-- /Note:/ Consider using 'lastFailedEvaluationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresLastFailedEvaluationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Lude.Maybe Lude.Timestamp)
cresLastFailedEvaluationTime = Lens.lens (lastFailedEvaluationTime :: ConfigRuleEvaluationStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastFailedEvaluationTime = a} :: ConfigRuleEvaluationStatus)
{-# DEPRECATED cresLastFailedEvaluationTime "Use generic-lens or generic-optics with 'lastFailedEvaluationTime' instead." #-}

-- | The time that you first activated the AWS Config rule.
--
-- /Note:/ Consider using 'firstActivatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresFirstActivatedTime :: Lens.Lens' ConfigRuleEvaluationStatus (Lude.Maybe Lude.Timestamp)
cresFirstActivatedTime = Lens.lens (firstActivatedTime :: ConfigRuleEvaluationStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {firstActivatedTime = a} :: ConfigRuleEvaluationStatus)
{-# DEPRECATED cresFirstActivatedTime "Use generic-lens or generic-optics with 'firstActivatedTime' instead." #-}

-- | The time that AWS Config last successfully evaluated your AWS resources against the rule.
--
-- /Note:/ Consider using 'lastSuccessfulEvaluationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresLastSuccessfulEvaluationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Lude.Maybe Lude.Timestamp)
cresLastSuccessfulEvaluationTime = Lens.lens (lastSuccessfulEvaluationTime :: ConfigRuleEvaluationStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastSuccessfulEvaluationTime = a} :: ConfigRuleEvaluationStatus)
{-# DEPRECATED cresLastSuccessfulEvaluationTime "Use generic-lens or generic-optics with 'lastSuccessfulEvaluationTime' instead." #-}

-- | The time that you last turned off the AWS Config rule.
--
-- /Note:/ Consider using 'lastDeactivatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresLastDeactivatedTime :: Lens.Lens' ConfigRuleEvaluationStatus (Lude.Maybe Lude.Timestamp)
cresLastDeactivatedTime = Lens.lens (lastDeactivatedTime :: ConfigRuleEvaluationStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastDeactivatedTime = a} :: ConfigRuleEvaluationStatus)
{-# DEPRECATED cresLastDeactivatedTime "Use generic-lens or generic-optics with 'lastDeactivatedTime' instead." #-}

-- | The name of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresConfigRuleName :: Lens.Lens' ConfigRuleEvaluationStatus (Lude.Maybe Lude.Text)
cresConfigRuleName = Lens.lens (configRuleName :: ConfigRuleEvaluationStatus -> Lude.Maybe Lude.Text) (\s a -> s {configRuleName = a} :: ConfigRuleEvaluationStatus)
{-# DEPRECATED cresConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | The error message that AWS Config returned when the rule last failed.
--
-- /Note:/ Consider using 'lastErrorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresLastErrorMessage :: Lens.Lens' ConfigRuleEvaluationStatus (Lude.Maybe Lude.Text)
cresLastErrorMessage = Lens.lens (lastErrorMessage :: ConfigRuleEvaluationStatus -> Lude.Maybe Lude.Text) (\s a -> s {lastErrorMessage = a} :: ConfigRuleEvaluationStatus)
{-# DEPRECATED cresLastErrorMessage "Use generic-lens or generic-optics with 'lastErrorMessage' instead." #-}

-- | The ID of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresConfigRuleId :: Lens.Lens' ConfigRuleEvaluationStatus (Lude.Maybe Lude.Text)
cresConfigRuleId = Lens.lens (configRuleId :: ConfigRuleEvaluationStatus -> Lude.Maybe Lude.Text) (\s a -> s {configRuleId = a} :: ConfigRuleEvaluationStatus)
{-# DEPRECATED cresConfigRuleId "Use generic-lens or generic-optics with 'configRuleId' instead." #-}

-- | The time that AWS Config last failed to invoke the AWS Config rule to evaluate your AWS resources.
--
-- /Note:/ Consider using 'lastFailedInvocationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresLastFailedInvocationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Lude.Maybe Lude.Timestamp)
cresLastFailedInvocationTime = Lens.lens (lastFailedInvocationTime :: ConfigRuleEvaluationStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastFailedInvocationTime = a} :: ConfigRuleEvaluationStatus)
{-# DEPRECATED cresLastFailedInvocationTime "Use generic-lens or generic-optics with 'lastFailedInvocationTime' instead." #-}

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
cresFirstEvaluationStarted :: Lens.Lens' ConfigRuleEvaluationStatus (Lude.Maybe Lude.Bool)
cresFirstEvaluationStarted = Lens.lens (firstEvaluationStarted :: ConfigRuleEvaluationStatus -> Lude.Maybe Lude.Bool) (\s a -> s {firstEvaluationStarted = a} :: ConfigRuleEvaluationStatus)
{-# DEPRECATED cresFirstEvaluationStarted "Use generic-lens or generic-optics with 'firstEvaluationStarted' instead." #-}

-- | The time that AWS Config last successfully invoked the AWS Config rule to evaluate your AWS resources.
--
-- /Note:/ Consider using 'lastSuccessfulInvocationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresLastSuccessfulInvocationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Lude.Maybe Lude.Timestamp)
cresLastSuccessfulInvocationTime = Lens.lens (lastSuccessfulInvocationTime :: ConfigRuleEvaluationStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastSuccessfulInvocationTime = a} :: ConfigRuleEvaluationStatus)
{-# DEPRECATED cresLastSuccessfulInvocationTime "Use generic-lens or generic-optics with 'lastSuccessfulInvocationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cresConfigRuleARN :: Lens.Lens' ConfigRuleEvaluationStatus (Lude.Maybe Lude.Text)
cresConfigRuleARN = Lens.lens (configRuleARN :: ConfigRuleEvaluationStatus -> Lude.Maybe Lude.Text) (\s a -> s {configRuleARN = a} :: ConfigRuleEvaluationStatus)
{-# DEPRECATED cresConfigRuleARN "Use generic-lens or generic-optics with 'configRuleARN' instead." #-}

instance Lude.FromJSON ConfigRuleEvaluationStatus where
  parseJSON =
    Lude.withObject
      "ConfigRuleEvaluationStatus"
      ( \x ->
          ConfigRuleEvaluationStatus'
            Lude.<$> (x Lude..:? "LastErrorCode")
            Lude.<*> (x Lude..:? "LastFailedEvaluationTime")
            Lude.<*> (x Lude..:? "FirstActivatedTime")
            Lude.<*> (x Lude..:? "LastSuccessfulEvaluationTime")
            Lude.<*> (x Lude..:? "LastDeactivatedTime")
            Lude.<*> (x Lude..:? "ConfigRuleName")
            Lude.<*> (x Lude..:? "LastErrorMessage")
            Lude.<*> (x Lude..:? "ConfigRuleId")
            Lude.<*> (x Lude..:? "LastFailedInvocationTime")
            Lude.<*> (x Lude..:? "FirstEvaluationStarted")
            Lude.<*> (x Lude..:? "LastSuccessfulInvocationTime")
            Lude.<*> (x Lude..:? "ConfigRuleArn")
      )
