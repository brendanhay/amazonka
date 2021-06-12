{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRuleEvaluationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigRuleEvaluationStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Status information for your AWS managed Config rules. The status
-- includes information such as the last time the rule ran, the last time
-- it failed, and the related error for the last failure.
--
-- This action does not return status information about custom AWS Config
-- rules.
--
-- /See:/ 'newConfigRuleEvaluationStatus' smart constructor.
data ConfigRuleEvaluationStatus = ConfigRuleEvaluationStatus'
  { -- | The error message that AWS Config returned when the rule last failed.
    lastErrorMessage :: Core.Maybe Core.Text,
    -- | The ID of the AWS Config rule.
    configRuleId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Config rule.
    configRuleArn :: Core.Maybe Core.Text,
    -- | The name of the AWS Config rule.
    configRuleName :: Core.Maybe Core.Text,
    -- | Indicates whether AWS Config has evaluated your resources against the
    -- rule at least once.
    --
    -- -   @true@ - AWS Config has evaluated your AWS resources against the
    --     rule at least once.
    --
    -- -   @false@ - AWS Config has not once finished evaluating your AWS
    --     resources against the rule.
    firstEvaluationStarted :: Core.Maybe Core.Bool,
    -- | The time that AWS Config last failed to evaluate your AWS resources
    -- against the rule.
    lastFailedEvaluationTime :: Core.Maybe Core.POSIX,
    -- | The time that you first activated the AWS Config rule.
    firstActivatedTime :: Core.Maybe Core.POSIX,
    -- | The error code that AWS Config returned when the rule last failed.
    lastErrorCode :: Core.Maybe Core.Text,
    -- | The time that AWS Config last failed to invoke the AWS Config rule to
    -- evaluate your AWS resources.
    lastFailedInvocationTime :: Core.Maybe Core.POSIX,
    -- | The time that AWS Config last successfully invoked the AWS Config rule
    -- to evaluate your AWS resources.
    lastSuccessfulInvocationTime :: Core.Maybe Core.POSIX,
    -- | The time that you last turned off the AWS Config rule.
    lastDeactivatedTime :: Core.Maybe Core.POSIX,
    -- | The time that AWS Config last successfully evaluated your AWS resources
    -- against the rule.
    lastSuccessfulEvaluationTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfigRuleEvaluationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastErrorMessage', 'configRuleEvaluationStatus_lastErrorMessage' - The error message that AWS Config returned when the rule last failed.
--
-- 'configRuleId', 'configRuleEvaluationStatus_configRuleId' - The ID of the AWS Config rule.
--
-- 'configRuleArn', 'configRuleEvaluationStatus_configRuleArn' - The Amazon Resource Name (ARN) of the AWS Config rule.
--
-- 'configRuleName', 'configRuleEvaluationStatus_configRuleName' - The name of the AWS Config rule.
--
-- 'firstEvaluationStarted', 'configRuleEvaluationStatus_firstEvaluationStarted' - Indicates whether AWS Config has evaluated your resources against the
-- rule at least once.
--
-- -   @true@ - AWS Config has evaluated your AWS resources against the
--     rule at least once.
--
-- -   @false@ - AWS Config has not once finished evaluating your AWS
--     resources against the rule.
--
-- 'lastFailedEvaluationTime', 'configRuleEvaluationStatus_lastFailedEvaluationTime' - The time that AWS Config last failed to evaluate your AWS resources
-- against the rule.
--
-- 'firstActivatedTime', 'configRuleEvaluationStatus_firstActivatedTime' - The time that you first activated the AWS Config rule.
--
-- 'lastErrorCode', 'configRuleEvaluationStatus_lastErrorCode' - The error code that AWS Config returned when the rule last failed.
--
-- 'lastFailedInvocationTime', 'configRuleEvaluationStatus_lastFailedInvocationTime' - The time that AWS Config last failed to invoke the AWS Config rule to
-- evaluate your AWS resources.
--
-- 'lastSuccessfulInvocationTime', 'configRuleEvaluationStatus_lastSuccessfulInvocationTime' - The time that AWS Config last successfully invoked the AWS Config rule
-- to evaluate your AWS resources.
--
-- 'lastDeactivatedTime', 'configRuleEvaluationStatus_lastDeactivatedTime' - The time that you last turned off the AWS Config rule.
--
-- 'lastSuccessfulEvaluationTime', 'configRuleEvaluationStatus_lastSuccessfulEvaluationTime' - The time that AWS Config last successfully evaluated your AWS resources
-- against the rule.
newConfigRuleEvaluationStatus ::
  ConfigRuleEvaluationStatus
newConfigRuleEvaluationStatus =
  ConfigRuleEvaluationStatus'
    { lastErrorMessage =
        Core.Nothing,
      configRuleId = Core.Nothing,
      configRuleArn = Core.Nothing,
      configRuleName = Core.Nothing,
      firstEvaluationStarted = Core.Nothing,
      lastFailedEvaluationTime = Core.Nothing,
      firstActivatedTime = Core.Nothing,
      lastErrorCode = Core.Nothing,
      lastFailedInvocationTime = Core.Nothing,
      lastSuccessfulInvocationTime = Core.Nothing,
      lastDeactivatedTime = Core.Nothing,
      lastSuccessfulEvaluationTime = Core.Nothing
    }

-- | The error message that AWS Config returned when the rule last failed.
configRuleEvaluationStatus_lastErrorMessage :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.Text)
configRuleEvaluationStatus_lastErrorMessage = Lens.lens (\ConfigRuleEvaluationStatus' {lastErrorMessage} -> lastErrorMessage) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastErrorMessage = a} :: ConfigRuleEvaluationStatus)

-- | The ID of the AWS Config rule.
configRuleEvaluationStatus_configRuleId :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.Text)
configRuleEvaluationStatus_configRuleId = Lens.lens (\ConfigRuleEvaluationStatus' {configRuleId} -> configRuleId) (\s@ConfigRuleEvaluationStatus' {} a -> s {configRuleId = a} :: ConfigRuleEvaluationStatus)

-- | The Amazon Resource Name (ARN) of the AWS Config rule.
configRuleEvaluationStatus_configRuleArn :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.Text)
configRuleEvaluationStatus_configRuleArn = Lens.lens (\ConfigRuleEvaluationStatus' {configRuleArn} -> configRuleArn) (\s@ConfigRuleEvaluationStatus' {} a -> s {configRuleArn = a} :: ConfigRuleEvaluationStatus)

-- | The name of the AWS Config rule.
configRuleEvaluationStatus_configRuleName :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.Text)
configRuleEvaluationStatus_configRuleName = Lens.lens (\ConfigRuleEvaluationStatus' {configRuleName} -> configRuleName) (\s@ConfigRuleEvaluationStatus' {} a -> s {configRuleName = a} :: ConfigRuleEvaluationStatus)

-- | Indicates whether AWS Config has evaluated your resources against the
-- rule at least once.
--
-- -   @true@ - AWS Config has evaluated your AWS resources against the
--     rule at least once.
--
-- -   @false@ - AWS Config has not once finished evaluating your AWS
--     resources against the rule.
configRuleEvaluationStatus_firstEvaluationStarted :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.Bool)
configRuleEvaluationStatus_firstEvaluationStarted = Lens.lens (\ConfigRuleEvaluationStatus' {firstEvaluationStarted} -> firstEvaluationStarted) (\s@ConfigRuleEvaluationStatus' {} a -> s {firstEvaluationStarted = a} :: ConfigRuleEvaluationStatus)

-- | The time that AWS Config last failed to evaluate your AWS resources
-- against the rule.
configRuleEvaluationStatus_lastFailedEvaluationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.UTCTime)
configRuleEvaluationStatus_lastFailedEvaluationTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastFailedEvaluationTime} -> lastFailedEvaluationTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastFailedEvaluationTime = a} :: ConfigRuleEvaluationStatus) Core.. Lens.mapping Core._Time

-- | The time that you first activated the AWS Config rule.
configRuleEvaluationStatus_firstActivatedTime :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.UTCTime)
configRuleEvaluationStatus_firstActivatedTime = Lens.lens (\ConfigRuleEvaluationStatus' {firstActivatedTime} -> firstActivatedTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {firstActivatedTime = a} :: ConfigRuleEvaluationStatus) Core.. Lens.mapping Core._Time

-- | The error code that AWS Config returned when the rule last failed.
configRuleEvaluationStatus_lastErrorCode :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.Text)
configRuleEvaluationStatus_lastErrorCode = Lens.lens (\ConfigRuleEvaluationStatus' {lastErrorCode} -> lastErrorCode) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastErrorCode = a} :: ConfigRuleEvaluationStatus)

-- | The time that AWS Config last failed to invoke the AWS Config rule to
-- evaluate your AWS resources.
configRuleEvaluationStatus_lastFailedInvocationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.UTCTime)
configRuleEvaluationStatus_lastFailedInvocationTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastFailedInvocationTime} -> lastFailedInvocationTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastFailedInvocationTime = a} :: ConfigRuleEvaluationStatus) Core.. Lens.mapping Core._Time

-- | The time that AWS Config last successfully invoked the AWS Config rule
-- to evaluate your AWS resources.
configRuleEvaluationStatus_lastSuccessfulInvocationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.UTCTime)
configRuleEvaluationStatus_lastSuccessfulInvocationTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastSuccessfulInvocationTime} -> lastSuccessfulInvocationTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastSuccessfulInvocationTime = a} :: ConfigRuleEvaluationStatus) Core.. Lens.mapping Core._Time

-- | The time that you last turned off the AWS Config rule.
configRuleEvaluationStatus_lastDeactivatedTime :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.UTCTime)
configRuleEvaluationStatus_lastDeactivatedTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastDeactivatedTime} -> lastDeactivatedTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastDeactivatedTime = a} :: ConfigRuleEvaluationStatus) Core.. Lens.mapping Core._Time

-- | The time that AWS Config last successfully evaluated your AWS resources
-- against the rule.
configRuleEvaluationStatus_lastSuccessfulEvaluationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Core.Maybe Core.UTCTime)
configRuleEvaluationStatus_lastSuccessfulEvaluationTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastSuccessfulEvaluationTime} -> lastSuccessfulEvaluationTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastSuccessfulEvaluationTime = a} :: ConfigRuleEvaluationStatus) Core.. Lens.mapping Core._Time

instance Core.FromJSON ConfigRuleEvaluationStatus where
  parseJSON =
    Core.withObject
      "ConfigRuleEvaluationStatus"
      ( \x ->
          ConfigRuleEvaluationStatus'
            Core.<$> (x Core..:? "LastErrorMessage")
            Core.<*> (x Core..:? "ConfigRuleId")
            Core.<*> (x Core..:? "ConfigRuleArn")
            Core.<*> (x Core..:? "ConfigRuleName")
            Core.<*> (x Core..:? "FirstEvaluationStarted")
            Core.<*> (x Core..:? "LastFailedEvaluationTime")
            Core.<*> (x Core..:? "FirstActivatedTime")
            Core.<*> (x Core..:? "LastErrorCode")
            Core.<*> (x Core..:? "LastFailedInvocationTime")
            Core.<*> (x Core..:? "LastSuccessfulInvocationTime")
            Core.<*> (x Core..:? "LastDeactivatedTime")
            Core.<*> (x Core..:? "LastSuccessfulEvaluationTime")
      )

instance Core.Hashable ConfigRuleEvaluationStatus

instance Core.NFData ConfigRuleEvaluationStatus
