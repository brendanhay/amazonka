{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    lastErrorMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AWS Config rule.
    configRuleId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Config rule.
    configRuleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the AWS Config rule.
    configRuleName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether AWS Config has evaluated your resources against the
    -- rule at least once.
    --
    -- -   @true@ - AWS Config has evaluated your AWS resources against the
    --     rule at least once.
    --
    -- -   @false@ - AWS Config has not once finished evaluating your AWS
    --     resources against the rule.
    firstEvaluationStarted :: Prelude.Maybe Prelude.Bool,
    -- | The time that AWS Config last failed to evaluate your AWS resources
    -- against the rule.
    lastFailedEvaluationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time that you first activated the AWS Config rule.
    firstActivatedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The error code that AWS Config returned when the rule last failed.
    lastErrorCode :: Prelude.Maybe Prelude.Text,
    -- | The time that AWS Config last failed to invoke the AWS Config rule to
    -- evaluate your AWS resources.
    lastFailedInvocationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time that AWS Config last successfully invoked the AWS Config rule
    -- to evaluate your AWS resources.
    lastSuccessfulInvocationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time that you last turned off the AWS Config rule.
    lastDeactivatedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time that AWS Config last successfully evaluated your AWS resources
    -- against the rule.
    lastSuccessfulEvaluationTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      configRuleId = Prelude.Nothing,
      configRuleArn = Prelude.Nothing,
      configRuleName = Prelude.Nothing,
      firstEvaluationStarted = Prelude.Nothing,
      lastFailedEvaluationTime = Prelude.Nothing,
      firstActivatedTime = Prelude.Nothing,
      lastErrorCode = Prelude.Nothing,
      lastFailedInvocationTime = Prelude.Nothing,
      lastSuccessfulInvocationTime = Prelude.Nothing,
      lastDeactivatedTime = Prelude.Nothing,
      lastSuccessfulEvaluationTime = Prelude.Nothing
    }

-- | The error message that AWS Config returned when the rule last failed.
configRuleEvaluationStatus_lastErrorMessage :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
configRuleEvaluationStatus_lastErrorMessage = Lens.lens (\ConfigRuleEvaluationStatus' {lastErrorMessage} -> lastErrorMessage) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastErrorMessage = a} :: ConfigRuleEvaluationStatus)

-- | The ID of the AWS Config rule.
configRuleEvaluationStatus_configRuleId :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
configRuleEvaluationStatus_configRuleId = Lens.lens (\ConfigRuleEvaluationStatus' {configRuleId} -> configRuleId) (\s@ConfigRuleEvaluationStatus' {} a -> s {configRuleId = a} :: ConfigRuleEvaluationStatus)

-- | The Amazon Resource Name (ARN) of the AWS Config rule.
configRuleEvaluationStatus_configRuleArn :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
configRuleEvaluationStatus_configRuleArn = Lens.lens (\ConfigRuleEvaluationStatus' {configRuleArn} -> configRuleArn) (\s@ConfigRuleEvaluationStatus' {} a -> s {configRuleArn = a} :: ConfigRuleEvaluationStatus)

-- | The name of the AWS Config rule.
configRuleEvaluationStatus_configRuleName :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
configRuleEvaluationStatus_configRuleName = Lens.lens (\ConfigRuleEvaluationStatus' {configRuleName} -> configRuleName) (\s@ConfigRuleEvaluationStatus' {} a -> s {configRuleName = a} :: ConfigRuleEvaluationStatus)

-- | Indicates whether AWS Config has evaluated your resources against the
-- rule at least once.
--
-- -   @true@ - AWS Config has evaluated your AWS resources against the
--     rule at least once.
--
-- -   @false@ - AWS Config has not once finished evaluating your AWS
--     resources against the rule.
configRuleEvaluationStatus_firstEvaluationStarted :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Bool)
configRuleEvaluationStatus_firstEvaluationStarted = Lens.lens (\ConfigRuleEvaluationStatus' {firstEvaluationStarted} -> firstEvaluationStarted) (\s@ConfigRuleEvaluationStatus' {} a -> s {firstEvaluationStarted = a} :: ConfigRuleEvaluationStatus)

-- | The time that AWS Config last failed to evaluate your AWS resources
-- against the rule.
configRuleEvaluationStatus_lastFailedEvaluationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
configRuleEvaluationStatus_lastFailedEvaluationTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastFailedEvaluationTime} -> lastFailedEvaluationTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastFailedEvaluationTime = a} :: ConfigRuleEvaluationStatus) Prelude.. Lens.mapping Prelude._Time

-- | The time that you first activated the AWS Config rule.
configRuleEvaluationStatus_firstActivatedTime :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
configRuleEvaluationStatus_firstActivatedTime = Lens.lens (\ConfigRuleEvaluationStatus' {firstActivatedTime} -> firstActivatedTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {firstActivatedTime = a} :: ConfigRuleEvaluationStatus) Prelude.. Lens.mapping Prelude._Time

-- | The error code that AWS Config returned when the rule last failed.
configRuleEvaluationStatus_lastErrorCode :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
configRuleEvaluationStatus_lastErrorCode = Lens.lens (\ConfigRuleEvaluationStatus' {lastErrorCode} -> lastErrorCode) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastErrorCode = a} :: ConfigRuleEvaluationStatus)

-- | The time that AWS Config last failed to invoke the AWS Config rule to
-- evaluate your AWS resources.
configRuleEvaluationStatus_lastFailedInvocationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
configRuleEvaluationStatus_lastFailedInvocationTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastFailedInvocationTime} -> lastFailedInvocationTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastFailedInvocationTime = a} :: ConfigRuleEvaluationStatus) Prelude.. Lens.mapping Prelude._Time

-- | The time that AWS Config last successfully invoked the AWS Config rule
-- to evaluate your AWS resources.
configRuleEvaluationStatus_lastSuccessfulInvocationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
configRuleEvaluationStatus_lastSuccessfulInvocationTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastSuccessfulInvocationTime} -> lastSuccessfulInvocationTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastSuccessfulInvocationTime = a} :: ConfigRuleEvaluationStatus) Prelude.. Lens.mapping Prelude._Time

-- | The time that you last turned off the AWS Config rule.
configRuleEvaluationStatus_lastDeactivatedTime :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
configRuleEvaluationStatus_lastDeactivatedTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastDeactivatedTime} -> lastDeactivatedTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastDeactivatedTime = a} :: ConfigRuleEvaluationStatus) Prelude.. Lens.mapping Prelude._Time

-- | The time that AWS Config last successfully evaluated your AWS resources
-- against the rule.
configRuleEvaluationStatus_lastSuccessfulEvaluationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
configRuleEvaluationStatus_lastSuccessfulEvaluationTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastSuccessfulEvaluationTime} -> lastSuccessfulEvaluationTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastSuccessfulEvaluationTime = a} :: ConfigRuleEvaluationStatus) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON ConfigRuleEvaluationStatus where
  parseJSON =
    Prelude.withObject
      "ConfigRuleEvaluationStatus"
      ( \x ->
          ConfigRuleEvaluationStatus'
            Prelude.<$> (x Prelude..:? "LastErrorMessage")
            Prelude.<*> (x Prelude..:? "ConfigRuleId")
            Prelude.<*> (x Prelude..:? "ConfigRuleArn")
            Prelude.<*> (x Prelude..:? "ConfigRuleName")
            Prelude.<*> (x Prelude..:? "FirstEvaluationStarted")
            Prelude.<*> (x Prelude..:? "LastFailedEvaluationTime")
            Prelude.<*> (x Prelude..:? "FirstActivatedTime")
            Prelude.<*> (x Prelude..:? "LastErrorCode")
            Prelude.<*> (x Prelude..:? "LastFailedInvocationTime")
            Prelude.<*> (x Prelude..:? "LastSuccessfulInvocationTime")
            Prelude.<*> (x Prelude..:? "LastDeactivatedTime")
            Prelude.<*> (x Prelude..:? "LastSuccessfulEvaluationTime")
      )

instance Prelude.Hashable ConfigRuleEvaluationStatus

instance Prelude.NFData ConfigRuleEvaluationStatus
