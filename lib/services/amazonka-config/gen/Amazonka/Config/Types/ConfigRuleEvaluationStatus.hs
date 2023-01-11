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
-- Module      : Amazonka.Config.Types.ConfigRuleEvaluationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConfigRuleEvaluationStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Status information for your Config Managed rules and Config Custom
-- Policy rules. The status includes information such as the last time the
-- rule ran, the last time it failed, and the related error for the last
-- failure.
--
-- This action does not return status information about Config Custom
-- Lambda rules.
--
-- /See:/ 'newConfigRuleEvaluationStatus' smart constructor.
data ConfigRuleEvaluationStatus = ConfigRuleEvaluationStatus'
  { -- | The Amazon Resource Name (ARN) of the Config rule.
    configRuleArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Config rule.
    configRuleId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Config rule.
    configRuleName :: Prelude.Maybe Prelude.Text,
    -- | The time that you first activated the Config rule.
    firstActivatedTime :: Prelude.Maybe Data.POSIX,
    -- | Indicates whether Config has evaluated your resources against the rule
    -- at least once.
    --
    -- -   @true@ - Config has evaluated your Amazon Web Services resources
    --     against the rule at least once.
    --
    -- -   @false@ - Config has not finished evaluating your Amazon Web
    --     Services resources against the rule at least once.
    firstEvaluationStarted :: Prelude.Maybe Prelude.Bool,
    -- | The time that you last turned off the Config rule.
    lastDeactivatedTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the last attempted delivery of a debug log for your Config
    -- Custom Policy rules. Either @Successful@ or @Failed@.
    lastDebugLogDeliveryStatus :: Prelude.Maybe Prelude.Text,
    -- | The reason Config was not able to deliver a debug log. This is for the
    -- last failed attempt to retrieve a debug log for your Config Custom
    -- Policy rules.
    lastDebugLogDeliveryStatusReason :: Prelude.Maybe Prelude.Text,
    -- | The time Config last attempted to deliver a debug log for your Config
    -- Custom Policy rules.
    lastDebugLogDeliveryTime :: Prelude.Maybe Data.POSIX,
    -- | The error code that Config returned when the rule last failed.
    lastErrorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message that Config returned when the rule last failed.
    lastErrorMessage :: Prelude.Maybe Prelude.Text,
    -- | The time that Config last failed to evaluate your Amazon Web Services
    -- resources against the rule.
    lastFailedEvaluationTime :: Prelude.Maybe Data.POSIX,
    -- | The time that Config last failed to invoke the Config rule to evaluate
    -- your Amazon Web Services resources.
    lastFailedInvocationTime :: Prelude.Maybe Data.POSIX,
    -- | The time that Config last successfully evaluated your Amazon Web
    -- Services resources against the rule.
    lastSuccessfulEvaluationTime :: Prelude.Maybe Data.POSIX,
    -- | The time that Config last successfully invoked the Config rule to
    -- evaluate your Amazon Web Services resources.
    lastSuccessfulInvocationTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigRuleEvaluationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleArn', 'configRuleEvaluationStatus_configRuleArn' - The Amazon Resource Name (ARN) of the Config rule.
--
-- 'configRuleId', 'configRuleEvaluationStatus_configRuleId' - The ID of the Config rule.
--
-- 'configRuleName', 'configRuleEvaluationStatus_configRuleName' - The name of the Config rule.
--
-- 'firstActivatedTime', 'configRuleEvaluationStatus_firstActivatedTime' - The time that you first activated the Config rule.
--
-- 'firstEvaluationStarted', 'configRuleEvaluationStatus_firstEvaluationStarted' - Indicates whether Config has evaluated your resources against the rule
-- at least once.
--
-- -   @true@ - Config has evaluated your Amazon Web Services resources
--     against the rule at least once.
--
-- -   @false@ - Config has not finished evaluating your Amazon Web
--     Services resources against the rule at least once.
--
-- 'lastDeactivatedTime', 'configRuleEvaluationStatus_lastDeactivatedTime' - The time that you last turned off the Config rule.
--
-- 'lastDebugLogDeliveryStatus', 'configRuleEvaluationStatus_lastDebugLogDeliveryStatus' - The status of the last attempted delivery of a debug log for your Config
-- Custom Policy rules. Either @Successful@ or @Failed@.
--
-- 'lastDebugLogDeliveryStatusReason', 'configRuleEvaluationStatus_lastDebugLogDeliveryStatusReason' - The reason Config was not able to deliver a debug log. This is for the
-- last failed attempt to retrieve a debug log for your Config Custom
-- Policy rules.
--
-- 'lastDebugLogDeliveryTime', 'configRuleEvaluationStatus_lastDebugLogDeliveryTime' - The time Config last attempted to deliver a debug log for your Config
-- Custom Policy rules.
--
-- 'lastErrorCode', 'configRuleEvaluationStatus_lastErrorCode' - The error code that Config returned when the rule last failed.
--
-- 'lastErrorMessage', 'configRuleEvaluationStatus_lastErrorMessage' - The error message that Config returned when the rule last failed.
--
-- 'lastFailedEvaluationTime', 'configRuleEvaluationStatus_lastFailedEvaluationTime' - The time that Config last failed to evaluate your Amazon Web Services
-- resources against the rule.
--
-- 'lastFailedInvocationTime', 'configRuleEvaluationStatus_lastFailedInvocationTime' - The time that Config last failed to invoke the Config rule to evaluate
-- your Amazon Web Services resources.
--
-- 'lastSuccessfulEvaluationTime', 'configRuleEvaluationStatus_lastSuccessfulEvaluationTime' - The time that Config last successfully evaluated your Amazon Web
-- Services resources against the rule.
--
-- 'lastSuccessfulInvocationTime', 'configRuleEvaluationStatus_lastSuccessfulInvocationTime' - The time that Config last successfully invoked the Config rule to
-- evaluate your Amazon Web Services resources.
newConfigRuleEvaluationStatus ::
  ConfigRuleEvaluationStatus
newConfigRuleEvaluationStatus =
  ConfigRuleEvaluationStatus'
    { configRuleArn =
        Prelude.Nothing,
      configRuleId = Prelude.Nothing,
      configRuleName = Prelude.Nothing,
      firstActivatedTime = Prelude.Nothing,
      firstEvaluationStarted = Prelude.Nothing,
      lastDeactivatedTime = Prelude.Nothing,
      lastDebugLogDeliveryStatus = Prelude.Nothing,
      lastDebugLogDeliveryStatusReason =
        Prelude.Nothing,
      lastDebugLogDeliveryTime = Prelude.Nothing,
      lastErrorCode = Prelude.Nothing,
      lastErrorMessage = Prelude.Nothing,
      lastFailedEvaluationTime = Prelude.Nothing,
      lastFailedInvocationTime = Prelude.Nothing,
      lastSuccessfulEvaluationTime = Prelude.Nothing,
      lastSuccessfulInvocationTime = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Config rule.
configRuleEvaluationStatus_configRuleArn :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
configRuleEvaluationStatus_configRuleArn = Lens.lens (\ConfigRuleEvaluationStatus' {configRuleArn} -> configRuleArn) (\s@ConfigRuleEvaluationStatus' {} a -> s {configRuleArn = a} :: ConfigRuleEvaluationStatus)

-- | The ID of the Config rule.
configRuleEvaluationStatus_configRuleId :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
configRuleEvaluationStatus_configRuleId = Lens.lens (\ConfigRuleEvaluationStatus' {configRuleId} -> configRuleId) (\s@ConfigRuleEvaluationStatus' {} a -> s {configRuleId = a} :: ConfigRuleEvaluationStatus)

-- | The name of the Config rule.
configRuleEvaluationStatus_configRuleName :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
configRuleEvaluationStatus_configRuleName = Lens.lens (\ConfigRuleEvaluationStatus' {configRuleName} -> configRuleName) (\s@ConfigRuleEvaluationStatus' {} a -> s {configRuleName = a} :: ConfigRuleEvaluationStatus)

-- | The time that you first activated the Config rule.
configRuleEvaluationStatus_firstActivatedTime :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
configRuleEvaluationStatus_firstActivatedTime = Lens.lens (\ConfigRuleEvaluationStatus' {firstActivatedTime} -> firstActivatedTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {firstActivatedTime = a} :: ConfigRuleEvaluationStatus) Prelude.. Lens.mapping Data._Time

-- | Indicates whether Config has evaluated your resources against the rule
-- at least once.
--
-- -   @true@ - Config has evaluated your Amazon Web Services resources
--     against the rule at least once.
--
-- -   @false@ - Config has not finished evaluating your Amazon Web
--     Services resources against the rule at least once.
configRuleEvaluationStatus_firstEvaluationStarted :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Bool)
configRuleEvaluationStatus_firstEvaluationStarted = Lens.lens (\ConfigRuleEvaluationStatus' {firstEvaluationStarted} -> firstEvaluationStarted) (\s@ConfigRuleEvaluationStatus' {} a -> s {firstEvaluationStarted = a} :: ConfigRuleEvaluationStatus)

-- | The time that you last turned off the Config rule.
configRuleEvaluationStatus_lastDeactivatedTime :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
configRuleEvaluationStatus_lastDeactivatedTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastDeactivatedTime} -> lastDeactivatedTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastDeactivatedTime = a} :: ConfigRuleEvaluationStatus) Prelude.. Lens.mapping Data._Time

-- | The status of the last attempted delivery of a debug log for your Config
-- Custom Policy rules. Either @Successful@ or @Failed@.
configRuleEvaluationStatus_lastDebugLogDeliveryStatus :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
configRuleEvaluationStatus_lastDebugLogDeliveryStatus = Lens.lens (\ConfigRuleEvaluationStatus' {lastDebugLogDeliveryStatus} -> lastDebugLogDeliveryStatus) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastDebugLogDeliveryStatus = a} :: ConfigRuleEvaluationStatus)

-- | The reason Config was not able to deliver a debug log. This is for the
-- last failed attempt to retrieve a debug log for your Config Custom
-- Policy rules.
configRuleEvaluationStatus_lastDebugLogDeliveryStatusReason :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
configRuleEvaluationStatus_lastDebugLogDeliveryStatusReason = Lens.lens (\ConfigRuleEvaluationStatus' {lastDebugLogDeliveryStatusReason} -> lastDebugLogDeliveryStatusReason) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastDebugLogDeliveryStatusReason = a} :: ConfigRuleEvaluationStatus)

-- | The time Config last attempted to deliver a debug log for your Config
-- Custom Policy rules.
configRuleEvaluationStatus_lastDebugLogDeliveryTime :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
configRuleEvaluationStatus_lastDebugLogDeliveryTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastDebugLogDeliveryTime} -> lastDebugLogDeliveryTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastDebugLogDeliveryTime = a} :: ConfigRuleEvaluationStatus) Prelude.. Lens.mapping Data._Time

-- | The error code that Config returned when the rule last failed.
configRuleEvaluationStatus_lastErrorCode :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
configRuleEvaluationStatus_lastErrorCode = Lens.lens (\ConfigRuleEvaluationStatus' {lastErrorCode} -> lastErrorCode) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastErrorCode = a} :: ConfigRuleEvaluationStatus)

-- | The error message that Config returned when the rule last failed.
configRuleEvaluationStatus_lastErrorMessage :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
configRuleEvaluationStatus_lastErrorMessage = Lens.lens (\ConfigRuleEvaluationStatus' {lastErrorMessage} -> lastErrorMessage) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastErrorMessage = a} :: ConfigRuleEvaluationStatus)

-- | The time that Config last failed to evaluate your Amazon Web Services
-- resources against the rule.
configRuleEvaluationStatus_lastFailedEvaluationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
configRuleEvaluationStatus_lastFailedEvaluationTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastFailedEvaluationTime} -> lastFailedEvaluationTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastFailedEvaluationTime = a} :: ConfigRuleEvaluationStatus) Prelude.. Lens.mapping Data._Time

-- | The time that Config last failed to invoke the Config rule to evaluate
-- your Amazon Web Services resources.
configRuleEvaluationStatus_lastFailedInvocationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
configRuleEvaluationStatus_lastFailedInvocationTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastFailedInvocationTime} -> lastFailedInvocationTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastFailedInvocationTime = a} :: ConfigRuleEvaluationStatus) Prelude.. Lens.mapping Data._Time

-- | The time that Config last successfully evaluated your Amazon Web
-- Services resources against the rule.
configRuleEvaluationStatus_lastSuccessfulEvaluationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
configRuleEvaluationStatus_lastSuccessfulEvaluationTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastSuccessfulEvaluationTime} -> lastSuccessfulEvaluationTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastSuccessfulEvaluationTime = a} :: ConfigRuleEvaluationStatus) Prelude.. Lens.mapping Data._Time

-- | The time that Config last successfully invoked the Config rule to
-- evaluate your Amazon Web Services resources.
configRuleEvaluationStatus_lastSuccessfulInvocationTime :: Lens.Lens' ConfigRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
configRuleEvaluationStatus_lastSuccessfulInvocationTime = Lens.lens (\ConfigRuleEvaluationStatus' {lastSuccessfulInvocationTime} -> lastSuccessfulInvocationTime) (\s@ConfigRuleEvaluationStatus' {} a -> s {lastSuccessfulInvocationTime = a} :: ConfigRuleEvaluationStatus) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ConfigRuleEvaluationStatus where
  parseJSON =
    Data.withObject
      "ConfigRuleEvaluationStatus"
      ( \x ->
          ConfigRuleEvaluationStatus'
            Prelude.<$> (x Data..:? "ConfigRuleArn")
            Prelude.<*> (x Data..:? "ConfigRuleId")
            Prelude.<*> (x Data..:? "ConfigRuleName")
            Prelude.<*> (x Data..:? "FirstActivatedTime")
            Prelude.<*> (x Data..:? "FirstEvaluationStarted")
            Prelude.<*> (x Data..:? "LastDeactivatedTime")
            Prelude.<*> (x Data..:? "LastDebugLogDeliveryStatus")
            Prelude.<*> (x Data..:? "LastDebugLogDeliveryStatusReason")
            Prelude.<*> (x Data..:? "LastDebugLogDeliveryTime")
            Prelude.<*> (x Data..:? "LastErrorCode")
            Prelude.<*> (x Data..:? "LastErrorMessage")
            Prelude.<*> (x Data..:? "LastFailedEvaluationTime")
            Prelude.<*> (x Data..:? "LastFailedInvocationTime")
            Prelude.<*> (x Data..:? "LastSuccessfulEvaluationTime")
            Prelude.<*> (x Data..:? "LastSuccessfulInvocationTime")
      )

instance Prelude.Hashable ConfigRuleEvaluationStatus where
  hashWithSalt _salt ConfigRuleEvaluationStatus' {..} =
    _salt `Prelude.hashWithSalt` configRuleArn
      `Prelude.hashWithSalt` configRuleId
      `Prelude.hashWithSalt` configRuleName
      `Prelude.hashWithSalt` firstActivatedTime
      `Prelude.hashWithSalt` firstEvaluationStarted
      `Prelude.hashWithSalt` lastDeactivatedTime
      `Prelude.hashWithSalt` lastDebugLogDeliveryStatus
      `Prelude.hashWithSalt` lastDebugLogDeliveryStatusReason
      `Prelude.hashWithSalt` lastDebugLogDeliveryTime
      `Prelude.hashWithSalt` lastErrorCode
      `Prelude.hashWithSalt` lastErrorMessage
      `Prelude.hashWithSalt` lastFailedEvaluationTime
      `Prelude.hashWithSalt` lastFailedInvocationTime
      `Prelude.hashWithSalt` lastSuccessfulEvaluationTime
      `Prelude.hashWithSalt` lastSuccessfulInvocationTime

instance Prelude.NFData ConfigRuleEvaluationStatus where
  rnf ConfigRuleEvaluationStatus' {..} =
    Prelude.rnf configRuleArn
      `Prelude.seq` Prelude.rnf configRuleId
      `Prelude.seq` Prelude.rnf configRuleName
      `Prelude.seq` Prelude.rnf firstActivatedTime
      `Prelude.seq` Prelude.rnf firstEvaluationStarted
      `Prelude.seq` Prelude.rnf lastDeactivatedTime
      `Prelude.seq` Prelude.rnf lastDebugLogDeliveryStatus
      `Prelude.seq` Prelude.rnf lastDebugLogDeliveryStatusReason
      `Prelude.seq` Prelude.rnf lastDebugLogDeliveryTime
      `Prelude.seq` Prelude.rnf lastErrorCode
      `Prelude.seq` Prelude.rnf lastErrorMessage
      `Prelude.seq` Prelude.rnf lastFailedEvaluationTime
      `Prelude.seq` Prelude.rnf lastFailedInvocationTime
      `Prelude.seq` Prelude.rnf lastSuccessfulEvaluationTime
      `Prelude.seq` Prelude.rnf lastSuccessfulInvocationTime
