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
-- Module      : Amazonka.SageMaker.Types.ProfilerRuleEvaluationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProfilerRuleEvaluationStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.RuleEvaluationStatus

-- | Information about the status of the rule evaluation.
--
-- /See:/ 'newProfilerRuleEvaluationStatus' smart constructor.
data ProfilerRuleEvaluationStatus = ProfilerRuleEvaluationStatus'
  { -- | Timestamp when the rule evaluation status was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the rule configuration.
    ruleConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the rule evaluation job.
    ruleEvaluationJobArn :: Prelude.Maybe Prelude.Text,
    -- | Status of the rule evaluation.
    ruleEvaluationStatus :: Prelude.Maybe RuleEvaluationStatus,
    -- | Details from the rule evaluation.
    statusDetails :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfilerRuleEvaluationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'profilerRuleEvaluationStatus_lastModifiedTime' - Timestamp when the rule evaluation status was last modified.
--
-- 'ruleConfigurationName', 'profilerRuleEvaluationStatus_ruleConfigurationName' - The name of the rule configuration.
--
-- 'ruleEvaluationJobArn', 'profilerRuleEvaluationStatus_ruleEvaluationJobArn' - The Amazon Resource Name (ARN) of the rule evaluation job.
--
-- 'ruleEvaluationStatus', 'profilerRuleEvaluationStatus_ruleEvaluationStatus' - Status of the rule evaluation.
--
-- 'statusDetails', 'profilerRuleEvaluationStatus_statusDetails' - Details from the rule evaluation.
newProfilerRuleEvaluationStatus ::
  ProfilerRuleEvaluationStatus
newProfilerRuleEvaluationStatus =
  ProfilerRuleEvaluationStatus'
    { lastModifiedTime =
        Prelude.Nothing,
      ruleConfigurationName = Prelude.Nothing,
      ruleEvaluationJobArn = Prelude.Nothing,
      ruleEvaluationStatus = Prelude.Nothing,
      statusDetails = Prelude.Nothing
    }

-- | Timestamp when the rule evaluation status was last modified.
profilerRuleEvaluationStatus_lastModifiedTime :: Lens.Lens' ProfilerRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
profilerRuleEvaluationStatus_lastModifiedTime = Lens.lens (\ProfilerRuleEvaluationStatus' {lastModifiedTime} -> lastModifiedTime) (\s@ProfilerRuleEvaluationStatus' {} a -> s {lastModifiedTime = a} :: ProfilerRuleEvaluationStatus) Prelude.. Lens.mapping Data._Time

-- | The name of the rule configuration.
profilerRuleEvaluationStatus_ruleConfigurationName :: Lens.Lens' ProfilerRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
profilerRuleEvaluationStatus_ruleConfigurationName = Lens.lens (\ProfilerRuleEvaluationStatus' {ruleConfigurationName} -> ruleConfigurationName) (\s@ProfilerRuleEvaluationStatus' {} a -> s {ruleConfigurationName = a} :: ProfilerRuleEvaluationStatus)

-- | The Amazon Resource Name (ARN) of the rule evaluation job.
profilerRuleEvaluationStatus_ruleEvaluationJobArn :: Lens.Lens' ProfilerRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
profilerRuleEvaluationStatus_ruleEvaluationJobArn = Lens.lens (\ProfilerRuleEvaluationStatus' {ruleEvaluationJobArn} -> ruleEvaluationJobArn) (\s@ProfilerRuleEvaluationStatus' {} a -> s {ruleEvaluationJobArn = a} :: ProfilerRuleEvaluationStatus)

-- | Status of the rule evaluation.
profilerRuleEvaluationStatus_ruleEvaluationStatus :: Lens.Lens' ProfilerRuleEvaluationStatus (Prelude.Maybe RuleEvaluationStatus)
profilerRuleEvaluationStatus_ruleEvaluationStatus = Lens.lens (\ProfilerRuleEvaluationStatus' {ruleEvaluationStatus} -> ruleEvaluationStatus) (\s@ProfilerRuleEvaluationStatus' {} a -> s {ruleEvaluationStatus = a} :: ProfilerRuleEvaluationStatus)

-- | Details from the rule evaluation.
profilerRuleEvaluationStatus_statusDetails :: Lens.Lens' ProfilerRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
profilerRuleEvaluationStatus_statusDetails = Lens.lens (\ProfilerRuleEvaluationStatus' {statusDetails} -> statusDetails) (\s@ProfilerRuleEvaluationStatus' {} a -> s {statusDetails = a} :: ProfilerRuleEvaluationStatus)

instance Data.FromJSON ProfilerRuleEvaluationStatus where
  parseJSON =
    Data.withObject
      "ProfilerRuleEvaluationStatus"
      ( \x ->
          ProfilerRuleEvaluationStatus'
            Prelude.<$> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "RuleConfigurationName")
            Prelude.<*> (x Data..:? "RuleEvaluationJobArn")
            Prelude.<*> (x Data..:? "RuleEvaluationStatus")
            Prelude.<*> (x Data..:? "StatusDetails")
      )

instance
  Prelude.Hashable
    ProfilerRuleEvaluationStatus
  where
  hashWithSalt _salt ProfilerRuleEvaluationStatus' {..} =
    _salt
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` ruleConfigurationName
      `Prelude.hashWithSalt` ruleEvaluationJobArn
      `Prelude.hashWithSalt` ruleEvaluationStatus
      `Prelude.hashWithSalt` statusDetails

instance Prelude.NFData ProfilerRuleEvaluationStatus where
  rnf ProfilerRuleEvaluationStatus' {..} =
    Prelude.rnf lastModifiedTime `Prelude.seq`
      Prelude.rnf ruleConfigurationName `Prelude.seq`
        Prelude.rnf ruleEvaluationJobArn `Prelude.seq`
          Prelude.rnf ruleEvaluationStatus `Prelude.seq`
            Prelude.rnf statusDetails
