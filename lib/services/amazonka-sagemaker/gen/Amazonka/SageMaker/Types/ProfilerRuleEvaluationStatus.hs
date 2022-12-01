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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProfilerRuleEvaluationStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.RuleEvaluationStatus

-- | Information about the status of the rule evaluation.
--
-- /See:/ 'newProfilerRuleEvaluationStatus' smart constructor.
data ProfilerRuleEvaluationStatus = ProfilerRuleEvaluationStatus'
  { -- | Details from the rule evaluation.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | Timestamp when the rule evaluation status was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the rule evaluation job.
    ruleEvaluationJobArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule configuration.
    ruleConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | Status of the rule evaluation.
    ruleEvaluationStatus :: Prelude.Maybe RuleEvaluationStatus
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
-- 'statusDetails', 'profilerRuleEvaluationStatus_statusDetails' - Details from the rule evaluation.
--
-- 'lastModifiedTime', 'profilerRuleEvaluationStatus_lastModifiedTime' - Timestamp when the rule evaluation status was last modified.
--
-- 'ruleEvaluationJobArn', 'profilerRuleEvaluationStatus_ruleEvaluationJobArn' - The Amazon Resource Name (ARN) of the rule evaluation job.
--
-- 'ruleConfigurationName', 'profilerRuleEvaluationStatus_ruleConfigurationName' - The name of the rule configuration.
--
-- 'ruleEvaluationStatus', 'profilerRuleEvaluationStatus_ruleEvaluationStatus' - Status of the rule evaluation.
newProfilerRuleEvaluationStatus ::
  ProfilerRuleEvaluationStatus
newProfilerRuleEvaluationStatus =
  ProfilerRuleEvaluationStatus'
    { statusDetails =
        Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      ruleEvaluationJobArn = Prelude.Nothing,
      ruleConfigurationName = Prelude.Nothing,
      ruleEvaluationStatus = Prelude.Nothing
    }

-- | Details from the rule evaluation.
profilerRuleEvaluationStatus_statusDetails :: Lens.Lens' ProfilerRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
profilerRuleEvaluationStatus_statusDetails = Lens.lens (\ProfilerRuleEvaluationStatus' {statusDetails} -> statusDetails) (\s@ProfilerRuleEvaluationStatus' {} a -> s {statusDetails = a} :: ProfilerRuleEvaluationStatus)

-- | Timestamp when the rule evaluation status was last modified.
profilerRuleEvaluationStatus_lastModifiedTime :: Lens.Lens' ProfilerRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
profilerRuleEvaluationStatus_lastModifiedTime = Lens.lens (\ProfilerRuleEvaluationStatus' {lastModifiedTime} -> lastModifiedTime) (\s@ProfilerRuleEvaluationStatus' {} a -> s {lastModifiedTime = a} :: ProfilerRuleEvaluationStatus) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the rule evaluation job.
profilerRuleEvaluationStatus_ruleEvaluationJobArn :: Lens.Lens' ProfilerRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
profilerRuleEvaluationStatus_ruleEvaluationJobArn = Lens.lens (\ProfilerRuleEvaluationStatus' {ruleEvaluationJobArn} -> ruleEvaluationJobArn) (\s@ProfilerRuleEvaluationStatus' {} a -> s {ruleEvaluationJobArn = a} :: ProfilerRuleEvaluationStatus)

-- | The name of the rule configuration.
profilerRuleEvaluationStatus_ruleConfigurationName :: Lens.Lens' ProfilerRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
profilerRuleEvaluationStatus_ruleConfigurationName = Lens.lens (\ProfilerRuleEvaluationStatus' {ruleConfigurationName} -> ruleConfigurationName) (\s@ProfilerRuleEvaluationStatus' {} a -> s {ruleConfigurationName = a} :: ProfilerRuleEvaluationStatus)

-- | Status of the rule evaluation.
profilerRuleEvaluationStatus_ruleEvaluationStatus :: Lens.Lens' ProfilerRuleEvaluationStatus (Prelude.Maybe RuleEvaluationStatus)
profilerRuleEvaluationStatus_ruleEvaluationStatus = Lens.lens (\ProfilerRuleEvaluationStatus' {ruleEvaluationStatus} -> ruleEvaluationStatus) (\s@ProfilerRuleEvaluationStatus' {} a -> s {ruleEvaluationStatus = a} :: ProfilerRuleEvaluationStatus)

instance Core.FromJSON ProfilerRuleEvaluationStatus where
  parseJSON =
    Core.withObject
      "ProfilerRuleEvaluationStatus"
      ( \x ->
          ProfilerRuleEvaluationStatus'
            Prelude.<$> (x Core..:? "StatusDetails")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "RuleEvaluationJobArn")
            Prelude.<*> (x Core..:? "RuleConfigurationName")
            Prelude.<*> (x Core..:? "RuleEvaluationStatus")
      )

instance
  Prelude.Hashable
    ProfilerRuleEvaluationStatus
  where
  hashWithSalt _salt ProfilerRuleEvaluationStatus' {..} =
    _salt `Prelude.hashWithSalt` statusDetails
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` ruleEvaluationJobArn
      `Prelude.hashWithSalt` ruleConfigurationName
      `Prelude.hashWithSalt` ruleEvaluationStatus

instance Prelude.NFData ProfilerRuleEvaluationStatus where
  rnf ProfilerRuleEvaluationStatus' {..} =
    Prelude.rnf statusDetails
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf ruleEvaluationJobArn
      `Prelude.seq` Prelude.rnf ruleConfigurationName
      `Prelude.seq` Prelude.rnf ruleEvaluationStatus
