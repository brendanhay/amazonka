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
-- Module      : Amazonka.SageMaker.Types.DebugRuleEvaluationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DebugRuleEvaluationStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.RuleEvaluationStatus

-- | Information about the status of the rule evaluation.
--
-- /See:/ 'newDebugRuleEvaluationStatus' smart constructor.
data DebugRuleEvaluationStatus = DebugRuleEvaluationStatus'
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
-- Create a value of 'DebugRuleEvaluationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'debugRuleEvaluationStatus_lastModifiedTime' - Timestamp when the rule evaluation status was last modified.
--
-- 'ruleConfigurationName', 'debugRuleEvaluationStatus_ruleConfigurationName' - The name of the rule configuration.
--
-- 'ruleEvaluationJobArn', 'debugRuleEvaluationStatus_ruleEvaluationJobArn' - The Amazon Resource Name (ARN) of the rule evaluation job.
--
-- 'ruleEvaluationStatus', 'debugRuleEvaluationStatus_ruleEvaluationStatus' - Status of the rule evaluation.
--
-- 'statusDetails', 'debugRuleEvaluationStatus_statusDetails' - Details from the rule evaluation.
newDebugRuleEvaluationStatus ::
  DebugRuleEvaluationStatus
newDebugRuleEvaluationStatus =
  DebugRuleEvaluationStatus'
    { lastModifiedTime =
        Prelude.Nothing,
      ruleConfigurationName = Prelude.Nothing,
      ruleEvaluationJobArn = Prelude.Nothing,
      ruleEvaluationStatus = Prelude.Nothing,
      statusDetails = Prelude.Nothing
    }

-- | Timestamp when the rule evaluation status was last modified.
debugRuleEvaluationStatus_lastModifiedTime :: Lens.Lens' DebugRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
debugRuleEvaluationStatus_lastModifiedTime = Lens.lens (\DebugRuleEvaluationStatus' {lastModifiedTime} -> lastModifiedTime) (\s@DebugRuleEvaluationStatus' {} a -> s {lastModifiedTime = a} :: DebugRuleEvaluationStatus) Prelude.. Lens.mapping Data._Time

-- | The name of the rule configuration.
debugRuleEvaluationStatus_ruleConfigurationName :: Lens.Lens' DebugRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
debugRuleEvaluationStatus_ruleConfigurationName = Lens.lens (\DebugRuleEvaluationStatus' {ruleConfigurationName} -> ruleConfigurationName) (\s@DebugRuleEvaluationStatus' {} a -> s {ruleConfigurationName = a} :: DebugRuleEvaluationStatus)

-- | The Amazon Resource Name (ARN) of the rule evaluation job.
debugRuleEvaluationStatus_ruleEvaluationJobArn :: Lens.Lens' DebugRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
debugRuleEvaluationStatus_ruleEvaluationJobArn = Lens.lens (\DebugRuleEvaluationStatus' {ruleEvaluationJobArn} -> ruleEvaluationJobArn) (\s@DebugRuleEvaluationStatus' {} a -> s {ruleEvaluationJobArn = a} :: DebugRuleEvaluationStatus)

-- | Status of the rule evaluation.
debugRuleEvaluationStatus_ruleEvaluationStatus :: Lens.Lens' DebugRuleEvaluationStatus (Prelude.Maybe RuleEvaluationStatus)
debugRuleEvaluationStatus_ruleEvaluationStatus = Lens.lens (\DebugRuleEvaluationStatus' {ruleEvaluationStatus} -> ruleEvaluationStatus) (\s@DebugRuleEvaluationStatus' {} a -> s {ruleEvaluationStatus = a} :: DebugRuleEvaluationStatus)

-- | Details from the rule evaluation.
debugRuleEvaluationStatus_statusDetails :: Lens.Lens' DebugRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
debugRuleEvaluationStatus_statusDetails = Lens.lens (\DebugRuleEvaluationStatus' {statusDetails} -> statusDetails) (\s@DebugRuleEvaluationStatus' {} a -> s {statusDetails = a} :: DebugRuleEvaluationStatus)

instance Data.FromJSON DebugRuleEvaluationStatus where
  parseJSON =
    Data.withObject
      "DebugRuleEvaluationStatus"
      ( \x ->
          DebugRuleEvaluationStatus'
            Prelude.<$> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "RuleConfigurationName")
            Prelude.<*> (x Data..:? "RuleEvaluationJobArn")
            Prelude.<*> (x Data..:? "RuleEvaluationStatus")
            Prelude.<*> (x Data..:? "StatusDetails")
      )

instance Prelude.Hashable DebugRuleEvaluationStatus where
  hashWithSalt _salt DebugRuleEvaluationStatus' {..} =
    _salt `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` ruleConfigurationName
      `Prelude.hashWithSalt` ruleEvaluationJobArn
      `Prelude.hashWithSalt` ruleEvaluationStatus
      `Prelude.hashWithSalt` statusDetails

instance Prelude.NFData DebugRuleEvaluationStatus where
  rnf DebugRuleEvaluationStatus' {..} =
    Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf ruleConfigurationName
      `Prelude.seq` Prelude.rnf ruleEvaluationJobArn
      `Prelude.seq` Prelude.rnf ruleEvaluationStatus
      `Prelude.seq` Prelude.rnf statusDetails
