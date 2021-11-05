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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DebugRuleEvaluationStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.RuleEvaluationStatus

-- | Information about the status of the rule evaluation.
--
-- /See:/ 'newDebugRuleEvaluationStatus' smart constructor.
data DebugRuleEvaluationStatus = DebugRuleEvaluationStatus'
  { -- | Timestamp when the rule evaluation status was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | Details from the rule evaluation.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | Status of the rule evaluation.
    ruleEvaluationStatus :: Prelude.Maybe RuleEvaluationStatus,
    -- | The Amazon Resource Name (ARN) of the rule evaluation job.
    ruleEvaluationJobArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule configuration.
    ruleConfigurationName :: Prelude.Maybe Prelude.Text
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
-- 'statusDetails', 'debugRuleEvaluationStatus_statusDetails' - Details from the rule evaluation.
--
-- 'ruleEvaluationStatus', 'debugRuleEvaluationStatus_ruleEvaluationStatus' - Status of the rule evaluation.
--
-- 'ruleEvaluationJobArn', 'debugRuleEvaluationStatus_ruleEvaluationJobArn' - The Amazon Resource Name (ARN) of the rule evaluation job.
--
-- 'ruleConfigurationName', 'debugRuleEvaluationStatus_ruleConfigurationName' - The name of the rule configuration.
newDebugRuleEvaluationStatus ::
  DebugRuleEvaluationStatus
newDebugRuleEvaluationStatus =
  DebugRuleEvaluationStatus'
    { lastModifiedTime =
        Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      ruleEvaluationStatus = Prelude.Nothing,
      ruleEvaluationJobArn = Prelude.Nothing,
      ruleConfigurationName = Prelude.Nothing
    }

-- | Timestamp when the rule evaluation status was last modified.
debugRuleEvaluationStatus_lastModifiedTime :: Lens.Lens' DebugRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
debugRuleEvaluationStatus_lastModifiedTime = Lens.lens (\DebugRuleEvaluationStatus' {lastModifiedTime} -> lastModifiedTime) (\s@DebugRuleEvaluationStatus' {} a -> s {lastModifiedTime = a} :: DebugRuleEvaluationStatus) Prelude.. Lens.mapping Core._Time

-- | Details from the rule evaluation.
debugRuleEvaluationStatus_statusDetails :: Lens.Lens' DebugRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
debugRuleEvaluationStatus_statusDetails = Lens.lens (\DebugRuleEvaluationStatus' {statusDetails} -> statusDetails) (\s@DebugRuleEvaluationStatus' {} a -> s {statusDetails = a} :: DebugRuleEvaluationStatus)

-- | Status of the rule evaluation.
debugRuleEvaluationStatus_ruleEvaluationStatus :: Lens.Lens' DebugRuleEvaluationStatus (Prelude.Maybe RuleEvaluationStatus)
debugRuleEvaluationStatus_ruleEvaluationStatus = Lens.lens (\DebugRuleEvaluationStatus' {ruleEvaluationStatus} -> ruleEvaluationStatus) (\s@DebugRuleEvaluationStatus' {} a -> s {ruleEvaluationStatus = a} :: DebugRuleEvaluationStatus)

-- | The Amazon Resource Name (ARN) of the rule evaluation job.
debugRuleEvaluationStatus_ruleEvaluationJobArn :: Lens.Lens' DebugRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
debugRuleEvaluationStatus_ruleEvaluationJobArn = Lens.lens (\DebugRuleEvaluationStatus' {ruleEvaluationJobArn} -> ruleEvaluationJobArn) (\s@DebugRuleEvaluationStatus' {} a -> s {ruleEvaluationJobArn = a} :: DebugRuleEvaluationStatus)

-- | The name of the rule configuration.
debugRuleEvaluationStatus_ruleConfigurationName :: Lens.Lens' DebugRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
debugRuleEvaluationStatus_ruleConfigurationName = Lens.lens (\DebugRuleEvaluationStatus' {ruleConfigurationName} -> ruleConfigurationName) (\s@DebugRuleEvaluationStatus' {} a -> s {ruleConfigurationName = a} :: DebugRuleEvaluationStatus)

instance Core.FromJSON DebugRuleEvaluationStatus where
  parseJSON =
    Core.withObject
      "DebugRuleEvaluationStatus"
      ( \x ->
          DebugRuleEvaluationStatus'
            Prelude.<$> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "StatusDetails")
            Prelude.<*> (x Core..:? "RuleEvaluationStatus")
            Prelude.<*> (x Core..:? "RuleEvaluationJobArn")
            Prelude.<*> (x Core..:? "RuleConfigurationName")
      )

instance Prelude.Hashable DebugRuleEvaluationStatus

instance Prelude.NFData DebugRuleEvaluationStatus
