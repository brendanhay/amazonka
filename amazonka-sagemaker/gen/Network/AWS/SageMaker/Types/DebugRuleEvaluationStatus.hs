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
-- Module      : Network.AWS.SageMaker.Types.DebugRuleEvaluationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DebugRuleEvaluationStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.RuleEvaluationStatus

-- | Information about the status of the rule evaluation.
--
-- /See:/ 'newDebugRuleEvaluationStatus' smart constructor.
data DebugRuleEvaluationStatus = DebugRuleEvaluationStatus'
  { -- | The name of the rule configuration.
    ruleConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | Details from the rule evaluation.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | Status of the rule evaluation.
    ruleEvaluationStatus :: Prelude.Maybe RuleEvaluationStatus,
    -- | Timestamp when the rule evaluation status was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the rule evaluation job.
    ruleEvaluationJobArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DebugRuleEvaluationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleConfigurationName', 'debugRuleEvaluationStatus_ruleConfigurationName' - The name of the rule configuration.
--
-- 'statusDetails', 'debugRuleEvaluationStatus_statusDetails' - Details from the rule evaluation.
--
-- 'ruleEvaluationStatus', 'debugRuleEvaluationStatus_ruleEvaluationStatus' - Status of the rule evaluation.
--
-- 'lastModifiedTime', 'debugRuleEvaluationStatus_lastModifiedTime' - Timestamp when the rule evaluation status was last modified.
--
-- 'ruleEvaluationJobArn', 'debugRuleEvaluationStatus_ruleEvaluationJobArn' - The Amazon Resource Name (ARN) of the rule evaluation job.
newDebugRuleEvaluationStatus ::
  DebugRuleEvaluationStatus
newDebugRuleEvaluationStatus =
  DebugRuleEvaluationStatus'
    { ruleConfigurationName =
        Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      ruleEvaluationStatus = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      ruleEvaluationJobArn = Prelude.Nothing
    }

-- | The name of the rule configuration.
debugRuleEvaluationStatus_ruleConfigurationName :: Lens.Lens' DebugRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
debugRuleEvaluationStatus_ruleConfigurationName = Lens.lens (\DebugRuleEvaluationStatus' {ruleConfigurationName} -> ruleConfigurationName) (\s@DebugRuleEvaluationStatus' {} a -> s {ruleConfigurationName = a} :: DebugRuleEvaluationStatus)

-- | Details from the rule evaluation.
debugRuleEvaluationStatus_statusDetails :: Lens.Lens' DebugRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
debugRuleEvaluationStatus_statusDetails = Lens.lens (\DebugRuleEvaluationStatus' {statusDetails} -> statusDetails) (\s@DebugRuleEvaluationStatus' {} a -> s {statusDetails = a} :: DebugRuleEvaluationStatus)

-- | Status of the rule evaluation.
debugRuleEvaluationStatus_ruleEvaluationStatus :: Lens.Lens' DebugRuleEvaluationStatus (Prelude.Maybe RuleEvaluationStatus)
debugRuleEvaluationStatus_ruleEvaluationStatus = Lens.lens (\DebugRuleEvaluationStatus' {ruleEvaluationStatus} -> ruleEvaluationStatus) (\s@DebugRuleEvaluationStatus' {} a -> s {ruleEvaluationStatus = a} :: DebugRuleEvaluationStatus)

-- | Timestamp when the rule evaluation status was last modified.
debugRuleEvaluationStatus_lastModifiedTime :: Lens.Lens' DebugRuleEvaluationStatus (Prelude.Maybe Prelude.UTCTime)
debugRuleEvaluationStatus_lastModifiedTime = Lens.lens (\DebugRuleEvaluationStatus' {lastModifiedTime} -> lastModifiedTime) (\s@DebugRuleEvaluationStatus' {} a -> s {lastModifiedTime = a} :: DebugRuleEvaluationStatus) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the rule evaluation job.
debugRuleEvaluationStatus_ruleEvaluationJobArn :: Lens.Lens' DebugRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
debugRuleEvaluationStatus_ruleEvaluationJobArn = Lens.lens (\DebugRuleEvaluationStatus' {ruleEvaluationJobArn} -> ruleEvaluationJobArn) (\s@DebugRuleEvaluationStatus' {} a -> s {ruleEvaluationJobArn = a} :: DebugRuleEvaluationStatus)

instance Prelude.FromJSON DebugRuleEvaluationStatus where
  parseJSON =
    Prelude.withObject
      "DebugRuleEvaluationStatus"
      ( \x ->
          DebugRuleEvaluationStatus'
            Prelude.<$> (x Prelude..:? "RuleConfigurationName")
            Prelude.<*> (x Prelude..:? "StatusDetails")
            Prelude.<*> (x Prelude..:? "RuleEvaluationStatus")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "RuleEvaluationJobArn")
      )

instance Prelude.Hashable DebugRuleEvaluationStatus

instance Prelude.NFData DebugRuleEvaluationStatus
