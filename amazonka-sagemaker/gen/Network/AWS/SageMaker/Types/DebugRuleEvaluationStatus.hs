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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.RuleEvaluationStatus

-- | Information about the status of the rule evaluation.
--
-- /See:/ 'newDebugRuleEvaluationStatus' smart constructor.
data DebugRuleEvaluationStatus = DebugRuleEvaluationStatus'
  { -- | The name of the rule configuration.
    ruleConfigurationName :: Core.Maybe Core.Text,
    -- | Details from the rule evaluation.
    statusDetails :: Core.Maybe Core.Text,
    -- | Status of the rule evaluation.
    ruleEvaluationStatus :: Core.Maybe RuleEvaluationStatus,
    -- | Timestamp when the rule evaluation status was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the rule evaluation job.
    ruleEvaluationJobArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      statusDetails = Core.Nothing,
      ruleEvaluationStatus = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      ruleEvaluationJobArn = Core.Nothing
    }

-- | The name of the rule configuration.
debugRuleEvaluationStatus_ruleConfigurationName :: Lens.Lens' DebugRuleEvaluationStatus (Core.Maybe Core.Text)
debugRuleEvaluationStatus_ruleConfigurationName = Lens.lens (\DebugRuleEvaluationStatus' {ruleConfigurationName} -> ruleConfigurationName) (\s@DebugRuleEvaluationStatus' {} a -> s {ruleConfigurationName = a} :: DebugRuleEvaluationStatus)

-- | Details from the rule evaluation.
debugRuleEvaluationStatus_statusDetails :: Lens.Lens' DebugRuleEvaluationStatus (Core.Maybe Core.Text)
debugRuleEvaluationStatus_statusDetails = Lens.lens (\DebugRuleEvaluationStatus' {statusDetails} -> statusDetails) (\s@DebugRuleEvaluationStatus' {} a -> s {statusDetails = a} :: DebugRuleEvaluationStatus)

-- | Status of the rule evaluation.
debugRuleEvaluationStatus_ruleEvaluationStatus :: Lens.Lens' DebugRuleEvaluationStatus (Core.Maybe RuleEvaluationStatus)
debugRuleEvaluationStatus_ruleEvaluationStatus = Lens.lens (\DebugRuleEvaluationStatus' {ruleEvaluationStatus} -> ruleEvaluationStatus) (\s@DebugRuleEvaluationStatus' {} a -> s {ruleEvaluationStatus = a} :: DebugRuleEvaluationStatus)

-- | Timestamp when the rule evaluation status was last modified.
debugRuleEvaluationStatus_lastModifiedTime :: Lens.Lens' DebugRuleEvaluationStatus (Core.Maybe Core.UTCTime)
debugRuleEvaluationStatus_lastModifiedTime = Lens.lens (\DebugRuleEvaluationStatus' {lastModifiedTime} -> lastModifiedTime) (\s@DebugRuleEvaluationStatus' {} a -> s {lastModifiedTime = a} :: DebugRuleEvaluationStatus) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the rule evaluation job.
debugRuleEvaluationStatus_ruleEvaluationJobArn :: Lens.Lens' DebugRuleEvaluationStatus (Core.Maybe Core.Text)
debugRuleEvaluationStatus_ruleEvaluationJobArn = Lens.lens (\DebugRuleEvaluationStatus' {ruleEvaluationJobArn} -> ruleEvaluationJobArn) (\s@DebugRuleEvaluationStatus' {} a -> s {ruleEvaluationJobArn = a} :: DebugRuleEvaluationStatus)

instance Core.FromJSON DebugRuleEvaluationStatus where
  parseJSON =
    Core.withObject
      "DebugRuleEvaluationStatus"
      ( \x ->
          DebugRuleEvaluationStatus'
            Core.<$> (x Core..:? "RuleConfigurationName")
            Core.<*> (x Core..:? "StatusDetails")
            Core.<*> (x Core..:? "RuleEvaluationStatus")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "RuleEvaluationJobArn")
      )

instance Core.Hashable DebugRuleEvaluationStatus

instance Core.NFData DebugRuleEvaluationStatus
