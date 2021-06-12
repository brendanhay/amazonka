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
-- Module      : Network.AWS.SageMaker.Types.ProfilerRuleEvaluationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProfilerRuleEvaluationStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.RuleEvaluationStatus

-- | Information about the status of the rule evaluation.
--
-- /See:/ 'newProfilerRuleEvaluationStatus' smart constructor.
data ProfilerRuleEvaluationStatus = ProfilerRuleEvaluationStatus'
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
-- Create a value of 'ProfilerRuleEvaluationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleConfigurationName', 'profilerRuleEvaluationStatus_ruleConfigurationName' - The name of the rule configuration.
--
-- 'statusDetails', 'profilerRuleEvaluationStatus_statusDetails' - Details from the rule evaluation.
--
-- 'ruleEvaluationStatus', 'profilerRuleEvaluationStatus_ruleEvaluationStatus' - Status of the rule evaluation.
--
-- 'lastModifiedTime', 'profilerRuleEvaluationStatus_lastModifiedTime' - Timestamp when the rule evaluation status was last modified.
--
-- 'ruleEvaluationJobArn', 'profilerRuleEvaluationStatus_ruleEvaluationJobArn' - The Amazon Resource Name (ARN) of the rule evaluation job.
newProfilerRuleEvaluationStatus ::
  ProfilerRuleEvaluationStatus
newProfilerRuleEvaluationStatus =
  ProfilerRuleEvaluationStatus'
    { ruleConfigurationName =
        Core.Nothing,
      statusDetails = Core.Nothing,
      ruleEvaluationStatus = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      ruleEvaluationJobArn = Core.Nothing
    }

-- | The name of the rule configuration.
profilerRuleEvaluationStatus_ruleConfigurationName :: Lens.Lens' ProfilerRuleEvaluationStatus (Core.Maybe Core.Text)
profilerRuleEvaluationStatus_ruleConfigurationName = Lens.lens (\ProfilerRuleEvaluationStatus' {ruleConfigurationName} -> ruleConfigurationName) (\s@ProfilerRuleEvaluationStatus' {} a -> s {ruleConfigurationName = a} :: ProfilerRuleEvaluationStatus)

-- | Details from the rule evaluation.
profilerRuleEvaluationStatus_statusDetails :: Lens.Lens' ProfilerRuleEvaluationStatus (Core.Maybe Core.Text)
profilerRuleEvaluationStatus_statusDetails = Lens.lens (\ProfilerRuleEvaluationStatus' {statusDetails} -> statusDetails) (\s@ProfilerRuleEvaluationStatus' {} a -> s {statusDetails = a} :: ProfilerRuleEvaluationStatus)

-- | Status of the rule evaluation.
profilerRuleEvaluationStatus_ruleEvaluationStatus :: Lens.Lens' ProfilerRuleEvaluationStatus (Core.Maybe RuleEvaluationStatus)
profilerRuleEvaluationStatus_ruleEvaluationStatus = Lens.lens (\ProfilerRuleEvaluationStatus' {ruleEvaluationStatus} -> ruleEvaluationStatus) (\s@ProfilerRuleEvaluationStatus' {} a -> s {ruleEvaluationStatus = a} :: ProfilerRuleEvaluationStatus)

-- | Timestamp when the rule evaluation status was last modified.
profilerRuleEvaluationStatus_lastModifiedTime :: Lens.Lens' ProfilerRuleEvaluationStatus (Core.Maybe Core.UTCTime)
profilerRuleEvaluationStatus_lastModifiedTime = Lens.lens (\ProfilerRuleEvaluationStatus' {lastModifiedTime} -> lastModifiedTime) (\s@ProfilerRuleEvaluationStatus' {} a -> s {lastModifiedTime = a} :: ProfilerRuleEvaluationStatus) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the rule evaluation job.
profilerRuleEvaluationStatus_ruleEvaluationJobArn :: Lens.Lens' ProfilerRuleEvaluationStatus (Core.Maybe Core.Text)
profilerRuleEvaluationStatus_ruleEvaluationJobArn = Lens.lens (\ProfilerRuleEvaluationStatus' {ruleEvaluationJobArn} -> ruleEvaluationJobArn) (\s@ProfilerRuleEvaluationStatus' {} a -> s {ruleEvaluationJobArn = a} :: ProfilerRuleEvaluationStatus)

instance Core.FromJSON ProfilerRuleEvaluationStatus where
  parseJSON =
    Core.withObject
      "ProfilerRuleEvaluationStatus"
      ( \x ->
          ProfilerRuleEvaluationStatus'
            Core.<$> (x Core..:? "RuleConfigurationName")
            Core.<*> (x Core..:? "StatusDetails")
            Core.<*> (x Core..:? "RuleEvaluationStatus")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "RuleEvaluationJobArn")
      )

instance Core.Hashable ProfilerRuleEvaluationStatus

instance Core.NFData ProfilerRuleEvaluationStatus
