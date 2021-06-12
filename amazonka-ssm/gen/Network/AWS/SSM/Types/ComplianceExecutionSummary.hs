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
-- Module      : Network.AWS.SSM.Types.ComplianceExecutionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceExecutionSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A summary of the call execution that includes an execution ID, the type
-- of execution (for example, @Command@), and the date\/time of the
-- execution using a datetime object that is saved in the following format:
-- yyyy-MM-dd\'T\'HH:mm:ss\'Z\'.
--
-- /See:/ 'newComplianceExecutionSummary' smart constructor.
data ComplianceExecutionSummary = ComplianceExecutionSummary'
  { -- | An ID created by the system when @PutComplianceItems@ was called. For
    -- example, @CommandID@ is a valid execution ID. You can use this ID in
    -- subsequent calls.
    executionId :: Core.Maybe Core.Text,
    -- | The type of execution. For example, @Command@ is a valid execution type.
    executionType :: Core.Maybe Core.Text,
    -- | The time the execution ran as a datetime object that is saved in the
    -- following format: yyyy-MM-dd\'T\'HH:mm:ss\'Z\'.
    executionTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ComplianceExecutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionId', 'complianceExecutionSummary_executionId' - An ID created by the system when @PutComplianceItems@ was called. For
-- example, @CommandID@ is a valid execution ID. You can use this ID in
-- subsequent calls.
--
-- 'executionType', 'complianceExecutionSummary_executionType' - The type of execution. For example, @Command@ is a valid execution type.
--
-- 'executionTime', 'complianceExecutionSummary_executionTime' - The time the execution ran as a datetime object that is saved in the
-- following format: yyyy-MM-dd\'T\'HH:mm:ss\'Z\'.
newComplianceExecutionSummary ::
  -- | 'executionTime'
  Core.UTCTime ->
  ComplianceExecutionSummary
newComplianceExecutionSummary pExecutionTime_ =
  ComplianceExecutionSummary'
    { executionId =
        Core.Nothing,
      executionType = Core.Nothing,
      executionTime =
        Core._Time Lens.# pExecutionTime_
    }

-- | An ID created by the system when @PutComplianceItems@ was called. For
-- example, @CommandID@ is a valid execution ID. You can use this ID in
-- subsequent calls.
complianceExecutionSummary_executionId :: Lens.Lens' ComplianceExecutionSummary (Core.Maybe Core.Text)
complianceExecutionSummary_executionId = Lens.lens (\ComplianceExecutionSummary' {executionId} -> executionId) (\s@ComplianceExecutionSummary' {} a -> s {executionId = a} :: ComplianceExecutionSummary)

-- | The type of execution. For example, @Command@ is a valid execution type.
complianceExecutionSummary_executionType :: Lens.Lens' ComplianceExecutionSummary (Core.Maybe Core.Text)
complianceExecutionSummary_executionType = Lens.lens (\ComplianceExecutionSummary' {executionType} -> executionType) (\s@ComplianceExecutionSummary' {} a -> s {executionType = a} :: ComplianceExecutionSummary)

-- | The time the execution ran as a datetime object that is saved in the
-- following format: yyyy-MM-dd\'T\'HH:mm:ss\'Z\'.
complianceExecutionSummary_executionTime :: Lens.Lens' ComplianceExecutionSummary Core.UTCTime
complianceExecutionSummary_executionTime = Lens.lens (\ComplianceExecutionSummary' {executionTime} -> executionTime) (\s@ComplianceExecutionSummary' {} a -> s {executionTime = a} :: ComplianceExecutionSummary) Core.. Core._Time

instance Core.FromJSON ComplianceExecutionSummary where
  parseJSON =
    Core.withObject
      "ComplianceExecutionSummary"
      ( \x ->
          ComplianceExecutionSummary'
            Core.<$> (x Core..:? "ExecutionId")
            Core.<*> (x Core..:? "ExecutionType")
            Core.<*> (x Core..: "ExecutionTime")
      )

instance Core.Hashable ComplianceExecutionSummary

instance Core.NFData ComplianceExecutionSummary

instance Core.ToJSON ComplianceExecutionSummary where
  toJSON ComplianceExecutionSummary' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExecutionId" Core..=) Core.<$> executionId,
            ("ExecutionType" Core..=) Core.<$> executionType,
            Core.Just ("ExecutionTime" Core..= executionTime)
          ]
      )
