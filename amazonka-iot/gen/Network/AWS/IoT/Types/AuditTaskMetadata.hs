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
-- Module      : Network.AWS.IoT.Types.AuditTaskMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditTaskMetadata where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.AuditTaskStatus
import Network.AWS.IoT.Types.AuditTaskType
import qualified Network.AWS.Lens as Lens

-- | The audits that were performed.
--
-- /See:/ 'newAuditTaskMetadata' smart constructor.
data AuditTaskMetadata = AuditTaskMetadata'
  { -- | The ID of this audit.
    taskId :: Core.Maybe Core.Text,
    -- | The status of this audit. One of \"IN_PROGRESS\", \"COMPLETED\",
    -- \"FAILED\", or \"CANCELED\".
    taskStatus :: Core.Maybe AuditTaskStatus,
    -- | The type of this audit. One of \"ON_DEMAND_AUDIT_TASK\" or
    -- \"SCHEDULED_AUDIT_TASK\".
    taskType :: Core.Maybe AuditTaskType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AuditTaskMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'auditTaskMetadata_taskId' - The ID of this audit.
--
-- 'taskStatus', 'auditTaskMetadata_taskStatus' - The status of this audit. One of \"IN_PROGRESS\", \"COMPLETED\",
-- \"FAILED\", or \"CANCELED\".
--
-- 'taskType', 'auditTaskMetadata_taskType' - The type of this audit. One of \"ON_DEMAND_AUDIT_TASK\" or
-- \"SCHEDULED_AUDIT_TASK\".
newAuditTaskMetadata ::
  AuditTaskMetadata
newAuditTaskMetadata =
  AuditTaskMetadata'
    { taskId = Core.Nothing,
      taskStatus = Core.Nothing,
      taskType = Core.Nothing
    }

-- | The ID of this audit.
auditTaskMetadata_taskId :: Lens.Lens' AuditTaskMetadata (Core.Maybe Core.Text)
auditTaskMetadata_taskId = Lens.lens (\AuditTaskMetadata' {taskId} -> taskId) (\s@AuditTaskMetadata' {} a -> s {taskId = a} :: AuditTaskMetadata)

-- | The status of this audit. One of \"IN_PROGRESS\", \"COMPLETED\",
-- \"FAILED\", or \"CANCELED\".
auditTaskMetadata_taskStatus :: Lens.Lens' AuditTaskMetadata (Core.Maybe AuditTaskStatus)
auditTaskMetadata_taskStatus = Lens.lens (\AuditTaskMetadata' {taskStatus} -> taskStatus) (\s@AuditTaskMetadata' {} a -> s {taskStatus = a} :: AuditTaskMetadata)

-- | The type of this audit. One of \"ON_DEMAND_AUDIT_TASK\" or
-- \"SCHEDULED_AUDIT_TASK\".
auditTaskMetadata_taskType :: Lens.Lens' AuditTaskMetadata (Core.Maybe AuditTaskType)
auditTaskMetadata_taskType = Lens.lens (\AuditTaskMetadata' {taskType} -> taskType) (\s@AuditTaskMetadata' {} a -> s {taskType = a} :: AuditTaskMetadata)

instance Core.FromJSON AuditTaskMetadata where
  parseJSON =
    Core.withObject
      "AuditTaskMetadata"
      ( \x ->
          AuditTaskMetadata'
            Core.<$> (x Core..:? "taskId")
            Core.<*> (x Core..:? "taskStatus")
            Core.<*> (x Core..:? "taskType")
      )

instance Core.Hashable AuditTaskMetadata

instance Core.NFData AuditTaskMetadata
