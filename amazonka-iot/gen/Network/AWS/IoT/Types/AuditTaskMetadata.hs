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
-- Module      : Network.AWS.IoT.Types.AuditTaskMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditTaskMetadata where

import Network.AWS.IoT.Types.AuditTaskStatus
import Network.AWS.IoT.Types.AuditTaskType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The audits that were performed.
--
-- /See:/ 'newAuditTaskMetadata' smart constructor.
data AuditTaskMetadata = AuditTaskMetadata'
  { -- | The ID of this audit.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The status of this audit. One of \"IN_PROGRESS\", \"COMPLETED\",
    -- \"FAILED\", or \"CANCELED\".
    taskStatus :: Prelude.Maybe AuditTaskStatus,
    -- | The type of this audit. One of \"ON_DEMAND_AUDIT_TASK\" or
    -- \"SCHEDULED_AUDIT_TASK\".
    taskType :: Prelude.Maybe AuditTaskType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { taskId = Prelude.Nothing,
      taskStatus = Prelude.Nothing,
      taskType = Prelude.Nothing
    }

-- | The ID of this audit.
auditTaskMetadata_taskId :: Lens.Lens' AuditTaskMetadata (Prelude.Maybe Prelude.Text)
auditTaskMetadata_taskId = Lens.lens (\AuditTaskMetadata' {taskId} -> taskId) (\s@AuditTaskMetadata' {} a -> s {taskId = a} :: AuditTaskMetadata)

-- | The status of this audit. One of \"IN_PROGRESS\", \"COMPLETED\",
-- \"FAILED\", or \"CANCELED\".
auditTaskMetadata_taskStatus :: Lens.Lens' AuditTaskMetadata (Prelude.Maybe AuditTaskStatus)
auditTaskMetadata_taskStatus = Lens.lens (\AuditTaskMetadata' {taskStatus} -> taskStatus) (\s@AuditTaskMetadata' {} a -> s {taskStatus = a} :: AuditTaskMetadata)

-- | The type of this audit. One of \"ON_DEMAND_AUDIT_TASK\" or
-- \"SCHEDULED_AUDIT_TASK\".
auditTaskMetadata_taskType :: Lens.Lens' AuditTaskMetadata (Prelude.Maybe AuditTaskType)
auditTaskMetadata_taskType = Lens.lens (\AuditTaskMetadata' {taskType} -> taskType) (\s@AuditTaskMetadata' {} a -> s {taskType = a} :: AuditTaskMetadata)

instance Prelude.FromJSON AuditTaskMetadata where
  parseJSON =
    Prelude.withObject
      "AuditTaskMetadata"
      ( \x ->
          AuditTaskMetadata'
            Prelude.<$> (x Prelude..:? "taskId")
            Prelude.<*> (x Prelude..:? "taskStatus")
            Prelude.<*> (x Prelude..:? "taskType")
      )

instance Prelude.Hashable AuditTaskMetadata

instance Prelude.NFData AuditTaskMetadata
