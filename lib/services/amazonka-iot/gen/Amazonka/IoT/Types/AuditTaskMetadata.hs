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
-- Module      : Amazonka.IoT.Types.AuditTaskMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuditTaskMetadata where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.AuditTaskStatus
import Amazonka.IoT.Types.AuditTaskType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The audits that were performed.
--
-- /See:/ 'newAuditTaskMetadata' smart constructor.
data AuditTaskMetadata = AuditTaskMetadata'
  { -- | The type of this audit. One of \"ON_DEMAND_AUDIT_TASK\" or
    -- \"SCHEDULED_AUDIT_TASK\".
    taskType :: Prelude.Maybe AuditTaskType,
    -- | The ID of this audit.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The status of this audit. One of \"IN_PROGRESS\", \"COMPLETED\",
    -- \"FAILED\", or \"CANCELED\".
    taskStatus :: Prelude.Maybe AuditTaskStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuditTaskMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskType', 'auditTaskMetadata_taskType' - The type of this audit. One of \"ON_DEMAND_AUDIT_TASK\" or
-- \"SCHEDULED_AUDIT_TASK\".
--
-- 'taskId', 'auditTaskMetadata_taskId' - The ID of this audit.
--
-- 'taskStatus', 'auditTaskMetadata_taskStatus' - The status of this audit. One of \"IN_PROGRESS\", \"COMPLETED\",
-- \"FAILED\", or \"CANCELED\".
newAuditTaskMetadata ::
  AuditTaskMetadata
newAuditTaskMetadata =
  AuditTaskMetadata'
    { taskType = Prelude.Nothing,
      taskId = Prelude.Nothing,
      taskStatus = Prelude.Nothing
    }

-- | The type of this audit. One of \"ON_DEMAND_AUDIT_TASK\" or
-- \"SCHEDULED_AUDIT_TASK\".
auditTaskMetadata_taskType :: Lens.Lens' AuditTaskMetadata (Prelude.Maybe AuditTaskType)
auditTaskMetadata_taskType = Lens.lens (\AuditTaskMetadata' {taskType} -> taskType) (\s@AuditTaskMetadata' {} a -> s {taskType = a} :: AuditTaskMetadata)

-- | The ID of this audit.
auditTaskMetadata_taskId :: Lens.Lens' AuditTaskMetadata (Prelude.Maybe Prelude.Text)
auditTaskMetadata_taskId = Lens.lens (\AuditTaskMetadata' {taskId} -> taskId) (\s@AuditTaskMetadata' {} a -> s {taskId = a} :: AuditTaskMetadata)

-- | The status of this audit. One of \"IN_PROGRESS\", \"COMPLETED\",
-- \"FAILED\", or \"CANCELED\".
auditTaskMetadata_taskStatus :: Lens.Lens' AuditTaskMetadata (Prelude.Maybe AuditTaskStatus)
auditTaskMetadata_taskStatus = Lens.lens (\AuditTaskMetadata' {taskStatus} -> taskStatus) (\s@AuditTaskMetadata' {} a -> s {taskStatus = a} :: AuditTaskMetadata)

instance Core.FromJSON AuditTaskMetadata where
  parseJSON =
    Core.withObject
      "AuditTaskMetadata"
      ( \x ->
          AuditTaskMetadata'
            Prelude.<$> (x Core..:? "taskType")
            Prelude.<*> (x Core..:? "taskId")
            Prelude.<*> (x Core..:? "taskStatus")
      )

instance Prelude.Hashable AuditTaskMetadata where
  hashWithSalt _salt AuditTaskMetadata' {..} =
    _salt `Prelude.hashWithSalt` taskType
      `Prelude.hashWithSalt` taskId
      `Prelude.hashWithSalt` taskStatus

instance Prelude.NFData AuditTaskMetadata where
  rnf AuditTaskMetadata' {..} =
    Prelude.rnf taskType
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf taskStatus
