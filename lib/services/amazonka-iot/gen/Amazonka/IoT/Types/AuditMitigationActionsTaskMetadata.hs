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
-- Module      : Amazonka.IoT.Types.AuditMitigationActionsTaskMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuditMitigationActionsTaskMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.AuditMitigationActionsTaskStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about an audit mitigation actions task that is returned by
-- @ListAuditMitigationActionsTasks@.
--
-- /See:/ 'newAuditMitigationActionsTaskMetadata' smart constructor.
data AuditMitigationActionsTaskMetadata = AuditMitigationActionsTaskMetadata'
  { -- | The time at which the audit mitigation actions task was started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier for the task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the audit mitigation actions task.
    taskStatus :: Prelude.Maybe AuditMitigationActionsTaskStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuditMitigationActionsTaskMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'auditMitigationActionsTaskMetadata_startTime' - The time at which the audit mitigation actions task was started.
--
-- 'taskId', 'auditMitigationActionsTaskMetadata_taskId' - The unique identifier for the task.
--
-- 'taskStatus', 'auditMitigationActionsTaskMetadata_taskStatus' - The current state of the audit mitigation actions task.
newAuditMitigationActionsTaskMetadata ::
  AuditMitigationActionsTaskMetadata
newAuditMitigationActionsTaskMetadata =
  AuditMitigationActionsTaskMetadata'
    { startTime =
        Prelude.Nothing,
      taskId = Prelude.Nothing,
      taskStatus = Prelude.Nothing
    }

-- | The time at which the audit mitigation actions task was started.
auditMitigationActionsTaskMetadata_startTime :: Lens.Lens' AuditMitigationActionsTaskMetadata (Prelude.Maybe Prelude.UTCTime)
auditMitigationActionsTaskMetadata_startTime = Lens.lens (\AuditMitigationActionsTaskMetadata' {startTime} -> startTime) (\s@AuditMitigationActionsTaskMetadata' {} a -> s {startTime = a} :: AuditMitigationActionsTaskMetadata) Prelude.. Lens.mapping Data._Time

-- | The unique identifier for the task.
auditMitigationActionsTaskMetadata_taskId :: Lens.Lens' AuditMitigationActionsTaskMetadata (Prelude.Maybe Prelude.Text)
auditMitigationActionsTaskMetadata_taskId = Lens.lens (\AuditMitigationActionsTaskMetadata' {taskId} -> taskId) (\s@AuditMitigationActionsTaskMetadata' {} a -> s {taskId = a} :: AuditMitigationActionsTaskMetadata)

-- | The current state of the audit mitigation actions task.
auditMitigationActionsTaskMetadata_taskStatus :: Lens.Lens' AuditMitigationActionsTaskMetadata (Prelude.Maybe AuditMitigationActionsTaskStatus)
auditMitigationActionsTaskMetadata_taskStatus = Lens.lens (\AuditMitigationActionsTaskMetadata' {taskStatus} -> taskStatus) (\s@AuditMitigationActionsTaskMetadata' {} a -> s {taskStatus = a} :: AuditMitigationActionsTaskMetadata)

instance
  Data.FromJSON
    AuditMitigationActionsTaskMetadata
  where
  parseJSON =
    Data.withObject
      "AuditMitigationActionsTaskMetadata"
      ( \x ->
          AuditMitigationActionsTaskMetadata'
            Prelude.<$> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "taskId")
            Prelude.<*> (x Data..:? "taskStatus")
      )

instance
  Prelude.Hashable
    AuditMitigationActionsTaskMetadata
  where
  hashWithSalt
    _salt
    AuditMitigationActionsTaskMetadata' {..} =
      _salt `Prelude.hashWithSalt` startTime
        `Prelude.hashWithSalt` taskId
        `Prelude.hashWithSalt` taskStatus

instance
  Prelude.NFData
    AuditMitigationActionsTaskMetadata
  where
  rnf AuditMitigationActionsTaskMetadata' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf taskStatus
