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
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionsTaskMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditMitigationActionsTaskMetadata where

import Network.AWS.IoT.Types.AuditMitigationActionsTaskStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an audit mitigation actions task that is returned by
-- @ListAuditMitigationActionsTasks@.
--
-- /See:/ 'newAuditMitigationActionsTaskMetadata' smart constructor.
data AuditMitigationActionsTaskMetadata = AuditMitigationActionsTaskMetadata'
  { -- | The unique identifier for the task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The time at which the audit mitigation actions task was started.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The current state of the audit mitigation actions task.
    taskStatus :: Prelude.Maybe AuditMitigationActionsTaskStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AuditMitigationActionsTaskMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'auditMitigationActionsTaskMetadata_taskId' - The unique identifier for the task.
--
-- 'startTime', 'auditMitigationActionsTaskMetadata_startTime' - The time at which the audit mitigation actions task was started.
--
-- 'taskStatus', 'auditMitigationActionsTaskMetadata_taskStatus' - The current state of the audit mitigation actions task.
newAuditMitigationActionsTaskMetadata ::
  AuditMitigationActionsTaskMetadata
newAuditMitigationActionsTaskMetadata =
  AuditMitigationActionsTaskMetadata'
    { taskId =
        Prelude.Nothing,
      startTime = Prelude.Nothing,
      taskStatus = Prelude.Nothing
    }

-- | The unique identifier for the task.
auditMitigationActionsTaskMetadata_taskId :: Lens.Lens' AuditMitigationActionsTaskMetadata (Prelude.Maybe Prelude.Text)
auditMitigationActionsTaskMetadata_taskId = Lens.lens (\AuditMitigationActionsTaskMetadata' {taskId} -> taskId) (\s@AuditMitigationActionsTaskMetadata' {} a -> s {taskId = a} :: AuditMitigationActionsTaskMetadata)

-- | The time at which the audit mitigation actions task was started.
auditMitigationActionsTaskMetadata_startTime :: Lens.Lens' AuditMitigationActionsTaskMetadata (Prelude.Maybe Prelude.UTCTime)
auditMitigationActionsTaskMetadata_startTime = Lens.lens (\AuditMitigationActionsTaskMetadata' {startTime} -> startTime) (\s@AuditMitigationActionsTaskMetadata' {} a -> s {startTime = a} :: AuditMitigationActionsTaskMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The current state of the audit mitigation actions task.
auditMitigationActionsTaskMetadata_taskStatus :: Lens.Lens' AuditMitigationActionsTaskMetadata (Prelude.Maybe AuditMitigationActionsTaskStatus)
auditMitigationActionsTaskMetadata_taskStatus = Lens.lens (\AuditMitigationActionsTaskMetadata' {taskStatus} -> taskStatus) (\s@AuditMitigationActionsTaskMetadata' {} a -> s {taskStatus = a} :: AuditMitigationActionsTaskMetadata)

instance
  Prelude.FromJSON
    AuditMitigationActionsTaskMetadata
  where
  parseJSON =
    Prelude.withObject
      "AuditMitigationActionsTaskMetadata"
      ( \x ->
          AuditMitigationActionsTaskMetadata'
            Prelude.<$> (x Prelude..:? "taskId")
            Prelude.<*> (x Prelude..:? "startTime")
            Prelude.<*> (x Prelude..:? "taskStatus")
      )

instance
  Prelude.Hashable
    AuditMitigationActionsTaskMetadata

instance
  Prelude.NFData
    AuditMitigationActionsTaskMetadata
