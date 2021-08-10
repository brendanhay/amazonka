{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.StartReplicationTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the replication task.
--
-- For more information about AWS DMS tasks, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html Working with Migration Tasks>
-- in the /AWS Database Migration Service User Guide./
module Network.AWS.DMS.StartReplicationTask
  ( -- * Creating a Request
    StartReplicationTask (..),
    newStartReplicationTask,

    -- * Request Lenses
    startReplicationTask_cdcStartTime,
    startReplicationTask_cdcStopPosition,
    startReplicationTask_cdcStartPosition,
    startReplicationTask_replicationTaskArn,
    startReplicationTask_startReplicationTaskType,

    -- * Destructuring the Response
    StartReplicationTaskResponse (..),
    newStartReplicationTaskResponse,

    -- * Response Lenses
    startReplicationTaskResponse_replicationTask,
    startReplicationTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newStartReplicationTask' smart constructor.
data StartReplicationTask = StartReplicationTask'
  { -- | Indicates the start time for a change data capture (CDC) operation. Use
    -- either CdcStartTime or CdcStartPosition to specify when you want a CDC
    -- operation to start. Specifying both values results in an error.
    --
    -- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
    cdcStartTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates when you want a change data capture (CDC) operation to stop.
    -- The value can be either server time or commit time.
    --
    -- Server time example: --cdc-stop-position
    -- “server_time:2018-02-09T12:12:12”
    --
    -- Commit time example: --cdc-stop-position “commit_time:
    -- 2018-02-09T12:12:12 “
    cdcStopPosition :: Prelude.Maybe Prelude.Text,
    -- | Indicates when you want a change data capture (CDC) operation to start.
    -- Use either CdcStartPosition or CdcStartTime to specify when you want a
    -- CDC operation to start. Specifying both values results in an error.
    --
    -- The value can be in date, checkpoint, or LSN\/SCN format.
    --
    -- Date Example: --cdc-start-position “2018-03-08T12:12:12”
    --
    -- Checkpoint Example: --cdc-start-position
    -- \"checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93\"
    --
    -- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
    --
    -- When you use this task setting with a source PostgreSQL database, a
    -- logical replication slot should already be created and associated with
    -- the source endpoint. You can verify this by setting the @slotName@ extra
    -- connection attribute to the name of this logical replication slot. For
    -- more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html#CHAP_Source.PostgreSQL.ConnectionAttrib Extra Connection Attributes When Using PostgreSQL as a Source for AWS DMS>.
    cdcStartPosition :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the replication task to be started.
    replicationTaskArn :: Prelude.Text,
    -- | A type of replication task.
    startReplicationTaskType :: StartReplicationTaskTypeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReplicationTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cdcStartTime', 'startReplicationTask_cdcStartTime' - Indicates the start time for a change data capture (CDC) operation. Use
-- either CdcStartTime or CdcStartPosition to specify when you want a CDC
-- operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
--
-- 'cdcStopPosition', 'startReplicationTask_cdcStopPosition' - Indicates when you want a change data capture (CDC) operation to stop.
-- The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position
-- “server_time:2018-02-09T12:12:12”
--
-- Commit time example: --cdc-stop-position “commit_time:
-- 2018-02-09T12:12:12 “
--
-- 'cdcStartPosition', 'startReplicationTask_cdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start.
-- Use either CdcStartPosition or CdcStartTime to specify when you want a
-- CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN\/SCN format.
--
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
--
-- Checkpoint Example: --cdc-start-position
-- \"checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93\"
--
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- When you use this task setting with a source PostgreSQL database, a
-- logical replication slot should already be created and associated with
-- the source endpoint. You can verify this by setting the @slotName@ extra
-- connection attribute to the name of this logical replication slot. For
-- more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html#CHAP_Source.PostgreSQL.ConnectionAttrib Extra Connection Attributes When Using PostgreSQL as a Source for AWS DMS>.
--
-- 'replicationTaskArn', 'startReplicationTask_replicationTaskArn' - The Amazon Resource Name (ARN) of the replication task to be started.
--
-- 'startReplicationTaskType', 'startReplicationTask_startReplicationTaskType' - A type of replication task.
newStartReplicationTask ::
  -- | 'replicationTaskArn'
  Prelude.Text ->
  -- | 'startReplicationTaskType'
  StartReplicationTaskTypeValue ->
  StartReplicationTask
newStartReplicationTask
  pReplicationTaskArn_
  pStartReplicationTaskType_ =
    StartReplicationTask'
      { cdcStartTime =
          Prelude.Nothing,
        cdcStopPosition = Prelude.Nothing,
        cdcStartPosition = Prelude.Nothing,
        replicationTaskArn = pReplicationTaskArn_,
        startReplicationTaskType =
          pStartReplicationTaskType_
      }

-- | Indicates the start time for a change data capture (CDC) operation. Use
-- either CdcStartTime or CdcStartPosition to specify when you want a CDC
-- operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
startReplicationTask_cdcStartTime :: Lens.Lens' StartReplicationTask (Prelude.Maybe Prelude.UTCTime)
startReplicationTask_cdcStartTime = Lens.lens (\StartReplicationTask' {cdcStartTime} -> cdcStartTime) (\s@StartReplicationTask' {} a -> s {cdcStartTime = a} :: StartReplicationTask) Prelude.. Lens.mapping Core._Time

-- | Indicates when you want a change data capture (CDC) operation to stop.
-- The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position
-- “server_time:2018-02-09T12:12:12”
--
-- Commit time example: --cdc-stop-position “commit_time:
-- 2018-02-09T12:12:12 “
startReplicationTask_cdcStopPosition :: Lens.Lens' StartReplicationTask (Prelude.Maybe Prelude.Text)
startReplicationTask_cdcStopPosition = Lens.lens (\StartReplicationTask' {cdcStopPosition} -> cdcStopPosition) (\s@StartReplicationTask' {} a -> s {cdcStopPosition = a} :: StartReplicationTask)

-- | Indicates when you want a change data capture (CDC) operation to start.
-- Use either CdcStartPosition or CdcStartTime to specify when you want a
-- CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN\/SCN format.
--
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
--
-- Checkpoint Example: --cdc-start-position
-- \"checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93\"
--
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- When you use this task setting with a source PostgreSQL database, a
-- logical replication slot should already be created and associated with
-- the source endpoint. You can verify this by setting the @slotName@ extra
-- connection attribute to the name of this logical replication slot. For
-- more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html#CHAP_Source.PostgreSQL.ConnectionAttrib Extra Connection Attributes When Using PostgreSQL as a Source for AWS DMS>.
startReplicationTask_cdcStartPosition :: Lens.Lens' StartReplicationTask (Prelude.Maybe Prelude.Text)
startReplicationTask_cdcStartPosition = Lens.lens (\StartReplicationTask' {cdcStartPosition} -> cdcStartPosition) (\s@StartReplicationTask' {} a -> s {cdcStartPosition = a} :: StartReplicationTask)

-- | The Amazon Resource Name (ARN) of the replication task to be started.
startReplicationTask_replicationTaskArn :: Lens.Lens' StartReplicationTask Prelude.Text
startReplicationTask_replicationTaskArn = Lens.lens (\StartReplicationTask' {replicationTaskArn} -> replicationTaskArn) (\s@StartReplicationTask' {} a -> s {replicationTaskArn = a} :: StartReplicationTask)

-- | A type of replication task.
startReplicationTask_startReplicationTaskType :: Lens.Lens' StartReplicationTask StartReplicationTaskTypeValue
startReplicationTask_startReplicationTaskType = Lens.lens (\StartReplicationTask' {startReplicationTaskType} -> startReplicationTaskType) (\s@StartReplicationTask' {} a -> s {startReplicationTaskType = a} :: StartReplicationTask)

instance Core.AWSRequest StartReplicationTask where
  type
    AWSResponse StartReplicationTask =
      StartReplicationTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartReplicationTaskResponse'
            Prelude.<$> (x Core..?> "ReplicationTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartReplicationTask

instance Prelude.NFData StartReplicationTask

instance Core.ToHeaders StartReplicationTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.StartReplicationTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartReplicationTask where
  toJSON StartReplicationTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CdcStartTime" Core..=) Prelude.<$> cdcStartTime,
            ("CdcStopPosition" Core..=)
              Prelude.<$> cdcStopPosition,
            ("CdcStartPosition" Core..=)
              Prelude.<$> cdcStartPosition,
            Prelude.Just
              ("ReplicationTaskArn" Core..= replicationTaskArn),
            Prelude.Just
              ( "StartReplicationTaskType"
                  Core..= startReplicationTaskType
              )
          ]
      )

instance Core.ToPath StartReplicationTask where
  toPath = Prelude.const "/"

instance Core.ToQuery StartReplicationTask where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newStartReplicationTaskResponse' smart constructor.
data StartReplicationTaskResponse = StartReplicationTaskResponse'
  { -- | The replication task started.
    replicationTask :: Prelude.Maybe ReplicationTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReplicationTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTask', 'startReplicationTaskResponse_replicationTask' - The replication task started.
--
-- 'httpStatus', 'startReplicationTaskResponse_httpStatus' - The response's http status code.
newStartReplicationTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartReplicationTaskResponse
newStartReplicationTaskResponse pHttpStatus_ =
  StartReplicationTaskResponse'
    { replicationTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The replication task started.
startReplicationTaskResponse_replicationTask :: Lens.Lens' StartReplicationTaskResponse (Prelude.Maybe ReplicationTask)
startReplicationTaskResponse_replicationTask = Lens.lens (\StartReplicationTaskResponse' {replicationTask} -> replicationTask) (\s@StartReplicationTaskResponse' {} a -> s {replicationTask = a} :: StartReplicationTaskResponse)

-- | The response's http status code.
startReplicationTaskResponse_httpStatus :: Lens.Lens' StartReplicationTaskResponse Prelude.Int
startReplicationTaskResponse_httpStatus = Lens.lens (\StartReplicationTaskResponse' {httpStatus} -> httpStatus) (\s@StartReplicationTaskResponse' {} a -> s {httpStatus = a} :: StartReplicationTaskResponse)

instance Prelude.NFData StartReplicationTaskResponse
