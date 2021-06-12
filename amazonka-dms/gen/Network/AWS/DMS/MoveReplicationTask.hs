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
-- Module      : Network.AWS.DMS.MoveReplicationTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves a replication task from its current replication instance to a
-- different target replication instance using the specified parameters.
-- The target replication instance must be created with the same or later
-- AWS DMS version as the current replication instance.
module Network.AWS.DMS.MoveReplicationTask
  ( -- * Creating a Request
    MoveReplicationTask (..),
    newMoveReplicationTask,

    -- * Request Lenses
    moveReplicationTask_replicationTaskArn,
    moveReplicationTask_targetReplicationInstanceArn,

    -- * Destructuring the Response
    MoveReplicationTaskResponse (..),
    newMoveReplicationTaskResponse,

    -- * Response Lenses
    moveReplicationTaskResponse_replicationTask,
    moveReplicationTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newMoveReplicationTask' smart constructor.
data MoveReplicationTask = MoveReplicationTask'
  { -- | The Amazon Resource Name (ARN) of the task that you want to move.
    replicationTaskArn :: Core.Text,
    -- | The ARN of the replication instance where you want to move the task to.
    targetReplicationInstanceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MoveReplicationTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTaskArn', 'moveReplicationTask_replicationTaskArn' - The Amazon Resource Name (ARN) of the task that you want to move.
--
-- 'targetReplicationInstanceArn', 'moveReplicationTask_targetReplicationInstanceArn' - The ARN of the replication instance where you want to move the task to.
newMoveReplicationTask ::
  -- | 'replicationTaskArn'
  Core.Text ->
  -- | 'targetReplicationInstanceArn'
  Core.Text ->
  MoveReplicationTask
newMoveReplicationTask
  pReplicationTaskArn_
  pTargetReplicationInstanceArn_ =
    MoveReplicationTask'
      { replicationTaskArn =
          pReplicationTaskArn_,
        targetReplicationInstanceArn =
          pTargetReplicationInstanceArn_
      }

-- | The Amazon Resource Name (ARN) of the task that you want to move.
moveReplicationTask_replicationTaskArn :: Lens.Lens' MoveReplicationTask Core.Text
moveReplicationTask_replicationTaskArn = Lens.lens (\MoveReplicationTask' {replicationTaskArn} -> replicationTaskArn) (\s@MoveReplicationTask' {} a -> s {replicationTaskArn = a} :: MoveReplicationTask)

-- | The ARN of the replication instance where you want to move the task to.
moveReplicationTask_targetReplicationInstanceArn :: Lens.Lens' MoveReplicationTask Core.Text
moveReplicationTask_targetReplicationInstanceArn = Lens.lens (\MoveReplicationTask' {targetReplicationInstanceArn} -> targetReplicationInstanceArn) (\s@MoveReplicationTask' {} a -> s {targetReplicationInstanceArn = a} :: MoveReplicationTask)

instance Core.AWSRequest MoveReplicationTask where
  type
    AWSResponse MoveReplicationTask =
      MoveReplicationTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          MoveReplicationTaskResponse'
            Core.<$> (x Core..?> "ReplicationTask")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable MoveReplicationTask

instance Core.NFData MoveReplicationTask

instance Core.ToHeaders MoveReplicationTask where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.MoveReplicationTask" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON MoveReplicationTask where
  toJSON MoveReplicationTask' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ReplicationTaskArn" Core..= replicationTaskArn),
            Core.Just
              ( "TargetReplicationInstanceArn"
                  Core..= targetReplicationInstanceArn
              )
          ]
      )

instance Core.ToPath MoveReplicationTask where
  toPath = Core.const "/"

instance Core.ToQuery MoveReplicationTask where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newMoveReplicationTaskResponse' smart constructor.
data MoveReplicationTaskResponse = MoveReplicationTaskResponse'
  { -- | The replication task that was moved.
    replicationTask :: Core.Maybe ReplicationTask,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MoveReplicationTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTask', 'moveReplicationTaskResponse_replicationTask' - The replication task that was moved.
--
-- 'httpStatus', 'moveReplicationTaskResponse_httpStatus' - The response's http status code.
newMoveReplicationTaskResponse ::
  -- | 'httpStatus'
  Core.Int ->
  MoveReplicationTaskResponse
newMoveReplicationTaskResponse pHttpStatus_ =
  MoveReplicationTaskResponse'
    { replicationTask =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The replication task that was moved.
moveReplicationTaskResponse_replicationTask :: Lens.Lens' MoveReplicationTaskResponse (Core.Maybe ReplicationTask)
moveReplicationTaskResponse_replicationTask = Lens.lens (\MoveReplicationTaskResponse' {replicationTask} -> replicationTask) (\s@MoveReplicationTaskResponse' {} a -> s {replicationTask = a} :: MoveReplicationTaskResponse)

-- | The response's http status code.
moveReplicationTaskResponse_httpStatus :: Lens.Lens' MoveReplicationTaskResponse Core.Int
moveReplicationTaskResponse_httpStatus = Lens.lens (\MoveReplicationTaskResponse' {httpStatus} -> httpStatus) (\s@MoveReplicationTaskResponse' {} a -> s {httpStatus = a} :: MoveReplicationTaskResponse)

instance Core.NFData MoveReplicationTaskResponse
