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
-- Module      : Network.AWS.DMS.DeleteReplicationTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified replication task.
module Network.AWS.DMS.DeleteReplicationTask
  ( -- * Creating a Request
    DeleteReplicationTask (..),
    newDeleteReplicationTask,

    -- * Request Lenses
    deleteReplicationTask_replicationTaskArn,

    -- * Destructuring the Response
    DeleteReplicationTaskResponse (..),
    newDeleteReplicationTaskResponse,

    -- * Response Lenses
    deleteReplicationTaskResponse_replicationTask,
    deleteReplicationTaskResponse_httpStatus,
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
-- /See:/ 'newDeleteReplicationTask' smart constructor.
data DeleteReplicationTask = DeleteReplicationTask'
  { -- | The Amazon Resource Name (ARN) of the replication task to be deleted.
    replicationTaskArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReplicationTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTaskArn', 'deleteReplicationTask_replicationTaskArn' - The Amazon Resource Name (ARN) of the replication task to be deleted.
newDeleteReplicationTask ::
  -- | 'replicationTaskArn'
  Prelude.Text ->
  DeleteReplicationTask
newDeleteReplicationTask pReplicationTaskArn_ =
  DeleteReplicationTask'
    { replicationTaskArn =
        pReplicationTaskArn_
    }

-- | The Amazon Resource Name (ARN) of the replication task to be deleted.
deleteReplicationTask_replicationTaskArn :: Lens.Lens' DeleteReplicationTask Prelude.Text
deleteReplicationTask_replicationTaskArn = Lens.lens (\DeleteReplicationTask' {replicationTaskArn} -> replicationTaskArn) (\s@DeleteReplicationTask' {} a -> s {replicationTaskArn = a} :: DeleteReplicationTask)

instance Core.AWSRequest DeleteReplicationTask where
  type
    AWSResponse DeleteReplicationTask =
      DeleteReplicationTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteReplicationTaskResponse'
            Prelude.<$> (x Core..?> "ReplicationTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReplicationTask

instance Prelude.NFData DeleteReplicationTask

instance Core.ToHeaders DeleteReplicationTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DeleteReplicationTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteReplicationTask where
  toJSON DeleteReplicationTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ReplicationTaskArn" Core..= replicationTaskArn)
          ]
      )

instance Core.ToPath DeleteReplicationTask where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteReplicationTask where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDeleteReplicationTaskResponse' smart constructor.
data DeleteReplicationTaskResponse = DeleteReplicationTaskResponse'
  { -- | The deleted replication task.
    replicationTask :: Prelude.Maybe ReplicationTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReplicationTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTask', 'deleteReplicationTaskResponse_replicationTask' - The deleted replication task.
--
-- 'httpStatus', 'deleteReplicationTaskResponse_httpStatus' - The response's http status code.
newDeleteReplicationTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteReplicationTaskResponse
newDeleteReplicationTaskResponse pHttpStatus_ =
  DeleteReplicationTaskResponse'
    { replicationTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The deleted replication task.
deleteReplicationTaskResponse_replicationTask :: Lens.Lens' DeleteReplicationTaskResponse (Prelude.Maybe ReplicationTask)
deleteReplicationTaskResponse_replicationTask = Lens.lens (\DeleteReplicationTaskResponse' {replicationTask} -> replicationTask) (\s@DeleteReplicationTaskResponse' {} a -> s {replicationTask = a} :: DeleteReplicationTaskResponse)

-- | The response's http status code.
deleteReplicationTaskResponse_httpStatus :: Lens.Lens' DeleteReplicationTaskResponse Prelude.Int
deleteReplicationTaskResponse_httpStatus = Lens.lens (\DeleteReplicationTaskResponse' {httpStatus} -> httpStatus) (\s@DeleteReplicationTaskResponse' {} a -> s {httpStatus = a} :: DeleteReplicationTaskResponse)

instance Prelude.NFData DeleteReplicationTaskResponse
