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
-- Module      : Amazonka.DMS.DeleteReplicationTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified replication task.
module Amazonka.DMS.DeleteReplicationTask
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteReplicationTaskResponse'
            Prelude.<$> (x Data..?> "ReplicationTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReplicationTask where
  hashWithSalt _salt DeleteReplicationTask' {..} =
    _salt `Prelude.hashWithSalt` replicationTaskArn

instance Prelude.NFData DeleteReplicationTask where
  rnf DeleteReplicationTask' {..} =
    Prelude.rnf replicationTaskArn

instance Data.ToHeaders DeleteReplicationTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DeleteReplicationTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteReplicationTask where
  toJSON DeleteReplicationTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ReplicationTaskArn" Data..= replicationTaskArn)
          ]
      )

instance Data.ToPath DeleteReplicationTask where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteReplicationTask where
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

instance Prelude.NFData DeleteReplicationTaskResponse where
  rnf DeleteReplicationTaskResponse' {..} =
    Prelude.rnf replicationTask
      `Prelude.seq` Prelude.rnf httpStatus
