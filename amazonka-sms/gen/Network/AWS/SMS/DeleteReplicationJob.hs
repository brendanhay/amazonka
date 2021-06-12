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
-- Module      : Network.AWS.SMS.DeleteReplicationJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified replication job.
--
-- After you delete a replication job, there are no further replication
-- runs. AWS deletes the contents of the Amazon S3 bucket used to store AWS
-- SMS artifacts. The AMIs created by the replication runs are not deleted.
module Network.AWS.SMS.DeleteReplicationJob
  ( -- * Creating a Request
    DeleteReplicationJob (..),
    newDeleteReplicationJob,

    -- * Request Lenses
    deleteReplicationJob_replicationJobId,

    -- * Destructuring the Response
    DeleteReplicationJobResponse (..),
    newDeleteReplicationJobResponse,

    -- * Response Lenses
    deleteReplicationJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newDeleteReplicationJob' smart constructor.
data DeleteReplicationJob = DeleteReplicationJob'
  { -- | The ID of the replication job.
    replicationJobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteReplicationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationJobId', 'deleteReplicationJob_replicationJobId' - The ID of the replication job.
newDeleteReplicationJob ::
  -- | 'replicationJobId'
  Core.Text ->
  DeleteReplicationJob
newDeleteReplicationJob pReplicationJobId_ =
  DeleteReplicationJob'
    { replicationJobId =
        pReplicationJobId_
    }

-- | The ID of the replication job.
deleteReplicationJob_replicationJobId :: Lens.Lens' DeleteReplicationJob Core.Text
deleteReplicationJob_replicationJobId = Lens.lens (\DeleteReplicationJob' {replicationJobId} -> replicationJobId) (\s@DeleteReplicationJob' {} a -> s {replicationJobId = a} :: DeleteReplicationJob)

instance Core.AWSRequest DeleteReplicationJob where
  type
    AWSResponse DeleteReplicationJob =
      DeleteReplicationJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteReplicationJobResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteReplicationJob

instance Core.NFData DeleteReplicationJob

instance Core.ToHeaders DeleteReplicationJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.DeleteReplicationJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteReplicationJob where
  toJSON DeleteReplicationJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("replicationJobId" Core..= replicationJobId)
          ]
      )

instance Core.ToPath DeleteReplicationJob where
  toPath = Core.const "/"

instance Core.ToQuery DeleteReplicationJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteReplicationJobResponse' smart constructor.
data DeleteReplicationJobResponse = DeleteReplicationJobResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteReplicationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteReplicationJobResponse_httpStatus' - The response's http status code.
newDeleteReplicationJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteReplicationJobResponse
newDeleteReplicationJobResponse pHttpStatus_ =
  DeleteReplicationJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteReplicationJobResponse_httpStatus :: Lens.Lens' DeleteReplicationJobResponse Core.Int
deleteReplicationJobResponse_httpStatus = Lens.lens (\DeleteReplicationJobResponse' {httpStatus} -> httpStatus) (\s@DeleteReplicationJobResponse' {} a -> s {httpStatus = a} :: DeleteReplicationJobResponse)

instance Core.NFData DeleteReplicationJobResponse
