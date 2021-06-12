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
-- Module      : Network.AWS.DMS.DeleteReplicationTaskAssessmentRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the record of a single premigration assessment run.
--
-- This operation removes all metadata that AWS DMS maintains about this
-- assessment run. However, the operation leaves untouched all information
-- about this assessment run that is stored in your Amazon S3 bucket.
module Network.AWS.DMS.DeleteReplicationTaskAssessmentRun
  ( -- * Creating a Request
    DeleteReplicationTaskAssessmentRun (..),
    newDeleteReplicationTaskAssessmentRun,

    -- * Request Lenses
    deleteReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn,

    -- * Destructuring the Response
    DeleteReplicationTaskAssessmentRunResponse (..),
    newDeleteReplicationTaskAssessmentRunResponse,

    -- * Response Lenses
    deleteReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun,
    deleteReplicationTaskAssessmentRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteReplicationTaskAssessmentRun' smart constructor.
data DeleteReplicationTaskAssessmentRun = DeleteReplicationTaskAssessmentRun'
  { -- | Amazon Resource Name (ARN) of the premigration assessment run to be
    -- deleted.
    replicationTaskAssessmentRunArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteReplicationTaskAssessmentRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTaskAssessmentRunArn', 'deleteReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn' - Amazon Resource Name (ARN) of the premigration assessment run to be
-- deleted.
newDeleteReplicationTaskAssessmentRun ::
  -- | 'replicationTaskAssessmentRunArn'
  Core.Text ->
  DeleteReplicationTaskAssessmentRun
newDeleteReplicationTaskAssessmentRun
  pReplicationTaskAssessmentRunArn_ =
    DeleteReplicationTaskAssessmentRun'
      { replicationTaskAssessmentRunArn =
          pReplicationTaskAssessmentRunArn_
      }

-- | Amazon Resource Name (ARN) of the premigration assessment run to be
-- deleted.
deleteReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn :: Lens.Lens' DeleteReplicationTaskAssessmentRun Core.Text
deleteReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn = Lens.lens (\DeleteReplicationTaskAssessmentRun' {replicationTaskAssessmentRunArn} -> replicationTaskAssessmentRunArn) (\s@DeleteReplicationTaskAssessmentRun' {} a -> s {replicationTaskAssessmentRunArn = a} :: DeleteReplicationTaskAssessmentRun)

instance
  Core.AWSRequest
    DeleteReplicationTaskAssessmentRun
  where
  type
    AWSResponse DeleteReplicationTaskAssessmentRun =
      DeleteReplicationTaskAssessmentRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteReplicationTaskAssessmentRunResponse'
            Core.<$> (x Core..?> "ReplicationTaskAssessmentRun")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeleteReplicationTaskAssessmentRun

instance
  Core.NFData
    DeleteReplicationTaskAssessmentRun

instance
  Core.ToHeaders
    DeleteReplicationTaskAssessmentRun
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DeleteReplicationTaskAssessmentRun" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DeleteReplicationTaskAssessmentRun
  where
  toJSON DeleteReplicationTaskAssessmentRun' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ReplicationTaskAssessmentRunArn"
                  Core..= replicationTaskAssessmentRunArn
              )
          ]
      )

instance
  Core.ToPath
    DeleteReplicationTaskAssessmentRun
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DeleteReplicationTaskAssessmentRun
  where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDeleteReplicationTaskAssessmentRunResponse' smart constructor.
data DeleteReplicationTaskAssessmentRunResponse = DeleteReplicationTaskAssessmentRunResponse'
  { -- | The @ReplicationTaskAssessmentRun@ object for the deleted assessment
    -- run.
    replicationTaskAssessmentRun :: Core.Maybe ReplicationTaskAssessmentRun,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteReplicationTaskAssessmentRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTaskAssessmentRun', 'deleteReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun' - The @ReplicationTaskAssessmentRun@ object for the deleted assessment
-- run.
--
-- 'httpStatus', 'deleteReplicationTaskAssessmentRunResponse_httpStatus' - The response's http status code.
newDeleteReplicationTaskAssessmentRunResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteReplicationTaskAssessmentRunResponse
newDeleteReplicationTaskAssessmentRunResponse
  pHttpStatus_ =
    DeleteReplicationTaskAssessmentRunResponse'
      { replicationTaskAssessmentRun =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @ReplicationTaskAssessmentRun@ object for the deleted assessment
-- run.
deleteReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun :: Lens.Lens' DeleteReplicationTaskAssessmentRunResponse (Core.Maybe ReplicationTaskAssessmentRun)
deleteReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun = Lens.lens (\DeleteReplicationTaskAssessmentRunResponse' {replicationTaskAssessmentRun} -> replicationTaskAssessmentRun) (\s@DeleteReplicationTaskAssessmentRunResponse' {} a -> s {replicationTaskAssessmentRun = a} :: DeleteReplicationTaskAssessmentRunResponse)

-- | The response's http status code.
deleteReplicationTaskAssessmentRunResponse_httpStatus :: Lens.Lens' DeleteReplicationTaskAssessmentRunResponse Core.Int
deleteReplicationTaskAssessmentRunResponse_httpStatus = Lens.lens (\DeleteReplicationTaskAssessmentRunResponse' {httpStatus} -> httpStatus) (\s@DeleteReplicationTaskAssessmentRunResponse' {} a -> s {httpStatus = a} :: DeleteReplicationTaskAssessmentRunResponse)

instance
  Core.NFData
    DeleteReplicationTaskAssessmentRunResponse
