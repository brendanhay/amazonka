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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteReplicationTaskAssessmentRun' smart constructor.
data DeleteReplicationTaskAssessmentRun = DeleteReplicationTaskAssessmentRun'
  { -- | Amazon Resource Name (ARN) of the premigration assessment run to be
    -- deleted.
    replicationTaskAssessmentRunArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteReplicationTaskAssessmentRun
newDeleteReplicationTaskAssessmentRun
  pReplicationTaskAssessmentRunArn_ =
    DeleteReplicationTaskAssessmentRun'
      { replicationTaskAssessmentRunArn =
          pReplicationTaskAssessmentRunArn_
      }

-- | Amazon Resource Name (ARN) of the premigration assessment run to be
-- deleted.
deleteReplicationTaskAssessmentRun_replicationTaskAssessmentRunArn :: Lens.Lens' DeleteReplicationTaskAssessmentRun Prelude.Text
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
            Prelude.<$> (x Core..?> "ReplicationTaskAssessmentRun")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteReplicationTaskAssessmentRun

instance
  Prelude.NFData
    DeleteReplicationTaskAssessmentRun

instance
  Core.ToHeaders
    DeleteReplicationTaskAssessmentRun
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DeleteReplicationTaskAssessmentRun" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DeleteReplicationTaskAssessmentRun
  where
  toJSON DeleteReplicationTaskAssessmentRun' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ReplicationTaskAssessmentRunArn"
                  Core..= replicationTaskAssessmentRunArn
              )
          ]
      )

instance
  Core.ToPath
    DeleteReplicationTaskAssessmentRun
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeleteReplicationTaskAssessmentRun
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDeleteReplicationTaskAssessmentRunResponse' smart constructor.
data DeleteReplicationTaskAssessmentRunResponse = DeleteReplicationTaskAssessmentRunResponse'
  { -- | The @ReplicationTaskAssessmentRun@ object for the deleted assessment
    -- run.
    replicationTaskAssessmentRun :: Prelude.Maybe ReplicationTaskAssessmentRun,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteReplicationTaskAssessmentRunResponse
newDeleteReplicationTaskAssessmentRunResponse
  pHttpStatus_ =
    DeleteReplicationTaskAssessmentRunResponse'
      { replicationTaskAssessmentRun =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @ReplicationTaskAssessmentRun@ object for the deleted assessment
-- run.
deleteReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun :: Lens.Lens' DeleteReplicationTaskAssessmentRunResponse (Prelude.Maybe ReplicationTaskAssessmentRun)
deleteReplicationTaskAssessmentRunResponse_replicationTaskAssessmentRun = Lens.lens (\DeleteReplicationTaskAssessmentRunResponse' {replicationTaskAssessmentRun} -> replicationTaskAssessmentRun) (\s@DeleteReplicationTaskAssessmentRunResponse' {} a -> s {replicationTaskAssessmentRun = a} :: DeleteReplicationTaskAssessmentRunResponse)

-- | The response's http status code.
deleteReplicationTaskAssessmentRunResponse_httpStatus :: Lens.Lens' DeleteReplicationTaskAssessmentRunResponse Prelude.Int
deleteReplicationTaskAssessmentRunResponse_httpStatus = Lens.lens (\DeleteReplicationTaskAssessmentRunResponse' {httpStatus} -> httpStatus) (\s@DeleteReplicationTaskAssessmentRunResponse' {} a -> s {httpStatus = a} :: DeleteReplicationTaskAssessmentRunResponse)

instance
  Prelude.NFData
    DeleteReplicationTaskAssessmentRunResponse
