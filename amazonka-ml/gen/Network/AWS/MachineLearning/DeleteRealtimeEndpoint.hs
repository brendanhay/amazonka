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
-- Module      : Network.AWS.MachineLearning.DeleteRealtimeEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a real time endpoint of an @MLModel@.
module Network.AWS.MachineLearning.DeleteRealtimeEndpoint
  ( -- * Creating a Request
    DeleteRealtimeEndpoint (..),
    newDeleteRealtimeEndpoint,

    -- * Request Lenses
    deleteRealtimeEndpoint_mLModelId,

    -- * Destructuring the Response
    DeleteRealtimeEndpointResponse (..),
    newDeleteRealtimeEndpointResponse,

    -- * Response Lenses
    deleteRealtimeEndpointResponse_realtimeEndpointInfo,
    deleteRealtimeEndpointResponse_mLModelId,
    deleteRealtimeEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRealtimeEndpoint' smart constructor.
data DeleteRealtimeEndpoint = DeleteRealtimeEndpoint'
  { -- | The ID assigned to the @MLModel@ during creation.
    mLModelId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRealtimeEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mLModelId', 'deleteRealtimeEndpoint_mLModelId' - The ID assigned to the @MLModel@ during creation.
newDeleteRealtimeEndpoint ::
  -- | 'mLModelId'
  Core.Text ->
  DeleteRealtimeEndpoint
newDeleteRealtimeEndpoint pMLModelId_ =
  DeleteRealtimeEndpoint' {mLModelId = pMLModelId_}

-- | The ID assigned to the @MLModel@ during creation.
deleteRealtimeEndpoint_mLModelId :: Lens.Lens' DeleteRealtimeEndpoint Core.Text
deleteRealtimeEndpoint_mLModelId = Lens.lens (\DeleteRealtimeEndpoint' {mLModelId} -> mLModelId) (\s@DeleteRealtimeEndpoint' {} a -> s {mLModelId = a} :: DeleteRealtimeEndpoint)

instance Core.AWSRequest DeleteRealtimeEndpoint where
  type
    AWSResponse DeleteRealtimeEndpoint =
      DeleteRealtimeEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRealtimeEndpointResponse'
            Core.<$> (x Core..?> "RealtimeEndpointInfo")
            Core.<*> (x Core..?> "MLModelId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteRealtimeEndpoint

instance Core.NFData DeleteRealtimeEndpoint

instance Core.ToHeaders DeleteRealtimeEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonML_20141212.DeleteRealtimeEndpoint" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteRealtimeEndpoint where
  toJSON DeleteRealtimeEndpoint' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("MLModelId" Core..= mLModelId)]
      )

instance Core.ToPath DeleteRealtimeEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRealtimeEndpoint where
  toQuery = Core.const Core.mempty

-- | Represents the output of an @DeleteRealtimeEndpoint@ operation.
--
-- The result contains the @MLModelId@ and the endpoint information for the
-- @MLModel@.
--
-- /See:/ 'newDeleteRealtimeEndpointResponse' smart constructor.
data DeleteRealtimeEndpointResponse = DeleteRealtimeEndpointResponse'
  { -- | The endpoint information of the @MLModel@
    realtimeEndpointInfo :: Core.Maybe RealtimeEndpointInfo,
    -- | A user-supplied ID that uniquely identifies the @MLModel@. This value
    -- should be identical to the value of the @MLModelId@ in the request.
    mLModelId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRealtimeEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'realtimeEndpointInfo', 'deleteRealtimeEndpointResponse_realtimeEndpointInfo' - The endpoint information of the @MLModel@
--
-- 'mLModelId', 'deleteRealtimeEndpointResponse_mLModelId' - A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelId@ in the request.
--
-- 'httpStatus', 'deleteRealtimeEndpointResponse_httpStatus' - The response's http status code.
newDeleteRealtimeEndpointResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteRealtimeEndpointResponse
newDeleteRealtimeEndpointResponse pHttpStatus_ =
  DeleteRealtimeEndpointResponse'
    { realtimeEndpointInfo =
        Core.Nothing,
      mLModelId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The endpoint information of the @MLModel@
deleteRealtimeEndpointResponse_realtimeEndpointInfo :: Lens.Lens' DeleteRealtimeEndpointResponse (Core.Maybe RealtimeEndpointInfo)
deleteRealtimeEndpointResponse_realtimeEndpointInfo = Lens.lens (\DeleteRealtimeEndpointResponse' {realtimeEndpointInfo} -> realtimeEndpointInfo) (\s@DeleteRealtimeEndpointResponse' {} a -> s {realtimeEndpointInfo = a} :: DeleteRealtimeEndpointResponse)

-- | A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelId@ in the request.
deleteRealtimeEndpointResponse_mLModelId :: Lens.Lens' DeleteRealtimeEndpointResponse (Core.Maybe Core.Text)
deleteRealtimeEndpointResponse_mLModelId = Lens.lens (\DeleteRealtimeEndpointResponse' {mLModelId} -> mLModelId) (\s@DeleteRealtimeEndpointResponse' {} a -> s {mLModelId = a} :: DeleteRealtimeEndpointResponse)

-- | The response's http status code.
deleteRealtimeEndpointResponse_httpStatus :: Lens.Lens' DeleteRealtimeEndpointResponse Core.Int
deleteRealtimeEndpointResponse_httpStatus = Lens.lens (\DeleteRealtimeEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteRealtimeEndpointResponse' {} a -> s {httpStatus = a} :: DeleteRealtimeEndpointResponse)

instance Core.NFData DeleteRealtimeEndpointResponse
