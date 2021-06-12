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
-- Module      : Network.AWS.MachineLearning.CreateRealtimeEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a real-time endpoint for the @MLModel@. The endpoint contains
-- the URI of the @MLModel@; that is, the location to send real-time
-- prediction requests for the specified @MLModel@.
module Network.AWS.MachineLearning.CreateRealtimeEndpoint
  ( -- * Creating a Request
    CreateRealtimeEndpoint (..),
    newCreateRealtimeEndpoint,

    -- * Request Lenses
    createRealtimeEndpoint_mLModelId,

    -- * Destructuring the Response
    CreateRealtimeEndpointResponse (..),
    newCreateRealtimeEndpointResponse,

    -- * Response Lenses
    createRealtimeEndpointResponse_realtimeEndpointInfo,
    createRealtimeEndpointResponse_mLModelId,
    createRealtimeEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRealtimeEndpoint' smart constructor.
data CreateRealtimeEndpoint = CreateRealtimeEndpoint'
  { -- | The ID assigned to the @MLModel@ during creation.
    mLModelId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRealtimeEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mLModelId', 'createRealtimeEndpoint_mLModelId' - The ID assigned to the @MLModel@ during creation.
newCreateRealtimeEndpoint ::
  -- | 'mLModelId'
  Core.Text ->
  CreateRealtimeEndpoint
newCreateRealtimeEndpoint pMLModelId_ =
  CreateRealtimeEndpoint' {mLModelId = pMLModelId_}

-- | The ID assigned to the @MLModel@ during creation.
createRealtimeEndpoint_mLModelId :: Lens.Lens' CreateRealtimeEndpoint Core.Text
createRealtimeEndpoint_mLModelId = Lens.lens (\CreateRealtimeEndpoint' {mLModelId} -> mLModelId) (\s@CreateRealtimeEndpoint' {} a -> s {mLModelId = a} :: CreateRealtimeEndpoint)

instance Core.AWSRequest CreateRealtimeEndpoint where
  type
    AWSResponse CreateRealtimeEndpoint =
      CreateRealtimeEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRealtimeEndpointResponse'
            Core.<$> (x Core..?> "RealtimeEndpointInfo")
            Core.<*> (x Core..?> "MLModelId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateRealtimeEndpoint

instance Core.NFData CreateRealtimeEndpoint

instance Core.ToHeaders CreateRealtimeEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonML_20141212.CreateRealtimeEndpoint" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateRealtimeEndpoint where
  toJSON CreateRealtimeEndpoint' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("MLModelId" Core..= mLModelId)]
      )

instance Core.ToPath CreateRealtimeEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery CreateRealtimeEndpoint where
  toQuery = Core.const Core.mempty

-- | Represents the output of an @CreateRealtimeEndpoint@ operation.
--
-- The result contains the @MLModelId@ and the endpoint information for the
-- @MLModel@.
--
-- The endpoint information includes the URI of the @MLModel@; that is, the
-- location to send online prediction requests for the specified @MLModel@.
--
-- /See:/ 'newCreateRealtimeEndpointResponse' smart constructor.
data CreateRealtimeEndpointResponse = CreateRealtimeEndpointResponse'
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
-- Create a value of 'CreateRealtimeEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'realtimeEndpointInfo', 'createRealtimeEndpointResponse_realtimeEndpointInfo' - The endpoint information of the @MLModel@
--
-- 'mLModelId', 'createRealtimeEndpointResponse_mLModelId' - A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelId@ in the request.
--
-- 'httpStatus', 'createRealtimeEndpointResponse_httpStatus' - The response's http status code.
newCreateRealtimeEndpointResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateRealtimeEndpointResponse
newCreateRealtimeEndpointResponse pHttpStatus_ =
  CreateRealtimeEndpointResponse'
    { realtimeEndpointInfo =
        Core.Nothing,
      mLModelId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The endpoint information of the @MLModel@
createRealtimeEndpointResponse_realtimeEndpointInfo :: Lens.Lens' CreateRealtimeEndpointResponse (Core.Maybe RealtimeEndpointInfo)
createRealtimeEndpointResponse_realtimeEndpointInfo = Lens.lens (\CreateRealtimeEndpointResponse' {realtimeEndpointInfo} -> realtimeEndpointInfo) (\s@CreateRealtimeEndpointResponse' {} a -> s {realtimeEndpointInfo = a} :: CreateRealtimeEndpointResponse)

-- | A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelId@ in the request.
createRealtimeEndpointResponse_mLModelId :: Lens.Lens' CreateRealtimeEndpointResponse (Core.Maybe Core.Text)
createRealtimeEndpointResponse_mLModelId = Lens.lens (\CreateRealtimeEndpointResponse' {mLModelId} -> mLModelId) (\s@CreateRealtimeEndpointResponse' {} a -> s {mLModelId = a} :: CreateRealtimeEndpointResponse)

-- | The response's http status code.
createRealtimeEndpointResponse_httpStatus :: Lens.Lens' CreateRealtimeEndpointResponse Core.Int
createRealtimeEndpointResponse_httpStatus = Lens.lens (\CreateRealtimeEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateRealtimeEndpointResponse' {} a -> s {httpStatus = a} :: CreateRealtimeEndpointResponse)

instance Core.NFData CreateRealtimeEndpointResponse
