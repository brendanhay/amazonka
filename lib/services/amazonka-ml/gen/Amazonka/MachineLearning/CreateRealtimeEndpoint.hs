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
-- Module      : Amazonka.MachineLearning.CreateRealtimeEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a real-time endpoint for the @MLModel@. The endpoint contains
-- the URI of the @MLModel@; that is, the location to send real-time
-- prediction requests for the specified @MLModel@.
module Amazonka.MachineLearning.CreateRealtimeEndpoint
  ( -- * Creating a Request
    CreateRealtimeEndpoint (..),
    newCreateRealtimeEndpoint,

    -- * Request Lenses
    createRealtimeEndpoint_mLModelId,

    -- * Destructuring the Response
    CreateRealtimeEndpointResponse (..),
    newCreateRealtimeEndpointResponse,

    -- * Response Lenses
    createRealtimeEndpointResponse_mLModelId,
    createRealtimeEndpointResponse_realtimeEndpointInfo,
    createRealtimeEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRealtimeEndpoint' smart constructor.
data CreateRealtimeEndpoint = CreateRealtimeEndpoint'
  { -- | The ID assigned to the @MLModel@ during creation.
    mLModelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  CreateRealtimeEndpoint
newCreateRealtimeEndpoint pMLModelId_ =
  CreateRealtimeEndpoint' {mLModelId = pMLModelId_}

-- | The ID assigned to the @MLModel@ during creation.
createRealtimeEndpoint_mLModelId :: Lens.Lens' CreateRealtimeEndpoint Prelude.Text
createRealtimeEndpoint_mLModelId = Lens.lens (\CreateRealtimeEndpoint' {mLModelId} -> mLModelId) (\s@CreateRealtimeEndpoint' {} a -> s {mLModelId = a} :: CreateRealtimeEndpoint)

instance Core.AWSRequest CreateRealtimeEndpoint where
  type
    AWSResponse CreateRealtimeEndpoint =
      CreateRealtimeEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRealtimeEndpointResponse'
            Prelude.<$> (x Data..?> "MLModelId")
            Prelude.<*> (x Data..?> "RealtimeEndpointInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRealtimeEndpoint where
  hashWithSalt _salt CreateRealtimeEndpoint' {..} =
    _salt `Prelude.hashWithSalt` mLModelId

instance Prelude.NFData CreateRealtimeEndpoint where
  rnf CreateRealtimeEndpoint' {..} =
    Prelude.rnf mLModelId

instance Data.ToHeaders CreateRealtimeEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonML_20141212.CreateRealtimeEndpoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRealtimeEndpoint where
  toJSON CreateRealtimeEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("MLModelId" Data..= mLModelId)]
      )

instance Data.ToPath CreateRealtimeEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateRealtimeEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of an @CreateRealtimeEndpoint@ operation.
--
-- The result contains the @MLModelId@ and the endpoint information for the
-- @MLModel@.
--
-- __Note:__ The endpoint information includes the URI of the @MLModel@;
-- that is, the location to send online prediction requests for the
-- specified @MLModel@.
--
-- /See:/ 'newCreateRealtimeEndpointResponse' smart constructor.
data CreateRealtimeEndpointResponse = CreateRealtimeEndpointResponse'
  { -- | A user-supplied ID that uniquely identifies the @MLModel@. This value
    -- should be identical to the value of the @MLModelId@ in the request.
    mLModelId :: Prelude.Maybe Prelude.Text,
    -- | The endpoint information of the @MLModel@
    realtimeEndpointInfo :: Prelude.Maybe RealtimeEndpointInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRealtimeEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mLModelId', 'createRealtimeEndpointResponse_mLModelId' - A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelId@ in the request.
--
-- 'realtimeEndpointInfo', 'createRealtimeEndpointResponse_realtimeEndpointInfo' - The endpoint information of the @MLModel@
--
-- 'httpStatus', 'createRealtimeEndpointResponse_httpStatus' - The response's http status code.
newCreateRealtimeEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRealtimeEndpointResponse
newCreateRealtimeEndpointResponse pHttpStatus_ =
  CreateRealtimeEndpointResponse'
    { mLModelId =
        Prelude.Nothing,
      realtimeEndpointInfo = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelId@ in the request.
createRealtimeEndpointResponse_mLModelId :: Lens.Lens' CreateRealtimeEndpointResponse (Prelude.Maybe Prelude.Text)
createRealtimeEndpointResponse_mLModelId = Lens.lens (\CreateRealtimeEndpointResponse' {mLModelId} -> mLModelId) (\s@CreateRealtimeEndpointResponse' {} a -> s {mLModelId = a} :: CreateRealtimeEndpointResponse)

-- | The endpoint information of the @MLModel@
createRealtimeEndpointResponse_realtimeEndpointInfo :: Lens.Lens' CreateRealtimeEndpointResponse (Prelude.Maybe RealtimeEndpointInfo)
createRealtimeEndpointResponse_realtimeEndpointInfo = Lens.lens (\CreateRealtimeEndpointResponse' {realtimeEndpointInfo} -> realtimeEndpointInfo) (\s@CreateRealtimeEndpointResponse' {} a -> s {realtimeEndpointInfo = a} :: CreateRealtimeEndpointResponse)

-- | The response's http status code.
createRealtimeEndpointResponse_httpStatus :: Lens.Lens' CreateRealtimeEndpointResponse Prelude.Int
createRealtimeEndpointResponse_httpStatus = Lens.lens (\CreateRealtimeEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateRealtimeEndpointResponse' {} a -> s {httpStatus = a} :: CreateRealtimeEndpointResponse)

instance
  Prelude.NFData
    CreateRealtimeEndpointResponse
  where
  rnf CreateRealtimeEndpointResponse' {..} =
    Prelude.rnf mLModelId
      `Prelude.seq` Prelude.rnf realtimeEndpointInfo
      `Prelude.seq` Prelude.rnf httpStatus
