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
-- Module      : Amazonka.MachineLearning.DeleteRealtimeEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a real time endpoint of an @MLModel@.
module Amazonka.MachineLearning.DeleteRealtimeEndpoint
  ( -- * Creating a Request
    DeleteRealtimeEndpoint (..),
    newDeleteRealtimeEndpoint,

    -- * Request Lenses
    deleteRealtimeEndpoint_mLModelId,

    -- * Destructuring the Response
    DeleteRealtimeEndpointResponse (..),
    newDeleteRealtimeEndpointResponse,

    -- * Response Lenses
    deleteRealtimeEndpointResponse_mLModelId,
    deleteRealtimeEndpointResponse_realtimeEndpointInfo,
    deleteRealtimeEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRealtimeEndpoint' smart constructor.
data DeleteRealtimeEndpoint = DeleteRealtimeEndpoint'
  { -- | The ID assigned to the @MLModel@ during creation.
    mLModelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteRealtimeEndpoint
newDeleteRealtimeEndpoint pMLModelId_ =
  DeleteRealtimeEndpoint' {mLModelId = pMLModelId_}

-- | The ID assigned to the @MLModel@ during creation.
deleteRealtimeEndpoint_mLModelId :: Lens.Lens' DeleteRealtimeEndpoint Prelude.Text
deleteRealtimeEndpoint_mLModelId = Lens.lens (\DeleteRealtimeEndpoint' {mLModelId} -> mLModelId) (\s@DeleteRealtimeEndpoint' {} a -> s {mLModelId = a} :: DeleteRealtimeEndpoint)

instance Core.AWSRequest DeleteRealtimeEndpoint where
  type
    AWSResponse DeleteRealtimeEndpoint =
      DeleteRealtimeEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRealtimeEndpointResponse'
            Prelude.<$> (x Data..?> "MLModelId")
            Prelude.<*> (x Data..?> "RealtimeEndpointInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRealtimeEndpoint where
  hashWithSalt _salt DeleteRealtimeEndpoint' {..} =
    _salt `Prelude.hashWithSalt` mLModelId

instance Prelude.NFData DeleteRealtimeEndpoint where
  rnf DeleteRealtimeEndpoint' {..} =
    Prelude.rnf mLModelId

instance Data.ToHeaders DeleteRealtimeEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonML_20141212.DeleteRealtimeEndpoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRealtimeEndpoint where
  toJSON DeleteRealtimeEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("MLModelId" Data..= mLModelId)]
      )

instance Data.ToPath DeleteRealtimeEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRealtimeEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of an @DeleteRealtimeEndpoint@ operation.
--
-- The result contains the @MLModelId@ and the endpoint information for the
-- @MLModel@.
--
-- /See:/ 'newDeleteRealtimeEndpointResponse' smart constructor.
data DeleteRealtimeEndpointResponse = DeleteRealtimeEndpointResponse'
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
-- Create a value of 'DeleteRealtimeEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mLModelId', 'deleteRealtimeEndpointResponse_mLModelId' - A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelId@ in the request.
--
-- 'realtimeEndpointInfo', 'deleteRealtimeEndpointResponse_realtimeEndpointInfo' - The endpoint information of the @MLModel@
--
-- 'httpStatus', 'deleteRealtimeEndpointResponse_httpStatus' - The response's http status code.
newDeleteRealtimeEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRealtimeEndpointResponse
newDeleteRealtimeEndpointResponse pHttpStatus_ =
  DeleteRealtimeEndpointResponse'
    { mLModelId =
        Prelude.Nothing,
      realtimeEndpointInfo = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelId@ in the request.
deleteRealtimeEndpointResponse_mLModelId :: Lens.Lens' DeleteRealtimeEndpointResponse (Prelude.Maybe Prelude.Text)
deleteRealtimeEndpointResponse_mLModelId = Lens.lens (\DeleteRealtimeEndpointResponse' {mLModelId} -> mLModelId) (\s@DeleteRealtimeEndpointResponse' {} a -> s {mLModelId = a} :: DeleteRealtimeEndpointResponse)

-- | The endpoint information of the @MLModel@
deleteRealtimeEndpointResponse_realtimeEndpointInfo :: Lens.Lens' DeleteRealtimeEndpointResponse (Prelude.Maybe RealtimeEndpointInfo)
deleteRealtimeEndpointResponse_realtimeEndpointInfo = Lens.lens (\DeleteRealtimeEndpointResponse' {realtimeEndpointInfo} -> realtimeEndpointInfo) (\s@DeleteRealtimeEndpointResponse' {} a -> s {realtimeEndpointInfo = a} :: DeleteRealtimeEndpointResponse)

-- | The response's http status code.
deleteRealtimeEndpointResponse_httpStatus :: Lens.Lens' DeleteRealtimeEndpointResponse Prelude.Int
deleteRealtimeEndpointResponse_httpStatus = Lens.lens (\DeleteRealtimeEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteRealtimeEndpointResponse' {} a -> s {httpStatus = a} :: DeleteRealtimeEndpointResponse)

instance
  Prelude.NFData
    DeleteRealtimeEndpointResponse
  where
  rnf DeleteRealtimeEndpointResponse' {..} =
    Prelude.rnf mLModelId
      `Prelude.seq` Prelude.rnf realtimeEndpointInfo
      `Prelude.seq` Prelude.rnf httpStatus
