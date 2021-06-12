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
-- Module      : Network.AWS.MachineLearning.DeleteMLModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the @DELETED@ status to an @MLModel@, rendering it unusable.
--
-- After using the @DeleteMLModel@ operation, you can use the @GetMLModel@
-- operation to verify that the status of the @MLModel@ changed to DELETED.
--
-- __Caution:__ The result of the @DeleteMLModel@ operation is
-- irreversible.
module Network.AWS.MachineLearning.DeleteMLModel
  ( -- * Creating a Request
    DeleteMLModel (..),
    newDeleteMLModel,

    -- * Request Lenses
    deleteMLModel_mLModelId,

    -- * Destructuring the Response
    DeleteMLModelResponse (..),
    newDeleteMLModelResponse,

    -- * Response Lenses
    deleteMLModelResponse_mLModelId,
    deleteMLModelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteMLModel' smart constructor.
data DeleteMLModel = DeleteMLModel'
  { -- | A user-supplied ID that uniquely identifies the @MLModel@.
    mLModelId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMLModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mLModelId', 'deleteMLModel_mLModelId' - A user-supplied ID that uniquely identifies the @MLModel@.
newDeleteMLModel ::
  -- | 'mLModelId'
  Core.Text ->
  DeleteMLModel
newDeleteMLModel pMLModelId_ =
  DeleteMLModel' {mLModelId = pMLModelId_}

-- | A user-supplied ID that uniquely identifies the @MLModel@.
deleteMLModel_mLModelId :: Lens.Lens' DeleteMLModel Core.Text
deleteMLModel_mLModelId = Lens.lens (\DeleteMLModel' {mLModelId} -> mLModelId) (\s@DeleteMLModel' {} a -> s {mLModelId = a} :: DeleteMLModel)

instance Core.AWSRequest DeleteMLModel where
  type
    AWSResponse DeleteMLModel =
      DeleteMLModelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMLModelResponse'
            Core.<$> (x Core..?> "MLModelId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteMLModel

instance Core.NFData DeleteMLModel

instance Core.ToHeaders DeleteMLModel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonML_20141212.DeleteMLModel" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteMLModel where
  toJSON DeleteMLModel' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("MLModelId" Core..= mLModelId)]
      )

instance Core.ToPath DeleteMLModel where
  toPath = Core.const "/"

instance Core.ToQuery DeleteMLModel where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @DeleteMLModel@ operation.
--
-- You can use the @GetMLModel@ operation and check the value of the
-- @Status@ parameter to see whether an @MLModel@ is marked as @DELETED@.
--
-- /See:/ 'newDeleteMLModelResponse' smart constructor.
data DeleteMLModelResponse = DeleteMLModelResponse'
  { -- | A user-supplied ID that uniquely identifies the @MLModel@. This value
    -- should be identical to the value of the @MLModelID@ in the request.
    mLModelId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMLModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mLModelId', 'deleteMLModelResponse_mLModelId' - A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelID@ in the request.
--
-- 'httpStatus', 'deleteMLModelResponse_httpStatus' - The response's http status code.
newDeleteMLModelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteMLModelResponse
newDeleteMLModelResponse pHttpStatus_ =
  DeleteMLModelResponse'
    { mLModelId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelID@ in the request.
deleteMLModelResponse_mLModelId :: Lens.Lens' DeleteMLModelResponse (Core.Maybe Core.Text)
deleteMLModelResponse_mLModelId = Lens.lens (\DeleteMLModelResponse' {mLModelId} -> mLModelId) (\s@DeleteMLModelResponse' {} a -> s {mLModelId = a} :: DeleteMLModelResponse)

-- | The response's http status code.
deleteMLModelResponse_httpStatus :: Lens.Lens' DeleteMLModelResponse Core.Int
deleteMLModelResponse_httpStatus = Lens.lens (\DeleteMLModelResponse' {httpStatus} -> httpStatus) (\s@DeleteMLModelResponse' {} a -> s {httpStatus = a} :: DeleteMLModelResponse)

instance Core.NFData DeleteMLModelResponse
