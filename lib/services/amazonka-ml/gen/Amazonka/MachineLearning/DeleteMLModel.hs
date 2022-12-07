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
-- Module      : Amazonka.MachineLearning.DeleteMLModel
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.MachineLearning.DeleteMLModel
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMLModel' smart constructor.
data DeleteMLModel = DeleteMLModel'
  { -- | A user-supplied ID that uniquely identifies the @MLModel@.
    mLModelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteMLModel
newDeleteMLModel pMLModelId_ =
  DeleteMLModel' {mLModelId = pMLModelId_}

-- | A user-supplied ID that uniquely identifies the @MLModel@.
deleteMLModel_mLModelId :: Lens.Lens' DeleteMLModel Prelude.Text
deleteMLModel_mLModelId = Lens.lens (\DeleteMLModel' {mLModelId} -> mLModelId) (\s@DeleteMLModel' {} a -> s {mLModelId = a} :: DeleteMLModel)

instance Core.AWSRequest DeleteMLModel where
  type
    AWSResponse DeleteMLModel =
      DeleteMLModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMLModelResponse'
            Prelude.<$> (x Data..?> "MLModelId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMLModel where
  hashWithSalt _salt DeleteMLModel' {..} =
    _salt `Prelude.hashWithSalt` mLModelId

instance Prelude.NFData DeleteMLModel where
  rnf DeleteMLModel' {..} = Prelude.rnf mLModelId

instance Data.ToHeaders DeleteMLModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonML_20141212.DeleteMLModel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteMLModel where
  toJSON DeleteMLModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("MLModelId" Data..= mLModelId)]
      )

instance Data.ToPath DeleteMLModel where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteMLModel where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @DeleteMLModel@ operation.
--
-- You can use the @GetMLModel@ operation and check the value of the
-- @Status@ parameter to see whether an @MLModel@ is marked as @DELETED@.
--
-- /See:/ 'newDeleteMLModelResponse' smart constructor.
data DeleteMLModelResponse = DeleteMLModelResponse'
  { -- | A user-supplied ID that uniquely identifies the @MLModel@. This value
    -- should be identical to the value of the @MLModelID@ in the request.
    mLModelId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteMLModelResponse
newDeleteMLModelResponse pHttpStatus_ =
  DeleteMLModelResponse'
    { mLModelId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelID@ in the request.
deleteMLModelResponse_mLModelId :: Lens.Lens' DeleteMLModelResponse (Prelude.Maybe Prelude.Text)
deleteMLModelResponse_mLModelId = Lens.lens (\DeleteMLModelResponse' {mLModelId} -> mLModelId) (\s@DeleteMLModelResponse' {} a -> s {mLModelId = a} :: DeleteMLModelResponse)

-- | The response's http status code.
deleteMLModelResponse_httpStatus :: Lens.Lens' DeleteMLModelResponse Prelude.Int
deleteMLModelResponse_httpStatus = Lens.lens (\DeleteMLModelResponse' {httpStatus} -> httpStatus) (\s@DeleteMLModelResponse' {} a -> s {httpStatus = a} :: DeleteMLModelResponse)

instance Prelude.NFData DeleteMLModelResponse where
  rnf DeleteMLModelResponse' {..} =
    Prelude.rnf mLModelId
      `Prelude.seq` Prelude.rnf httpStatus
