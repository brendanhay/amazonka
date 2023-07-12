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
-- Module      : Amazonka.Glue.DeleteMLTransform
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Glue machine learning transform. Machine learning transforms
-- are a special type of transform that use machine learning to learn the
-- details of the transformation to be performed by learning from examples
-- provided by humans. These transformations are then saved by Glue. If you
-- no longer need a transform, you can delete it by calling
-- @DeleteMLTransforms@. However, any Glue jobs that still reference the
-- deleted transform will no longer succeed.
module Amazonka.Glue.DeleteMLTransform
  ( -- * Creating a Request
    DeleteMLTransform (..),
    newDeleteMLTransform,

    -- * Request Lenses
    deleteMLTransform_transformId,

    -- * Destructuring the Response
    DeleteMLTransformResponse (..),
    newDeleteMLTransformResponse,

    -- * Response Lenses
    deleteMLTransformResponse_transformId,
    deleteMLTransformResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMLTransform' smart constructor.
data DeleteMLTransform = DeleteMLTransform'
  { -- | The unique identifier of the transform to delete.
    transformId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMLTransform' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformId', 'deleteMLTransform_transformId' - The unique identifier of the transform to delete.
newDeleteMLTransform ::
  -- | 'transformId'
  Prelude.Text ->
  DeleteMLTransform
newDeleteMLTransform pTransformId_ =
  DeleteMLTransform' {transformId = pTransformId_}

-- | The unique identifier of the transform to delete.
deleteMLTransform_transformId :: Lens.Lens' DeleteMLTransform Prelude.Text
deleteMLTransform_transformId = Lens.lens (\DeleteMLTransform' {transformId} -> transformId) (\s@DeleteMLTransform' {} a -> s {transformId = a} :: DeleteMLTransform)

instance Core.AWSRequest DeleteMLTransform where
  type
    AWSResponse DeleteMLTransform =
      DeleteMLTransformResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMLTransformResponse'
            Prelude.<$> (x Data..?> "TransformId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMLTransform where
  hashWithSalt _salt DeleteMLTransform' {..} =
    _salt `Prelude.hashWithSalt` transformId

instance Prelude.NFData DeleteMLTransform where
  rnf DeleteMLTransform' {..} = Prelude.rnf transformId

instance Data.ToHeaders DeleteMLTransform where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.DeleteMLTransform" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteMLTransform where
  toJSON DeleteMLTransform' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TransformId" Data..= transformId)]
      )

instance Data.ToPath DeleteMLTransform where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteMLTransform where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMLTransformResponse' smart constructor.
data DeleteMLTransformResponse = DeleteMLTransformResponse'
  { -- | The unique identifier of the transform that was deleted.
    transformId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMLTransformResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformId', 'deleteMLTransformResponse_transformId' - The unique identifier of the transform that was deleted.
--
-- 'httpStatus', 'deleteMLTransformResponse_httpStatus' - The response's http status code.
newDeleteMLTransformResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMLTransformResponse
newDeleteMLTransformResponse pHttpStatus_ =
  DeleteMLTransformResponse'
    { transformId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the transform that was deleted.
deleteMLTransformResponse_transformId :: Lens.Lens' DeleteMLTransformResponse (Prelude.Maybe Prelude.Text)
deleteMLTransformResponse_transformId = Lens.lens (\DeleteMLTransformResponse' {transformId} -> transformId) (\s@DeleteMLTransformResponse' {} a -> s {transformId = a} :: DeleteMLTransformResponse)

-- | The response's http status code.
deleteMLTransformResponse_httpStatus :: Lens.Lens' DeleteMLTransformResponse Prelude.Int
deleteMLTransformResponse_httpStatus = Lens.lens (\DeleteMLTransformResponse' {httpStatus} -> httpStatus) (\s@DeleteMLTransformResponse' {} a -> s {httpStatus = a} :: DeleteMLTransformResponse)

instance Prelude.NFData DeleteMLTransformResponse where
  rnf DeleteMLTransformResponse' {..} =
    Prelude.rnf transformId
      `Prelude.seq` Prelude.rnf httpStatus
