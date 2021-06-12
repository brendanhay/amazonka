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
-- Module      : Network.AWS.Glue.DeleteMLTransform
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Glue machine learning transform. Machine learning
-- transforms are a special type of transform that use machine learning to
-- learn the details of the transformation to be performed by learning from
-- examples provided by humans. These transformations are then saved by AWS
-- Glue. If you no longer need a transform, you can delete it by calling
-- @DeleteMLTransforms@. However, any AWS Glue jobs that still reference
-- the deleted transform will no longer succeed.
module Network.AWS.Glue.DeleteMLTransform
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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteMLTransform' smart constructor.
data DeleteMLTransform = DeleteMLTransform'
  { -- | The unique identifier of the transform to delete.
    transformId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteMLTransform
newDeleteMLTransform pTransformId_ =
  DeleteMLTransform' {transformId = pTransformId_}

-- | The unique identifier of the transform to delete.
deleteMLTransform_transformId :: Lens.Lens' DeleteMLTransform Core.Text
deleteMLTransform_transformId = Lens.lens (\DeleteMLTransform' {transformId} -> transformId) (\s@DeleteMLTransform' {} a -> s {transformId = a} :: DeleteMLTransform)

instance Core.AWSRequest DeleteMLTransform where
  type
    AWSResponse DeleteMLTransform =
      DeleteMLTransformResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMLTransformResponse'
            Core.<$> (x Core..?> "TransformId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteMLTransform

instance Core.NFData DeleteMLTransform

instance Core.ToHeaders DeleteMLTransform where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.DeleteMLTransform" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteMLTransform where
  toJSON DeleteMLTransform' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TransformId" Core..= transformId)]
      )

instance Core.ToPath DeleteMLTransform where
  toPath = Core.const "/"

instance Core.ToQuery DeleteMLTransform where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteMLTransformResponse' smart constructor.
data DeleteMLTransformResponse = DeleteMLTransformResponse'
  { -- | The unique identifier of the transform that was deleted.
    transformId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteMLTransformResponse
newDeleteMLTransformResponse pHttpStatus_ =
  DeleteMLTransformResponse'
    { transformId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the transform that was deleted.
deleteMLTransformResponse_transformId :: Lens.Lens' DeleteMLTransformResponse (Core.Maybe Core.Text)
deleteMLTransformResponse_transformId = Lens.lens (\DeleteMLTransformResponse' {transformId} -> transformId) (\s@DeleteMLTransformResponse' {} a -> s {transformId = a} :: DeleteMLTransformResponse)

-- | The response's http status code.
deleteMLTransformResponse_httpStatus :: Lens.Lens' DeleteMLTransformResponse Core.Int
deleteMLTransformResponse_httpStatus = Lens.lens (\DeleteMLTransformResponse' {httpStatus} -> httpStatus) (\s@DeleteMLTransformResponse' {} a -> s {httpStatus = a} :: DeleteMLTransformResponse)

instance Core.NFData DeleteMLTransformResponse
