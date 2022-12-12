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
-- Module      : Amazonka.ECR.BatchDeleteImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of specified images within a repository. Images are
-- specified with either an @imageTag@ or @imageDigest@.
--
-- You can remove a tag from an image by specifying the image\'s tag in
-- your request. When you remove the last tag from an image, the image is
-- deleted from your repository.
--
-- You can completely delete an image (and all of its tags) by specifying
-- the image\'s digest in your request.
module Amazonka.ECR.BatchDeleteImage
  ( -- * Creating a Request
    BatchDeleteImage (..),
    newBatchDeleteImage,

    -- * Request Lenses
    batchDeleteImage_registryId,
    batchDeleteImage_repositoryName,
    batchDeleteImage_imageIds,

    -- * Destructuring the Response
    BatchDeleteImageResponse (..),
    newBatchDeleteImageResponse,

    -- * Response Lenses
    batchDeleteImageResponse_failures,
    batchDeleteImageResponse_imageIds,
    batchDeleteImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Deletes specified images within a specified repository. Images are
-- specified with either the @imageTag@ or @imageDigest@.
--
-- /See:/ 'newBatchDeleteImage' smart constructor.
data BatchDeleteImage = BatchDeleteImage'
  { -- | The Amazon Web Services account ID associated with the registry that
    -- contains the image to delete. If you do not specify a registry, the
    -- default registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository that contains the image to delete.
    repositoryName :: Prelude.Text,
    -- | A list of image ID references that correspond to images to delete. The
    -- format of the @imageIds@ reference is @imageTag=tag@ or
    -- @imageDigest=digest@.
    imageIds :: [ImageIdentifier]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'batchDeleteImage_registryId' - The Amazon Web Services account ID associated with the registry that
-- contains the image to delete. If you do not specify a registry, the
-- default registry is assumed.
--
-- 'repositoryName', 'batchDeleteImage_repositoryName' - The repository that contains the image to delete.
--
-- 'imageIds', 'batchDeleteImage_imageIds' - A list of image ID references that correspond to images to delete. The
-- format of the @imageIds@ reference is @imageTag=tag@ or
-- @imageDigest=digest@.
newBatchDeleteImage ::
  -- | 'repositoryName'
  Prelude.Text ->
  BatchDeleteImage
newBatchDeleteImage pRepositoryName_ =
  BatchDeleteImage'
    { registryId = Prelude.Nothing,
      repositoryName = pRepositoryName_,
      imageIds = Prelude.mempty
    }

-- | The Amazon Web Services account ID associated with the registry that
-- contains the image to delete. If you do not specify a registry, the
-- default registry is assumed.
batchDeleteImage_registryId :: Lens.Lens' BatchDeleteImage (Prelude.Maybe Prelude.Text)
batchDeleteImage_registryId = Lens.lens (\BatchDeleteImage' {registryId} -> registryId) (\s@BatchDeleteImage' {} a -> s {registryId = a} :: BatchDeleteImage)

-- | The repository that contains the image to delete.
batchDeleteImage_repositoryName :: Lens.Lens' BatchDeleteImage Prelude.Text
batchDeleteImage_repositoryName = Lens.lens (\BatchDeleteImage' {repositoryName} -> repositoryName) (\s@BatchDeleteImage' {} a -> s {repositoryName = a} :: BatchDeleteImage)

-- | A list of image ID references that correspond to images to delete. The
-- format of the @imageIds@ reference is @imageTag=tag@ or
-- @imageDigest=digest@.
batchDeleteImage_imageIds :: Lens.Lens' BatchDeleteImage [ImageIdentifier]
batchDeleteImage_imageIds = Lens.lens (\BatchDeleteImage' {imageIds} -> imageIds) (\s@BatchDeleteImage' {} a -> s {imageIds = a} :: BatchDeleteImage) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDeleteImage where
  type
    AWSResponse BatchDeleteImage =
      BatchDeleteImageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteImageResponse'
            Prelude.<$> (x Data..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "imageIds" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDeleteImage where
  hashWithSalt _salt BatchDeleteImage' {..} =
    _salt `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` imageIds

instance Prelude.NFData BatchDeleteImage where
  rnf BatchDeleteImage' {..} =
    Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf imageIds

instance Data.ToHeaders BatchDeleteImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerRegistry_V20150921.BatchDeleteImage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDeleteImage where
  toJSON BatchDeleteImage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just ("imageIds" Data..= imageIds)
          ]
      )

instance Data.ToPath BatchDeleteImage where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDeleteImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteImageResponse' smart constructor.
data BatchDeleteImageResponse = BatchDeleteImageResponse'
  { -- | Any failures associated with the call.
    failures :: Prelude.Maybe [ImageFailure],
    -- | The image IDs of the deleted images.
    imageIds :: Prelude.Maybe [ImageIdentifier],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'batchDeleteImageResponse_failures' - Any failures associated with the call.
--
-- 'imageIds', 'batchDeleteImageResponse_imageIds' - The image IDs of the deleted images.
--
-- 'httpStatus', 'batchDeleteImageResponse_httpStatus' - The response's http status code.
newBatchDeleteImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteImageResponse
newBatchDeleteImageResponse pHttpStatus_ =
  BatchDeleteImageResponse'
    { failures =
        Prelude.Nothing,
      imageIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any failures associated with the call.
batchDeleteImageResponse_failures :: Lens.Lens' BatchDeleteImageResponse (Prelude.Maybe [ImageFailure])
batchDeleteImageResponse_failures = Lens.lens (\BatchDeleteImageResponse' {failures} -> failures) (\s@BatchDeleteImageResponse' {} a -> s {failures = a} :: BatchDeleteImageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The image IDs of the deleted images.
batchDeleteImageResponse_imageIds :: Lens.Lens' BatchDeleteImageResponse (Prelude.Maybe [ImageIdentifier])
batchDeleteImageResponse_imageIds = Lens.lens (\BatchDeleteImageResponse' {imageIds} -> imageIds) (\s@BatchDeleteImageResponse' {} a -> s {imageIds = a} :: BatchDeleteImageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeleteImageResponse_httpStatus :: Lens.Lens' BatchDeleteImageResponse Prelude.Int
batchDeleteImageResponse_httpStatus = Lens.lens (\BatchDeleteImageResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteImageResponse' {} a -> s {httpStatus = a} :: BatchDeleteImageResponse)

instance Prelude.NFData BatchDeleteImageResponse where
  rnf BatchDeleteImageResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf imageIds
      `Prelude.seq` Prelude.rnf httpStatus
