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
-- Module      : Amazonka.ECR.BatchGetImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets detailed information for an image. Images are specified with either
-- an @imageTag@ or @imageDigest@.
--
-- When an image is pulled, the BatchGetImage API is called once to
-- retrieve the image manifest.
module Amazonka.ECR.BatchGetImage
  ( -- * Creating a Request
    BatchGetImage (..),
    newBatchGetImage,

    -- * Request Lenses
    batchGetImage_acceptedMediaTypes,
    batchGetImage_registryId,
    batchGetImage_repositoryName,
    batchGetImage_imageIds,

    -- * Destructuring the Response
    BatchGetImageResponse (..),
    newBatchGetImageResponse,

    -- * Response Lenses
    batchGetImageResponse_failures,
    batchGetImageResponse_images,
    batchGetImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetImage' smart constructor.
data BatchGetImage = BatchGetImage'
  { -- | The accepted media types for the request.
    --
    -- Valid values: @application\/vnd.docker.distribution.manifest.v1+json@ |
    -- @application\/vnd.docker.distribution.manifest.v2+json@ |
    -- @application\/vnd.oci.image.manifest.v1+json@
    acceptedMediaTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon Web Services account ID associated with the registry that
    -- contains the images to describe. If you do not specify a registry, the
    -- default registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository that contains the images to describe.
    repositoryName :: Prelude.Text,
    -- | A list of image ID references that correspond to images to describe. The
    -- format of the @imageIds@ reference is @imageTag=tag@ or
    -- @imageDigest=digest@.
    imageIds :: [ImageIdentifier]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptedMediaTypes', 'batchGetImage_acceptedMediaTypes' - The accepted media types for the request.
--
-- Valid values: @application\/vnd.docker.distribution.manifest.v1+json@ |
-- @application\/vnd.docker.distribution.manifest.v2+json@ |
-- @application\/vnd.oci.image.manifest.v1+json@
--
-- 'registryId', 'batchGetImage_registryId' - The Amazon Web Services account ID associated with the registry that
-- contains the images to describe. If you do not specify a registry, the
-- default registry is assumed.
--
-- 'repositoryName', 'batchGetImage_repositoryName' - The repository that contains the images to describe.
--
-- 'imageIds', 'batchGetImage_imageIds' - A list of image ID references that correspond to images to describe. The
-- format of the @imageIds@ reference is @imageTag=tag@ or
-- @imageDigest=digest@.
newBatchGetImage ::
  -- | 'repositoryName'
  Prelude.Text ->
  BatchGetImage
newBatchGetImage pRepositoryName_ =
  BatchGetImage'
    { acceptedMediaTypes =
        Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = pRepositoryName_,
      imageIds = Prelude.mempty
    }

-- | The accepted media types for the request.
--
-- Valid values: @application\/vnd.docker.distribution.manifest.v1+json@ |
-- @application\/vnd.docker.distribution.manifest.v2+json@ |
-- @application\/vnd.oci.image.manifest.v1+json@
batchGetImage_acceptedMediaTypes :: Lens.Lens' BatchGetImage (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchGetImage_acceptedMediaTypes = Lens.lens (\BatchGetImage' {acceptedMediaTypes} -> acceptedMediaTypes) (\s@BatchGetImage' {} a -> s {acceptedMediaTypes = a} :: BatchGetImage) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID associated with the registry that
-- contains the images to describe. If you do not specify a registry, the
-- default registry is assumed.
batchGetImage_registryId :: Lens.Lens' BatchGetImage (Prelude.Maybe Prelude.Text)
batchGetImage_registryId = Lens.lens (\BatchGetImage' {registryId} -> registryId) (\s@BatchGetImage' {} a -> s {registryId = a} :: BatchGetImage)

-- | The repository that contains the images to describe.
batchGetImage_repositoryName :: Lens.Lens' BatchGetImage Prelude.Text
batchGetImage_repositoryName = Lens.lens (\BatchGetImage' {repositoryName} -> repositoryName) (\s@BatchGetImage' {} a -> s {repositoryName = a} :: BatchGetImage)

-- | A list of image ID references that correspond to images to describe. The
-- format of the @imageIds@ reference is @imageTag=tag@ or
-- @imageDigest=digest@.
batchGetImage_imageIds :: Lens.Lens' BatchGetImage [ImageIdentifier]
batchGetImage_imageIds = Lens.lens (\BatchGetImage' {imageIds} -> imageIds) (\s@BatchGetImage' {} a -> s {imageIds = a} :: BatchGetImage) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetImage where
  type
    AWSResponse BatchGetImage =
      BatchGetImageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetImageResponse'
            Prelude.<$> (x Data..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "images" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetImage where
  hashWithSalt _salt BatchGetImage' {..} =
    _salt
      `Prelude.hashWithSalt` acceptedMediaTypes
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` imageIds

instance Prelude.NFData BatchGetImage where
  rnf BatchGetImage' {..} =
    Prelude.rnf acceptedMediaTypes
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf imageIds

instance Data.ToHeaders BatchGetImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerRegistry_V20150921.BatchGetImage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetImage where
  toJSON BatchGetImage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("acceptedMediaTypes" Data..=)
              Prelude.<$> acceptedMediaTypes,
            ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just ("imageIds" Data..= imageIds)
          ]
      )

instance Data.ToPath BatchGetImage where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetImageResponse' smart constructor.
data BatchGetImageResponse = BatchGetImageResponse'
  { -- | Any failures associated with the call.
    failures :: Prelude.Maybe [ImageFailure],
    -- | A list of image objects corresponding to the image references in the
    -- request.
    images :: Prelude.Maybe [Image],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'batchGetImageResponse_failures' - Any failures associated with the call.
--
-- 'images', 'batchGetImageResponse_images' - A list of image objects corresponding to the image references in the
-- request.
--
-- 'httpStatus', 'batchGetImageResponse_httpStatus' - The response's http status code.
newBatchGetImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetImageResponse
newBatchGetImageResponse pHttpStatus_ =
  BatchGetImageResponse'
    { failures = Prelude.Nothing,
      images = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any failures associated with the call.
batchGetImageResponse_failures :: Lens.Lens' BatchGetImageResponse (Prelude.Maybe [ImageFailure])
batchGetImageResponse_failures = Lens.lens (\BatchGetImageResponse' {failures} -> failures) (\s@BatchGetImageResponse' {} a -> s {failures = a} :: BatchGetImageResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of image objects corresponding to the image references in the
-- request.
batchGetImageResponse_images :: Lens.Lens' BatchGetImageResponse (Prelude.Maybe [Image])
batchGetImageResponse_images = Lens.lens (\BatchGetImageResponse' {images} -> images) (\s@BatchGetImageResponse' {} a -> s {images = a} :: BatchGetImageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetImageResponse_httpStatus :: Lens.Lens' BatchGetImageResponse Prelude.Int
batchGetImageResponse_httpStatus = Lens.lens (\BatchGetImageResponse' {httpStatus} -> httpStatus) (\s@BatchGetImageResponse' {} a -> s {httpStatus = a} :: BatchGetImageResponse)

instance Prelude.NFData BatchGetImageResponse where
  rnf BatchGetImageResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf images
      `Prelude.seq` Prelude.rnf httpStatus
