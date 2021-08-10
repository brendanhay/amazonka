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
-- Module      : Network.AWS.ECR.PutImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the image manifest and tags associated with an image.
--
-- When an image is pushed and all new image layers have been uploaded, the
-- PutImage API is called once to create or update the image manifest and
-- the tags associated with the image.
--
-- This operation is used by the Amazon ECR proxy and is not generally used
-- by customers for pulling and pushing images. In most cases, you should
-- use the @docker@ CLI to pull, tag, and push images.
module Network.AWS.ECR.PutImage
  ( -- * Creating a Request
    PutImage (..),
    newPutImage,

    -- * Request Lenses
    putImage_imageDigest,
    putImage_registryId,
    putImage_imageTag,
    putImage_imageManifestMediaType,
    putImage_repositoryName,
    putImage_imageManifest,

    -- * Destructuring the Response
    PutImageResponse (..),
    newPutImageResponse,

    -- * Response Lenses
    putImageResponse_image,
    putImageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutImage' smart constructor.
data PutImage = PutImage'
  { -- | The image digest of the image manifest corresponding to the image.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID associated with the registry that contains the
    -- repository in which to put the image. If you do not specify a registry,
    -- the default registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The tag to associate with the image. This parameter is required for
    -- images that use the Docker Image Manifest V2 Schema 2 or Open Container
    -- Initiative (OCI) formats.
    imageTag :: Prelude.Maybe Prelude.Text,
    -- | The media type of the image manifest. If you push an image manifest that
    -- does not contain the @mediaType@ field, you must specify the
    -- @imageManifestMediaType@ in the request.
    imageManifestMediaType :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository in which to put the image.
    repositoryName :: Prelude.Text,
    -- | The image manifest corresponding to the image to be uploaded.
    imageManifest :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageDigest', 'putImage_imageDigest' - The image digest of the image manifest corresponding to the image.
--
-- 'registryId', 'putImage_registryId' - The AWS account ID associated with the registry that contains the
-- repository in which to put the image. If you do not specify a registry,
-- the default registry is assumed.
--
-- 'imageTag', 'putImage_imageTag' - The tag to associate with the image. This parameter is required for
-- images that use the Docker Image Manifest V2 Schema 2 or Open Container
-- Initiative (OCI) formats.
--
-- 'imageManifestMediaType', 'putImage_imageManifestMediaType' - The media type of the image manifest. If you push an image manifest that
-- does not contain the @mediaType@ field, you must specify the
-- @imageManifestMediaType@ in the request.
--
-- 'repositoryName', 'putImage_repositoryName' - The name of the repository in which to put the image.
--
-- 'imageManifest', 'putImage_imageManifest' - The image manifest corresponding to the image to be uploaded.
newPutImage ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'imageManifest'
  Prelude.Text ->
  PutImage
newPutImage pRepositoryName_ pImageManifest_ =
  PutImage'
    { imageDigest = Prelude.Nothing,
      registryId = Prelude.Nothing,
      imageTag = Prelude.Nothing,
      imageManifestMediaType = Prelude.Nothing,
      repositoryName = pRepositoryName_,
      imageManifest = pImageManifest_
    }

-- | The image digest of the image manifest corresponding to the image.
putImage_imageDigest :: Lens.Lens' PutImage (Prelude.Maybe Prelude.Text)
putImage_imageDigest = Lens.lens (\PutImage' {imageDigest} -> imageDigest) (\s@PutImage' {} a -> s {imageDigest = a} :: PutImage)

-- | The AWS account ID associated with the registry that contains the
-- repository in which to put the image. If you do not specify a registry,
-- the default registry is assumed.
putImage_registryId :: Lens.Lens' PutImage (Prelude.Maybe Prelude.Text)
putImage_registryId = Lens.lens (\PutImage' {registryId} -> registryId) (\s@PutImage' {} a -> s {registryId = a} :: PutImage)

-- | The tag to associate with the image. This parameter is required for
-- images that use the Docker Image Manifest V2 Schema 2 or Open Container
-- Initiative (OCI) formats.
putImage_imageTag :: Lens.Lens' PutImage (Prelude.Maybe Prelude.Text)
putImage_imageTag = Lens.lens (\PutImage' {imageTag} -> imageTag) (\s@PutImage' {} a -> s {imageTag = a} :: PutImage)

-- | The media type of the image manifest. If you push an image manifest that
-- does not contain the @mediaType@ field, you must specify the
-- @imageManifestMediaType@ in the request.
putImage_imageManifestMediaType :: Lens.Lens' PutImage (Prelude.Maybe Prelude.Text)
putImage_imageManifestMediaType = Lens.lens (\PutImage' {imageManifestMediaType} -> imageManifestMediaType) (\s@PutImage' {} a -> s {imageManifestMediaType = a} :: PutImage)

-- | The name of the repository in which to put the image.
putImage_repositoryName :: Lens.Lens' PutImage Prelude.Text
putImage_repositoryName = Lens.lens (\PutImage' {repositoryName} -> repositoryName) (\s@PutImage' {} a -> s {repositoryName = a} :: PutImage)

-- | The image manifest corresponding to the image to be uploaded.
putImage_imageManifest :: Lens.Lens' PutImage Prelude.Text
putImage_imageManifest = Lens.lens (\PutImage' {imageManifest} -> imageManifest) (\s@PutImage' {} a -> s {imageManifest = a} :: PutImage)

instance Core.AWSRequest PutImage where
  type AWSResponse PutImage = PutImageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutImageResponse'
            Prelude.<$> (x Core..?> "image")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutImage

instance Prelude.NFData PutImage

instance Core.ToHeaders PutImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.PutImage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutImage where
  toJSON PutImage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("imageDigest" Core..=) Prelude.<$> imageDigest,
            ("registryId" Core..=) Prelude.<$> registryId,
            ("imageTag" Core..=) Prelude.<$> imageTag,
            ("imageManifestMediaType" Core..=)
              Prelude.<$> imageManifestMediaType,
            Prelude.Just
              ("repositoryName" Core..= repositoryName),
            Prelude.Just
              ("imageManifest" Core..= imageManifest)
          ]
      )

instance Core.ToPath PutImage where
  toPath = Prelude.const "/"

instance Core.ToQuery PutImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutImageResponse' smart constructor.
data PutImageResponse = PutImageResponse'
  { -- | Details of the image uploaded.
    image :: Prelude.Maybe Image,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'image', 'putImageResponse_image' - Details of the image uploaded.
--
-- 'httpStatus', 'putImageResponse_httpStatus' - The response's http status code.
newPutImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutImageResponse
newPutImageResponse pHttpStatus_ =
  PutImageResponse'
    { image = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details of the image uploaded.
putImageResponse_image :: Lens.Lens' PutImageResponse (Prelude.Maybe Image)
putImageResponse_image = Lens.lens (\PutImageResponse' {image} -> image) (\s@PutImageResponse' {} a -> s {image = a} :: PutImageResponse)

-- | The response's http status code.
putImageResponse_httpStatus :: Lens.Lens' PutImageResponse Prelude.Int
putImageResponse_httpStatus = Lens.lens (\PutImageResponse' {httpStatus} -> httpStatus) (\s@PutImageResponse' {} a -> s {httpStatus = a} :: PutImageResponse)

instance Prelude.NFData PutImageResponse
