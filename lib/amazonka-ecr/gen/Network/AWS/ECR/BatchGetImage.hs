{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.BatchGetImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets detailed information for an image. Images are specified with either an @imageTag@ or @imageDigest@ .
--
-- When an image is pulled, the BatchGetImage API is called once to retrieve the image manifest.
module Network.AWS.ECR.BatchGetImage
  ( -- * Creating a request
    BatchGetImage (..),
    mkBatchGetImage,

    -- ** Request lenses
    bgiRegistryId,
    bgiImageIds,
    bgiRepositoryName,
    bgiAcceptedMediaTypes,

    -- * Destructuring the response
    BatchGetImageResponse (..),
    mkBatchGetImageResponse,

    -- ** Response lenses
    bgirsImages,
    bgirsFailures,
    bgirsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetImage' smart constructor.
data BatchGetImage = BatchGetImage'
  { -- | The AWS account ID associated with the registry that contains the images to describe. If you do not specify a registry, the default registry is assumed.
    registryId :: Lude.Maybe Lude.Text,
    -- | A list of image ID references that correspond to images to describe. The format of the @imageIds@ reference is @imageTag=tag@ or @imageDigest=digest@ .
    imageIds :: [ImageIdentifier],
    -- | The repository that contains the images to describe.
    repositoryName :: Lude.Text,
    -- | The accepted media types for the request.
    --
    -- Valid values: @application/vnd.docker.distribution.manifest.v1+json@ | @application/vnd.docker.distribution.manifest.v2+json@ | @application/vnd.oci.image.manifest.v1+json@
    acceptedMediaTypes :: Lude.Maybe (Lude.NonEmpty Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetImage' with the minimum fields required to make a request.
--
-- * 'registryId' - The AWS account ID associated with the registry that contains the images to describe. If you do not specify a registry, the default registry is assumed.
-- * 'imageIds' - A list of image ID references that correspond to images to describe. The format of the @imageIds@ reference is @imageTag=tag@ or @imageDigest=digest@ .
-- * 'repositoryName' - The repository that contains the images to describe.
-- * 'acceptedMediaTypes' - The accepted media types for the request.
--
-- Valid values: @application/vnd.docker.distribution.manifest.v1+json@ | @application/vnd.docker.distribution.manifest.v2+json@ | @application/vnd.oci.image.manifest.v1+json@
mkBatchGetImage ::
  -- | 'repositoryName'
  Lude.Text ->
  BatchGetImage
mkBatchGetImage pRepositoryName_ =
  BatchGetImage'
    { registryId = Lude.Nothing,
      imageIds = Lude.mempty,
      repositoryName = pRepositoryName_,
      acceptedMediaTypes = Lude.Nothing
    }

-- | The AWS account ID associated with the registry that contains the images to describe. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgiRegistryId :: Lens.Lens' BatchGetImage (Lude.Maybe Lude.Text)
bgiRegistryId = Lens.lens (registryId :: BatchGetImage -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: BatchGetImage)
{-# DEPRECATED bgiRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | A list of image ID references that correspond to images to describe. The format of the @imageIds@ reference is @imageTag=tag@ or @imageDigest=digest@ .
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgiImageIds :: Lens.Lens' BatchGetImage [ImageIdentifier]
bgiImageIds = Lens.lens (imageIds :: BatchGetImage -> [ImageIdentifier]) (\s a -> s {imageIds = a} :: BatchGetImage)
{-# DEPRECATED bgiImageIds "Use generic-lens or generic-optics with 'imageIds' instead." #-}

-- | The repository that contains the images to describe.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgiRepositoryName :: Lens.Lens' BatchGetImage Lude.Text
bgiRepositoryName = Lens.lens (repositoryName :: BatchGetImage -> Lude.Text) (\s a -> s {repositoryName = a} :: BatchGetImage)
{-# DEPRECATED bgiRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The accepted media types for the request.
--
-- Valid values: @application/vnd.docker.distribution.manifest.v1+json@ | @application/vnd.docker.distribution.manifest.v2+json@ | @application/vnd.oci.image.manifest.v1+json@
--
-- /Note:/ Consider using 'acceptedMediaTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgiAcceptedMediaTypes :: Lens.Lens' BatchGetImage (Lude.Maybe (Lude.NonEmpty Lude.Text))
bgiAcceptedMediaTypes = Lens.lens (acceptedMediaTypes :: BatchGetImage -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {acceptedMediaTypes = a} :: BatchGetImage)
{-# DEPRECATED bgiAcceptedMediaTypes "Use generic-lens or generic-optics with 'acceptedMediaTypes' instead." #-}

instance Lude.AWSRequest BatchGetImage where
  type Rs BatchGetImage = BatchGetImageResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetImageResponse'
            Lude.<$> (x Lude..?> "images" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "failures" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetImage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.BatchGetImage" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetImage where
  toJSON BatchGetImage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("imageIds" Lude..= imageIds),
            Lude.Just ("repositoryName" Lude..= repositoryName),
            ("acceptedMediaTypes" Lude..=) Lude.<$> acceptedMediaTypes
          ]
      )

instance Lude.ToPath BatchGetImage where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetImage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetImageResponse' smart constructor.
data BatchGetImageResponse = BatchGetImageResponse'
  { -- | A list of image objects corresponding to the image references in the request.
    images :: Lude.Maybe [Image],
    -- | Any failures associated with the call.
    failures :: Lude.Maybe [ImageFailure],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetImageResponse' with the minimum fields required to make a request.
--
-- * 'images' - A list of image objects corresponding to the image references in the request.
-- * 'failures' - Any failures associated with the call.
-- * 'responseStatus' - The response status code.
mkBatchGetImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetImageResponse
mkBatchGetImageResponse pResponseStatus_ =
  BatchGetImageResponse'
    { images = Lude.Nothing,
      failures = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of image objects corresponding to the image references in the request.
--
-- /Note:/ Consider using 'images' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgirsImages :: Lens.Lens' BatchGetImageResponse (Lude.Maybe [Image])
bgirsImages = Lens.lens (images :: BatchGetImageResponse -> Lude.Maybe [Image]) (\s a -> s {images = a} :: BatchGetImageResponse)
{-# DEPRECATED bgirsImages "Use generic-lens or generic-optics with 'images' instead." #-}

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgirsFailures :: Lens.Lens' BatchGetImageResponse (Lude.Maybe [ImageFailure])
bgirsFailures = Lens.lens (failures :: BatchGetImageResponse -> Lude.Maybe [ImageFailure]) (\s a -> s {failures = a} :: BatchGetImageResponse)
{-# DEPRECATED bgirsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgirsResponseStatus :: Lens.Lens' BatchGetImageResponse Lude.Int
bgirsResponseStatus = Lens.lens (responseStatus :: BatchGetImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetImageResponse)
{-# DEPRECATED bgirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
