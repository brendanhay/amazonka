{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.PutImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the image manifest and tags associated with an image.
--
-- When an image is pushed and all new image layers have been uploaded, the PutImage API is called once to create or update the image manifest and the tags associated with the image.
module Network.AWS.ECR.PutImage
  ( -- * Creating a request
    PutImage (..),
    mkPutImage,

    -- ** Request lenses
    piRegistryId,
    piImageManifestMediaType,
    piImageDigest,
    piImageTag,
    piRepositoryName,
    piImageManifest,

    -- * Destructuring the response
    PutImageResponse (..),
    mkPutImageResponse,

    -- ** Response lenses
    pirsImage,
    pirsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutImage' smart constructor.
data PutImage = PutImage'
  { registryId :: Lude.Maybe Lude.Text,
    imageManifestMediaType :: Lude.Maybe Lude.Text,
    imageDigest :: Lude.Maybe Lude.Text,
    imageTag :: Lude.Maybe Lude.Text,
    repositoryName :: Lude.Text,
    imageManifest :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutImage' with the minimum fields required to make a request.
--
-- * 'imageDigest' - The image digest of the image manifest corresponding to the image.
-- * 'imageManifest' - The image manifest corresponding to the image to be uploaded.
-- * 'imageManifestMediaType' - The media type of the image manifest. If you push an image manifest that does not contain the @mediaType@ field, you must specify the @imageManifestMediaType@ in the request.
-- * 'imageTag' - The tag to associate with the image. This parameter is required for images that use the Docker Image Manifest V2 Schema 2 or Open Container Initiative (OCI) formats.
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository in which to put the image. If you do not specify a registry, the default registry is assumed.
-- * 'repositoryName' - The name of the repository in which to put the image.
mkPutImage ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'imageManifest'
  Lude.Text ->
  PutImage
mkPutImage pRepositoryName_ pImageManifest_ =
  PutImage'
    { registryId = Lude.Nothing,
      imageManifestMediaType = Lude.Nothing,
      imageDigest = Lude.Nothing,
      imageTag = Lude.Nothing,
      repositoryName = pRepositoryName_,
      imageManifest = pImageManifest_
    }

-- | The AWS account ID associated with the registry that contains the repository in which to put the image. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRegistryId :: Lens.Lens' PutImage (Lude.Maybe Lude.Text)
piRegistryId = Lens.lens (registryId :: PutImage -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: PutImage)
{-# DEPRECATED piRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The media type of the image manifest. If you push an image manifest that does not contain the @mediaType@ field, you must specify the @imageManifestMediaType@ in the request.
--
-- /Note:/ Consider using 'imageManifestMediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piImageManifestMediaType :: Lens.Lens' PutImage (Lude.Maybe Lude.Text)
piImageManifestMediaType = Lens.lens (imageManifestMediaType :: PutImage -> Lude.Maybe Lude.Text) (\s a -> s {imageManifestMediaType = a} :: PutImage)
{-# DEPRECATED piImageManifestMediaType "Use generic-lens or generic-optics with 'imageManifestMediaType' instead." #-}

-- | The image digest of the image manifest corresponding to the image.
--
-- /Note:/ Consider using 'imageDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piImageDigest :: Lens.Lens' PutImage (Lude.Maybe Lude.Text)
piImageDigest = Lens.lens (imageDigest :: PutImage -> Lude.Maybe Lude.Text) (\s a -> s {imageDigest = a} :: PutImage)
{-# DEPRECATED piImageDigest "Use generic-lens or generic-optics with 'imageDigest' instead." #-}

-- | The tag to associate with the image. This parameter is required for images that use the Docker Image Manifest V2 Schema 2 or Open Container Initiative (OCI) formats.
--
-- /Note:/ Consider using 'imageTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piImageTag :: Lens.Lens' PutImage (Lude.Maybe Lude.Text)
piImageTag = Lens.lens (imageTag :: PutImage -> Lude.Maybe Lude.Text) (\s a -> s {imageTag = a} :: PutImage)
{-# DEPRECATED piImageTag "Use generic-lens or generic-optics with 'imageTag' instead." #-}

-- | The name of the repository in which to put the image.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRepositoryName :: Lens.Lens' PutImage Lude.Text
piRepositoryName = Lens.lens (repositoryName :: PutImage -> Lude.Text) (\s a -> s {repositoryName = a} :: PutImage)
{-# DEPRECATED piRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The image manifest corresponding to the image to be uploaded.
--
-- /Note:/ Consider using 'imageManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piImageManifest :: Lens.Lens' PutImage Lude.Text
piImageManifest = Lens.lens (imageManifest :: PutImage -> Lude.Text) (\s a -> s {imageManifest = a} :: PutImage)
{-# DEPRECATED piImageManifest "Use generic-lens or generic-optics with 'imageManifest' instead." #-}

instance Lude.AWSRequest PutImage where
  type Rs PutImage = PutImageResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutImageResponse'
            Lude.<$> (x Lude..?> "image") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutImage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.PutImage" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutImage where
  toJSON PutImage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            ("imageManifestMediaType" Lude..=) Lude.<$> imageManifestMediaType,
            ("imageDigest" Lude..=) Lude.<$> imageDigest,
            ("imageTag" Lude..=) Lude.<$> imageTag,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("imageManifest" Lude..= imageManifest)
          ]
      )

instance Lude.ToPath PutImage where
  toPath = Lude.const "/"

instance Lude.ToQuery PutImage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutImageResponse' smart constructor.
data PutImageResponse = PutImageResponse'
  { image ::
      Lude.Maybe Image,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutImageResponse' with the minimum fields required to make a request.
--
-- * 'image' - Details of the image uploaded.
-- * 'responseStatus' - The response status code.
mkPutImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutImageResponse
mkPutImageResponse pResponseStatus_ =
  PutImageResponse'
    { image = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Details of the image uploaded.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsImage :: Lens.Lens' PutImageResponse (Lude.Maybe Image)
pirsImage = Lens.lens (image :: PutImageResponse -> Lude.Maybe Image) (\s a -> s {image = a} :: PutImageResponse)
{-# DEPRECATED pirsImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsResponseStatus :: Lens.Lens' PutImageResponse Lude.Int
pirsResponseStatus = Lens.lens (responseStatus :: PutImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutImageResponse)
{-# DEPRECATED pirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
