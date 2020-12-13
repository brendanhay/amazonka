{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.GetDownloadURLForLayer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the pre-signed Amazon S3 download URL corresponding to an image layer. You can only get URLs for image layers that are referenced in an image.
--
-- When an image is pulled, the GetDownloadUrlForLayer API is called once per image layer that is not already cached.
module Network.AWS.ECR.GetDownloadURLForLayer
  ( -- * Creating a request
    GetDownloadURLForLayer (..),
    mkGetDownloadURLForLayer,

    -- ** Request lenses
    gduflRegistryId,
    gduflLayerDigest,
    gduflRepositoryName,

    -- * Destructuring the response
    GetDownloadURLForLayerResponse (..),
    mkGetDownloadURLForLayerResponse,

    -- ** Response lenses
    gduflrsLayerDigest,
    gduflrsDownloadURL,
    gduflrsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDownloadURLForLayer' smart constructor.
data GetDownloadURLForLayer = GetDownloadURLForLayer'
  { -- | The AWS account ID associated with the registry that contains the image layer to download. If you do not specify a registry, the default registry is assumed.
    registryId :: Lude.Maybe Lude.Text,
    -- | The digest of the image layer to download.
    layerDigest :: Lude.Text,
    -- | The name of the repository that is associated with the image layer to download.
    repositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDownloadURLForLayer' with the minimum fields required to make a request.
--
-- * 'registryId' - The AWS account ID associated with the registry that contains the image layer to download. If you do not specify a registry, the default registry is assumed.
-- * 'layerDigest' - The digest of the image layer to download.
-- * 'repositoryName' - The name of the repository that is associated with the image layer to download.
mkGetDownloadURLForLayer ::
  -- | 'layerDigest'
  Lude.Text ->
  -- | 'repositoryName'
  Lude.Text ->
  GetDownloadURLForLayer
mkGetDownloadURLForLayer pLayerDigest_ pRepositoryName_ =
  GetDownloadURLForLayer'
    { registryId = Lude.Nothing,
      layerDigest = pLayerDigest_,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the image layer to download. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gduflRegistryId :: Lens.Lens' GetDownloadURLForLayer (Lude.Maybe Lude.Text)
gduflRegistryId = Lens.lens (registryId :: GetDownloadURLForLayer -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: GetDownloadURLForLayer)
{-# DEPRECATED gduflRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The digest of the image layer to download.
--
-- /Note:/ Consider using 'layerDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gduflLayerDigest :: Lens.Lens' GetDownloadURLForLayer Lude.Text
gduflLayerDigest = Lens.lens (layerDigest :: GetDownloadURLForLayer -> Lude.Text) (\s a -> s {layerDigest = a} :: GetDownloadURLForLayer)
{-# DEPRECATED gduflLayerDigest "Use generic-lens or generic-optics with 'layerDigest' instead." #-}

-- | The name of the repository that is associated with the image layer to download.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gduflRepositoryName :: Lens.Lens' GetDownloadURLForLayer Lude.Text
gduflRepositoryName = Lens.lens (repositoryName :: GetDownloadURLForLayer -> Lude.Text) (\s a -> s {repositoryName = a} :: GetDownloadURLForLayer)
{-# DEPRECATED gduflRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest GetDownloadURLForLayer where
  type Rs GetDownloadURLForLayer = GetDownloadURLForLayerResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDownloadURLForLayerResponse'
            Lude.<$> (x Lude..?> "layerDigest")
            Lude.<*> (x Lude..?> "downloadUrl")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDownloadURLForLayer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.GetDownloadUrlForLayer" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDownloadURLForLayer where
  toJSON GetDownloadURLForLayer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("layerDigest" Lude..= layerDigest),
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath GetDownloadURLForLayer where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDownloadURLForLayer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDownloadURLForLayerResponse' smart constructor.
data GetDownloadURLForLayerResponse = GetDownloadURLForLayerResponse'
  { -- | The digest of the image layer to download.
    layerDigest :: Lude.Maybe Lude.Text,
    -- | The pre-signed Amazon S3 download URL for the requested layer.
    downloadURL :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDownloadURLForLayerResponse' with the minimum fields required to make a request.
--
-- * 'layerDigest' - The digest of the image layer to download.
-- * 'downloadURL' - The pre-signed Amazon S3 download URL for the requested layer.
-- * 'responseStatus' - The response status code.
mkGetDownloadURLForLayerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDownloadURLForLayerResponse
mkGetDownloadURLForLayerResponse pResponseStatus_ =
  GetDownloadURLForLayerResponse'
    { layerDigest = Lude.Nothing,
      downloadURL = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The digest of the image layer to download.
--
-- /Note:/ Consider using 'layerDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gduflrsLayerDigest :: Lens.Lens' GetDownloadURLForLayerResponse (Lude.Maybe Lude.Text)
gduflrsLayerDigest = Lens.lens (layerDigest :: GetDownloadURLForLayerResponse -> Lude.Maybe Lude.Text) (\s a -> s {layerDigest = a} :: GetDownloadURLForLayerResponse)
{-# DEPRECATED gduflrsLayerDigest "Use generic-lens or generic-optics with 'layerDigest' instead." #-}

-- | The pre-signed Amazon S3 download URL for the requested layer.
--
-- /Note:/ Consider using 'downloadURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gduflrsDownloadURL :: Lens.Lens' GetDownloadURLForLayerResponse (Lude.Maybe Lude.Text)
gduflrsDownloadURL = Lens.lens (downloadURL :: GetDownloadURLForLayerResponse -> Lude.Maybe Lude.Text) (\s a -> s {downloadURL = a} :: GetDownloadURLForLayerResponse)
{-# DEPRECATED gduflrsDownloadURL "Use generic-lens or generic-optics with 'downloadURL' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gduflrsResponseStatus :: Lens.Lens' GetDownloadURLForLayerResponse Lude.Int
gduflrsResponseStatus = Lens.lens (responseStatus :: GetDownloadURLForLayerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDownloadURLForLayerResponse)
{-# DEPRECATED gduflrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
