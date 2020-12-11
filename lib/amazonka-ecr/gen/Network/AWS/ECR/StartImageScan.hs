{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.StartImageScan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an image vulnerability scan. An image scan can only be started once per day on an individual image. This limit includes if an image was scanned on initial push. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/image-scanning.html Image Scanning> in the /Amazon Elastic Container Registry User Guide/ .
module Network.AWS.ECR.StartImageScan
  ( -- * Creating a request
    StartImageScan (..),
    mkStartImageScan,

    -- ** Request lenses
    sisRegistryId,
    sisRepositoryName,
    sisImageId,

    -- * Destructuring the response
    StartImageScanResponse (..),
    mkStartImageScanResponse,

    -- ** Response lenses
    sisrsRegistryId,
    sisrsImageScanStatus,
    sisrsImageId,
    sisrsRepositoryName,
    sisrsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartImageScan' smart constructor.
data StartImageScan = StartImageScan'
  { registryId ::
      Lude.Maybe Lude.Text,
    repositoryName :: Lude.Text,
    imageId :: ImageIdentifier
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartImageScan' with the minimum fields required to make a request.
--
-- * 'imageId' - Undocumented field.
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository in which to start an image scan request. If you do not specify a registry, the default registry is assumed.
-- * 'repositoryName' - The name of the repository that contains the images to scan.
mkStartImageScan ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'imageId'
  ImageIdentifier ->
  StartImageScan
mkStartImageScan pRepositoryName_ pImageId_ =
  StartImageScan'
    { registryId = Lude.Nothing,
      repositoryName = pRepositoryName_,
      imageId = pImageId_
    }

-- | The AWS account ID associated with the registry that contains the repository in which to start an image scan request. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisRegistryId :: Lens.Lens' StartImageScan (Lude.Maybe Lude.Text)
sisRegistryId = Lens.lens (registryId :: StartImageScan -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: StartImageScan)
{-# DEPRECATED sisRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The name of the repository that contains the images to scan.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisRepositoryName :: Lens.Lens' StartImageScan Lude.Text
sisRepositoryName = Lens.lens (repositoryName :: StartImageScan -> Lude.Text) (\s a -> s {repositoryName = a} :: StartImageScan)
{-# DEPRECATED sisRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisImageId :: Lens.Lens' StartImageScan ImageIdentifier
sisImageId = Lens.lens (imageId :: StartImageScan -> ImageIdentifier) (\s a -> s {imageId = a} :: StartImageScan)
{-# DEPRECATED sisImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

instance Lude.AWSRequest StartImageScan where
  type Rs StartImageScan = StartImageScanResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartImageScanResponse'
            Lude.<$> (x Lude..?> "registryId")
            Lude.<*> (x Lude..?> "imageScanStatus")
            Lude.<*> (x Lude..?> "imageId")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartImageScan where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.StartImageScan" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartImageScan where
  toJSON StartImageScan' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("imageId" Lude..= imageId)
          ]
      )

instance Lude.ToPath StartImageScan where
  toPath = Lude.const "/"

instance Lude.ToQuery StartImageScan where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartImageScanResponse' smart constructor.
data StartImageScanResponse = StartImageScanResponse'
  { registryId ::
      Lude.Maybe Lude.Text,
    imageScanStatus :: Lude.Maybe ImageScanStatus,
    imageId :: Lude.Maybe ImageIdentifier,
    repositoryName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'StartImageScanResponse' with the minimum fields required to make a request.
--
-- * 'imageId' - Undocumented field.
-- * 'imageScanStatus' - The current state of the scan.
-- * 'registryId' - The registry ID associated with the request.
-- * 'repositoryName' - The repository name associated with the request.
-- * 'responseStatus' - The response status code.
mkStartImageScanResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartImageScanResponse
mkStartImageScanResponse pResponseStatus_ =
  StartImageScanResponse'
    { registryId = Lude.Nothing,
      imageScanStatus = Lude.Nothing,
      imageId = Lude.Nothing,
      repositoryName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisrsRegistryId :: Lens.Lens' StartImageScanResponse (Lude.Maybe Lude.Text)
sisrsRegistryId = Lens.lens (registryId :: StartImageScanResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: StartImageScanResponse)
{-# DEPRECATED sisrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The current state of the scan.
--
-- /Note:/ Consider using 'imageScanStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisrsImageScanStatus :: Lens.Lens' StartImageScanResponse (Lude.Maybe ImageScanStatus)
sisrsImageScanStatus = Lens.lens (imageScanStatus :: StartImageScanResponse -> Lude.Maybe ImageScanStatus) (\s a -> s {imageScanStatus = a} :: StartImageScanResponse)
{-# DEPRECATED sisrsImageScanStatus "Use generic-lens or generic-optics with 'imageScanStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisrsImageId :: Lens.Lens' StartImageScanResponse (Lude.Maybe ImageIdentifier)
sisrsImageId = Lens.lens (imageId :: StartImageScanResponse -> Lude.Maybe ImageIdentifier) (\s a -> s {imageId = a} :: StartImageScanResponse)
{-# DEPRECATED sisrsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisrsRepositoryName :: Lens.Lens' StartImageScanResponse (Lude.Maybe Lude.Text)
sisrsRepositoryName = Lens.lens (repositoryName :: StartImageScanResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: StartImageScanResponse)
{-# DEPRECATED sisrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisrsResponseStatus :: Lens.Lens' StartImageScanResponse Lude.Int
sisrsResponseStatus = Lens.lens (responseStatus :: StartImageScanResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartImageScanResponse)
{-# DEPRECATED sisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
