{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.PutImageScanningConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the image scanning configuration for the specified repository.
module Network.AWS.ECR.PutImageScanningConfiguration
  ( -- * Creating a request
    PutImageScanningConfiguration (..),
    mkPutImageScanningConfiguration,

    -- ** Request lenses
    piscRegistryId,
    piscImageScanningConfiguration,
    piscRepositoryName,

    -- * Destructuring the response
    PutImageScanningConfigurationResponse (..),
    mkPutImageScanningConfigurationResponse,

    -- ** Response lenses
    piscrsRegistryId,
    piscrsImageScanningConfiguration,
    piscrsRepositoryName,
    piscrsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutImageScanningConfiguration' smart constructor.
data PutImageScanningConfiguration = PutImageScanningConfiguration'
  { -- | The AWS account ID associated with the registry that contains the repository in which to update the image scanning configuration setting. If you do not specify a registry, the default registry is assumed.
    registryId :: Lude.Maybe Lude.Text,
    -- | The image scanning configuration for the repository. This setting determines whether images are scanned for known vulnerabilities after being pushed to the repository.
    imageScanningConfiguration :: ImageScanningConfiguration,
    -- | The name of the repository in which to update the image scanning configuration setting.
    repositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutImageScanningConfiguration' with the minimum fields required to make a request.
--
-- * 'registryId' - The AWS account ID associated with the registry that contains the repository in which to update the image scanning configuration setting. If you do not specify a registry, the default registry is assumed.
-- * 'imageScanningConfiguration' - The image scanning configuration for the repository. This setting determines whether images are scanned for known vulnerabilities after being pushed to the repository.
-- * 'repositoryName' - The name of the repository in which to update the image scanning configuration setting.
mkPutImageScanningConfiguration ::
  -- | 'imageScanningConfiguration'
  ImageScanningConfiguration ->
  -- | 'repositoryName'
  Lude.Text ->
  PutImageScanningConfiguration
mkPutImageScanningConfiguration
  pImageScanningConfiguration_
  pRepositoryName_ =
    PutImageScanningConfiguration'
      { registryId = Lude.Nothing,
        imageScanningConfiguration = pImageScanningConfiguration_,
        repositoryName = pRepositoryName_
      }

-- | The AWS account ID associated with the registry that contains the repository in which to update the image scanning configuration setting. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscRegistryId :: Lens.Lens' PutImageScanningConfiguration (Lude.Maybe Lude.Text)
piscRegistryId = Lens.lens (registryId :: PutImageScanningConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: PutImageScanningConfiguration)
{-# DEPRECATED piscRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The image scanning configuration for the repository. This setting determines whether images are scanned for known vulnerabilities after being pushed to the repository.
--
-- /Note:/ Consider using 'imageScanningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscImageScanningConfiguration :: Lens.Lens' PutImageScanningConfiguration ImageScanningConfiguration
piscImageScanningConfiguration = Lens.lens (imageScanningConfiguration :: PutImageScanningConfiguration -> ImageScanningConfiguration) (\s a -> s {imageScanningConfiguration = a} :: PutImageScanningConfiguration)
{-# DEPRECATED piscImageScanningConfiguration "Use generic-lens or generic-optics with 'imageScanningConfiguration' instead." #-}

-- | The name of the repository in which to update the image scanning configuration setting.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscRepositoryName :: Lens.Lens' PutImageScanningConfiguration Lude.Text
piscRepositoryName = Lens.lens (repositoryName :: PutImageScanningConfiguration -> Lude.Text) (\s a -> s {repositoryName = a} :: PutImageScanningConfiguration)
{-# DEPRECATED piscRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest PutImageScanningConfiguration where
  type
    Rs PutImageScanningConfiguration =
      PutImageScanningConfigurationResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutImageScanningConfigurationResponse'
            Lude.<$> (x Lude..?> "registryId")
            Lude.<*> (x Lude..?> "imageScanningConfiguration")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutImageScanningConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.PutImageScanningConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutImageScanningConfiguration where
  toJSON PutImageScanningConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just
              ("imageScanningConfiguration" Lude..= imageScanningConfiguration),
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath PutImageScanningConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery PutImageScanningConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutImageScanningConfigurationResponse' smart constructor.
data PutImageScanningConfigurationResponse = PutImageScanningConfigurationResponse'
  { -- | The registry ID associated with the request.
    registryId :: Lude.Maybe Lude.Text,
    -- | The image scanning configuration setting for the repository.
    imageScanningConfiguration :: Lude.Maybe ImageScanningConfiguration,
    -- | The repository name associated with the request.
    repositoryName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutImageScanningConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'registryId' - The registry ID associated with the request.
-- * 'imageScanningConfiguration' - The image scanning configuration setting for the repository.
-- * 'repositoryName' - The repository name associated with the request.
-- * 'responseStatus' - The response status code.
mkPutImageScanningConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutImageScanningConfigurationResponse
mkPutImageScanningConfigurationResponse pResponseStatus_ =
  PutImageScanningConfigurationResponse'
    { registryId = Lude.Nothing,
      imageScanningConfiguration = Lude.Nothing,
      repositoryName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscrsRegistryId :: Lens.Lens' PutImageScanningConfigurationResponse (Lude.Maybe Lude.Text)
piscrsRegistryId = Lens.lens (registryId :: PutImageScanningConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: PutImageScanningConfigurationResponse)
{-# DEPRECATED piscrsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The image scanning configuration setting for the repository.
--
-- /Note:/ Consider using 'imageScanningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscrsImageScanningConfiguration :: Lens.Lens' PutImageScanningConfigurationResponse (Lude.Maybe ImageScanningConfiguration)
piscrsImageScanningConfiguration = Lens.lens (imageScanningConfiguration :: PutImageScanningConfigurationResponse -> Lude.Maybe ImageScanningConfiguration) (\s a -> s {imageScanningConfiguration = a} :: PutImageScanningConfigurationResponse)
{-# DEPRECATED piscrsImageScanningConfiguration "Use generic-lens or generic-optics with 'imageScanningConfiguration' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscrsRepositoryName :: Lens.Lens' PutImageScanningConfigurationResponse (Lude.Maybe Lude.Text)
piscrsRepositoryName = Lens.lens (repositoryName :: PutImageScanningConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: PutImageScanningConfigurationResponse)
{-# DEPRECATED piscrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piscrsResponseStatus :: Lens.Lens' PutImageScanningConfigurationResponse Lude.Int
piscrsResponseStatus = Lens.lens (responseStatus :: PutImageScanningConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutImageScanningConfigurationResponse)
{-# DEPRECATED piscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
