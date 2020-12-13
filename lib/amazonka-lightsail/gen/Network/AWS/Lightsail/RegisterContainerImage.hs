{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.RegisterContainerImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a container image to your Amazon Lightsail container service.
module Network.AWS.Lightsail.RegisterContainerImage
  ( -- * Creating a request
    RegisterContainerImage (..),
    mkRegisterContainerImage,

    -- ** Request lenses
    rciServiceName,
    rciDigest,
    rciLabel,

    -- * Destructuring the response
    RegisterContainerImageResponse (..),
    mkRegisterContainerImageResponse,

    -- ** Response lenses
    rcirsContainerImage,
    rcirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterContainerImage' smart constructor.
data RegisterContainerImage = RegisterContainerImage'
  { -- | The name of the container service for which to register a container image.
    serviceName :: Lude.Text,
    -- | The digest of the container image to be registered.
    digest :: Lude.Text,
    -- | The label for the container image when it's registered to the container service.
    --
    -- Use a descriptive label that you can use to track the different versions of your registered container images.
    -- Use the @GetContainerImages@ action to return the container images registered to a Lightsail container service. The label is the @<imagelabel>@ portion of the following image name example:
    --
    --     * @:container-service-1.<imagelabel>.1@
    --
    --
    -- If the name of your container service is @mycontainerservice@ , and the label that you specify is @mystaticwebsite@ , then the name of the registered container image will be @:mycontainerservice.mystaticwebsite.1@ .
    -- The number at the end of these image name examples represents the version of the registered container image. If you push and register another container image to the same Lightsail container service, with the same label, then the version number for the new registered container image will be @2@ . If you push and register another container image, the version number will be @3@ , and so on.
    label :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterContainerImage' with the minimum fields required to make a request.
--
-- * 'serviceName' - The name of the container service for which to register a container image.
-- * 'digest' - The digest of the container image to be registered.
-- * 'label' - The label for the container image when it's registered to the container service.
--
-- Use a descriptive label that you can use to track the different versions of your registered container images.
-- Use the @GetContainerImages@ action to return the container images registered to a Lightsail container service. The label is the @<imagelabel>@ portion of the following image name example:
--
--     * @:container-service-1.<imagelabel>.1@
--
--
-- If the name of your container service is @mycontainerservice@ , and the label that you specify is @mystaticwebsite@ , then the name of the registered container image will be @:mycontainerservice.mystaticwebsite.1@ .
-- The number at the end of these image name examples represents the version of the registered container image. If you push and register another container image to the same Lightsail container service, with the same label, then the version number for the new registered container image will be @2@ . If you push and register another container image, the version number will be @3@ , and so on.
mkRegisterContainerImage ::
  -- | 'serviceName'
  Lude.Text ->
  -- | 'digest'
  Lude.Text ->
  -- | 'label'
  Lude.Text ->
  RegisterContainerImage
mkRegisterContainerImage pServiceName_ pDigest_ pLabel_ =
  RegisterContainerImage'
    { serviceName = pServiceName_,
      digest = pDigest_,
      label = pLabel_
    }

-- | The name of the container service for which to register a container image.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciServiceName :: Lens.Lens' RegisterContainerImage Lude.Text
rciServiceName = Lens.lens (serviceName :: RegisterContainerImage -> Lude.Text) (\s a -> s {serviceName = a} :: RegisterContainerImage)
{-# DEPRECATED rciServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The digest of the container image to be registered.
--
-- /Note:/ Consider using 'digest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciDigest :: Lens.Lens' RegisterContainerImage Lude.Text
rciDigest = Lens.lens (digest :: RegisterContainerImage -> Lude.Text) (\s a -> s {digest = a} :: RegisterContainerImage)
{-# DEPRECATED rciDigest "Use generic-lens or generic-optics with 'digest' instead." #-}

-- | The label for the container image when it's registered to the container service.
--
-- Use a descriptive label that you can use to track the different versions of your registered container images.
-- Use the @GetContainerImages@ action to return the container images registered to a Lightsail container service. The label is the @<imagelabel>@ portion of the following image name example:
--
--     * @:container-service-1.<imagelabel>.1@
--
--
-- If the name of your container service is @mycontainerservice@ , and the label that you specify is @mystaticwebsite@ , then the name of the registered container image will be @:mycontainerservice.mystaticwebsite.1@ .
-- The number at the end of these image name examples represents the version of the registered container image. If you push and register another container image to the same Lightsail container service, with the same label, then the version number for the new registered container image will be @2@ . If you push and register another container image, the version number will be @3@ , and so on.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciLabel :: Lens.Lens' RegisterContainerImage Lude.Text
rciLabel = Lens.lens (label :: RegisterContainerImage -> Lude.Text) (\s a -> s {label = a} :: RegisterContainerImage)
{-# DEPRECATED rciLabel "Use generic-lens or generic-optics with 'label' instead." #-}

instance Lude.AWSRequest RegisterContainerImage where
  type Rs RegisterContainerImage = RegisterContainerImageResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterContainerImageResponse'
            Lude.<$> (x Lude..?> "containerImage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterContainerImage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.RegisterContainerImage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterContainerImage where
  toJSON RegisterContainerImage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("serviceName" Lude..= serviceName),
            Lude.Just ("digest" Lude..= digest),
            Lude.Just ("label" Lude..= label)
          ]
      )

instance Lude.ToPath RegisterContainerImage where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterContainerImage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterContainerImageResponse' smart constructor.
data RegisterContainerImageResponse = RegisterContainerImageResponse'
  { containerImage :: Lude.Maybe ContainerImage,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterContainerImageResponse' with the minimum fields required to make a request.
--
-- * 'containerImage' -
-- * 'responseStatus' - The response status code.
mkRegisterContainerImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterContainerImageResponse
mkRegisterContainerImageResponse pResponseStatus_ =
  RegisterContainerImageResponse'
    { containerImage = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'containerImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcirsContainerImage :: Lens.Lens' RegisterContainerImageResponse (Lude.Maybe ContainerImage)
rcirsContainerImage = Lens.lens (containerImage :: RegisterContainerImageResponse -> Lude.Maybe ContainerImage) (\s a -> s {containerImage = a} :: RegisterContainerImageResponse)
{-# DEPRECATED rcirsContainerImage "Use generic-lens or generic-optics with 'containerImage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcirsResponseStatus :: Lens.Lens' RegisterContainerImageResponse Lude.Int
rcirsResponseStatus = Lens.lens (responseStatus :: RegisterContainerImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterContainerImageResponse)
{-# DEPRECATED rcirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
