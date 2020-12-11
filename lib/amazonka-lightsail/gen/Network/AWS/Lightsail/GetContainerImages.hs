{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetContainerImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the container images that are registered to your Amazon Lightsail container service.
module Network.AWS.Lightsail.GetContainerImages
  ( -- * Creating a request
    GetContainerImages (..),
    mkGetContainerImages,

    -- ** Request lenses
    gciServiceName,

    -- * Destructuring the response
    GetContainerImagesResponse (..),
    mkGetContainerImagesResponse,

    -- ** Response lenses
    gcirsContainerImages,
    gcirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetContainerImages' smart constructor.
newtype GetContainerImages = GetContainerImages'
  { serviceName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContainerImages' with the minimum fields required to make a request.
--
-- * 'serviceName' - The name of the container service for which to return registered container images.
mkGetContainerImages ::
  -- | 'serviceName'
  Lude.Text ->
  GetContainerImages
mkGetContainerImages pServiceName_ =
  GetContainerImages' {serviceName = pServiceName_}

-- | The name of the container service for which to return registered container images.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gciServiceName :: Lens.Lens' GetContainerImages Lude.Text
gciServiceName = Lens.lens (serviceName :: GetContainerImages -> Lude.Text) (\s a -> s {serviceName = a} :: GetContainerImages)
{-# DEPRECATED gciServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Lude.AWSRequest GetContainerImages where
  type Rs GetContainerImages = GetContainerImagesResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetContainerImagesResponse'
            Lude.<$> (x Lude..?> "containerImages" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetContainerImages where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetContainerImages" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetContainerImages where
  toJSON GetContainerImages' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("serviceName" Lude..= serviceName)])

instance Lude.ToPath GetContainerImages where
  toPath = Lude.const "/"

instance Lude.ToQuery GetContainerImages where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetContainerImagesResponse' smart constructor.
data GetContainerImagesResponse = GetContainerImagesResponse'
  { containerImages ::
      Lude.Maybe [ContainerImage],
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

-- | Creates a value of 'GetContainerImagesResponse' with the minimum fields required to make a request.
--
-- * 'containerImages' - An array of objects that describe container images that are registered to the container service.
-- * 'responseStatus' - The response status code.
mkGetContainerImagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetContainerImagesResponse
mkGetContainerImagesResponse pResponseStatus_ =
  GetContainerImagesResponse'
    { containerImages = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe container images that are registered to the container service.
--
-- /Note:/ Consider using 'containerImages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsContainerImages :: Lens.Lens' GetContainerImagesResponse (Lude.Maybe [ContainerImage])
gcirsContainerImages = Lens.lens (containerImages :: GetContainerImagesResponse -> Lude.Maybe [ContainerImage]) (\s a -> s {containerImages = a} :: GetContainerImagesResponse)
{-# DEPRECATED gcirsContainerImages "Use generic-lens or generic-optics with 'containerImages' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsResponseStatus :: Lens.Lens' GetContainerImagesResponse Lude.Int
gcirsResponseStatus = Lens.lens (responseStatus :: GetContainerImagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetContainerImagesResponse)
{-# DEPRECATED gcirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
