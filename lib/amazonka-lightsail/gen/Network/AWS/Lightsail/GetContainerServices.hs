{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetContainerServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more of your Amazon Lightsail container services.
module Network.AWS.Lightsail.GetContainerServices
  ( -- * Creating a request
    GetContainerServices (..),
    mkGetContainerServices,

    -- ** Request lenses
    gcsServiceName,

    -- * Destructuring the response
    GetContainerServicesResponse (..),
    mkGetContainerServicesResponse,

    -- ** Response lenses
    gcsrsContainerServices,
    gcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetContainerServices' smart constructor.
newtype GetContainerServices = GetContainerServices'
  { -- | The name of the container service for which to return information.
    --
    -- When omitted, the response includes all of your container services in the AWS Region where the request is made.
    serviceName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContainerServices' with the minimum fields required to make a request.
--
-- * 'serviceName' - The name of the container service for which to return information.
--
-- When omitted, the response includes all of your container services in the AWS Region where the request is made.
mkGetContainerServices ::
  GetContainerServices
mkGetContainerServices =
  GetContainerServices' {serviceName = Lude.Nothing}

-- | The name of the container service for which to return information.
--
-- When omitted, the response includes all of your container services in the AWS Region where the request is made.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsServiceName :: Lens.Lens' GetContainerServices (Lude.Maybe Lude.Text)
gcsServiceName = Lens.lens (serviceName :: GetContainerServices -> Lude.Maybe Lude.Text) (\s a -> s {serviceName = a} :: GetContainerServices)
{-# DEPRECATED gcsServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Lude.AWSRequest GetContainerServices where
  type Rs GetContainerServices = GetContainerServicesResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetContainerServicesResponse'
            Lude.<$> (x Lude..?> "containerServices" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetContainerServices where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetContainerServices" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetContainerServices where
  toJSON GetContainerServices' {..} =
    Lude.object
      (Lude.catMaybes [("serviceName" Lude..=) Lude.<$> serviceName])

instance Lude.ToPath GetContainerServices where
  toPath = Lude.const "/"

instance Lude.ToQuery GetContainerServices where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetContainerServicesResponse' smart constructor.
data GetContainerServicesResponse = GetContainerServicesResponse'
  { -- | An array of objects that describe one or more container services.
    containerServices :: Lude.Maybe [ContainerService],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContainerServicesResponse' with the minimum fields required to make a request.
--
-- * 'containerServices' - An array of objects that describe one or more container services.
-- * 'responseStatus' - The response status code.
mkGetContainerServicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetContainerServicesResponse
mkGetContainerServicesResponse pResponseStatus_ =
  GetContainerServicesResponse'
    { containerServices = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe one or more container services.
--
-- /Note:/ Consider using 'containerServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsContainerServices :: Lens.Lens' GetContainerServicesResponse (Lude.Maybe [ContainerService])
gcsrsContainerServices = Lens.lens (containerServices :: GetContainerServicesResponse -> Lude.Maybe [ContainerService]) (\s a -> s {containerServices = a} :: GetContainerServicesResponse)
{-# DEPRECATED gcsrsContainerServices "Use generic-lens or generic-optics with 'containerServices' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsResponseStatus :: Lens.Lens' GetContainerServicesResponse Lude.Int
gcsrsResponseStatus = Lens.lens (responseStatus :: GetContainerServicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetContainerServicesResponse)
{-# DEPRECATED gcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
