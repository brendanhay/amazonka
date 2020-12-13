{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.GetService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the settings for a specified service.
module Network.AWS.Route53AutoNaming.GetService
  ( -- * Creating a request
    GetService (..),
    mkGetService,

    -- ** Request lenses
    gsId,

    -- * Destructuring the response
    GetServiceResponse (..),
    mkGetServiceResponse,

    -- ** Response lenses
    gsrsService,
    gsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkGetService' smart constructor.
newtype GetService = GetService'
  { -- | The ID of the service that you want to get settings for.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetService' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the service that you want to get settings for.
mkGetService ::
  -- | 'id'
  Lude.Text ->
  GetService
mkGetService pId_ = GetService' {id = pId_}

-- | The ID of the service that you want to get settings for.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsId :: Lens.Lens' GetService Lude.Text
gsId = Lens.lens (id :: GetService -> Lude.Text) (\s a -> s {id = a} :: GetService)
{-# DEPRECATED gsId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetService where
  type Rs GetService = GetServiceResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetServiceResponse'
            Lude.<$> (x Lude..?> "Service") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetService where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53AutoNaming_v20170314.GetService" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetService where
  toJSON GetService' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Id" Lude..= id)])

instance Lude.ToPath GetService where
  toPath = Lude.const "/"

instance Lude.ToQuery GetService where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetServiceResponse' smart constructor.
data GetServiceResponse = GetServiceResponse'
  { -- | A complex type that contains information about the service.
    service :: Lude.Maybe ServiceInfo,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetServiceResponse' with the minimum fields required to make a request.
--
-- * 'service' - A complex type that contains information about the service.
-- * 'responseStatus' - The response status code.
mkGetServiceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetServiceResponse
mkGetServiceResponse pResponseStatus_ =
  GetServiceResponse'
    { service = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A complex type that contains information about the service.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsService :: Lens.Lens' GetServiceResponse (Lude.Maybe ServiceInfo)
gsrsService = Lens.lens (service :: GetServiceResponse -> Lude.Maybe ServiceInfo) (\s a -> s {service = a} :: GetServiceResponse)
{-# DEPRECATED gsrsService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsResponseStatus :: Lens.Lens' GetServiceResponse Lude.Int
gsrsResponseStatus = Lens.lens (responseStatus :: GetServiceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetServiceResponse)
{-# DEPRECATED gsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
