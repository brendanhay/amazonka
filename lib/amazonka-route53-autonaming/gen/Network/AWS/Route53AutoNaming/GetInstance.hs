{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.GetInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified instance.
module Network.AWS.Route53AutoNaming.GetInstance
  ( -- * Creating a request
    GetInstance (..),
    mkGetInstance,

    -- ** Request lenses
    giServiceId,
    giInstanceId,

    -- * Destructuring the response
    GetInstanceResponse (..),
    mkGetInstanceResponse,

    -- ** Response lenses
    girsInstance,
    girsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkGetInstance' smart constructor.
data GetInstance = GetInstance'
  { serviceId :: Lude.Text,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance that you want to get information about.
-- * 'serviceId' - The ID of the service that the instance is associated with.
mkGetInstance ::
  -- | 'serviceId'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  GetInstance
mkGetInstance pServiceId_ pInstanceId_ =
  GetInstance' {serviceId = pServiceId_, instanceId = pInstanceId_}

-- | The ID of the service that the instance is associated with.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giServiceId :: Lens.Lens' GetInstance Lude.Text
giServiceId = Lens.lens (serviceId :: GetInstance -> Lude.Text) (\s a -> s {serviceId = a} :: GetInstance)
{-# DEPRECATED giServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | The ID of the instance that you want to get information about.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giInstanceId :: Lens.Lens' GetInstance Lude.Text
giInstanceId = Lens.lens (instanceId :: GetInstance -> Lude.Text) (\s a -> s {instanceId = a} :: GetInstance)
{-# DEPRECATED giInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest GetInstance where
  type Rs GetInstance = GetInstanceResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInstanceResponse'
            Lude.<$> (x Lude..?> "Instance") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53AutoNaming_v20170314.GetInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetInstance where
  toJSON GetInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ServiceId" Lude..= serviceId),
            Lude.Just ("InstanceId" Lude..= instanceId)
          ]
      )

instance Lude.ToPath GetInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInstanceResponse' smart constructor.
data GetInstanceResponse = GetInstanceResponse'
  { instance' ::
      Lude.Maybe Instance,
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

-- | Creates a value of 'GetInstanceResponse' with the minimum fields required to make a request.
--
-- * 'instance'' - A complex type that contains information about a specified instance.
-- * 'responseStatus' - The response status code.
mkGetInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInstanceResponse
mkGetInstanceResponse pResponseStatus_ =
  GetInstanceResponse'
    { instance' = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A complex type that contains information about a specified instance.
--
-- /Note:/ Consider using 'instance'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsInstance :: Lens.Lens' GetInstanceResponse (Lude.Maybe Instance)
girsInstance = Lens.lens (instance' :: GetInstanceResponse -> Lude.Maybe Instance) (\s a -> s {instance' = a} :: GetInstanceResponse)
{-# DEPRECATED girsInstance "Use generic-lens or generic-optics with 'instance'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsResponseStatus :: Lens.Lens' GetInstanceResponse Lude.Int
girsResponseStatus = Lens.lens (responseStatus :: GetInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInstanceResponse)
{-# DEPRECATED girsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
