{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstancePortStates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the firewall port states for a specific Amazon Lightsail instance, the IP addresses allowed to connect to the instance through the ports, and the protocol.
module Network.AWS.Lightsail.GetInstancePortStates
  ( -- * Creating a request
    GetInstancePortStates (..),
    mkGetInstancePortStates,

    -- ** Request lenses
    gipsInstanceName,

    -- * Destructuring the response
    GetInstancePortStatesResponse (..),
    mkGetInstancePortStatesResponse,

    -- ** Response lenses
    gipsrsPortStates,
    gipsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetInstancePortStates' smart constructor.
newtype GetInstancePortStates = GetInstancePortStates'
  { -- | The name of the instance for which to return firewall port states.
    instanceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstancePortStates' with the minimum fields required to make a request.
--
-- * 'instanceName' - The name of the instance for which to return firewall port states.
mkGetInstancePortStates ::
  -- | 'instanceName'
  Lude.Text ->
  GetInstancePortStates
mkGetInstancePortStates pInstanceName_ =
  GetInstancePortStates' {instanceName = pInstanceName_}

-- | The name of the instance for which to return firewall port states.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsInstanceName :: Lens.Lens' GetInstancePortStates Lude.Text
gipsInstanceName = Lens.lens (instanceName :: GetInstancePortStates -> Lude.Text) (\s a -> s {instanceName = a} :: GetInstancePortStates)
{-# DEPRECATED gipsInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest GetInstancePortStates where
  type Rs GetInstancePortStates = GetInstancePortStatesResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInstancePortStatesResponse'
            Lude.<$> (x Lude..?> "portStates" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInstancePortStates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetInstancePortStates" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetInstancePortStates where
  toJSON GetInstancePortStates' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("instanceName" Lude..= instanceName)])

instance Lude.ToPath GetInstancePortStates where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInstancePortStates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInstancePortStatesResponse' smart constructor.
data GetInstancePortStatesResponse = GetInstancePortStatesResponse'
  { -- | An array of objects that describe the firewall port states for the specified instance.
    portStates :: Lude.Maybe [InstancePortState],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstancePortStatesResponse' with the minimum fields required to make a request.
--
-- * 'portStates' - An array of objects that describe the firewall port states for the specified instance.
-- * 'responseStatus' - The response status code.
mkGetInstancePortStatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInstancePortStatesResponse
mkGetInstancePortStatesResponse pResponseStatus_ =
  GetInstancePortStatesResponse'
    { portStates = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the firewall port states for the specified instance.
--
-- /Note:/ Consider using 'portStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsrsPortStates :: Lens.Lens' GetInstancePortStatesResponse (Lude.Maybe [InstancePortState])
gipsrsPortStates = Lens.lens (portStates :: GetInstancePortStatesResponse -> Lude.Maybe [InstancePortState]) (\s a -> s {portStates = a} :: GetInstancePortStatesResponse)
{-# DEPRECATED gipsrsPortStates "Use generic-lens or generic-optics with 'portStates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsrsResponseStatus :: Lens.Lens' GetInstancePortStatesResponse Lude.Int
gipsrsResponseStatus = Lens.lens (responseStatus :: GetInstancePortStatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInstancePortStatesResponse)
{-# DEPRECATED gipsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
