{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the network profile details by the network profile ARN.
module Network.AWS.AlexaBusiness.GetNetworkProfile
  ( -- * Creating a request
    GetNetworkProfile (..),
    mkGetNetworkProfile,

    -- ** Request lenses
    gnpNetworkProfileARN,

    -- * Destructuring the response
    GetNetworkProfileResponse (..),
    mkGetNetworkProfileResponse,

    -- ** Response lenses
    gnprsNetworkProfile,
    gnprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetNetworkProfile' smart constructor.
newtype GetNetworkProfile = GetNetworkProfile'
  { networkProfileARN ::
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

-- | Creates a value of 'GetNetworkProfile' with the minimum fields required to make a request.
--
-- * 'networkProfileARN' - The ARN of the network profile associated with a device.
mkGetNetworkProfile ::
  -- | 'networkProfileARN'
  Lude.Text ->
  GetNetworkProfile
mkGetNetworkProfile pNetworkProfileARN_ =
  GetNetworkProfile' {networkProfileARN = pNetworkProfileARN_}

-- | The ARN of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnpNetworkProfileARN :: Lens.Lens' GetNetworkProfile Lude.Text
gnpNetworkProfileARN = Lens.lens (networkProfileARN :: GetNetworkProfile -> Lude.Text) (\s a -> s {networkProfileARN = a} :: GetNetworkProfile)
{-# DEPRECATED gnpNetworkProfileARN "Use generic-lens or generic-optics with 'networkProfileARN' instead." #-}

instance Lude.AWSRequest GetNetworkProfile where
  type Rs GetNetworkProfile = GetNetworkProfileResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetNetworkProfileResponse'
            Lude.<$> (x Lude..?> "NetworkProfile")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetNetworkProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.GetNetworkProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetNetworkProfile where
  toJSON GetNetworkProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("NetworkProfileArn" Lude..= networkProfileARN)]
      )

instance Lude.ToPath GetNetworkProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery GetNetworkProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetNetworkProfileResponse' smart constructor.
data GetNetworkProfileResponse = GetNetworkProfileResponse'
  { networkProfile ::
      Lude.Maybe NetworkProfile,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetNetworkProfileResponse' with the minimum fields required to make a request.
--
-- * 'networkProfile' - The network profile associated with a device.
-- * 'responseStatus' - The response status code.
mkGetNetworkProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetNetworkProfileResponse
mkGetNetworkProfileResponse pResponseStatus_ =
  GetNetworkProfileResponse'
    { networkProfile = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnprsNetworkProfile :: Lens.Lens' GetNetworkProfileResponse (Lude.Maybe NetworkProfile)
gnprsNetworkProfile = Lens.lens (networkProfile :: GetNetworkProfileResponse -> Lude.Maybe NetworkProfile) (\s a -> s {networkProfile = a} :: GetNetworkProfileResponse)
{-# DEPRECATED gnprsNetworkProfile "Use generic-lens or generic-optics with 'networkProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnprsResponseStatus :: Lens.Lens' GetNetworkProfileResponse Lude.Int
gnprsResponseStatus = Lens.lens (responseStatus :: GetNetworkProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetNetworkProfileResponse)
{-# DEPRECATED gnprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
