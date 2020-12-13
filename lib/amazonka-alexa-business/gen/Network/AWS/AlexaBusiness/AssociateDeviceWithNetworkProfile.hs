{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.AssociateDeviceWithNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a device with the specified network profile.
module Network.AWS.AlexaBusiness.AssociateDeviceWithNetworkProfile
  ( -- * Creating a request
    AssociateDeviceWithNetworkProfile (..),
    mkAssociateDeviceWithNetworkProfile,

    -- ** Request lenses
    adwnpDeviceARN,
    adwnpNetworkProfileARN,

    -- * Destructuring the response
    AssociateDeviceWithNetworkProfileResponse (..),
    mkAssociateDeviceWithNetworkProfileResponse,

    -- ** Response lenses
    adwnprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateDeviceWithNetworkProfile' smart constructor.
data AssociateDeviceWithNetworkProfile = AssociateDeviceWithNetworkProfile'
  { -- | The device ARN.
    deviceARN :: Lude.Text,
    -- | The ARN of the network profile to associate with a device.
    networkProfileARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateDeviceWithNetworkProfile' with the minimum fields required to make a request.
--
-- * 'deviceARN' - The device ARN.
-- * 'networkProfileARN' - The ARN of the network profile to associate with a device.
mkAssociateDeviceWithNetworkProfile ::
  -- | 'deviceARN'
  Lude.Text ->
  -- | 'networkProfileARN'
  Lude.Text ->
  AssociateDeviceWithNetworkProfile
mkAssociateDeviceWithNetworkProfile pDeviceARN_ pNetworkProfileARN_ =
  AssociateDeviceWithNetworkProfile'
    { deviceARN = pDeviceARN_,
      networkProfileARN = pNetworkProfileARN_
    }

-- | The device ARN.
--
-- /Note:/ Consider using 'deviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adwnpDeviceARN :: Lens.Lens' AssociateDeviceWithNetworkProfile Lude.Text
adwnpDeviceARN = Lens.lens (deviceARN :: AssociateDeviceWithNetworkProfile -> Lude.Text) (\s a -> s {deviceARN = a} :: AssociateDeviceWithNetworkProfile)
{-# DEPRECATED adwnpDeviceARN "Use generic-lens or generic-optics with 'deviceARN' instead." #-}

-- | The ARN of the network profile to associate with a device.
--
-- /Note:/ Consider using 'networkProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adwnpNetworkProfileARN :: Lens.Lens' AssociateDeviceWithNetworkProfile Lude.Text
adwnpNetworkProfileARN = Lens.lens (networkProfileARN :: AssociateDeviceWithNetworkProfile -> Lude.Text) (\s a -> s {networkProfileARN = a} :: AssociateDeviceWithNetworkProfile)
{-# DEPRECATED adwnpNetworkProfileARN "Use generic-lens or generic-optics with 'networkProfileARN' instead." #-}

instance Lude.AWSRequest AssociateDeviceWithNetworkProfile where
  type
    Rs AssociateDeviceWithNetworkProfile =
      AssociateDeviceWithNetworkProfileResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateDeviceWithNetworkProfileResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateDeviceWithNetworkProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AlexaForBusiness.AssociateDeviceWithNetworkProfile" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateDeviceWithNetworkProfile where
  toJSON AssociateDeviceWithNetworkProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DeviceArn" Lude..= deviceARN),
            Lude.Just ("NetworkProfileArn" Lude..= networkProfileARN)
          ]
      )

instance Lude.ToPath AssociateDeviceWithNetworkProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateDeviceWithNetworkProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateDeviceWithNetworkProfileResponse' smart constructor.
newtype AssociateDeviceWithNetworkProfileResponse = AssociateDeviceWithNetworkProfileResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateDeviceWithNetworkProfileResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateDeviceWithNetworkProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateDeviceWithNetworkProfileResponse
mkAssociateDeviceWithNetworkProfileResponse pResponseStatus_ =
  AssociateDeviceWithNetworkProfileResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adwnprsResponseStatus :: Lens.Lens' AssociateDeviceWithNetworkProfileResponse Lude.Int
adwnprsResponseStatus = Lens.lens (responseStatus :: AssociateDeviceWithNetworkProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateDeviceWithNetworkProfileResponse)
{-# DEPRECATED adwnprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
