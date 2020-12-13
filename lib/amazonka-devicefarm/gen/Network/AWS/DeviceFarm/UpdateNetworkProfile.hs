{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.UpdateNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the network profile.
module Network.AWS.DeviceFarm.UpdateNetworkProfile
  ( -- * Creating a request
    UpdateNetworkProfile (..),
    mkUpdateNetworkProfile,

    -- ** Request lenses
    unpUplinkJitterMs,
    unpArn,
    unpUplinkLossPercent,
    unpDownlinkJitterMs,
    unpName,
    unpDownlinkLossPercent,
    unpType,
    unpUplinkDelayMs,
    unpUplinkBandwidthBits,
    unpDescription,
    unpDownlinkDelayMs,
    unpDownlinkBandwidthBits,

    -- * Destructuring the response
    UpdateNetworkProfileResponse (..),
    mkUpdateNetworkProfileResponse,

    -- ** Response lenses
    unprsNetworkProfile,
    unprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateNetworkProfile' smart constructor.
data UpdateNetworkProfile = UpdateNetworkProfile'
  { -- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
    uplinkJitterMs :: Lude.Maybe Lude.Integer,
    -- | The Amazon Resource Name (ARN) of the project for which you want to update network profile settings.
    arn :: Lude.Text,
    -- | Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
    uplinkLossPercent :: Lude.Maybe Lude.Natural,
    -- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
    downlinkJitterMs :: Lude.Maybe Lude.Integer,
    -- | The name of the network profile about which you are returning information.
    name :: Lude.Maybe Lude.Text,
    -- | Proportion of received packets that fail to arrive from 0 to 100 percent.
    downlinkLossPercent :: Lude.Maybe Lude.Natural,
    -- | The type of network profile to return information about. Valid values are listed here.
    type' :: Lude.Maybe NetworkProfileType,
    -- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
    uplinkDelayMs :: Lude.Maybe Lude.Integer,
    -- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
    uplinkBandwidthBits :: Lude.Maybe Lude.Integer,
    -- | The description of the network profile about which you are returning information.
    description :: Lude.Maybe Lude.Text,
    -- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
    downlinkDelayMs :: Lude.Maybe Lude.Integer,
    -- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
    downlinkBandwidthBits :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateNetworkProfile' with the minimum fields required to make a request.
--
-- * 'uplinkJitterMs' - Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
-- * 'arn' - The Amazon Resource Name (ARN) of the project for which you want to update network profile settings.
-- * 'uplinkLossPercent' - Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
-- * 'downlinkJitterMs' - Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
-- * 'name' - The name of the network profile about which you are returning information.
-- * 'downlinkLossPercent' - Proportion of received packets that fail to arrive from 0 to 100 percent.
-- * 'type'' - The type of network profile to return information about. Valid values are listed here.
-- * 'uplinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
-- * 'uplinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to 104857600.
-- * 'description' - The description of the network profile about which you are returning information.
-- * 'downlinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
-- * 'downlinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to 104857600.
mkUpdateNetworkProfile ::
  -- | 'arn'
  Lude.Text ->
  UpdateNetworkProfile
mkUpdateNetworkProfile pArn_ =
  UpdateNetworkProfile'
    { uplinkJitterMs = Lude.Nothing,
      arn = pArn_,
      uplinkLossPercent = Lude.Nothing,
      downlinkJitterMs = Lude.Nothing,
      name = Lude.Nothing,
      downlinkLossPercent = Lude.Nothing,
      type' = Lude.Nothing,
      uplinkDelayMs = Lude.Nothing,
      uplinkBandwidthBits = Lude.Nothing,
      description = Lude.Nothing,
      downlinkDelayMs = Lude.Nothing,
      downlinkBandwidthBits = Lude.Nothing
    }

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'uplinkJitterMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpUplinkJitterMs :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe Lude.Integer)
unpUplinkJitterMs = Lens.lens (uplinkJitterMs :: UpdateNetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {uplinkJitterMs = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpUplinkJitterMs "Use generic-lens or generic-optics with 'uplinkJitterMs' instead." #-}

-- | The Amazon Resource Name (ARN) of the project for which you want to update network profile settings.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpArn :: Lens.Lens' UpdateNetworkProfile Lude.Text
unpArn = Lens.lens (arn :: UpdateNetworkProfile -> Lude.Text) (\s a -> s {arn = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
--
-- /Note:/ Consider using 'uplinkLossPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpUplinkLossPercent :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe Lude.Natural)
unpUplinkLossPercent = Lens.lens (uplinkLossPercent :: UpdateNetworkProfile -> Lude.Maybe Lude.Natural) (\s a -> s {uplinkLossPercent = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpUplinkLossPercent "Use generic-lens or generic-optics with 'uplinkLossPercent' instead." #-}

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'downlinkJitterMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpDownlinkJitterMs :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe Lude.Integer)
unpDownlinkJitterMs = Lens.lens (downlinkJitterMs :: UpdateNetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {downlinkJitterMs = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpDownlinkJitterMs "Use generic-lens or generic-optics with 'downlinkJitterMs' instead." #-}

-- | The name of the network profile about which you are returning information.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpName :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe Lude.Text)
unpName = Lens.lens (name :: UpdateNetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Proportion of received packets that fail to arrive from 0 to 100 percent.
--
-- /Note:/ Consider using 'downlinkLossPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpDownlinkLossPercent :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe Lude.Natural)
unpDownlinkLossPercent = Lens.lens (downlinkLossPercent :: UpdateNetworkProfile -> Lude.Maybe Lude.Natural) (\s a -> s {downlinkLossPercent = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpDownlinkLossPercent "Use generic-lens or generic-optics with 'downlinkLossPercent' instead." #-}

-- | The type of network profile to return information about. Valid values are listed here.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpType :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe NetworkProfileType)
unpType = Lens.lens (type' :: UpdateNetworkProfile -> Lude.Maybe NetworkProfileType) (\s a -> s {type' = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'uplinkDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpUplinkDelayMs :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe Lude.Integer)
unpUplinkDelayMs = Lens.lens (uplinkDelayMs :: UpdateNetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {uplinkDelayMs = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpUplinkDelayMs "Use generic-lens or generic-optics with 'uplinkDelayMs' instead." #-}

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- /Note:/ Consider using 'uplinkBandwidthBits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpUplinkBandwidthBits :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe Lude.Integer)
unpUplinkBandwidthBits = Lens.lens (uplinkBandwidthBits :: UpdateNetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {uplinkBandwidthBits = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpUplinkBandwidthBits "Use generic-lens or generic-optics with 'uplinkBandwidthBits' instead." #-}

-- | The description of the network profile about which you are returning information.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpDescription :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe Lude.Text)
unpDescription = Lens.lens (description :: UpdateNetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'downlinkDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpDownlinkDelayMs :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe Lude.Integer)
unpDownlinkDelayMs = Lens.lens (downlinkDelayMs :: UpdateNetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {downlinkDelayMs = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpDownlinkDelayMs "Use generic-lens or generic-optics with 'downlinkDelayMs' instead." #-}

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- /Note:/ Consider using 'downlinkBandwidthBits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpDownlinkBandwidthBits :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe Lude.Integer)
unpDownlinkBandwidthBits = Lens.lens (downlinkBandwidthBits :: UpdateNetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {downlinkBandwidthBits = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpDownlinkBandwidthBits "Use generic-lens or generic-optics with 'downlinkBandwidthBits' instead." #-}

instance Lude.AWSRequest UpdateNetworkProfile where
  type Rs UpdateNetworkProfile = UpdateNetworkProfileResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateNetworkProfileResponse'
            Lude.<$> (x Lude..?> "networkProfile")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateNetworkProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.UpdateNetworkProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateNetworkProfile where
  toJSON UpdateNetworkProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("uplinkJitterMs" Lude..=) Lude.<$> uplinkJitterMs,
            Lude.Just ("arn" Lude..= arn),
            ("uplinkLossPercent" Lude..=) Lude.<$> uplinkLossPercent,
            ("downlinkJitterMs" Lude..=) Lude.<$> downlinkJitterMs,
            ("name" Lude..=) Lude.<$> name,
            ("downlinkLossPercent" Lude..=) Lude.<$> downlinkLossPercent,
            ("type" Lude..=) Lude.<$> type',
            ("uplinkDelayMs" Lude..=) Lude.<$> uplinkDelayMs,
            ("uplinkBandwidthBits" Lude..=) Lude.<$> uplinkBandwidthBits,
            ("description" Lude..=) Lude.<$> description,
            ("downlinkDelayMs" Lude..=) Lude.<$> downlinkDelayMs,
            ("downlinkBandwidthBits" Lude..=) Lude.<$> downlinkBandwidthBits
          ]
      )

instance Lude.ToPath UpdateNetworkProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateNetworkProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateNetworkProfileResponse' smart constructor.
data UpdateNetworkProfileResponse = UpdateNetworkProfileResponse'
  { -- | A list of the available network profiles.
    networkProfile :: Lude.Maybe NetworkProfile,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateNetworkProfileResponse' with the minimum fields required to make a request.
--
-- * 'networkProfile' - A list of the available network profiles.
-- * 'responseStatus' - The response status code.
mkUpdateNetworkProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateNetworkProfileResponse
mkUpdateNetworkProfileResponse pResponseStatus_ =
  UpdateNetworkProfileResponse'
    { networkProfile = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the available network profiles.
--
-- /Note:/ Consider using 'networkProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unprsNetworkProfile :: Lens.Lens' UpdateNetworkProfileResponse (Lude.Maybe NetworkProfile)
unprsNetworkProfile = Lens.lens (networkProfile :: UpdateNetworkProfileResponse -> Lude.Maybe NetworkProfile) (\s a -> s {networkProfile = a} :: UpdateNetworkProfileResponse)
{-# DEPRECATED unprsNetworkProfile "Use generic-lens or generic-optics with 'networkProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unprsResponseStatus :: Lens.Lens' UpdateNetworkProfileResponse Lude.Int
unprsResponseStatus = Lens.lens (responseStatus :: UpdateNetworkProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateNetworkProfileResponse)
{-# DEPRECATED unprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
