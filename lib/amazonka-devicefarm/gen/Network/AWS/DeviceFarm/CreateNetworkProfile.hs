{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network profile.
module Network.AWS.DeviceFarm.CreateNetworkProfile
  ( -- * Creating a request
    CreateNetworkProfile (..),
    mkCreateNetworkProfile,

    -- ** Request lenses
    cnpUplinkJitterMs,
    cnpUplinkLossPercent,
    cnpDownlinkJitterMs,
    cnpName,
    cnpDownlinkLossPercent,
    cnpProjectARN,
    cnpType,
    cnpUplinkDelayMs,
    cnpUplinkBandwidthBits,
    cnpDescription,
    cnpDownlinkDelayMs,
    cnpDownlinkBandwidthBits,

    -- * Destructuring the response
    CreateNetworkProfileResponse (..),
    mkCreateNetworkProfileResponse,

    -- ** Response lenses
    cnprsNetworkProfile,
    cnprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateNetworkProfile' smart constructor.
data CreateNetworkProfile = CreateNetworkProfile'
  { -- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
    uplinkJitterMs :: Lude.Maybe Lude.Integer,
    -- | Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
    uplinkLossPercent :: Lude.Maybe Lude.Natural,
    -- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
    downlinkJitterMs :: Lude.Maybe Lude.Integer,
    -- | The name for the new network profile.
    name :: Lude.Text,
    -- | Proportion of received packets that fail to arrive from 0 to 100 percent.
    downlinkLossPercent :: Lude.Maybe Lude.Natural,
    -- | The Amazon Resource Name (ARN) of the project for which you want to create a network profile.
    projectARN :: Lude.Text,
    -- | The type of network profile to create. Valid values are listed here.
    type' :: Lude.Maybe NetworkProfileType,
    -- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
    uplinkDelayMs :: Lude.Maybe Lude.Integer,
    -- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
    uplinkBandwidthBits :: Lude.Maybe Lude.Integer,
    -- | The description of the network profile.
    description :: Lude.Maybe Lude.Text,
    -- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
    downlinkDelayMs :: Lude.Maybe Lude.Integer,
    -- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
    downlinkBandwidthBits :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNetworkProfile' with the minimum fields required to make a request.
--
-- * 'uplinkJitterMs' - Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
-- * 'uplinkLossPercent' - Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
-- * 'downlinkJitterMs' - Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
-- * 'name' - The name for the new network profile.
-- * 'downlinkLossPercent' - Proportion of received packets that fail to arrive from 0 to 100 percent.
-- * 'projectARN' - The Amazon Resource Name (ARN) of the project for which you want to create a network profile.
-- * 'type'' - The type of network profile to create. Valid values are listed here.
-- * 'uplinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
-- * 'uplinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to 104857600.
-- * 'description' - The description of the network profile.
-- * 'downlinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
-- * 'downlinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to 104857600.
mkCreateNetworkProfile ::
  -- | 'name'
  Lude.Text ->
  -- | 'projectARN'
  Lude.Text ->
  CreateNetworkProfile
mkCreateNetworkProfile pName_ pProjectARN_ =
  CreateNetworkProfile'
    { uplinkJitterMs = Lude.Nothing,
      uplinkLossPercent = Lude.Nothing,
      downlinkJitterMs = Lude.Nothing,
      name = pName_,
      downlinkLossPercent = Lude.Nothing,
      projectARN = pProjectARN_,
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
cnpUplinkJitterMs :: Lens.Lens' CreateNetworkProfile (Lude.Maybe Lude.Integer)
cnpUplinkJitterMs = Lens.lens (uplinkJitterMs :: CreateNetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {uplinkJitterMs = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpUplinkJitterMs "Use generic-lens or generic-optics with 'uplinkJitterMs' instead." #-}

-- | Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
--
-- /Note:/ Consider using 'uplinkLossPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpUplinkLossPercent :: Lens.Lens' CreateNetworkProfile (Lude.Maybe Lude.Natural)
cnpUplinkLossPercent = Lens.lens (uplinkLossPercent :: CreateNetworkProfile -> Lude.Maybe Lude.Natural) (\s a -> s {uplinkLossPercent = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpUplinkLossPercent "Use generic-lens or generic-optics with 'uplinkLossPercent' instead." #-}

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'downlinkJitterMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDownlinkJitterMs :: Lens.Lens' CreateNetworkProfile (Lude.Maybe Lude.Integer)
cnpDownlinkJitterMs = Lens.lens (downlinkJitterMs :: CreateNetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {downlinkJitterMs = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpDownlinkJitterMs "Use generic-lens or generic-optics with 'downlinkJitterMs' instead." #-}

-- | The name for the new network profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpName :: Lens.Lens' CreateNetworkProfile Lude.Text
cnpName = Lens.lens (name :: CreateNetworkProfile -> Lude.Text) (\s a -> s {name = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Proportion of received packets that fail to arrive from 0 to 100 percent.
--
-- /Note:/ Consider using 'downlinkLossPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDownlinkLossPercent :: Lens.Lens' CreateNetworkProfile (Lude.Maybe Lude.Natural)
cnpDownlinkLossPercent = Lens.lens (downlinkLossPercent :: CreateNetworkProfile -> Lude.Maybe Lude.Natural) (\s a -> s {downlinkLossPercent = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpDownlinkLossPercent "Use generic-lens or generic-optics with 'downlinkLossPercent' instead." #-}

-- | The Amazon Resource Name (ARN) of the project for which you want to create a network profile.
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpProjectARN :: Lens.Lens' CreateNetworkProfile Lude.Text
cnpProjectARN = Lens.lens (projectARN :: CreateNetworkProfile -> Lude.Text) (\s a -> s {projectARN = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

-- | The type of network profile to create. Valid values are listed here.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpType :: Lens.Lens' CreateNetworkProfile (Lude.Maybe NetworkProfileType)
cnpType = Lens.lens (type' :: CreateNetworkProfile -> Lude.Maybe NetworkProfileType) (\s a -> s {type' = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'uplinkDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpUplinkDelayMs :: Lens.Lens' CreateNetworkProfile (Lude.Maybe Lude.Integer)
cnpUplinkDelayMs = Lens.lens (uplinkDelayMs :: CreateNetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {uplinkDelayMs = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpUplinkDelayMs "Use generic-lens or generic-optics with 'uplinkDelayMs' instead." #-}

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- /Note:/ Consider using 'uplinkBandwidthBits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpUplinkBandwidthBits :: Lens.Lens' CreateNetworkProfile (Lude.Maybe Lude.Integer)
cnpUplinkBandwidthBits = Lens.lens (uplinkBandwidthBits :: CreateNetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {uplinkBandwidthBits = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpUplinkBandwidthBits "Use generic-lens or generic-optics with 'uplinkBandwidthBits' instead." #-}

-- | The description of the network profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDescription :: Lens.Lens' CreateNetworkProfile (Lude.Maybe Lude.Text)
cnpDescription = Lens.lens (description :: CreateNetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'downlinkDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDownlinkDelayMs :: Lens.Lens' CreateNetworkProfile (Lude.Maybe Lude.Integer)
cnpDownlinkDelayMs = Lens.lens (downlinkDelayMs :: CreateNetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {downlinkDelayMs = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpDownlinkDelayMs "Use generic-lens or generic-optics with 'downlinkDelayMs' instead." #-}

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- /Note:/ Consider using 'downlinkBandwidthBits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDownlinkBandwidthBits :: Lens.Lens' CreateNetworkProfile (Lude.Maybe Lude.Integer)
cnpDownlinkBandwidthBits = Lens.lens (downlinkBandwidthBits :: CreateNetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {downlinkBandwidthBits = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpDownlinkBandwidthBits "Use generic-lens or generic-optics with 'downlinkBandwidthBits' instead." #-}

instance Lude.AWSRequest CreateNetworkProfile where
  type Rs CreateNetworkProfile = CreateNetworkProfileResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateNetworkProfileResponse'
            Lude.<$> (x Lude..?> "networkProfile")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateNetworkProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.CreateNetworkProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateNetworkProfile where
  toJSON CreateNetworkProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("uplinkJitterMs" Lude..=) Lude.<$> uplinkJitterMs,
            ("uplinkLossPercent" Lude..=) Lude.<$> uplinkLossPercent,
            ("downlinkJitterMs" Lude..=) Lude.<$> downlinkJitterMs,
            Lude.Just ("name" Lude..= name),
            ("downlinkLossPercent" Lude..=) Lude.<$> downlinkLossPercent,
            Lude.Just ("projectArn" Lude..= projectARN),
            ("type" Lude..=) Lude.<$> type',
            ("uplinkDelayMs" Lude..=) Lude.<$> uplinkDelayMs,
            ("uplinkBandwidthBits" Lude..=) Lude.<$> uplinkBandwidthBits,
            ("description" Lude..=) Lude.<$> description,
            ("downlinkDelayMs" Lude..=) Lude.<$> downlinkDelayMs,
            ("downlinkBandwidthBits" Lude..=) Lude.<$> downlinkBandwidthBits
          ]
      )

instance Lude.ToPath CreateNetworkProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateNetworkProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateNetworkProfileResponse' smart constructor.
data CreateNetworkProfileResponse = CreateNetworkProfileResponse'
  { -- | The network profile that is returned by the create network profile request.
    networkProfile :: Lude.Maybe NetworkProfile,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNetworkProfileResponse' with the minimum fields required to make a request.
--
-- * 'networkProfile' - The network profile that is returned by the create network profile request.
-- * 'responseStatus' - The response status code.
mkCreateNetworkProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateNetworkProfileResponse
mkCreateNetworkProfileResponse pResponseStatus_ =
  CreateNetworkProfileResponse'
    { networkProfile = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The network profile that is returned by the create network profile request.
--
-- /Note:/ Consider using 'networkProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnprsNetworkProfile :: Lens.Lens' CreateNetworkProfileResponse (Lude.Maybe NetworkProfile)
cnprsNetworkProfile = Lens.lens (networkProfile :: CreateNetworkProfileResponse -> Lude.Maybe NetworkProfile) (\s a -> s {networkProfile = a} :: CreateNetworkProfileResponse)
{-# DEPRECATED cnprsNetworkProfile "Use generic-lens or generic-optics with 'networkProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnprsResponseStatus :: Lens.Lens' CreateNetworkProfileResponse Lude.Int
cnprsResponseStatus = Lens.lens (responseStatus :: CreateNetworkProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateNetworkProfileResponse)
{-# DEPRECATED cnprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
