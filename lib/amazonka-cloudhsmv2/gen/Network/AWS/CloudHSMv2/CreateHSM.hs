{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.CreateHSM
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new hardware security module (HSM) in the specified AWS CloudHSM cluster.
module Network.AWS.CloudHSMv2.CreateHSM
  ( -- * Creating a request
    CreateHSM (..),
    mkCreateHSM,

    -- ** Request lenses
    chIPAddress,
    chAvailabilityZone,
    chClusterId,

    -- * Destructuring the response
    CreateHSMResponse (..),
    mkCreateHSMResponse,

    -- ** Response lenses
    chrsHSM,
    chrsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateHSM' smart constructor.
data CreateHSM = CreateHSM'
  { -- | The HSM's IP address. If you specify an IP address, use an available address from the subnet that maps to the Availability Zone where you are creating the HSM. If you don't specify an IP address, one is chosen for you from that subnet.
    ipAddress :: Lude.Maybe Lude.Text,
    -- | The Availability Zone where you are creating the HSM. To find the cluster's Availability Zones, use 'DescribeClusters' .
    availabilityZone :: Lude.Text,
    -- | The identifier (ID) of the HSM's cluster. To find the cluster ID, use 'DescribeClusters' .
    clusterId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHSM' with the minimum fields required to make a request.
--
-- * 'ipAddress' - The HSM's IP address. If you specify an IP address, use an available address from the subnet that maps to the Availability Zone where you are creating the HSM. If you don't specify an IP address, one is chosen for you from that subnet.
-- * 'availabilityZone' - The Availability Zone where you are creating the HSM. To find the cluster's Availability Zones, use 'DescribeClusters' .
-- * 'clusterId' - The identifier (ID) of the HSM's cluster. To find the cluster ID, use 'DescribeClusters' .
mkCreateHSM ::
  -- | 'availabilityZone'
  Lude.Text ->
  -- | 'clusterId'
  Lude.Text ->
  CreateHSM
mkCreateHSM pAvailabilityZone_ pClusterId_ =
  CreateHSM'
    { ipAddress = Lude.Nothing,
      availabilityZone = pAvailabilityZone_,
      clusterId = pClusterId_
    }

-- | The HSM's IP address. If you specify an IP address, use an available address from the subnet that maps to the Availability Zone where you are creating the HSM. If you don't specify an IP address, one is chosen for you from that subnet.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chIPAddress :: Lens.Lens' CreateHSM (Lude.Maybe Lude.Text)
chIPAddress = Lens.lens (ipAddress :: CreateHSM -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: CreateHSM)
{-# DEPRECATED chIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | The Availability Zone where you are creating the HSM. To find the cluster's Availability Zones, use 'DescribeClusters' .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chAvailabilityZone :: Lens.Lens' CreateHSM Lude.Text
chAvailabilityZone = Lens.lens (availabilityZone :: CreateHSM -> Lude.Text) (\s a -> s {availabilityZone = a} :: CreateHSM)
{-# DEPRECATED chAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The identifier (ID) of the HSM's cluster. To find the cluster ID, use 'DescribeClusters' .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chClusterId :: Lens.Lens' CreateHSM Lude.Text
chClusterId = Lens.lens (clusterId :: CreateHSM -> Lude.Text) (\s a -> s {clusterId = a} :: CreateHSM)
{-# DEPRECATED chClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Lude.AWSRequest CreateHSM where
  type Rs CreateHSM = CreateHSMResponse
  request = Req.postJSON cloudHSMv2Service
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateHSMResponse'
            Lude.<$> (x Lude..?> "Hsm") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateHSM where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("BaldrApiService.CreateHsm" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateHSM where
  toJSON CreateHSM' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IpAddress" Lude..=) Lude.<$> ipAddress,
            Lude.Just ("AvailabilityZone" Lude..= availabilityZone),
            Lude.Just ("ClusterId" Lude..= clusterId)
          ]
      )

instance Lude.ToPath CreateHSM where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateHSM where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateHSMResponse' smart constructor.
data CreateHSMResponse = CreateHSMResponse'
  { -- | Information about the HSM that was created.
    hsm :: Lude.Maybe HSM,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHSMResponse' with the minimum fields required to make a request.
--
-- * 'hsm' - Information about the HSM that was created.
-- * 'responseStatus' - The response status code.
mkCreateHSMResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateHSMResponse
mkCreateHSMResponse pResponseStatus_ =
  CreateHSMResponse'
    { hsm = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the HSM that was created.
--
-- /Note:/ Consider using 'hsm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chrsHSM :: Lens.Lens' CreateHSMResponse (Lude.Maybe HSM)
chrsHSM = Lens.lens (hsm :: CreateHSMResponse -> Lude.Maybe HSM) (\s a -> s {hsm = a} :: CreateHSMResponse)
{-# DEPRECATED chrsHSM "Use generic-lens or generic-optics with 'hsm' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chrsResponseStatus :: Lens.Lens' CreateHSMResponse Lude.Int
chrsResponseStatus = Lens.lens (responseStatus :: CreateHSMResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateHSMResponse)
{-# DEPRECATED chrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
