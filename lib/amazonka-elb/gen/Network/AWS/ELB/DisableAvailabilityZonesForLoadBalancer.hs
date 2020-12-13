{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified Availability Zones from the set of Availability Zones for the specified load balancer in EC2-Classic or a default VPC.
--
-- For load balancers in a non-default VPC, use 'DetachLoadBalancerFromSubnets' .
-- There must be at least one Availability Zone registered with a load balancer at all times. After an Availability Zone is removed, all instances registered with the load balancer that are in the removed Availability Zone go into the @OutOfService@ state. Then, the load balancer attempts to equally balance the traffic among its remaining Availability Zones.
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-az.html Add or Remove Availability Zones> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer
  ( -- * Creating a request
    DisableAvailabilityZonesForLoadBalancer (..),
    mkDisableAvailabilityZonesForLoadBalancer,

    -- ** Request lenses
    dazflbLoadBalancerName,
    dazflbAvailabilityZones,

    -- * Destructuring the response
    DisableAvailabilityZonesForLoadBalancerResponse (..),
    mkDisableAvailabilityZonesForLoadBalancerResponse,

    -- ** Response lenses
    dazflbrsAvailabilityZones,
    dazflbrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DisableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'mkDisableAvailabilityZonesForLoadBalancer' smart constructor.
data DisableAvailabilityZonesForLoadBalancer = DisableAvailabilityZonesForLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Lude.Text,
    -- | The Availability Zones.
    availabilityZones :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableAvailabilityZonesForLoadBalancer' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'availabilityZones' - The Availability Zones.
mkDisableAvailabilityZonesForLoadBalancer ::
  -- | 'loadBalancerName'
  Lude.Text ->
  DisableAvailabilityZonesForLoadBalancer
mkDisableAvailabilityZonesForLoadBalancer pLoadBalancerName_ =
  DisableAvailabilityZonesForLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_,
      availabilityZones = Lude.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazflbLoadBalancerName :: Lens.Lens' DisableAvailabilityZonesForLoadBalancer Lude.Text
dazflbLoadBalancerName = Lens.lens (loadBalancerName :: DisableAvailabilityZonesForLoadBalancer -> Lude.Text) (\s a -> s {loadBalancerName = a} :: DisableAvailabilityZonesForLoadBalancer)
{-# DEPRECATED dazflbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The Availability Zones.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazflbAvailabilityZones :: Lens.Lens' DisableAvailabilityZonesForLoadBalancer [Lude.Text]
dazflbAvailabilityZones = Lens.lens (availabilityZones :: DisableAvailabilityZonesForLoadBalancer -> [Lude.Text]) (\s a -> s {availabilityZones = a} :: DisableAvailabilityZonesForLoadBalancer)
{-# DEPRECATED dazflbAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

instance Lude.AWSRequest DisableAvailabilityZonesForLoadBalancer where
  type
    Rs DisableAvailabilityZonesForLoadBalancer =
      DisableAvailabilityZonesForLoadBalancerResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "DisableAvailabilityZonesForLoadBalancerResult"
      ( \s h x ->
          DisableAvailabilityZonesForLoadBalancerResponse'
            Lude.<$> ( x Lude..@? "AvailabilityZones" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableAvailabilityZonesForLoadBalancer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisableAvailabilityZonesForLoadBalancer where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableAvailabilityZonesForLoadBalancer where
  toQuery DisableAvailabilityZonesForLoadBalancer' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DisableAvailabilityZonesForLoadBalancer" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName,
        "AvailabilityZones"
          Lude.=: Lude.toQueryList "member" availabilityZones
      ]

-- | Contains the output for DisableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'mkDisableAvailabilityZonesForLoadBalancerResponse' smart constructor.
data DisableAvailabilityZonesForLoadBalancerResponse = DisableAvailabilityZonesForLoadBalancerResponse'
  { -- | The remaining Availability Zones for the load balancer.
    availabilityZones :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableAvailabilityZonesForLoadBalancerResponse' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - The remaining Availability Zones for the load balancer.
-- * 'responseStatus' - The response status code.
mkDisableAvailabilityZonesForLoadBalancerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableAvailabilityZonesForLoadBalancerResponse
mkDisableAvailabilityZonesForLoadBalancerResponse pResponseStatus_ =
  DisableAvailabilityZonesForLoadBalancerResponse'
    { availabilityZones =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The remaining Availability Zones for the load balancer.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazflbrsAvailabilityZones :: Lens.Lens' DisableAvailabilityZonesForLoadBalancerResponse (Lude.Maybe [Lude.Text])
dazflbrsAvailabilityZones = Lens.lens (availabilityZones :: DisableAvailabilityZonesForLoadBalancerResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: DisableAvailabilityZonesForLoadBalancerResponse)
{-# DEPRECATED dazflbrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazflbrsResponseStatus :: Lens.Lens' DisableAvailabilityZonesForLoadBalancerResponse Lude.Int
dazflbrsResponseStatus = Lens.lens (responseStatus :: DisableAvailabilityZonesForLoadBalancerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableAvailabilityZonesForLoadBalancerResponse)
{-# DEPRECATED dazflbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
