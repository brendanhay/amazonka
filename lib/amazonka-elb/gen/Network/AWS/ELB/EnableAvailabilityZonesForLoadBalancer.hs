{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified Availability Zones to the set of Availability Zones for the specified load balancer in EC2-Classic or a default VPC.
--
-- For load balancers in a non-default VPC, use 'AttachLoadBalancerToSubnets' .
-- The load balancer evenly distributes requests across all its registered Availability Zones that contain instances. For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-az.html Add or Remove Availability Zones> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
  ( -- * Creating a request
    EnableAvailabilityZonesForLoadBalancer (..),
    mkEnableAvailabilityZonesForLoadBalancer,

    -- ** Request lenses
    eazflbLoadBalancerName,
    eazflbAvailabilityZones,

    -- * Destructuring the response
    EnableAvailabilityZonesForLoadBalancerResponse (..),
    mkEnableAvailabilityZonesForLoadBalancerResponse,

    -- ** Response lenses
    eazflbrsAvailabilityZones,
    eazflbrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for EnableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'mkEnableAvailabilityZonesForLoadBalancer' smart constructor.
data EnableAvailabilityZonesForLoadBalancer = EnableAvailabilityZonesForLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Lude.Text,
    -- | The Availability Zones. These must be in the same region as the load balancer.
    availabilityZones :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableAvailabilityZonesForLoadBalancer' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'availabilityZones' - The Availability Zones. These must be in the same region as the load balancer.
mkEnableAvailabilityZonesForLoadBalancer ::
  -- | 'loadBalancerName'
  Lude.Text ->
  EnableAvailabilityZonesForLoadBalancer
mkEnableAvailabilityZonesForLoadBalancer pLoadBalancerName_ =
  EnableAvailabilityZonesForLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_,
      availabilityZones = Lude.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eazflbLoadBalancerName :: Lens.Lens' EnableAvailabilityZonesForLoadBalancer Lude.Text
eazflbLoadBalancerName = Lens.lens (loadBalancerName :: EnableAvailabilityZonesForLoadBalancer -> Lude.Text) (\s a -> s {loadBalancerName = a} :: EnableAvailabilityZonesForLoadBalancer)
{-# DEPRECATED eazflbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The Availability Zones. These must be in the same region as the load balancer.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eazflbAvailabilityZones :: Lens.Lens' EnableAvailabilityZonesForLoadBalancer [Lude.Text]
eazflbAvailabilityZones = Lens.lens (availabilityZones :: EnableAvailabilityZonesForLoadBalancer -> [Lude.Text]) (\s a -> s {availabilityZones = a} :: EnableAvailabilityZonesForLoadBalancer)
{-# DEPRECATED eazflbAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

instance Lude.AWSRequest EnableAvailabilityZonesForLoadBalancer where
  type
    Rs EnableAvailabilityZonesForLoadBalancer =
      EnableAvailabilityZonesForLoadBalancerResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "EnableAvailabilityZonesForLoadBalancerResult"
      ( \s h x ->
          EnableAvailabilityZonesForLoadBalancerResponse'
            Lude.<$> ( x Lude..@? "AvailabilityZones" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableAvailabilityZonesForLoadBalancer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EnableAvailabilityZonesForLoadBalancer where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableAvailabilityZonesForLoadBalancer where
  toQuery EnableAvailabilityZonesForLoadBalancer' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("EnableAvailabilityZonesForLoadBalancer" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName,
        "AvailabilityZones"
          Lude.=: Lude.toQueryList "member" availabilityZones
      ]

-- | Contains the output of EnableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'mkEnableAvailabilityZonesForLoadBalancerResponse' smart constructor.
data EnableAvailabilityZonesForLoadBalancerResponse = EnableAvailabilityZonesForLoadBalancerResponse'
  { -- | The updated list of Availability Zones for the load balancer.
    availabilityZones :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableAvailabilityZonesForLoadBalancerResponse' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - The updated list of Availability Zones for the load balancer.
-- * 'responseStatus' - The response status code.
mkEnableAvailabilityZonesForLoadBalancerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableAvailabilityZonesForLoadBalancerResponse
mkEnableAvailabilityZonesForLoadBalancerResponse pResponseStatus_ =
  EnableAvailabilityZonesForLoadBalancerResponse'
    { availabilityZones =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated list of Availability Zones for the load balancer.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eazflbrsAvailabilityZones :: Lens.Lens' EnableAvailabilityZonesForLoadBalancerResponse (Lude.Maybe [Lude.Text])
eazflbrsAvailabilityZones = Lens.lens (availabilityZones :: EnableAvailabilityZonesForLoadBalancerResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: EnableAvailabilityZonesForLoadBalancerResponse)
{-# DEPRECATED eazflbrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eazflbrsResponseStatus :: Lens.Lens' EnableAvailabilityZonesForLoadBalancerResponse Lude.Int
eazflbrsResponseStatus = Lens.lens (responseStatus :: EnableAvailabilityZonesForLoadBalancerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableAvailabilityZonesForLoadBalancerResponse)
{-# DEPRECATED eazflbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
