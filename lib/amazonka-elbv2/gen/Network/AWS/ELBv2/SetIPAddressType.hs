{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.SetIPAddressType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the type of IP addresses used by the subnets of the specified Application Load Balancer or Network Load Balancer.
module Network.AWS.ELBv2.SetIPAddressType
  ( -- * Creating a request
    SetIPAddressType (..),
    mkSetIPAddressType,

    -- ** Request lenses
    siatLoadBalancerARN,
    siatIPAddressType,

    -- * Destructuring the response
    SetIPAddressTypeResponse (..),
    mkSetIPAddressTypeResponse,

    -- ** Response lenses
    siatrsIPAddressType,
    siatrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetIPAddressType' smart constructor.
data SetIPAddressType = SetIPAddressType'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerARN :: Lude.Text,
    -- | The IP address type. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@ . You can’t specify @dualstack@ for a load balancer with a UDP or TCP_UDP listener.
    ipAddressType :: IPAddressType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetIPAddressType' with the minimum fields required to make a request.
--
-- * 'loadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
-- * 'ipAddressType' - The IP address type. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@ . You can’t specify @dualstack@ for a load balancer with a UDP or TCP_UDP listener.
mkSetIPAddressType ::
  -- | 'loadBalancerARN'
  Lude.Text ->
  -- | 'ipAddressType'
  IPAddressType ->
  SetIPAddressType
mkSetIPAddressType pLoadBalancerARN_ pIPAddressType_ =
  SetIPAddressType'
    { loadBalancerARN = pLoadBalancerARN_,
      ipAddressType = pIPAddressType_
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siatLoadBalancerARN :: Lens.Lens' SetIPAddressType Lude.Text
siatLoadBalancerARN = Lens.lens (loadBalancerARN :: SetIPAddressType -> Lude.Text) (\s a -> s {loadBalancerARN = a} :: SetIPAddressType)
{-# DEPRECATED siatLoadBalancerARN "Use generic-lens or generic-optics with 'loadBalancerARN' instead." #-}

-- | The IP address type. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@ . You can’t specify @dualstack@ for a load balancer with a UDP or TCP_UDP listener.
--
-- /Note:/ Consider using 'ipAddressType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siatIPAddressType :: Lens.Lens' SetIPAddressType IPAddressType
siatIPAddressType = Lens.lens (ipAddressType :: SetIPAddressType -> IPAddressType) (\s a -> s {ipAddressType = a} :: SetIPAddressType)
{-# DEPRECATED siatIPAddressType "Use generic-lens or generic-optics with 'ipAddressType' instead." #-}

instance Lude.AWSRequest SetIPAddressType where
  type Rs SetIPAddressType = SetIPAddressTypeResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "SetIpAddressTypeResult"
      ( \s h x ->
          SetIPAddressTypeResponse'
            Lude.<$> (x Lude..@? "IpAddressType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetIPAddressType where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetIPAddressType where
  toPath = Lude.const "/"

instance Lude.ToQuery SetIPAddressType where
  toQuery SetIPAddressType' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetIpAddressType" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "LoadBalancerArn" Lude.=: loadBalancerARN,
        "IpAddressType" Lude.=: ipAddressType
      ]

-- | /See:/ 'mkSetIPAddressTypeResponse' smart constructor.
data SetIPAddressTypeResponse = SetIPAddressTypeResponse'
  { -- | The IP address type.
    ipAddressType :: Lude.Maybe IPAddressType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetIPAddressTypeResponse' with the minimum fields required to make a request.
--
-- * 'ipAddressType' - The IP address type.
-- * 'responseStatus' - The response status code.
mkSetIPAddressTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetIPAddressTypeResponse
mkSetIPAddressTypeResponse pResponseStatus_ =
  SetIPAddressTypeResponse'
    { ipAddressType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The IP address type.
--
-- /Note:/ Consider using 'ipAddressType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siatrsIPAddressType :: Lens.Lens' SetIPAddressTypeResponse (Lude.Maybe IPAddressType)
siatrsIPAddressType = Lens.lens (ipAddressType :: SetIPAddressTypeResponse -> Lude.Maybe IPAddressType) (\s a -> s {ipAddressType = a} :: SetIPAddressTypeResponse)
{-# DEPRECATED siatrsIPAddressType "Use generic-lens or generic-optics with 'ipAddressType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siatrsResponseStatus :: Lens.Lens' SetIPAddressTypeResponse Lude.Int
siatrsResponseStatus = Lens.lens (responseStatus :: SetIPAddressTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetIPAddressTypeResponse)
{-# DEPRECATED siatrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
