{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.ModifyLoadBalancerAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the attributes of the specified load balancer.
--
-- You can modify the load balancer attributes, such as @AccessLogs@ , @ConnectionDraining@ , and @CrossZoneLoadBalancing@ by either enabling or disabling them. Or, you can modify the load balancer attribute @ConnectionSettings@ by specifying an idle connection timeout value for your load balancer.
-- For more information, see the following in the /Classic Load Balancers Guide/ :
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-crosszone-lb.html Cross-Zone Load Balancing>
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-conn-drain.html Connection Draining>
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/access-log-collection.html Access Logs>
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-idle-timeout.html Idle Connection Timeout>
module Network.AWS.ELB.ModifyLoadBalancerAttributes
  ( -- * Creating a request
    ModifyLoadBalancerAttributes (..),
    mkModifyLoadBalancerAttributes,

    -- ** Request lenses
    mlbaLoadBalancerName,
    mlbaLoadBalancerAttributes,

    -- * Destructuring the response
    ModifyLoadBalancerAttributesResponse (..),
    mkModifyLoadBalancerAttributesResponse,

    -- ** Response lenses
    mlbarsLoadBalancerName,
    mlbarsLoadBalancerAttributes,
    mlbarsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for ModifyLoadBalancerAttributes.
--
-- /See:/ 'mkModifyLoadBalancerAttributes' smart constructor.
data ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributes'
  { -- | The name of the load balancer.
    loadBalancerName :: Lude.Text,
    -- | The attributes for the load balancer.
    loadBalancerAttributes :: LoadBalancerAttributes
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyLoadBalancerAttributes' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'loadBalancerAttributes' - The attributes for the load balancer.
mkModifyLoadBalancerAttributes ::
  -- | 'loadBalancerName'
  Lude.Text ->
  -- | 'loadBalancerAttributes'
  LoadBalancerAttributes ->
  ModifyLoadBalancerAttributes
mkModifyLoadBalancerAttributes
  pLoadBalancerName_
  pLoadBalancerAttributes_ =
    ModifyLoadBalancerAttributes'
      { loadBalancerName =
          pLoadBalancerName_,
        loadBalancerAttributes = pLoadBalancerAttributes_
      }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbaLoadBalancerName :: Lens.Lens' ModifyLoadBalancerAttributes Lude.Text
mlbaLoadBalancerName = Lens.lens (loadBalancerName :: ModifyLoadBalancerAttributes -> Lude.Text) (\s a -> s {loadBalancerName = a} :: ModifyLoadBalancerAttributes)
{-# DEPRECATED mlbaLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The attributes for the load balancer.
--
-- /Note:/ Consider using 'loadBalancerAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbaLoadBalancerAttributes :: Lens.Lens' ModifyLoadBalancerAttributes LoadBalancerAttributes
mlbaLoadBalancerAttributes = Lens.lens (loadBalancerAttributes :: ModifyLoadBalancerAttributes -> LoadBalancerAttributes) (\s a -> s {loadBalancerAttributes = a} :: ModifyLoadBalancerAttributes)
{-# DEPRECATED mlbaLoadBalancerAttributes "Use generic-lens or generic-optics with 'loadBalancerAttributes' instead." #-}

instance Lude.AWSRequest ModifyLoadBalancerAttributes where
  type
    Rs ModifyLoadBalancerAttributes =
      ModifyLoadBalancerAttributesResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "ModifyLoadBalancerAttributesResult"
      ( \s h x ->
          ModifyLoadBalancerAttributesResponse'
            Lude.<$> (x Lude..@? "LoadBalancerName")
            Lude.<*> (x Lude..@? "LoadBalancerAttributes")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyLoadBalancerAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyLoadBalancerAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyLoadBalancerAttributes where
  toQuery ModifyLoadBalancerAttributes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyLoadBalancerAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName,
        "LoadBalancerAttributes" Lude.=: loadBalancerAttributes
      ]

-- | Contains the output of ModifyLoadBalancerAttributes.
--
-- /See:/ 'mkModifyLoadBalancerAttributesResponse' smart constructor.
data ModifyLoadBalancerAttributesResponse = ModifyLoadBalancerAttributesResponse'
  { -- | The name of the load balancer.
    loadBalancerName :: Lude.Maybe Lude.Text,
    -- | Information about the load balancer attributes.
    loadBalancerAttributes :: Lude.Maybe LoadBalancerAttributes,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyLoadBalancerAttributesResponse' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'loadBalancerAttributes' - Information about the load balancer attributes.
-- * 'responseStatus' - The response status code.
mkModifyLoadBalancerAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyLoadBalancerAttributesResponse
mkModifyLoadBalancerAttributesResponse pResponseStatus_ =
  ModifyLoadBalancerAttributesResponse'
    { loadBalancerName =
        Lude.Nothing,
      loadBalancerAttributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbarsLoadBalancerName :: Lens.Lens' ModifyLoadBalancerAttributesResponse (Lude.Maybe Lude.Text)
mlbarsLoadBalancerName = Lens.lens (loadBalancerName :: ModifyLoadBalancerAttributesResponse -> Lude.Maybe Lude.Text) (\s a -> s {loadBalancerName = a} :: ModifyLoadBalancerAttributesResponse)
{-# DEPRECATED mlbarsLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | Information about the load balancer attributes.
--
-- /Note:/ Consider using 'loadBalancerAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbarsLoadBalancerAttributes :: Lens.Lens' ModifyLoadBalancerAttributesResponse (Lude.Maybe LoadBalancerAttributes)
mlbarsLoadBalancerAttributes = Lens.lens (loadBalancerAttributes :: ModifyLoadBalancerAttributesResponse -> Lude.Maybe LoadBalancerAttributes) (\s a -> s {loadBalancerAttributes = a} :: ModifyLoadBalancerAttributesResponse)
{-# DEPRECATED mlbarsLoadBalancerAttributes "Use generic-lens or generic-optics with 'loadBalancerAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbarsResponseStatus :: Lens.Lens' ModifyLoadBalancerAttributesResponse Lude.Int
mlbarsResponseStatus = Lens.lens (responseStatus :: ModifyLoadBalancerAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyLoadBalancerAttributesResponse)
{-# DEPRECATED mlbarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
