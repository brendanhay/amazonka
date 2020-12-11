{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.ModifyLoadBalancerAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attributes of the specified Application Load Balancer, Network Load Balancer, or Gateway Load Balancer.
--
-- If any of the specified attributes can't be modified as requested, the call fails. Any existing attributes that you do not modify retain their current values.
module Network.AWS.ELBv2.ModifyLoadBalancerAttributes
  ( -- * Creating a request
    ModifyLoadBalancerAttributes (..),
    mkModifyLoadBalancerAttributes,

    -- ** Request lenses
    mlbaLoadBalancerARN,
    mlbaAttributes,

    -- * Destructuring the response
    ModifyLoadBalancerAttributesResponse (..),
    mkModifyLoadBalancerAttributesResponse,

    -- ** Response lenses
    mlbarsAttributes,
    mlbarsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyLoadBalancerAttributes' smart constructor.
data ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributes'
  { loadBalancerARN ::
      Lude.Text,
    attributes ::
      [LoadBalancerAttribute]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyLoadBalancerAttributes' with the minimum fields required to make a request.
--
-- * 'attributes' - The load balancer attributes.
-- * 'loadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
mkModifyLoadBalancerAttributes ::
  -- | 'loadBalancerARN'
  Lude.Text ->
  ModifyLoadBalancerAttributes
mkModifyLoadBalancerAttributes pLoadBalancerARN_ =
  ModifyLoadBalancerAttributes'
    { loadBalancerARN =
        pLoadBalancerARN_,
      attributes = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbaLoadBalancerARN :: Lens.Lens' ModifyLoadBalancerAttributes Lude.Text
mlbaLoadBalancerARN = Lens.lens (loadBalancerARN :: ModifyLoadBalancerAttributes -> Lude.Text) (\s a -> s {loadBalancerARN = a} :: ModifyLoadBalancerAttributes)
{-# DEPRECATED mlbaLoadBalancerARN "Use generic-lens or generic-optics with 'loadBalancerARN' instead." #-}

-- | The load balancer attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbaAttributes :: Lens.Lens' ModifyLoadBalancerAttributes [LoadBalancerAttribute]
mlbaAttributes = Lens.lens (attributes :: ModifyLoadBalancerAttributes -> [LoadBalancerAttribute]) (\s a -> s {attributes = a} :: ModifyLoadBalancerAttributes)
{-# DEPRECATED mlbaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.AWSRequest ModifyLoadBalancerAttributes where
  type
    Rs ModifyLoadBalancerAttributes =
      ModifyLoadBalancerAttributesResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "ModifyLoadBalancerAttributesResult"
      ( \s h x ->
          ModifyLoadBalancerAttributesResponse'
            Lude.<$> ( x Lude..@? "Attributes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
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
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "LoadBalancerArn" Lude.=: loadBalancerARN,
        "Attributes" Lude.=: Lude.toQueryList "member" attributes
      ]

-- | /See:/ 'mkModifyLoadBalancerAttributesResponse' smart constructor.
data ModifyLoadBalancerAttributesResponse = ModifyLoadBalancerAttributesResponse'
  { attributes ::
      Lude.Maybe
        [LoadBalancerAttribute],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyLoadBalancerAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - Information about the load balancer attributes.
-- * 'responseStatus' - The response status code.
mkModifyLoadBalancerAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyLoadBalancerAttributesResponse
mkModifyLoadBalancerAttributesResponse pResponseStatus_ =
  ModifyLoadBalancerAttributesResponse'
    { attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the load balancer attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbarsAttributes :: Lens.Lens' ModifyLoadBalancerAttributesResponse (Lude.Maybe [LoadBalancerAttribute])
mlbarsAttributes = Lens.lens (attributes :: ModifyLoadBalancerAttributesResponse -> Lude.Maybe [LoadBalancerAttribute]) (\s a -> s {attributes = a} :: ModifyLoadBalancerAttributesResponse)
{-# DEPRECATED mlbarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbarsResponseStatus :: Lens.Lens' ModifyLoadBalancerAttributesResponse Lude.Int
mlbarsResponseStatus = Lens.lens (responseStatus :: ModifyLoadBalancerAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyLoadBalancerAttributesResponse)
{-# DEPRECATED mlbarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
