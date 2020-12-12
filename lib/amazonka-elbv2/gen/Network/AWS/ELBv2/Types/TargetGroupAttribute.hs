{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetGroupAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetGroupAttribute
  ( TargetGroupAttribute (..),

    -- * Smart constructor
    mkTargetGroupAttribute,

    -- * Lenses
    tgaValue,
    tgaKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a target group attribute.
--
-- /See:/ 'mkTargetGroupAttribute' smart constructor.
data TargetGroupAttribute = TargetGroupAttribute'
  { value ::
      Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetGroupAttribute' with the minimum fields required to make a request.
--
-- * 'key' - The name of the attribute.
--
-- The following attribute is supported by all load balancers:
--
--     * @deregistration_delay.timeout_seconds@ - The amount of time, in seconds, for Elastic Load Balancing to wait before changing the state of a deregistering target from @draining@ to @unused@ . The range is 0-3600 seconds. The default value is 300 seconds. If the target is a Lambda function, this attribute is not supported.
--
--
-- The following attributes are supported by both Application Load Balancers and Network Load Balancers:
--
--     * @stickiness.enabled@ - Indicates whether sticky sessions are enabled. The value is @true@ or @false@ . The default is @false@ .
--
--
--     * @stickiness.type@ - The type of sticky sessions. The possible values are @lb_cookie@ for Application Load Balancers or @source_ip@ for Network Load Balancers.
--
--
-- The following attributes are supported only if the load balancer is an Application Load Balancer and the target is an instance or an IP address:
--
--     * @load_balancing.algorithm.type@ - The load balancing algorithm determines how the load balancer selects targets when routing requests. The value is @round_robin@ or @least_outstanding_requests@ . The default is @round_robin@ .
--
--
--     * @slow_start.duration_seconds@ - The time period, in seconds, during which a newly registered target receives an increasing share of the traffic to the target group. After this time period ends, the target receives its full share of traffic. The range is 30-900 seconds (15 minutes). The default is 0 seconds (disabled).
--
--
--     * @stickiness.lb_cookie.duration_seconds@ - The time period, in seconds, during which requests from a client should be routed to the same target. After this time period expires, the load balancer-generated cookie is considered stale. The range is 1 second to 1 week (604800 seconds). The default value is 1 day (86400 seconds).
--
--
-- The following attribute is supported only if the load balancer is an Application Load Balancer and the target is a Lambda function:
--
--     * @lambda.multi_value_headers.enabled@ - Indicates whether the request and response headers that are exchanged between the load balancer and the Lambda function include arrays of values or strings. The value is @true@ or @false@ . The default is @false@ . If the value is @false@ and the request contains a duplicate header field name or query parameter key, the load balancer uses the last value sent by the client.
--
--
-- The following attributes are supported only by Network Load Balancers:
--
--     * @deregistration_delay.connection_termination.enabled@ - Indicates whether the load balancer terminates connections at the end of the deregistration timeout. The value is @true@ or @false@ . The default is @false@ .
--
--
--     * @proxy_protocol_v2.enabled@ - Indicates whether Proxy Protocol version 2 is enabled. The value is @true@ or @false@ . The default is @false@ .
--
--
-- * 'value' - The value of the attribute.
mkTargetGroupAttribute ::
  TargetGroupAttribute
mkTargetGroupAttribute =
  TargetGroupAttribute' {value = Lude.Nothing, key = Lude.Nothing}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaValue :: Lens.Lens' TargetGroupAttribute (Lude.Maybe Lude.Text)
tgaValue = Lens.lens (value :: TargetGroupAttribute -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: TargetGroupAttribute)
{-# DEPRECATED tgaValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the attribute.
--
-- The following attribute is supported by all load balancers:
--
--     * @deregistration_delay.timeout_seconds@ - The amount of time, in seconds, for Elastic Load Balancing to wait before changing the state of a deregistering target from @draining@ to @unused@ . The range is 0-3600 seconds. The default value is 300 seconds. If the target is a Lambda function, this attribute is not supported.
--
--
-- The following attributes are supported by both Application Load Balancers and Network Load Balancers:
--
--     * @stickiness.enabled@ - Indicates whether sticky sessions are enabled. The value is @true@ or @false@ . The default is @false@ .
--
--
--     * @stickiness.type@ - The type of sticky sessions. The possible values are @lb_cookie@ for Application Load Balancers or @source_ip@ for Network Load Balancers.
--
--
-- The following attributes are supported only if the load balancer is an Application Load Balancer and the target is an instance or an IP address:
--
--     * @load_balancing.algorithm.type@ - The load balancing algorithm determines how the load balancer selects targets when routing requests. The value is @round_robin@ or @least_outstanding_requests@ . The default is @round_robin@ .
--
--
--     * @slow_start.duration_seconds@ - The time period, in seconds, during which a newly registered target receives an increasing share of the traffic to the target group. After this time period ends, the target receives its full share of traffic. The range is 30-900 seconds (15 minutes). The default is 0 seconds (disabled).
--
--
--     * @stickiness.lb_cookie.duration_seconds@ - The time period, in seconds, during which requests from a client should be routed to the same target. After this time period expires, the load balancer-generated cookie is considered stale. The range is 1 second to 1 week (604800 seconds). The default value is 1 day (86400 seconds).
--
--
-- The following attribute is supported only if the load balancer is an Application Load Balancer and the target is a Lambda function:
--
--     * @lambda.multi_value_headers.enabled@ - Indicates whether the request and response headers that are exchanged between the load balancer and the Lambda function include arrays of values or strings. The value is @true@ or @false@ . The default is @false@ . If the value is @false@ and the request contains a duplicate header field name or query parameter key, the load balancer uses the last value sent by the client.
--
--
-- The following attributes are supported only by Network Load Balancers:
--
--     * @deregistration_delay.connection_termination.enabled@ - Indicates whether the load balancer terminates connections at the end of the deregistration timeout. The value is @true@ or @false@ . The default is @false@ .
--
--
--     * @proxy_protocol_v2.enabled@ - Indicates whether Proxy Protocol version 2 is enabled. The value is @true@ or @false@ . The default is @false@ .
--
--
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaKey :: Lens.Lens' TargetGroupAttribute (Lude.Maybe Lude.Text)
tgaKey = Lens.lens (key :: TargetGroupAttribute -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: TargetGroupAttribute)
{-# DEPRECATED tgaKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromXML TargetGroupAttribute where
  parseXML x =
    TargetGroupAttribute'
      Lude.<$> (x Lude..@? "Value") Lude.<*> (x Lude..@? "Key")

instance Lude.ToQuery TargetGroupAttribute where
  toQuery TargetGroupAttribute' {..} =
    Lude.mconcat ["Value" Lude.=: value, "Key" Lude.=: key]
