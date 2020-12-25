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
    tgaKey,
    tgaValue,
  )
where

import qualified Network.AWS.ELBv2.Types.Key as Types
import qualified Network.AWS.ELBv2.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a target group attribute.
--
-- /See:/ 'mkTargetGroupAttribute' smart constructor.
data TargetGroupAttribute = TargetGroupAttribute'
  { -- | The name of the attribute.
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
    key :: Core.Maybe Types.Key,
    -- | The value of the attribute.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetGroupAttribute' value with any optional fields omitted.
mkTargetGroupAttribute ::
  TargetGroupAttribute
mkTargetGroupAttribute =
  TargetGroupAttribute' {key = Core.Nothing, value = Core.Nothing}

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
tgaKey :: Lens.Lens' TargetGroupAttribute (Core.Maybe Types.Key)
tgaKey = Lens.field @"key"
{-# DEPRECATED tgaKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaValue :: Lens.Lens' TargetGroupAttribute (Core.Maybe Types.Value)
tgaValue = Lens.field @"value"
{-# DEPRECATED tgaValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromXML TargetGroupAttribute where
  parseXML x =
    TargetGroupAttribute'
      Core.<$> (x Core..@? "Key") Core.<*> (x Core..@? "Value")
