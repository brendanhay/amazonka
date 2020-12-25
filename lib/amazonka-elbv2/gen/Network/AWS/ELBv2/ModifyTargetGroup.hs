{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.ModifyTargetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the health checks used when evaluating the health state of the targets in the specified target group.
module Network.AWS.ELBv2.ModifyTargetGroup
  ( -- * Creating a request
    ModifyTargetGroup (..),
    mkModifyTargetGroup,

    -- ** Request lenses
    mtgTargetGroupArn,
    mtgHealthCheckEnabled,
    mtgHealthCheckIntervalSeconds,
    mtgHealthCheckPath,
    mtgHealthCheckPort,
    mtgHealthCheckProtocol,
    mtgHealthCheckTimeoutSeconds,
    mtgHealthyThresholdCount,
    mtgMatcher,
    mtgUnhealthyThresholdCount,

    -- * Destructuring the response
    ModifyTargetGroupResponse (..),
    mkModifyTargetGroupResponse,

    -- ** Response lenses
    mtgrrsTargetGroups,
    mtgrrsResponseStatus,
  )
where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyTargetGroup' smart constructor.
data ModifyTargetGroup = ModifyTargetGroup'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Types.TargetGroupArn,
    -- | Indicates whether health checks are enabled.
    healthCheckEnabled :: Core.Maybe Core.Bool,
    -- | The approximate amount of time, in seconds, between health checks of an individual target. For TCP health checks, the supported values are 10 or 30 seconds.
    --
    -- With Network Load Balancers, you can't modify this setting.
    healthCheckIntervalSeconds :: Core.Maybe Core.Natural,
    -- | [HTTP/HTTPS health checks] The destination for health checks on the targets.
    --
    -- [HTTP1 or HTTP2 protocol version] The ping path. The default is /.
    -- [GRPC protocol version] The path of a custom health check method with the format /package.service/method. The default is /AWS.ALB/healthcheck.
    healthCheckPath :: Core.Maybe Types.Path,
    -- | The port the load balancer uses when performing health checks on targets.
    healthCheckPort :: Core.Maybe Types.HealthCheckPort,
    -- | The protocol the load balancer uses when performing health checks on targets. The TCP protocol is supported for health checks only if the protocol of the target group is TCP, TLS, UDP, or TCP_UDP. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
    --
    -- With Network Load Balancers, you can't modify this setting.
    healthCheckProtocol :: Core.Maybe Types.ProtocolEnum,
    -- | [HTTP/HTTPS health checks] The amount of time, in seconds, during which no response means a failed health check.
    --
    -- With Network Load Balancers, you can't modify this setting.
    healthCheckTimeoutSeconds :: Core.Maybe Core.Natural,
    -- | The number of consecutive health checks successes required before considering an unhealthy target healthy.
    healthyThresholdCount :: Core.Maybe Core.Natural,
    -- | [HTTP/HTTPS health checks] The HTTP or gRPC codes to use when checking for a successful response from a target.
    --
    -- With Network Load Balancers, you can't modify this setting.
    matcher :: Core.Maybe Types.Matcher,
    -- | The number of consecutive health check failures required before considering the target unhealthy. For target groups with a protocol of TCP or TLS, this value must be the same as the healthy threshold count.
    unhealthyThresholdCount :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTargetGroup' value with any optional fields omitted.
mkModifyTargetGroup ::
  -- | 'targetGroupArn'
  Types.TargetGroupArn ->
  ModifyTargetGroup
mkModifyTargetGroup targetGroupArn =
  ModifyTargetGroup'
    { targetGroupArn,
      healthCheckEnabled = Core.Nothing,
      healthCheckIntervalSeconds = Core.Nothing,
      healthCheckPath = Core.Nothing,
      healthCheckPort = Core.Nothing,
      healthCheckProtocol = Core.Nothing,
      healthCheckTimeoutSeconds = Core.Nothing,
      healthyThresholdCount = Core.Nothing,
      matcher = Core.Nothing,
      unhealthyThresholdCount = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgTargetGroupArn :: Lens.Lens' ModifyTargetGroup Types.TargetGroupArn
mtgTargetGroupArn = Lens.field @"targetGroupArn"
{-# DEPRECATED mtgTargetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead." #-}

-- | Indicates whether health checks are enabled.
--
-- /Note:/ Consider using 'healthCheckEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckEnabled :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Bool)
mtgHealthCheckEnabled = Lens.field @"healthCheckEnabled"
{-# DEPRECATED mtgHealthCheckEnabled "Use generic-lens or generic-optics with 'healthCheckEnabled' instead." #-}

-- | The approximate amount of time, in seconds, between health checks of an individual target. For TCP health checks, the supported values are 10 or 30 seconds.
--
-- With Network Load Balancers, you can't modify this setting.
--
-- /Note:/ Consider using 'healthCheckIntervalSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckIntervalSeconds :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Natural)
mtgHealthCheckIntervalSeconds = Lens.field @"healthCheckIntervalSeconds"
{-# DEPRECATED mtgHealthCheckIntervalSeconds "Use generic-lens or generic-optics with 'healthCheckIntervalSeconds' instead." #-}

-- | [HTTP/HTTPS health checks] The destination for health checks on the targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is /.
-- [GRPC protocol version] The path of a custom health check method with the format /package.service/method. The default is /AWS.ALB/healthcheck.
--
-- /Note:/ Consider using 'healthCheckPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckPath :: Lens.Lens' ModifyTargetGroup (Core.Maybe Types.Path)
mtgHealthCheckPath = Lens.field @"healthCheckPath"
{-# DEPRECATED mtgHealthCheckPath "Use generic-lens or generic-optics with 'healthCheckPath' instead." #-}

-- | The port the load balancer uses when performing health checks on targets.
--
-- /Note:/ Consider using 'healthCheckPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckPort :: Lens.Lens' ModifyTargetGroup (Core.Maybe Types.HealthCheckPort)
mtgHealthCheckPort = Lens.field @"healthCheckPort"
{-# DEPRECATED mtgHealthCheckPort "Use generic-lens or generic-optics with 'healthCheckPort' instead." #-}

-- | The protocol the load balancer uses when performing health checks on targets. The TCP protocol is supported for health checks only if the protocol of the target group is TCP, TLS, UDP, or TCP_UDP. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
--
-- With Network Load Balancers, you can't modify this setting.
--
-- /Note:/ Consider using 'healthCheckProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckProtocol :: Lens.Lens' ModifyTargetGroup (Core.Maybe Types.ProtocolEnum)
mtgHealthCheckProtocol = Lens.field @"healthCheckProtocol"
{-# DEPRECATED mtgHealthCheckProtocol "Use generic-lens or generic-optics with 'healthCheckProtocol' instead." #-}

-- | [HTTP/HTTPS health checks] The amount of time, in seconds, during which no response means a failed health check.
--
-- With Network Load Balancers, you can't modify this setting.
--
-- /Note:/ Consider using 'healthCheckTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckTimeoutSeconds :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Natural)
mtgHealthCheckTimeoutSeconds = Lens.field @"healthCheckTimeoutSeconds"
{-# DEPRECATED mtgHealthCheckTimeoutSeconds "Use generic-lens or generic-optics with 'healthCheckTimeoutSeconds' instead." #-}

-- | The number of consecutive health checks successes required before considering an unhealthy target healthy.
--
-- /Note:/ Consider using 'healthyThresholdCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthyThresholdCount :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Natural)
mtgHealthyThresholdCount = Lens.field @"healthyThresholdCount"
{-# DEPRECATED mtgHealthyThresholdCount "Use generic-lens or generic-optics with 'healthyThresholdCount' instead." #-}

-- | [HTTP/HTTPS health checks] The HTTP or gRPC codes to use when checking for a successful response from a target.
--
-- With Network Load Balancers, you can't modify this setting.
--
-- /Note:/ Consider using 'matcher' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgMatcher :: Lens.Lens' ModifyTargetGroup (Core.Maybe Types.Matcher)
mtgMatcher = Lens.field @"matcher"
{-# DEPRECATED mtgMatcher "Use generic-lens or generic-optics with 'matcher' instead." #-}

-- | The number of consecutive health check failures required before considering the target unhealthy. For target groups with a protocol of TCP or TLS, this value must be the same as the healthy threshold count.
--
-- /Note:/ Consider using 'unhealthyThresholdCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgUnhealthyThresholdCount :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Natural)
mtgUnhealthyThresholdCount = Lens.field @"unhealthyThresholdCount"
{-# DEPRECATED mtgUnhealthyThresholdCount "Use generic-lens or generic-optics with 'unhealthyThresholdCount' instead." #-}

instance Core.AWSRequest ModifyTargetGroup where
  type Rs ModifyTargetGroup = ModifyTargetGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyTargetGroup")
                Core.<> (Core.pure ("Version", "2015-12-01"))
                Core.<> (Core.toQueryValue "TargetGroupArn" targetGroupArn)
                Core.<> ( Core.toQueryValue "HealthCheckEnabled"
                            Core.<$> healthCheckEnabled
                        )
                Core.<> ( Core.toQueryValue "HealthCheckIntervalSeconds"
                            Core.<$> healthCheckIntervalSeconds
                        )
                Core.<> (Core.toQueryValue "HealthCheckPath" Core.<$> healthCheckPath)
                Core.<> (Core.toQueryValue "HealthCheckPort" Core.<$> healthCheckPort)
                Core.<> ( Core.toQueryValue "HealthCheckProtocol"
                            Core.<$> healthCheckProtocol
                        )
                Core.<> ( Core.toQueryValue "HealthCheckTimeoutSeconds"
                            Core.<$> healthCheckTimeoutSeconds
                        )
                Core.<> ( Core.toQueryValue "HealthyThresholdCount"
                            Core.<$> healthyThresholdCount
                        )
                Core.<> (Core.toQueryValue "Matcher" Core.<$> matcher)
                Core.<> ( Core.toQueryValue "UnhealthyThresholdCount"
                            Core.<$> unhealthyThresholdCount
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyTargetGroupResult"
      ( \s h x ->
          ModifyTargetGroupResponse'
            Core.<$> (x Core..@? "TargetGroups" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyTargetGroupResponse' smart constructor.
data ModifyTargetGroupResponse = ModifyTargetGroupResponse'
  { -- | Information about the modified target group.
    targetGroups :: Core.Maybe [Types.TargetGroup],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTargetGroupResponse' value with any optional fields omitted.
mkModifyTargetGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyTargetGroupResponse
mkModifyTargetGroupResponse responseStatus =
  ModifyTargetGroupResponse'
    { targetGroups = Core.Nothing,
      responseStatus
    }

-- | Information about the modified target group.
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgrrsTargetGroups :: Lens.Lens' ModifyTargetGroupResponse (Core.Maybe [Types.TargetGroup])
mtgrrsTargetGroups = Lens.field @"targetGroups"
{-# DEPRECATED mtgrrsTargetGroups "Use generic-lens or generic-optics with 'targetGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgrrsResponseStatus :: Lens.Lens' ModifyTargetGroupResponse Core.Int
mtgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mtgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
