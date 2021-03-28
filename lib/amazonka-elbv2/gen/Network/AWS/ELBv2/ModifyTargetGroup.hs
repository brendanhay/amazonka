{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyTargetGroup (..)
    , mkModifyTargetGroup
    -- ** Request lenses
    , mtgTargetGroupArn
    , mtgHealthCheckEnabled
    , mtgHealthCheckIntervalSeconds
    , mtgHealthCheckPath
    , mtgHealthCheckPort
    , mtgHealthCheckProtocol
    , mtgHealthCheckTimeoutSeconds
    , mtgHealthyThresholdCount
    , mtgMatcher
    , mtgUnhealthyThresholdCount

    -- * Destructuring the response
    , ModifyTargetGroupResponse (..)
    , mkModifyTargetGroupResponse
    -- ** Response lenses
    , mtgrrsTargetGroups
    , mtgrrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyTargetGroup' smart constructor.
data ModifyTargetGroup = ModifyTargetGroup'
  { targetGroupArn :: Types.TargetGroupArn
    -- ^ The Amazon Resource Name (ARN) of the target group.
  , healthCheckEnabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether health checks are enabled.
  , healthCheckIntervalSeconds :: Core.Maybe Core.Natural
    -- ^ The approximate amount of time, in seconds, between health checks of an individual target. For TCP health checks, the supported values are 10 or 30 seconds.
--
-- With Network Load Balancers, you can't modify this setting.
  , healthCheckPath :: Core.Maybe Types.Path
    -- ^ [HTTP/HTTPS health checks] The destination for health checks on the targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is /.
-- [GRPC protocol version] The path of a custom health check method with the format /package.service/method. The default is /AWS.ALB/healthcheck.
  , healthCheckPort :: Core.Maybe Types.HealthCheckPort
    -- ^ The port the load balancer uses when performing health checks on targets.
  , healthCheckProtocol :: Core.Maybe Types.ProtocolEnum
    -- ^ The protocol the load balancer uses when performing health checks on targets. The TCP protocol is supported for health checks only if the protocol of the target group is TCP, TLS, UDP, or TCP_UDP. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
--
-- With Network Load Balancers, you can't modify this setting.
  , healthCheckTimeoutSeconds :: Core.Maybe Core.Natural
    -- ^ [HTTP/HTTPS health checks] The amount of time, in seconds, during which no response means a failed health check.
--
-- With Network Load Balancers, you can't modify this setting.
  , healthyThresholdCount :: Core.Maybe Core.Natural
    -- ^ The number of consecutive health checks successes required before considering an unhealthy target healthy.
  , matcher :: Core.Maybe Types.Matcher
    -- ^ [HTTP/HTTPS health checks] The HTTP or gRPC codes to use when checking for a successful response from a target.
--
-- With Network Load Balancers, you can't modify this setting.
  , unhealthyThresholdCount :: Core.Maybe Core.Natural
    -- ^ The number of consecutive health check failures required before considering the target unhealthy. For target groups with a protocol of TCP or TLS, this value must be the same as the healthy threshold count.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTargetGroup' value with any optional fields omitted.
mkModifyTargetGroup
    :: Types.TargetGroupArn -- ^ 'targetGroupArn'
    -> ModifyTargetGroup
mkModifyTargetGroup targetGroupArn
  = ModifyTargetGroup'{targetGroupArn,
                       healthCheckEnabled = Core.Nothing,
                       healthCheckIntervalSeconds = Core.Nothing,
                       healthCheckPath = Core.Nothing, healthCheckPort = Core.Nothing,
                       healthCheckProtocol = Core.Nothing,
                       healthCheckTimeoutSeconds = Core.Nothing,
                       healthyThresholdCount = Core.Nothing, matcher = Core.Nothing,
                       unhealthyThresholdCount = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgTargetGroupArn :: Lens.Lens' ModifyTargetGroup Types.TargetGroupArn
mtgTargetGroupArn = Lens.field @"targetGroupArn"
{-# INLINEABLE mtgTargetGroupArn #-}
{-# DEPRECATED targetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead"  #-}

-- | Indicates whether health checks are enabled.
--
-- /Note:/ Consider using 'healthCheckEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckEnabled :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Bool)
mtgHealthCheckEnabled = Lens.field @"healthCheckEnabled"
{-# INLINEABLE mtgHealthCheckEnabled #-}
{-# DEPRECATED healthCheckEnabled "Use generic-lens or generic-optics with 'healthCheckEnabled' instead"  #-}

-- | The approximate amount of time, in seconds, between health checks of an individual target. For TCP health checks, the supported values are 10 or 30 seconds.
--
-- With Network Load Balancers, you can't modify this setting.
--
-- /Note:/ Consider using 'healthCheckIntervalSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckIntervalSeconds :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Natural)
mtgHealthCheckIntervalSeconds = Lens.field @"healthCheckIntervalSeconds"
{-# INLINEABLE mtgHealthCheckIntervalSeconds #-}
{-# DEPRECATED healthCheckIntervalSeconds "Use generic-lens or generic-optics with 'healthCheckIntervalSeconds' instead"  #-}

-- | [HTTP/HTTPS health checks] The destination for health checks on the targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is /.
-- [GRPC protocol version] The path of a custom health check method with the format /package.service/method. The default is /AWS.ALB/healthcheck.
--
-- /Note:/ Consider using 'healthCheckPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckPath :: Lens.Lens' ModifyTargetGroup (Core.Maybe Types.Path)
mtgHealthCheckPath = Lens.field @"healthCheckPath"
{-# INLINEABLE mtgHealthCheckPath #-}
{-# DEPRECATED healthCheckPath "Use generic-lens or generic-optics with 'healthCheckPath' instead"  #-}

-- | The port the load balancer uses when performing health checks on targets.
--
-- /Note:/ Consider using 'healthCheckPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckPort :: Lens.Lens' ModifyTargetGroup (Core.Maybe Types.HealthCheckPort)
mtgHealthCheckPort = Lens.field @"healthCheckPort"
{-# INLINEABLE mtgHealthCheckPort #-}
{-# DEPRECATED healthCheckPort "Use generic-lens or generic-optics with 'healthCheckPort' instead"  #-}

-- | The protocol the load balancer uses when performing health checks on targets. The TCP protocol is supported for health checks only if the protocol of the target group is TCP, TLS, UDP, or TCP_UDP. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
--
-- With Network Load Balancers, you can't modify this setting.
--
-- /Note:/ Consider using 'healthCheckProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckProtocol :: Lens.Lens' ModifyTargetGroup (Core.Maybe Types.ProtocolEnum)
mtgHealthCheckProtocol = Lens.field @"healthCheckProtocol"
{-# INLINEABLE mtgHealthCheckProtocol #-}
{-# DEPRECATED healthCheckProtocol "Use generic-lens or generic-optics with 'healthCheckProtocol' instead"  #-}

-- | [HTTP/HTTPS health checks] The amount of time, in seconds, during which no response means a failed health check.
--
-- With Network Load Balancers, you can't modify this setting.
--
-- /Note:/ Consider using 'healthCheckTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckTimeoutSeconds :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Natural)
mtgHealthCheckTimeoutSeconds = Lens.field @"healthCheckTimeoutSeconds"
{-# INLINEABLE mtgHealthCheckTimeoutSeconds #-}
{-# DEPRECATED healthCheckTimeoutSeconds "Use generic-lens or generic-optics with 'healthCheckTimeoutSeconds' instead"  #-}

-- | The number of consecutive health checks successes required before considering an unhealthy target healthy.
--
-- /Note:/ Consider using 'healthyThresholdCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthyThresholdCount :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Natural)
mtgHealthyThresholdCount = Lens.field @"healthyThresholdCount"
{-# INLINEABLE mtgHealthyThresholdCount #-}
{-# DEPRECATED healthyThresholdCount "Use generic-lens or generic-optics with 'healthyThresholdCount' instead"  #-}

-- | [HTTP/HTTPS health checks] The HTTP or gRPC codes to use when checking for a successful response from a target.
--
-- With Network Load Balancers, you can't modify this setting.
--
-- /Note:/ Consider using 'matcher' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgMatcher :: Lens.Lens' ModifyTargetGroup (Core.Maybe Types.Matcher)
mtgMatcher = Lens.field @"matcher"
{-# INLINEABLE mtgMatcher #-}
{-# DEPRECATED matcher "Use generic-lens or generic-optics with 'matcher' instead"  #-}

-- | The number of consecutive health check failures required before considering the target unhealthy. For target groups with a protocol of TCP or TLS, this value must be the same as the healthy threshold count.
--
-- /Note:/ Consider using 'unhealthyThresholdCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgUnhealthyThresholdCount :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Natural)
mtgUnhealthyThresholdCount = Lens.field @"unhealthyThresholdCount"
{-# INLINEABLE mtgUnhealthyThresholdCount #-}
{-# DEPRECATED unhealthyThresholdCount "Use generic-lens or generic-optics with 'unhealthyThresholdCount' instead"  #-}

instance Core.ToQuery ModifyTargetGroup where
        toQuery ModifyTargetGroup{..}
          = Core.toQueryPair "Action" ("ModifyTargetGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "TargetGroupArn" targetGroupArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HealthCheckEnabled")
                healthCheckEnabled
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "HealthCheckIntervalSeconds")
                healthCheckIntervalSeconds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HealthCheckPath")
                healthCheckPath
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HealthCheckPort")
                healthCheckPort
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HealthCheckProtocol")
                healthCheckProtocol
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "HealthCheckTimeoutSeconds")
                healthCheckTimeoutSeconds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HealthyThresholdCount")
                healthyThresholdCount
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Matcher") matcher
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UnhealthyThresholdCount")
                unhealthyThresholdCount

instance Core.ToHeaders ModifyTargetGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyTargetGroup where
        type Rs ModifyTargetGroup = ModifyTargetGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ModifyTargetGroupResult"
              (\ s h x ->
                 ModifyTargetGroupResponse' Core.<$>
                   (x Core..@? "TargetGroups" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyTargetGroupResponse' smart constructor.
data ModifyTargetGroupResponse = ModifyTargetGroupResponse'
  { targetGroups :: Core.Maybe [Types.TargetGroup]
    -- ^ Information about the modified target group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTargetGroupResponse' value with any optional fields omitted.
mkModifyTargetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyTargetGroupResponse
mkModifyTargetGroupResponse responseStatus
  = ModifyTargetGroupResponse'{targetGroups = Core.Nothing,
                               responseStatus}

-- | Information about the modified target group.
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgrrsTargetGroups :: Lens.Lens' ModifyTargetGroupResponse (Core.Maybe [Types.TargetGroup])
mtgrrsTargetGroups = Lens.field @"targetGroups"
{-# INLINEABLE mtgrrsTargetGroups #-}
{-# DEPRECATED targetGroups "Use generic-lens or generic-optics with 'targetGroups' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgrrsResponseStatus :: Lens.Lens' ModifyTargetGroupResponse Core.Int
mtgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mtgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
