{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.CreateTargetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a target group.
--
-- For more information, see the following:
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-target-groups.html Target groups for your Application Load Balancers> 
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-target-groups.html Target groups for your Network Load Balancers> 
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/target-groups.html Target groups for your Gateway Load Balancers> 
--
--
-- This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple target groups with the same settings, each call succeeds.
module Network.AWS.ELBv2.CreateTargetGroup
    (
    -- * Creating a request
      CreateTargetGroup (..)
    , mkCreateTargetGroup
    -- ** Request lenses
    , ctgName
    , ctgHealthCheckEnabled
    , ctgHealthCheckIntervalSeconds
    , ctgHealthCheckPath
    , ctgHealthCheckPort
    , ctgHealthCheckProtocol
    , ctgHealthCheckTimeoutSeconds
    , ctgHealthyThresholdCount
    , ctgMatcher
    , ctgPort
    , ctgProtocol
    , ctgProtocolVersion
    , ctgTags
    , ctgTargetType
    , ctgUnhealthyThresholdCount
    , ctgVpcId

    -- * Destructuring the response
    , CreateTargetGroupResponse (..)
    , mkCreateTargetGroupResponse
    -- ** Response lenses
    , ctgrrsTargetGroups
    , ctgrrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTargetGroup' smart constructor.
data CreateTargetGroup = CreateTargetGroup'
  { name :: Types.Name
    -- ^ The name of the target group.
--
-- This name must be unique per region per account, can have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, and must not begin or end with a hyphen.
  , healthCheckEnabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether health checks are enabled. If the target type is @lambda@ , health checks are disabled by default but can be enabled. If the target type is @instance@ or @ip@ , health checks are always enabled and cannot be disabled.
  , healthCheckIntervalSeconds :: Core.Maybe Core.Natural
    -- ^ The approximate amount of time, in seconds, between health checks of an individual target. For TCP health checks, the supported values are 10 and 30 seconds. If the target type is @instance@ or @ip@ , the default is 30 seconds. If the target group protocol is GENEVE, the default is 10 seconds. If the target type is @lambda@ , the default is 35 seconds.
  , healthCheckPath :: Core.Maybe Types.HealthCheckPath
    -- ^ [HTTP/HTTPS health checks] The destination for health checks on the targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is /.
-- [GRPC protocol version] The path of a custom health check method with the format /package.service/method. The default is /AWS.ALB/healthcheck.
  , healthCheckPort :: Core.Maybe Types.HealthCheckPort
    -- ^ The port the load balancer uses when performing health checks on targets. If the protocol is HTTP, HTTPS, TCP, TLS, UDP, or TCP_UDP, the default is @traffic-port@ , which is the port on which each target receives traffic from the load balancer. If the protocol is GENEVE, the default is port 80.
  , healthCheckProtocol :: Core.Maybe Types.ProtocolEnum
    -- ^ The protocol the load balancer uses when performing health checks on targets. For Application Load Balancers, the default is HTTP. For Network Load Balancers and Gateway Load Balancers, the default is TCP. The TCP protocol is not supported for health checks if the protocol of the target group is HTTP or HTTPS. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
  , healthCheckTimeoutSeconds :: Core.Maybe Core.Natural
    -- ^ The amount of time, in seconds, during which no response from a target means a failed health check. For target groups with a protocol of HTTP, HTTPS, or GENEVE, the default is 5 seconds. For target groups with a protocol of TCP or TLS, this value must be 6 seconds for HTTP health checks and 10 seconds for TCP and HTTPS health checks. If the target type is @lambda@ , the default is 30 seconds.
  , healthyThresholdCount :: Core.Maybe Core.Natural
    -- ^ The number of consecutive health checks successes required before considering an unhealthy target healthy. For target groups with a protocol of HTTP or HTTPS, the default is 5. For target groups with a protocol of TCP, TLS, or GENEVE, the default is 3. If the target type is @lambda@ , the default is 5.
  , matcher :: Core.Maybe Types.Matcher
    -- ^ [HTTP/HTTPS health checks] The HTTP or gRPC codes to use when checking for a successful response from a target.
  , port :: Core.Maybe Core.Natural
    -- ^ The port on which the targets receive traffic. This port is used unless you specify a port override when registering the target. If the target is a Lambda function, this parameter does not apply. If the protocol is GENEVE, the supported port is 6081.
  , protocol :: Core.Maybe Types.ProtocolEnum
    -- ^ The protocol to use for routing traffic to the targets. For Application Load Balancers, the supported protocols are HTTP and HTTPS. For Network Load Balancers, the supported protocols are TCP, TLS, UDP, or TCP_UDP. For Gateway Load Balancers, the supported protocol is GENEVE. A TCP_UDP listener must be associated with a TCP_UDP target group. If the target is a Lambda function, this parameter does not apply.
  , protocolVersion :: Core.Maybe Types.ProtocolVersion
    -- ^ [HTTP/HTTPS protocol] The protocol version. Specify @GRPC@ to send requests to targets using gRPC. Specify @HTTP2@ to send requests to targets using HTTP/2. The default is @HTTP1@ , which sends requests to targets using HTTP/1.1.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ The tags to assign to the target group.
  , targetType :: Core.Maybe Types.TargetTypeEnum
    -- ^ The type of target that you must specify when registering targets with this target group. You can't specify targets for a target group using more than one target type.
--
--
--     * @instance@ - Register targets by instance ID. This is the default value.
--
--
--     * @ip@ - Register targets by IP address. You can specify IP addresses from the subnets of the virtual private cloud (VPC) for the target group, the RFC 1918 range (10.0.0.0/8, 172.16.0.0/12, and 192.168.0.0/16), and the RFC 6598 range (100.64.0.0/10). You can't specify publicly routable IP addresses.
--
--
--     * @lambda@ - Register a single Lambda function as a target.
--
--
  , unhealthyThresholdCount :: Core.Maybe Core.Natural
    -- ^ The number of consecutive health check failures required before considering a target unhealthy. If the target group protocol is HTTP or HTTPS, the default is 2. If the target group protocol is TCP or TLS, this value must be the same as the healthy threshold count. If the target group protocol is GENEVE, the default is 3. If the target type is @lambda@ , the default is 2.
  , vpcId :: Core.Maybe Types.VpcId
    -- ^ The identifier of the virtual private cloud (VPC). If the target is a Lambda function, this parameter does not apply. Otherwise, this parameter is required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTargetGroup' value with any optional fields omitted.
mkCreateTargetGroup
    :: Types.Name -- ^ 'name'
    -> CreateTargetGroup
mkCreateTargetGroup name
  = CreateTargetGroup'{name, healthCheckEnabled = Core.Nothing,
                       healthCheckIntervalSeconds = Core.Nothing,
                       healthCheckPath = Core.Nothing, healthCheckPort = Core.Nothing,
                       healthCheckProtocol = Core.Nothing,
                       healthCheckTimeoutSeconds = Core.Nothing,
                       healthyThresholdCount = Core.Nothing, matcher = Core.Nothing,
                       port = Core.Nothing, protocol = Core.Nothing,
                       protocolVersion = Core.Nothing, tags = Core.Nothing,
                       targetType = Core.Nothing, unhealthyThresholdCount = Core.Nothing,
                       vpcId = Core.Nothing}

-- | The name of the target group.
--
-- This name must be unique per region per account, can have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, and must not begin or end with a hyphen.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgName :: Lens.Lens' CreateTargetGroup Types.Name
ctgName = Lens.field @"name"
{-# INLINEABLE ctgName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Indicates whether health checks are enabled. If the target type is @lambda@ , health checks are disabled by default but can be enabled. If the target type is @instance@ or @ip@ , health checks are always enabled and cannot be disabled.
--
-- /Note:/ Consider using 'healthCheckEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgHealthCheckEnabled :: Lens.Lens' CreateTargetGroup (Core.Maybe Core.Bool)
ctgHealthCheckEnabled = Lens.field @"healthCheckEnabled"
{-# INLINEABLE ctgHealthCheckEnabled #-}
{-# DEPRECATED healthCheckEnabled "Use generic-lens or generic-optics with 'healthCheckEnabled' instead"  #-}

-- | The approximate amount of time, in seconds, between health checks of an individual target. For TCP health checks, the supported values are 10 and 30 seconds. If the target type is @instance@ or @ip@ , the default is 30 seconds. If the target group protocol is GENEVE, the default is 10 seconds. If the target type is @lambda@ , the default is 35 seconds.
--
-- /Note:/ Consider using 'healthCheckIntervalSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgHealthCheckIntervalSeconds :: Lens.Lens' CreateTargetGroup (Core.Maybe Core.Natural)
ctgHealthCheckIntervalSeconds = Lens.field @"healthCheckIntervalSeconds"
{-# INLINEABLE ctgHealthCheckIntervalSeconds #-}
{-# DEPRECATED healthCheckIntervalSeconds "Use generic-lens or generic-optics with 'healthCheckIntervalSeconds' instead"  #-}

-- | [HTTP/HTTPS health checks] The destination for health checks on the targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is /.
-- [GRPC protocol version] The path of a custom health check method with the format /package.service/method. The default is /AWS.ALB/healthcheck.
--
-- /Note:/ Consider using 'healthCheckPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgHealthCheckPath :: Lens.Lens' CreateTargetGroup (Core.Maybe Types.HealthCheckPath)
ctgHealthCheckPath = Lens.field @"healthCheckPath"
{-# INLINEABLE ctgHealthCheckPath #-}
{-# DEPRECATED healthCheckPath "Use generic-lens or generic-optics with 'healthCheckPath' instead"  #-}

-- | The port the load balancer uses when performing health checks on targets. If the protocol is HTTP, HTTPS, TCP, TLS, UDP, or TCP_UDP, the default is @traffic-port@ , which is the port on which each target receives traffic from the load balancer. If the protocol is GENEVE, the default is port 80.
--
-- /Note:/ Consider using 'healthCheckPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgHealthCheckPort :: Lens.Lens' CreateTargetGroup (Core.Maybe Types.HealthCheckPort)
ctgHealthCheckPort = Lens.field @"healthCheckPort"
{-# INLINEABLE ctgHealthCheckPort #-}
{-# DEPRECATED healthCheckPort "Use generic-lens or generic-optics with 'healthCheckPort' instead"  #-}

-- | The protocol the load balancer uses when performing health checks on targets. For Application Load Balancers, the default is HTTP. For Network Load Balancers and Gateway Load Balancers, the default is TCP. The TCP protocol is not supported for health checks if the protocol of the target group is HTTP or HTTPS. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
--
-- /Note:/ Consider using 'healthCheckProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgHealthCheckProtocol :: Lens.Lens' CreateTargetGroup (Core.Maybe Types.ProtocolEnum)
ctgHealthCheckProtocol = Lens.field @"healthCheckProtocol"
{-# INLINEABLE ctgHealthCheckProtocol #-}
{-# DEPRECATED healthCheckProtocol "Use generic-lens or generic-optics with 'healthCheckProtocol' instead"  #-}

-- | The amount of time, in seconds, during which no response from a target means a failed health check. For target groups with a protocol of HTTP, HTTPS, or GENEVE, the default is 5 seconds. For target groups with a protocol of TCP or TLS, this value must be 6 seconds for HTTP health checks and 10 seconds for TCP and HTTPS health checks. If the target type is @lambda@ , the default is 30 seconds.
--
-- /Note:/ Consider using 'healthCheckTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgHealthCheckTimeoutSeconds :: Lens.Lens' CreateTargetGroup (Core.Maybe Core.Natural)
ctgHealthCheckTimeoutSeconds = Lens.field @"healthCheckTimeoutSeconds"
{-# INLINEABLE ctgHealthCheckTimeoutSeconds #-}
{-# DEPRECATED healthCheckTimeoutSeconds "Use generic-lens or generic-optics with 'healthCheckTimeoutSeconds' instead"  #-}

-- | The number of consecutive health checks successes required before considering an unhealthy target healthy. For target groups with a protocol of HTTP or HTTPS, the default is 5. For target groups with a protocol of TCP, TLS, or GENEVE, the default is 3. If the target type is @lambda@ , the default is 5.
--
-- /Note:/ Consider using 'healthyThresholdCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgHealthyThresholdCount :: Lens.Lens' CreateTargetGroup (Core.Maybe Core.Natural)
ctgHealthyThresholdCount = Lens.field @"healthyThresholdCount"
{-# INLINEABLE ctgHealthyThresholdCount #-}
{-# DEPRECATED healthyThresholdCount "Use generic-lens or generic-optics with 'healthyThresholdCount' instead"  #-}

-- | [HTTP/HTTPS health checks] The HTTP or gRPC codes to use when checking for a successful response from a target.
--
-- /Note:/ Consider using 'matcher' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgMatcher :: Lens.Lens' CreateTargetGroup (Core.Maybe Types.Matcher)
ctgMatcher = Lens.field @"matcher"
{-# INLINEABLE ctgMatcher #-}
{-# DEPRECATED matcher "Use generic-lens or generic-optics with 'matcher' instead"  #-}

-- | The port on which the targets receive traffic. This port is used unless you specify a port override when registering the target. If the target is a Lambda function, this parameter does not apply. If the protocol is GENEVE, the supported port is 6081.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgPort :: Lens.Lens' CreateTargetGroup (Core.Maybe Core.Natural)
ctgPort = Lens.field @"port"
{-# INLINEABLE ctgPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The protocol to use for routing traffic to the targets. For Application Load Balancers, the supported protocols are HTTP and HTTPS. For Network Load Balancers, the supported protocols are TCP, TLS, UDP, or TCP_UDP. For Gateway Load Balancers, the supported protocol is GENEVE. A TCP_UDP listener must be associated with a TCP_UDP target group. If the target is a Lambda function, this parameter does not apply.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgProtocol :: Lens.Lens' CreateTargetGroup (Core.Maybe Types.ProtocolEnum)
ctgProtocol = Lens.field @"protocol"
{-# INLINEABLE ctgProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | [HTTP/HTTPS protocol] The protocol version. Specify @GRPC@ to send requests to targets using gRPC. Specify @HTTP2@ to send requests to targets using HTTP/2. The default is @HTTP1@ , which sends requests to targets using HTTP/1.1.
--
-- /Note:/ Consider using 'protocolVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgProtocolVersion :: Lens.Lens' CreateTargetGroup (Core.Maybe Types.ProtocolVersion)
ctgProtocolVersion = Lens.field @"protocolVersion"
{-# INLINEABLE ctgProtocolVersion #-}
{-# DEPRECATED protocolVersion "Use generic-lens or generic-optics with 'protocolVersion' instead"  #-}

-- | The tags to assign to the target group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgTags :: Lens.Lens' CreateTargetGroup (Core.Maybe (Core.NonEmpty Types.Tag))
ctgTags = Lens.field @"tags"
{-# INLINEABLE ctgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The type of target that you must specify when registering targets with this target group. You can't specify targets for a target group using more than one target type.
--
--
--     * @instance@ - Register targets by instance ID. This is the default value.
--
--
--     * @ip@ - Register targets by IP address. You can specify IP addresses from the subnets of the virtual private cloud (VPC) for the target group, the RFC 1918 range (10.0.0.0/8, 172.16.0.0/12, and 192.168.0.0/16), and the RFC 6598 range (100.64.0.0/10). You can't specify publicly routable IP addresses.
--
--
--     * @lambda@ - Register a single Lambda function as a target.
--
--
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgTargetType :: Lens.Lens' CreateTargetGroup (Core.Maybe Types.TargetTypeEnum)
ctgTargetType = Lens.field @"targetType"
{-# INLINEABLE ctgTargetType #-}
{-# DEPRECATED targetType "Use generic-lens or generic-optics with 'targetType' instead"  #-}

-- | The number of consecutive health check failures required before considering a target unhealthy. If the target group protocol is HTTP or HTTPS, the default is 2. If the target group protocol is TCP or TLS, this value must be the same as the healthy threshold count. If the target group protocol is GENEVE, the default is 3. If the target type is @lambda@ , the default is 2.
--
-- /Note:/ Consider using 'unhealthyThresholdCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgUnhealthyThresholdCount :: Lens.Lens' CreateTargetGroup (Core.Maybe Core.Natural)
ctgUnhealthyThresholdCount = Lens.field @"unhealthyThresholdCount"
{-# INLINEABLE ctgUnhealthyThresholdCount #-}
{-# DEPRECATED unhealthyThresholdCount "Use generic-lens or generic-optics with 'unhealthyThresholdCount' instead"  #-}

-- | The identifier of the virtual private cloud (VPC). If the target is a Lambda function, this parameter does not apply. Otherwise, this parameter is required.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgVpcId :: Lens.Lens' CreateTargetGroup (Core.Maybe Types.VpcId)
ctgVpcId = Lens.field @"vpcId"
{-# INLINEABLE ctgVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.ToQuery CreateTargetGroup where
        toQuery CreateTargetGroup{..}
          = Core.toQueryPair "Action" ("CreateTargetGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "Name" name
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
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Port") port
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Protocol") protocol
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ProtocolVersion")
                protocolVersion
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "member") tags)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TargetType") targetType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UnhealthyThresholdCount")
                unhealthyThresholdCount
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "VpcId") vpcId

instance Core.ToHeaders CreateTargetGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateTargetGroup where
        type Rs CreateTargetGroup = CreateTargetGroupResponse
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
          = Response.receiveXMLWrapper "CreateTargetGroupResult"
              (\ s h x ->
                 CreateTargetGroupResponse' Core.<$>
                   (x Core..@? "TargetGroups" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTargetGroupResponse' smart constructor.
data CreateTargetGroupResponse = CreateTargetGroupResponse'
  { targetGroups :: Core.Maybe [Types.TargetGroup]
    -- ^ Information about the target group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTargetGroupResponse' value with any optional fields omitted.
mkCreateTargetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTargetGroupResponse
mkCreateTargetGroupResponse responseStatus
  = CreateTargetGroupResponse'{targetGroups = Core.Nothing,
                               responseStatus}

-- | Information about the target group.
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsTargetGroups :: Lens.Lens' CreateTargetGroupResponse (Core.Maybe [Types.TargetGroup])
ctgrrsTargetGroups = Lens.field @"targetGroups"
{-# INLINEABLE ctgrrsTargetGroups #-}
{-# DEPRECATED targetGroups "Use generic-lens or generic-optics with 'targetGroups' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsResponseStatus :: Lens.Lens' CreateTargetGroupResponse Core.Int
ctgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
