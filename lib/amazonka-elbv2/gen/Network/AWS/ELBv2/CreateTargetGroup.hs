{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateTargetGroup (..),
    mkCreateTargetGroup,

    -- ** Request lenses
    ctgProtocolVersion,
    ctgMatcher,
    ctgHealthCheckPath,
    ctgHealthCheckEnabled,
    ctgUnhealthyThresholdCount,
    ctgVPCId,
    ctgProtocol,
    ctgHealthCheckIntervalSeconds,
    ctgTargetType,
    ctgHealthyThresholdCount,
    ctgHealthCheckProtocol,
    ctgName,
    ctgHealthCheckTimeoutSeconds,
    ctgHealthCheckPort,
    ctgTags,
    ctgPort,

    -- * Destructuring the response
    CreateTargetGroupResponse (..),
    mkCreateTargetGroupResponse,

    -- ** Response lenses
    ctgrsTargetGroups,
    ctgrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTargetGroup' smart constructor.
data CreateTargetGroup = CreateTargetGroup'
  { -- | [HTTP/HTTPS protocol] The protocol version. Specify @GRPC@ to send requests to targets using gRPC. Specify @HTTP2@ to send requests to targets using HTTP/2. The default is @HTTP1@ , which sends requests to targets using HTTP/1.1.
    protocolVersion :: Lude.Maybe Lude.Text,
    -- | [HTTP/HTTPS health checks] The HTTP or gRPC codes to use when checking for a successful response from a target.
    matcher :: Lude.Maybe Matcher,
    -- | [HTTP/HTTPS health checks] The destination for health checks on the targets.
    --
    -- [HTTP1 or HTTP2 protocol version] The ping path. The default is /.
    -- [GRPC protocol version] The path of a custom health check method with the format /package.service/method. The default is /AWS.ALB/healthcheck.
    healthCheckPath :: Lude.Maybe Lude.Text,
    -- | Indicates whether health checks are enabled. If the target type is @lambda@ , health checks are disabled by default but can be enabled. If the target type is @instance@ or @ip@ , health checks are always enabled and cannot be disabled.
    healthCheckEnabled :: Lude.Maybe Lude.Bool,
    -- | The number of consecutive health check failures required before considering a target unhealthy. If the target group protocol is HTTP or HTTPS, the default is 2. If the target group protocol is TCP or TLS, this value must be the same as the healthy threshold count. If the target group protocol is GENEVE, the default is 3. If the target type is @lambda@ , the default is 2.
    unhealthyThresholdCount :: Lude.Maybe Lude.Natural,
    -- | The identifier of the virtual private cloud (VPC). If the target is a Lambda function, this parameter does not apply. Otherwise, this parameter is required.
    vpcId :: Lude.Maybe Lude.Text,
    -- | The protocol to use for routing traffic to the targets. For Application Load Balancers, the supported protocols are HTTP and HTTPS. For Network Load Balancers, the supported protocols are TCP, TLS, UDP, or TCP_UDP. For Gateway Load Balancers, the supported protocol is GENEVE. A TCP_UDP listener must be associated with a TCP_UDP target group. If the target is a Lambda function, this parameter does not apply.
    protocol :: Lude.Maybe ProtocolEnum,
    -- | The approximate amount of time, in seconds, between health checks of an individual target. For TCP health checks, the supported values are 10 and 30 seconds. If the target type is @instance@ or @ip@ , the default is 30 seconds. If the target group protocol is GENEVE, the default is 10 seconds. If the target type is @lambda@ , the default is 35 seconds.
    healthCheckIntervalSeconds :: Lude.Maybe Lude.Natural,
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
    targetType :: Lude.Maybe TargetTypeEnum,
    -- | The number of consecutive health checks successes required before considering an unhealthy target healthy. For target groups with a protocol of HTTP or HTTPS, the default is 5. For target groups with a protocol of TCP, TLS, or GENEVE, the default is 3. If the target type is @lambda@ , the default is 5.
    healthyThresholdCount :: Lude.Maybe Lude.Natural,
    -- | The protocol the load balancer uses when performing health checks on targets. For Application Load Balancers, the default is HTTP. For Network Load Balancers and Gateway Load Balancers, the default is TCP. The TCP protocol is not supported for health checks if the protocol of the target group is HTTP or HTTPS. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
    healthCheckProtocol :: Lude.Maybe ProtocolEnum,
    -- | The name of the target group.
    --
    -- This name must be unique per region per account, can have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, and must not begin or end with a hyphen.
    name :: Lude.Text,
    -- | The amount of time, in seconds, during which no response from a target means a failed health check. For target groups with a protocol of HTTP, HTTPS, or GENEVE, the default is 5 seconds. For target groups with a protocol of TCP or TLS, this value must be 6 seconds for HTTP health checks and 10 seconds for TCP and HTTPS health checks. If the target type is @lambda@ , the default is 30 seconds.
    healthCheckTimeoutSeconds :: Lude.Maybe Lude.Natural,
    -- | The port the load balancer uses when performing health checks on targets. If the protocol is HTTP, HTTPS, TCP, TLS, UDP, or TCP_UDP, the default is @traffic-port@ , which is the port on which each target receives traffic from the load balancer. If the protocol is GENEVE, the default is port 80.
    healthCheckPort :: Lude.Maybe Lude.Text,
    -- | The tags to assign to the target group.
    tags :: Lude.Maybe (Lude.NonEmpty Tag),
    -- | The port on which the targets receive traffic. This port is used unless you specify a port override when registering the target. If the target is a Lambda function, this parameter does not apply. If the protocol is GENEVE, the supported port is 6081.
    port :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTargetGroup' with the minimum fields required to make a request.
--
-- * 'protocolVersion' - [HTTP/HTTPS protocol] The protocol version. Specify @GRPC@ to send requests to targets using gRPC. Specify @HTTP2@ to send requests to targets using HTTP/2. The default is @HTTP1@ , which sends requests to targets using HTTP/1.1.
-- * 'matcher' - [HTTP/HTTPS health checks] The HTTP or gRPC codes to use when checking for a successful response from a target.
-- * 'healthCheckPath' - [HTTP/HTTPS health checks] The destination for health checks on the targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is /.
-- [GRPC protocol version] The path of a custom health check method with the format /package.service/method. The default is /AWS.ALB/healthcheck.
-- * 'healthCheckEnabled' - Indicates whether health checks are enabled. If the target type is @lambda@ , health checks are disabled by default but can be enabled. If the target type is @instance@ or @ip@ , health checks are always enabled and cannot be disabled.
-- * 'unhealthyThresholdCount' - The number of consecutive health check failures required before considering a target unhealthy. If the target group protocol is HTTP or HTTPS, the default is 2. If the target group protocol is TCP or TLS, this value must be the same as the healthy threshold count. If the target group protocol is GENEVE, the default is 3. If the target type is @lambda@ , the default is 2.
-- * 'vpcId' - The identifier of the virtual private cloud (VPC). If the target is a Lambda function, this parameter does not apply. Otherwise, this parameter is required.
-- * 'protocol' - The protocol to use for routing traffic to the targets. For Application Load Balancers, the supported protocols are HTTP and HTTPS. For Network Load Balancers, the supported protocols are TCP, TLS, UDP, or TCP_UDP. For Gateway Load Balancers, the supported protocol is GENEVE. A TCP_UDP listener must be associated with a TCP_UDP target group. If the target is a Lambda function, this parameter does not apply.
-- * 'healthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an individual target. For TCP health checks, the supported values are 10 and 30 seconds. If the target type is @instance@ or @ip@ , the default is 30 seconds. If the target group protocol is GENEVE, the default is 10 seconds. If the target type is @lambda@ , the default is 35 seconds.
-- * 'targetType' - The type of target that you must specify when registering targets with this target group. You can't specify targets for a target group using more than one target type.
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
-- * 'healthyThresholdCount' - The number of consecutive health checks successes required before considering an unhealthy target healthy. For target groups with a protocol of HTTP or HTTPS, the default is 5. For target groups with a protocol of TCP, TLS, or GENEVE, the default is 3. If the target type is @lambda@ , the default is 5.
-- * 'healthCheckProtocol' - The protocol the load balancer uses when performing health checks on targets. For Application Load Balancers, the default is HTTP. For Network Load Balancers and Gateway Load Balancers, the default is TCP. The TCP protocol is not supported for health checks if the protocol of the target group is HTTP or HTTPS. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
-- * 'name' - The name of the target group.
--
-- This name must be unique per region per account, can have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, and must not begin or end with a hyphen.
-- * 'healthCheckTimeoutSeconds' - The amount of time, in seconds, during which no response from a target means a failed health check. For target groups with a protocol of HTTP, HTTPS, or GENEVE, the default is 5 seconds. For target groups with a protocol of TCP or TLS, this value must be 6 seconds for HTTP health checks and 10 seconds for TCP and HTTPS health checks. If the target type is @lambda@ , the default is 30 seconds.
-- * 'healthCheckPort' - The port the load balancer uses when performing health checks on targets. If the protocol is HTTP, HTTPS, TCP, TLS, UDP, or TCP_UDP, the default is @traffic-port@ , which is the port on which each target receives traffic from the load balancer. If the protocol is GENEVE, the default is port 80.
-- * 'tags' - The tags to assign to the target group.
-- * 'port' - The port on which the targets receive traffic. This port is used unless you specify a port override when registering the target. If the target is a Lambda function, this parameter does not apply. If the protocol is GENEVE, the supported port is 6081.
mkCreateTargetGroup ::
  -- | 'name'
  Lude.Text ->
  CreateTargetGroup
mkCreateTargetGroup pName_ =
  CreateTargetGroup'
    { protocolVersion = Lude.Nothing,
      matcher = Lude.Nothing,
      healthCheckPath = Lude.Nothing,
      healthCheckEnabled = Lude.Nothing,
      unhealthyThresholdCount = Lude.Nothing,
      vpcId = Lude.Nothing,
      protocol = Lude.Nothing,
      healthCheckIntervalSeconds = Lude.Nothing,
      targetType = Lude.Nothing,
      healthyThresholdCount = Lude.Nothing,
      healthCheckProtocol = Lude.Nothing,
      name = pName_,
      healthCheckTimeoutSeconds = Lude.Nothing,
      healthCheckPort = Lude.Nothing,
      tags = Lude.Nothing,
      port = Lude.Nothing
    }

-- | [HTTP/HTTPS protocol] The protocol version. Specify @GRPC@ to send requests to targets using gRPC. Specify @HTTP2@ to send requests to targets using HTTP/2. The default is @HTTP1@ , which sends requests to targets using HTTP/1.1.
--
-- /Note:/ Consider using 'protocolVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgProtocolVersion :: Lens.Lens' CreateTargetGroup (Lude.Maybe Lude.Text)
ctgProtocolVersion = Lens.lens (protocolVersion :: CreateTargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {protocolVersion = a} :: CreateTargetGroup)
{-# DEPRECATED ctgProtocolVersion "Use generic-lens or generic-optics with 'protocolVersion' instead." #-}

-- | [HTTP/HTTPS health checks] The HTTP or gRPC codes to use when checking for a successful response from a target.
--
-- /Note:/ Consider using 'matcher' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgMatcher :: Lens.Lens' CreateTargetGroup (Lude.Maybe Matcher)
ctgMatcher = Lens.lens (matcher :: CreateTargetGroup -> Lude.Maybe Matcher) (\s a -> s {matcher = a} :: CreateTargetGroup)
{-# DEPRECATED ctgMatcher "Use generic-lens or generic-optics with 'matcher' instead." #-}

-- | [HTTP/HTTPS health checks] The destination for health checks on the targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is /.
-- [GRPC protocol version] The path of a custom health check method with the format /package.service/method. The default is /AWS.ALB/healthcheck.
--
-- /Note:/ Consider using 'healthCheckPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgHealthCheckPath :: Lens.Lens' CreateTargetGroup (Lude.Maybe Lude.Text)
ctgHealthCheckPath = Lens.lens (healthCheckPath :: CreateTargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {healthCheckPath = a} :: CreateTargetGroup)
{-# DEPRECATED ctgHealthCheckPath "Use generic-lens or generic-optics with 'healthCheckPath' instead." #-}

-- | Indicates whether health checks are enabled. If the target type is @lambda@ , health checks are disabled by default but can be enabled. If the target type is @instance@ or @ip@ , health checks are always enabled and cannot be disabled.
--
-- /Note:/ Consider using 'healthCheckEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgHealthCheckEnabled :: Lens.Lens' CreateTargetGroup (Lude.Maybe Lude.Bool)
ctgHealthCheckEnabled = Lens.lens (healthCheckEnabled :: CreateTargetGroup -> Lude.Maybe Lude.Bool) (\s a -> s {healthCheckEnabled = a} :: CreateTargetGroup)
{-# DEPRECATED ctgHealthCheckEnabled "Use generic-lens or generic-optics with 'healthCheckEnabled' instead." #-}

-- | The number of consecutive health check failures required before considering a target unhealthy. If the target group protocol is HTTP or HTTPS, the default is 2. If the target group protocol is TCP or TLS, this value must be the same as the healthy threshold count. If the target group protocol is GENEVE, the default is 3. If the target type is @lambda@ , the default is 2.
--
-- /Note:/ Consider using 'unhealthyThresholdCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgUnhealthyThresholdCount :: Lens.Lens' CreateTargetGroup (Lude.Maybe Lude.Natural)
ctgUnhealthyThresholdCount = Lens.lens (unhealthyThresholdCount :: CreateTargetGroup -> Lude.Maybe Lude.Natural) (\s a -> s {unhealthyThresholdCount = a} :: CreateTargetGroup)
{-# DEPRECATED ctgUnhealthyThresholdCount "Use generic-lens or generic-optics with 'unhealthyThresholdCount' instead." #-}

-- | The identifier of the virtual private cloud (VPC). If the target is a Lambda function, this parameter does not apply. Otherwise, this parameter is required.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgVPCId :: Lens.Lens' CreateTargetGroup (Lude.Maybe Lude.Text)
ctgVPCId = Lens.lens (vpcId :: CreateTargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: CreateTargetGroup)
{-# DEPRECATED ctgVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The protocol to use for routing traffic to the targets. For Application Load Balancers, the supported protocols are HTTP and HTTPS. For Network Load Balancers, the supported protocols are TCP, TLS, UDP, or TCP_UDP. For Gateway Load Balancers, the supported protocol is GENEVE. A TCP_UDP listener must be associated with a TCP_UDP target group. If the target is a Lambda function, this parameter does not apply.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgProtocol :: Lens.Lens' CreateTargetGroup (Lude.Maybe ProtocolEnum)
ctgProtocol = Lens.lens (protocol :: CreateTargetGroup -> Lude.Maybe ProtocolEnum) (\s a -> s {protocol = a} :: CreateTargetGroup)
{-# DEPRECATED ctgProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The approximate amount of time, in seconds, between health checks of an individual target. For TCP health checks, the supported values are 10 and 30 seconds. If the target type is @instance@ or @ip@ , the default is 30 seconds. If the target group protocol is GENEVE, the default is 10 seconds. If the target type is @lambda@ , the default is 35 seconds.
--
-- /Note:/ Consider using 'healthCheckIntervalSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgHealthCheckIntervalSeconds :: Lens.Lens' CreateTargetGroup (Lude.Maybe Lude.Natural)
ctgHealthCheckIntervalSeconds = Lens.lens (healthCheckIntervalSeconds :: CreateTargetGroup -> Lude.Maybe Lude.Natural) (\s a -> s {healthCheckIntervalSeconds = a} :: CreateTargetGroup)
{-# DEPRECATED ctgHealthCheckIntervalSeconds "Use generic-lens or generic-optics with 'healthCheckIntervalSeconds' instead." #-}

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
ctgTargetType :: Lens.Lens' CreateTargetGroup (Lude.Maybe TargetTypeEnum)
ctgTargetType = Lens.lens (targetType :: CreateTargetGroup -> Lude.Maybe TargetTypeEnum) (\s a -> s {targetType = a} :: CreateTargetGroup)
{-# DEPRECATED ctgTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | The number of consecutive health checks successes required before considering an unhealthy target healthy. For target groups with a protocol of HTTP or HTTPS, the default is 5. For target groups with a protocol of TCP, TLS, or GENEVE, the default is 3. If the target type is @lambda@ , the default is 5.
--
-- /Note:/ Consider using 'healthyThresholdCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgHealthyThresholdCount :: Lens.Lens' CreateTargetGroup (Lude.Maybe Lude.Natural)
ctgHealthyThresholdCount = Lens.lens (healthyThresholdCount :: CreateTargetGroup -> Lude.Maybe Lude.Natural) (\s a -> s {healthyThresholdCount = a} :: CreateTargetGroup)
{-# DEPRECATED ctgHealthyThresholdCount "Use generic-lens or generic-optics with 'healthyThresholdCount' instead." #-}

-- | The protocol the load balancer uses when performing health checks on targets. For Application Load Balancers, the default is HTTP. For Network Load Balancers and Gateway Load Balancers, the default is TCP. The TCP protocol is not supported for health checks if the protocol of the target group is HTTP or HTTPS. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
--
-- /Note:/ Consider using 'healthCheckProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgHealthCheckProtocol :: Lens.Lens' CreateTargetGroup (Lude.Maybe ProtocolEnum)
ctgHealthCheckProtocol = Lens.lens (healthCheckProtocol :: CreateTargetGroup -> Lude.Maybe ProtocolEnum) (\s a -> s {healthCheckProtocol = a} :: CreateTargetGroup)
{-# DEPRECATED ctgHealthCheckProtocol "Use generic-lens or generic-optics with 'healthCheckProtocol' instead." #-}

-- | The name of the target group.
--
-- This name must be unique per region per account, can have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, and must not begin or end with a hyphen.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgName :: Lens.Lens' CreateTargetGroup Lude.Text
ctgName = Lens.lens (name :: CreateTargetGroup -> Lude.Text) (\s a -> s {name = a} :: CreateTargetGroup)
{-# DEPRECATED ctgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The amount of time, in seconds, during which no response from a target means a failed health check. For target groups with a protocol of HTTP, HTTPS, or GENEVE, the default is 5 seconds. For target groups with a protocol of TCP or TLS, this value must be 6 seconds for HTTP health checks and 10 seconds for TCP and HTTPS health checks. If the target type is @lambda@ , the default is 30 seconds.
--
-- /Note:/ Consider using 'healthCheckTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgHealthCheckTimeoutSeconds :: Lens.Lens' CreateTargetGroup (Lude.Maybe Lude.Natural)
ctgHealthCheckTimeoutSeconds = Lens.lens (healthCheckTimeoutSeconds :: CreateTargetGroup -> Lude.Maybe Lude.Natural) (\s a -> s {healthCheckTimeoutSeconds = a} :: CreateTargetGroup)
{-# DEPRECATED ctgHealthCheckTimeoutSeconds "Use generic-lens or generic-optics with 'healthCheckTimeoutSeconds' instead." #-}

-- | The port the load balancer uses when performing health checks on targets. If the protocol is HTTP, HTTPS, TCP, TLS, UDP, or TCP_UDP, the default is @traffic-port@ , which is the port on which each target receives traffic from the load balancer. If the protocol is GENEVE, the default is port 80.
--
-- /Note:/ Consider using 'healthCheckPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgHealthCheckPort :: Lens.Lens' CreateTargetGroup (Lude.Maybe Lude.Text)
ctgHealthCheckPort = Lens.lens (healthCheckPort :: CreateTargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {healthCheckPort = a} :: CreateTargetGroup)
{-# DEPRECATED ctgHealthCheckPort "Use generic-lens or generic-optics with 'healthCheckPort' instead." #-}

-- | The tags to assign to the target group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgTags :: Lens.Lens' CreateTargetGroup (Lude.Maybe (Lude.NonEmpty Tag))
ctgTags = Lens.lens (tags :: CreateTargetGroup -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateTargetGroup)
{-# DEPRECATED ctgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port on which the targets receive traffic. This port is used unless you specify a port override when registering the target. If the target is a Lambda function, this parameter does not apply. If the protocol is GENEVE, the supported port is 6081.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgPort :: Lens.Lens' CreateTargetGroup (Lude.Maybe Lude.Natural)
ctgPort = Lens.lens (port :: CreateTargetGroup -> Lude.Maybe Lude.Natural) (\s a -> s {port = a} :: CreateTargetGroup)
{-# DEPRECATED ctgPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.AWSRequest CreateTargetGroup where
  type Rs CreateTargetGroup = CreateTargetGroupResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "CreateTargetGroupResult"
      ( \s h x ->
          CreateTargetGroupResponse'
            Lude.<$> ( x Lude..@? "TargetGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTargetGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTargetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTargetGroup where
  toQuery CreateTargetGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateTargetGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "ProtocolVersion" Lude.=: protocolVersion,
        "Matcher" Lude.=: matcher,
        "HealthCheckPath" Lude.=: healthCheckPath,
        "HealthCheckEnabled" Lude.=: healthCheckEnabled,
        "UnhealthyThresholdCount" Lude.=: unhealthyThresholdCount,
        "VpcId" Lude.=: vpcId,
        "Protocol" Lude.=: protocol,
        "HealthCheckIntervalSeconds" Lude.=: healthCheckIntervalSeconds,
        "TargetType" Lude.=: targetType,
        "HealthyThresholdCount" Lude.=: healthyThresholdCount,
        "HealthCheckProtocol" Lude.=: healthCheckProtocol,
        "Name" Lude.=: name,
        "HealthCheckTimeoutSeconds" Lude.=: healthCheckTimeoutSeconds,
        "HealthCheckPort" Lude.=: healthCheckPort,
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "Port" Lude.=: port
      ]

-- | /See:/ 'mkCreateTargetGroupResponse' smart constructor.
data CreateTargetGroupResponse = CreateTargetGroupResponse'
  { -- | Information about the target group.
    targetGroups :: Lude.Maybe [TargetGroup],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTargetGroupResponse' with the minimum fields required to make a request.
--
-- * 'targetGroups' - Information about the target group.
-- * 'responseStatus' - The response status code.
mkCreateTargetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTargetGroupResponse
mkCreateTargetGroupResponse pResponseStatus_ =
  CreateTargetGroupResponse'
    { targetGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the target group.
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrsTargetGroups :: Lens.Lens' CreateTargetGroupResponse (Lude.Maybe [TargetGroup])
ctgrsTargetGroups = Lens.lens (targetGroups :: CreateTargetGroupResponse -> Lude.Maybe [TargetGroup]) (\s a -> s {targetGroups = a} :: CreateTargetGroupResponse)
{-# DEPRECATED ctgrsTargetGroups "Use generic-lens or generic-optics with 'targetGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrsResponseStatus :: Lens.Lens' CreateTargetGroupResponse Lude.Int
ctgrsResponseStatus = Lens.lens (responseStatus :: CreateTargetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTargetGroupResponse)
{-# DEPRECATED ctgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
