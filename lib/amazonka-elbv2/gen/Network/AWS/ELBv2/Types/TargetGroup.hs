{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetGroup
  ( TargetGroup (..),

    -- * Smart constructor
    mkTargetGroup,

    -- * Lenses
    tgHealthCheckEnabled,
    tgHealthCheckIntervalSeconds,
    tgHealthCheckPath,
    tgHealthCheckPort,
    tgHealthCheckProtocol,
    tgHealthCheckTimeoutSeconds,
    tgHealthyThresholdCount,
    tgLoadBalancerArns,
    tgMatcher,
    tgPort,
    tgProtocol,
    tgProtocolVersion,
    tgTargetGroupArn,
    tgTargetGroupName,
    tgTargetType,
    tgUnhealthyThresholdCount,
    tgVpcId,
  )
where

import qualified Network.AWS.ELBv2.Types.HealthCheckPort as Types
import qualified Network.AWS.ELBv2.Types.LoadBalancerArn as Types
import qualified Network.AWS.ELBv2.Types.Matcher as Types
import qualified Network.AWS.ELBv2.Types.Path as Types
import qualified Network.AWS.ELBv2.Types.ProtocolEnum as Types
import qualified Network.AWS.ELBv2.Types.ProtocolVersion as Types
import qualified Network.AWS.ELBv2.Types.TargetGroupArn as Types
import qualified Network.AWS.ELBv2.Types.TargetGroupName as Types
import qualified Network.AWS.ELBv2.Types.TargetTypeEnum as Types
import qualified Network.AWS.ELBv2.Types.VpcId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a target group.
--
-- /See:/ 'mkTargetGroup' smart constructor.
data TargetGroup = TargetGroup'
  { -- | Indicates whether health checks are enabled.
    healthCheckEnabled :: Core.Maybe Core.Bool,
    -- | The approximate amount of time, in seconds, between health checks of an individual target.
    healthCheckIntervalSeconds :: Core.Maybe Core.Natural,
    -- | The destination for health checks on the targets.
    healthCheckPath :: Core.Maybe Types.Path,
    -- | The port to use to connect with the target.
    healthCheckPort :: Core.Maybe Types.HealthCheckPort,
    -- | The protocol to use to connect with the target. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
    healthCheckProtocol :: Core.Maybe Types.ProtocolEnum,
    -- | The amount of time, in seconds, during which no response means a failed health check.
    healthCheckTimeoutSeconds :: Core.Maybe Core.Natural,
    -- | The number of consecutive health checks successes required before considering an unhealthy target healthy.
    healthyThresholdCount :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Names (ARN) of the load balancers that route traffic to this target group.
    loadBalancerArns :: Core.Maybe [Types.LoadBalancerArn],
    -- | The HTTP or gRPC codes to use when checking for a successful response from a target.
    matcher :: Core.Maybe Types.Matcher,
    -- | The port on which the targets are listening. Not used if the target is a Lambda function.
    port :: Core.Maybe Core.Natural,
    -- | The protocol to use for routing traffic to the targets.
    protocol :: Core.Maybe Types.ProtocolEnum,
    -- | [HTTP/HTTPS protocol] The protocol version. The possible values are @GRPC@ , @HTTP1@ , and @HTTP2@ .
    protocolVersion :: Core.Maybe Types.ProtocolVersion,
    -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Core.Maybe Types.TargetGroupArn,
    -- | The name of the target group.
    targetGroupName :: Core.Maybe Types.TargetGroupName,
    -- | The type of target that you must specify when registering targets with this target group. The possible values are @instance@ (register targets by instance ID), @ip@ (register targets by IP address), or @lambda@ (register a single Lambda function as a target).
    targetType :: Core.Maybe Types.TargetTypeEnum,
    -- | The number of consecutive health check failures required before considering the target unhealthy.
    unhealthyThresholdCount :: Core.Maybe Core.Natural,
    -- | The ID of the VPC for the targets.
    vpcId :: Core.Maybe Types.VpcId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetGroup' value with any optional fields omitted.
mkTargetGroup ::
  TargetGroup
mkTargetGroup =
  TargetGroup'
    { healthCheckEnabled = Core.Nothing,
      healthCheckIntervalSeconds = Core.Nothing,
      healthCheckPath = Core.Nothing,
      healthCheckPort = Core.Nothing,
      healthCheckProtocol = Core.Nothing,
      healthCheckTimeoutSeconds = Core.Nothing,
      healthyThresholdCount = Core.Nothing,
      loadBalancerArns = Core.Nothing,
      matcher = Core.Nothing,
      port = Core.Nothing,
      protocol = Core.Nothing,
      protocolVersion = Core.Nothing,
      targetGroupArn = Core.Nothing,
      targetGroupName = Core.Nothing,
      targetType = Core.Nothing,
      unhealthyThresholdCount = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | Indicates whether health checks are enabled.
--
-- /Note:/ Consider using 'healthCheckEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgHealthCheckEnabled :: Lens.Lens' TargetGroup (Core.Maybe Core.Bool)
tgHealthCheckEnabled = Lens.field @"healthCheckEnabled"
{-# DEPRECATED tgHealthCheckEnabled "Use generic-lens or generic-optics with 'healthCheckEnabled' instead." #-}

-- | The approximate amount of time, in seconds, between health checks of an individual target.
--
-- /Note:/ Consider using 'healthCheckIntervalSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgHealthCheckIntervalSeconds :: Lens.Lens' TargetGroup (Core.Maybe Core.Natural)
tgHealthCheckIntervalSeconds = Lens.field @"healthCheckIntervalSeconds"
{-# DEPRECATED tgHealthCheckIntervalSeconds "Use generic-lens or generic-optics with 'healthCheckIntervalSeconds' instead." #-}

-- | The destination for health checks on the targets.
--
-- /Note:/ Consider using 'healthCheckPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgHealthCheckPath :: Lens.Lens' TargetGroup (Core.Maybe Types.Path)
tgHealthCheckPath = Lens.field @"healthCheckPath"
{-# DEPRECATED tgHealthCheckPath "Use generic-lens or generic-optics with 'healthCheckPath' instead." #-}

-- | The port to use to connect with the target.
--
-- /Note:/ Consider using 'healthCheckPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgHealthCheckPort :: Lens.Lens' TargetGroup (Core.Maybe Types.HealthCheckPort)
tgHealthCheckPort = Lens.field @"healthCheckPort"
{-# DEPRECATED tgHealthCheckPort "Use generic-lens or generic-optics with 'healthCheckPort' instead." #-}

-- | The protocol to use to connect with the target. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
--
-- /Note:/ Consider using 'healthCheckProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgHealthCheckProtocol :: Lens.Lens' TargetGroup (Core.Maybe Types.ProtocolEnum)
tgHealthCheckProtocol = Lens.field @"healthCheckProtocol"
{-# DEPRECATED tgHealthCheckProtocol "Use generic-lens or generic-optics with 'healthCheckProtocol' instead." #-}

-- | The amount of time, in seconds, during which no response means a failed health check.
--
-- /Note:/ Consider using 'healthCheckTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgHealthCheckTimeoutSeconds :: Lens.Lens' TargetGroup (Core.Maybe Core.Natural)
tgHealthCheckTimeoutSeconds = Lens.field @"healthCheckTimeoutSeconds"
{-# DEPRECATED tgHealthCheckTimeoutSeconds "Use generic-lens or generic-optics with 'healthCheckTimeoutSeconds' instead." #-}

-- | The number of consecutive health checks successes required before considering an unhealthy target healthy.
--
-- /Note:/ Consider using 'healthyThresholdCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgHealthyThresholdCount :: Lens.Lens' TargetGroup (Core.Maybe Core.Natural)
tgHealthyThresholdCount = Lens.field @"healthyThresholdCount"
{-# DEPRECATED tgHealthyThresholdCount "Use generic-lens or generic-optics with 'healthyThresholdCount' instead." #-}

-- | The Amazon Resource Names (ARN) of the load balancers that route traffic to this target group.
--
-- /Note:/ Consider using 'loadBalancerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgLoadBalancerArns :: Lens.Lens' TargetGroup (Core.Maybe [Types.LoadBalancerArn])
tgLoadBalancerArns = Lens.field @"loadBalancerArns"
{-# DEPRECATED tgLoadBalancerArns "Use generic-lens or generic-optics with 'loadBalancerArns' instead." #-}

-- | The HTTP or gRPC codes to use when checking for a successful response from a target.
--
-- /Note:/ Consider using 'matcher' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgMatcher :: Lens.Lens' TargetGroup (Core.Maybe Types.Matcher)
tgMatcher = Lens.field @"matcher"
{-# DEPRECATED tgMatcher "Use generic-lens or generic-optics with 'matcher' instead." #-}

-- | The port on which the targets are listening. Not used if the target is a Lambda function.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgPort :: Lens.Lens' TargetGroup (Core.Maybe Core.Natural)
tgPort = Lens.field @"port"
{-# DEPRECATED tgPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The protocol to use for routing traffic to the targets.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgProtocol :: Lens.Lens' TargetGroup (Core.Maybe Types.ProtocolEnum)
tgProtocol = Lens.field @"protocol"
{-# DEPRECATED tgProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | [HTTP/HTTPS protocol] The protocol version. The possible values are @GRPC@ , @HTTP1@ , and @HTTP2@ .
--
-- /Note:/ Consider using 'protocolVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgProtocolVersion :: Lens.Lens' TargetGroup (Core.Maybe Types.ProtocolVersion)
tgProtocolVersion = Lens.field @"protocolVersion"
{-# DEPRECATED tgProtocolVersion "Use generic-lens or generic-optics with 'protocolVersion' instead." #-}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgTargetGroupArn :: Lens.Lens' TargetGroup (Core.Maybe Types.TargetGroupArn)
tgTargetGroupArn = Lens.field @"targetGroupArn"
{-# DEPRECATED tgTargetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead." #-}

-- | The name of the target group.
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgTargetGroupName :: Lens.Lens' TargetGroup (Core.Maybe Types.TargetGroupName)
tgTargetGroupName = Lens.field @"targetGroupName"
{-# DEPRECATED tgTargetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead." #-}

-- | The type of target that you must specify when registering targets with this target group. The possible values are @instance@ (register targets by instance ID), @ip@ (register targets by IP address), or @lambda@ (register a single Lambda function as a target).
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgTargetType :: Lens.Lens' TargetGroup (Core.Maybe Types.TargetTypeEnum)
tgTargetType = Lens.field @"targetType"
{-# DEPRECATED tgTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | The number of consecutive health check failures required before considering the target unhealthy.
--
-- /Note:/ Consider using 'unhealthyThresholdCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgUnhealthyThresholdCount :: Lens.Lens' TargetGroup (Core.Maybe Core.Natural)
tgUnhealthyThresholdCount = Lens.field @"unhealthyThresholdCount"
{-# DEPRECATED tgUnhealthyThresholdCount "Use generic-lens or generic-optics with 'unhealthyThresholdCount' instead." #-}

-- | The ID of the VPC for the targets.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgVpcId :: Lens.Lens' TargetGroup (Core.Maybe Types.VpcId)
tgVpcId = Lens.field @"vpcId"
{-# DEPRECATED tgVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML TargetGroup where
  parseXML x =
    TargetGroup'
      Core.<$> (x Core..@? "HealthCheckEnabled")
      Core.<*> (x Core..@? "HealthCheckIntervalSeconds")
      Core.<*> (x Core..@? "HealthCheckPath")
      Core.<*> (x Core..@? "HealthCheckPort")
      Core.<*> (x Core..@? "HealthCheckProtocol")
      Core.<*> (x Core..@? "HealthCheckTimeoutSeconds")
      Core.<*> (x Core..@? "HealthyThresholdCount")
      Core.<*> ( x Core..@? "LoadBalancerArns"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "Matcher")
      Core.<*> (x Core..@? "Port")
      Core.<*> (x Core..@? "Protocol")
      Core.<*> (x Core..@? "ProtocolVersion")
      Core.<*> (x Core..@? "TargetGroupArn")
      Core.<*> (x Core..@? "TargetGroupName")
      Core.<*> (x Core..@? "TargetType")
      Core.<*> (x Core..@? "UnhealthyThresholdCount")
      Core.<*> (x Core..@? "VpcId")
