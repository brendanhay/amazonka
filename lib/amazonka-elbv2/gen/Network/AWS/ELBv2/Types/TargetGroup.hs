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
    tgProtocolVersion,
    tgMatcher,
    tgHealthCheckPath,
    tgHealthCheckEnabled,
    tgUnhealthyThresholdCount,
    tgVPCId,
    tgTargetGroupARN,
    tgProtocol,
    tgHealthCheckIntervalSeconds,
    tgTargetType,
    tgHealthyThresholdCount,
    tgHealthCheckProtocol,
    tgLoadBalancerARNs,
    tgHealthCheckTimeoutSeconds,
    tgHealthCheckPort,
    tgTargetGroupName,
    tgPort,
  )
where

import Network.AWS.ELBv2.Types.Matcher
import Network.AWS.ELBv2.Types.ProtocolEnum
import Network.AWS.ELBv2.Types.TargetTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a target group.
--
-- /See:/ 'mkTargetGroup' smart constructor.
data TargetGroup = TargetGroup'
  { protocolVersion ::
      Lude.Maybe Lude.Text,
    matcher :: Lude.Maybe Matcher,
    healthCheckPath :: Lude.Maybe Lude.Text,
    healthCheckEnabled :: Lude.Maybe Lude.Bool,
    unhealthyThresholdCount :: Lude.Maybe Lude.Natural,
    vpcId :: Lude.Maybe Lude.Text,
    targetGroupARN :: Lude.Maybe Lude.Text,
    protocol :: Lude.Maybe ProtocolEnum,
    healthCheckIntervalSeconds :: Lude.Maybe Lude.Natural,
    targetType :: Lude.Maybe TargetTypeEnum,
    healthyThresholdCount :: Lude.Maybe Lude.Natural,
    healthCheckProtocol :: Lude.Maybe ProtocolEnum,
    loadBalancerARNs :: Lude.Maybe [Lude.Text],
    healthCheckTimeoutSeconds :: Lude.Maybe Lude.Natural,
    healthCheckPort :: Lude.Maybe Lude.Text,
    targetGroupName :: Lude.Maybe Lude.Text,
    port :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetGroup' with the minimum fields required to make a request.
--
-- * 'healthCheckEnabled' - Indicates whether health checks are enabled.
-- * 'healthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an individual target.
-- * 'healthCheckPath' - The destination for health checks on the targets.
-- * 'healthCheckPort' - The port to use to connect with the target.
-- * 'healthCheckProtocol' - The protocol to use to connect with the target. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
-- * 'healthCheckTimeoutSeconds' - The amount of time, in seconds, during which no response means a failed health check.
-- * 'healthyThresholdCount' - The number of consecutive health checks successes required before considering an unhealthy target healthy.
-- * 'loadBalancerARNs' - The Amazon Resource Names (ARN) of the load balancers that route traffic to this target group.
-- * 'matcher' - The HTTP or gRPC codes to use when checking for a successful response from a target.
-- * 'port' - The port on which the targets are listening. Not used if the target is a Lambda function.
-- * 'protocol' - The protocol to use for routing traffic to the targets.
-- * 'protocolVersion' - [HTTP/HTTPS protocol] The protocol version. The possible values are @GRPC@ , @HTTP1@ , and @HTTP2@ .
-- * 'targetGroupARN' - The Amazon Resource Name (ARN) of the target group.
-- * 'targetGroupName' - The name of the target group.
-- * 'targetType' - The type of target that you must specify when registering targets with this target group. The possible values are @instance@ (register targets by instance ID), @ip@ (register targets by IP address), or @lambda@ (register a single Lambda function as a target).
-- * 'unhealthyThresholdCount' - The number of consecutive health check failures required before considering the target unhealthy.
-- * 'vpcId' - The ID of the VPC for the targets.
mkTargetGroup ::
  TargetGroup
mkTargetGroup =
  TargetGroup'
    { protocolVersion = Lude.Nothing,
      matcher = Lude.Nothing,
      healthCheckPath = Lude.Nothing,
      healthCheckEnabled = Lude.Nothing,
      unhealthyThresholdCount = Lude.Nothing,
      vpcId = Lude.Nothing,
      targetGroupARN = Lude.Nothing,
      protocol = Lude.Nothing,
      healthCheckIntervalSeconds = Lude.Nothing,
      targetType = Lude.Nothing,
      healthyThresholdCount = Lude.Nothing,
      healthCheckProtocol = Lude.Nothing,
      loadBalancerARNs = Lude.Nothing,
      healthCheckTimeoutSeconds = Lude.Nothing,
      healthCheckPort = Lude.Nothing,
      targetGroupName = Lude.Nothing,
      port = Lude.Nothing
    }

-- | [HTTP/HTTPS protocol] The protocol version. The possible values are @GRPC@ , @HTTP1@ , and @HTTP2@ .
--
-- /Note:/ Consider using 'protocolVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgProtocolVersion :: Lens.Lens' TargetGroup (Lude.Maybe Lude.Text)
tgProtocolVersion = Lens.lens (protocolVersion :: TargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {protocolVersion = a} :: TargetGroup)
{-# DEPRECATED tgProtocolVersion "Use generic-lens or generic-optics with 'protocolVersion' instead." #-}

-- | The HTTP or gRPC codes to use when checking for a successful response from a target.
--
-- /Note:/ Consider using 'matcher' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgMatcher :: Lens.Lens' TargetGroup (Lude.Maybe Matcher)
tgMatcher = Lens.lens (matcher :: TargetGroup -> Lude.Maybe Matcher) (\s a -> s {matcher = a} :: TargetGroup)
{-# DEPRECATED tgMatcher "Use generic-lens or generic-optics with 'matcher' instead." #-}

-- | The destination for health checks on the targets.
--
-- /Note:/ Consider using 'healthCheckPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgHealthCheckPath :: Lens.Lens' TargetGroup (Lude.Maybe Lude.Text)
tgHealthCheckPath = Lens.lens (healthCheckPath :: TargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {healthCheckPath = a} :: TargetGroup)
{-# DEPRECATED tgHealthCheckPath "Use generic-lens or generic-optics with 'healthCheckPath' instead." #-}

-- | Indicates whether health checks are enabled.
--
-- /Note:/ Consider using 'healthCheckEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgHealthCheckEnabled :: Lens.Lens' TargetGroup (Lude.Maybe Lude.Bool)
tgHealthCheckEnabled = Lens.lens (healthCheckEnabled :: TargetGroup -> Lude.Maybe Lude.Bool) (\s a -> s {healthCheckEnabled = a} :: TargetGroup)
{-# DEPRECATED tgHealthCheckEnabled "Use generic-lens or generic-optics with 'healthCheckEnabled' instead." #-}

-- | The number of consecutive health check failures required before considering the target unhealthy.
--
-- /Note:/ Consider using 'unhealthyThresholdCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgUnhealthyThresholdCount :: Lens.Lens' TargetGroup (Lude.Maybe Lude.Natural)
tgUnhealthyThresholdCount = Lens.lens (unhealthyThresholdCount :: TargetGroup -> Lude.Maybe Lude.Natural) (\s a -> s {unhealthyThresholdCount = a} :: TargetGroup)
{-# DEPRECATED tgUnhealthyThresholdCount "Use generic-lens or generic-optics with 'unhealthyThresholdCount' instead." #-}

-- | The ID of the VPC for the targets.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgVPCId :: Lens.Lens' TargetGroup (Lude.Maybe Lude.Text)
tgVPCId = Lens.lens (vpcId :: TargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: TargetGroup)
{-# DEPRECATED tgVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgTargetGroupARN :: Lens.Lens' TargetGroup (Lude.Maybe Lude.Text)
tgTargetGroupARN = Lens.lens (targetGroupARN :: TargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {targetGroupARN = a} :: TargetGroup)
{-# DEPRECATED tgTargetGroupARN "Use generic-lens or generic-optics with 'targetGroupARN' instead." #-}

-- | The protocol to use for routing traffic to the targets.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgProtocol :: Lens.Lens' TargetGroup (Lude.Maybe ProtocolEnum)
tgProtocol = Lens.lens (protocol :: TargetGroup -> Lude.Maybe ProtocolEnum) (\s a -> s {protocol = a} :: TargetGroup)
{-# DEPRECATED tgProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The approximate amount of time, in seconds, between health checks of an individual target.
--
-- /Note:/ Consider using 'healthCheckIntervalSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgHealthCheckIntervalSeconds :: Lens.Lens' TargetGroup (Lude.Maybe Lude.Natural)
tgHealthCheckIntervalSeconds = Lens.lens (healthCheckIntervalSeconds :: TargetGroup -> Lude.Maybe Lude.Natural) (\s a -> s {healthCheckIntervalSeconds = a} :: TargetGroup)
{-# DEPRECATED tgHealthCheckIntervalSeconds "Use generic-lens or generic-optics with 'healthCheckIntervalSeconds' instead." #-}

-- | The type of target that you must specify when registering targets with this target group. The possible values are @instance@ (register targets by instance ID), @ip@ (register targets by IP address), or @lambda@ (register a single Lambda function as a target).
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgTargetType :: Lens.Lens' TargetGroup (Lude.Maybe TargetTypeEnum)
tgTargetType = Lens.lens (targetType :: TargetGroup -> Lude.Maybe TargetTypeEnum) (\s a -> s {targetType = a} :: TargetGroup)
{-# DEPRECATED tgTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | The number of consecutive health checks successes required before considering an unhealthy target healthy.
--
-- /Note:/ Consider using 'healthyThresholdCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgHealthyThresholdCount :: Lens.Lens' TargetGroup (Lude.Maybe Lude.Natural)
tgHealthyThresholdCount = Lens.lens (healthyThresholdCount :: TargetGroup -> Lude.Maybe Lude.Natural) (\s a -> s {healthyThresholdCount = a} :: TargetGroup)
{-# DEPRECATED tgHealthyThresholdCount "Use generic-lens or generic-optics with 'healthyThresholdCount' instead." #-}

-- | The protocol to use to connect with the target. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
--
-- /Note:/ Consider using 'healthCheckProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgHealthCheckProtocol :: Lens.Lens' TargetGroup (Lude.Maybe ProtocolEnum)
tgHealthCheckProtocol = Lens.lens (healthCheckProtocol :: TargetGroup -> Lude.Maybe ProtocolEnum) (\s a -> s {healthCheckProtocol = a} :: TargetGroup)
{-# DEPRECATED tgHealthCheckProtocol "Use generic-lens or generic-optics with 'healthCheckProtocol' instead." #-}

-- | The Amazon Resource Names (ARN) of the load balancers that route traffic to this target group.
--
-- /Note:/ Consider using 'loadBalancerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgLoadBalancerARNs :: Lens.Lens' TargetGroup (Lude.Maybe [Lude.Text])
tgLoadBalancerARNs = Lens.lens (loadBalancerARNs :: TargetGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {loadBalancerARNs = a} :: TargetGroup)
{-# DEPRECATED tgLoadBalancerARNs "Use generic-lens or generic-optics with 'loadBalancerARNs' instead." #-}

-- | The amount of time, in seconds, during which no response means a failed health check.
--
-- /Note:/ Consider using 'healthCheckTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgHealthCheckTimeoutSeconds :: Lens.Lens' TargetGroup (Lude.Maybe Lude.Natural)
tgHealthCheckTimeoutSeconds = Lens.lens (healthCheckTimeoutSeconds :: TargetGroup -> Lude.Maybe Lude.Natural) (\s a -> s {healthCheckTimeoutSeconds = a} :: TargetGroup)
{-# DEPRECATED tgHealthCheckTimeoutSeconds "Use generic-lens or generic-optics with 'healthCheckTimeoutSeconds' instead." #-}

-- | The port to use to connect with the target.
--
-- /Note:/ Consider using 'healthCheckPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgHealthCheckPort :: Lens.Lens' TargetGroup (Lude.Maybe Lude.Text)
tgHealthCheckPort = Lens.lens (healthCheckPort :: TargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {healthCheckPort = a} :: TargetGroup)
{-# DEPRECATED tgHealthCheckPort "Use generic-lens or generic-optics with 'healthCheckPort' instead." #-}

-- | The name of the target group.
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgTargetGroupName :: Lens.Lens' TargetGroup (Lude.Maybe Lude.Text)
tgTargetGroupName = Lens.lens (targetGroupName :: TargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {targetGroupName = a} :: TargetGroup)
{-# DEPRECATED tgTargetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead." #-}

-- | The port on which the targets are listening. Not used if the target is a Lambda function.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgPort :: Lens.Lens' TargetGroup (Lude.Maybe Lude.Natural)
tgPort = Lens.lens (port :: TargetGroup -> Lude.Maybe Lude.Natural) (\s a -> s {port = a} :: TargetGroup)
{-# DEPRECATED tgPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromXML TargetGroup where
  parseXML x =
    TargetGroup'
      Lude.<$> (x Lude..@? "ProtocolVersion")
      Lude.<*> (x Lude..@? "Matcher")
      Lude.<*> (x Lude..@? "HealthCheckPath")
      Lude.<*> (x Lude..@? "HealthCheckEnabled")
      Lude.<*> (x Lude..@? "UnhealthyThresholdCount")
      Lude.<*> (x Lude..@? "VpcId")
      Lude.<*> (x Lude..@? "TargetGroupArn")
      Lude.<*> (x Lude..@? "Protocol")
      Lude.<*> (x Lude..@? "HealthCheckIntervalSeconds")
      Lude.<*> (x Lude..@? "TargetType")
      Lude.<*> (x Lude..@? "HealthyThresholdCount")
      Lude.<*> (x Lude..@? "HealthCheckProtocol")
      Lude.<*> ( x Lude..@? "LoadBalancerArns" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "HealthCheckTimeoutSeconds")
      Lude.<*> (x Lude..@? "HealthCheckPort")
      Lude.<*> (x Lude..@? "TargetGroupName")
      Lude.<*> (x Lude..@? "Port")
