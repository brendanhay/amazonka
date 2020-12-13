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
    mtgMatcher,
    mtgHealthCheckPath,
    mtgHealthCheckEnabled,
    mtgUnhealthyThresholdCount,
    mtgTargetGroupARN,
    mtgHealthCheckIntervalSeconds,
    mtgHealthyThresholdCount,
    mtgHealthCheckProtocol,
    mtgHealthCheckTimeoutSeconds,
    mtgHealthCheckPort,

    -- * Destructuring the response
    ModifyTargetGroupResponse (..),
    mkModifyTargetGroupResponse,

    -- ** Response lenses
    mtgrsTargetGroups,
    mtgrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyTargetGroup' smart constructor.
data ModifyTargetGroup = ModifyTargetGroup'
  { -- | [HTTP/HTTPS health checks] The HTTP or gRPC codes to use when checking for a successful response from a target.
    --
    -- With Network Load Balancers, you can't modify this setting.
    matcher :: Lude.Maybe Matcher,
    -- | [HTTP/HTTPS health checks] The destination for health checks on the targets.
    --
    -- [HTTP1 or HTTP2 protocol version] The ping path. The default is /.
    -- [GRPC protocol version] The path of a custom health check method with the format /package.service/method. The default is /AWS.ALB/healthcheck.
    healthCheckPath :: Lude.Maybe Lude.Text,
    -- | Indicates whether health checks are enabled.
    healthCheckEnabled :: Lude.Maybe Lude.Bool,
    -- | The number of consecutive health check failures required before considering the target unhealthy. For target groups with a protocol of TCP or TLS, this value must be the same as the healthy threshold count.
    unhealthyThresholdCount :: Lude.Maybe Lude.Natural,
    -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupARN :: Lude.Text,
    -- | The approximate amount of time, in seconds, between health checks of an individual target. For TCP health checks, the supported values are 10 or 30 seconds.
    --
    -- With Network Load Balancers, you can't modify this setting.
    healthCheckIntervalSeconds :: Lude.Maybe Lude.Natural,
    -- | The number of consecutive health checks successes required before considering an unhealthy target healthy.
    healthyThresholdCount :: Lude.Maybe Lude.Natural,
    -- | The protocol the load balancer uses when performing health checks on targets. The TCP protocol is supported for health checks only if the protocol of the target group is TCP, TLS, UDP, or TCP_UDP. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
    --
    -- With Network Load Balancers, you can't modify this setting.
    healthCheckProtocol :: Lude.Maybe ProtocolEnum,
    -- | [HTTP/HTTPS health checks] The amount of time, in seconds, during which no response means a failed health check.
    --
    -- With Network Load Balancers, you can't modify this setting.
    healthCheckTimeoutSeconds :: Lude.Maybe Lude.Natural,
    -- | The port the load balancer uses when performing health checks on targets.
    healthCheckPort :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyTargetGroup' with the minimum fields required to make a request.
--
-- * 'matcher' - [HTTP/HTTPS health checks] The HTTP or gRPC codes to use when checking for a successful response from a target.
--
-- With Network Load Balancers, you can't modify this setting.
-- * 'healthCheckPath' - [HTTP/HTTPS health checks] The destination for health checks on the targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is /.
-- [GRPC protocol version] The path of a custom health check method with the format /package.service/method. The default is /AWS.ALB/healthcheck.
-- * 'healthCheckEnabled' - Indicates whether health checks are enabled.
-- * 'unhealthyThresholdCount' - The number of consecutive health check failures required before considering the target unhealthy. For target groups with a protocol of TCP or TLS, this value must be the same as the healthy threshold count.
-- * 'targetGroupARN' - The Amazon Resource Name (ARN) of the target group.
-- * 'healthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an individual target. For TCP health checks, the supported values are 10 or 30 seconds.
--
-- With Network Load Balancers, you can't modify this setting.
-- * 'healthyThresholdCount' - The number of consecutive health checks successes required before considering an unhealthy target healthy.
-- * 'healthCheckProtocol' - The protocol the load balancer uses when performing health checks on targets. The TCP protocol is supported for health checks only if the protocol of the target group is TCP, TLS, UDP, or TCP_UDP. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
--
-- With Network Load Balancers, you can't modify this setting.
-- * 'healthCheckTimeoutSeconds' - [HTTP/HTTPS health checks] The amount of time, in seconds, during which no response means a failed health check.
--
-- With Network Load Balancers, you can't modify this setting.
-- * 'healthCheckPort' - The port the load balancer uses when performing health checks on targets.
mkModifyTargetGroup ::
  -- | 'targetGroupARN'
  Lude.Text ->
  ModifyTargetGroup
mkModifyTargetGroup pTargetGroupARN_ =
  ModifyTargetGroup'
    { matcher = Lude.Nothing,
      healthCheckPath = Lude.Nothing,
      healthCheckEnabled = Lude.Nothing,
      unhealthyThresholdCount = Lude.Nothing,
      targetGroupARN = pTargetGroupARN_,
      healthCheckIntervalSeconds = Lude.Nothing,
      healthyThresholdCount = Lude.Nothing,
      healthCheckProtocol = Lude.Nothing,
      healthCheckTimeoutSeconds = Lude.Nothing,
      healthCheckPort = Lude.Nothing
    }

-- | [HTTP/HTTPS health checks] The HTTP or gRPC codes to use when checking for a successful response from a target.
--
-- With Network Load Balancers, you can't modify this setting.
--
-- /Note:/ Consider using 'matcher' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgMatcher :: Lens.Lens' ModifyTargetGroup (Lude.Maybe Matcher)
mtgMatcher = Lens.lens (matcher :: ModifyTargetGroup -> Lude.Maybe Matcher) (\s a -> s {matcher = a} :: ModifyTargetGroup)
{-# DEPRECATED mtgMatcher "Use generic-lens or generic-optics with 'matcher' instead." #-}

-- | [HTTP/HTTPS health checks] The destination for health checks on the targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is /.
-- [GRPC protocol version] The path of a custom health check method with the format /package.service/method. The default is /AWS.ALB/healthcheck.
--
-- /Note:/ Consider using 'healthCheckPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckPath :: Lens.Lens' ModifyTargetGroup (Lude.Maybe Lude.Text)
mtgHealthCheckPath = Lens.lens (healthCheckPath :: ModifyTargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {healthCheckPath = a} :: ModifyTargetGroup)
{-# DEPRECATED mtgHealthCheckPath "Use generic-lens or generic-optics with 'healthCheckPath' instead." #-}

-- | Indicates whether health checks are enabled.
--
-- /Note:/ Consider using 'healthCheckEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckEnabled :: Lens.Lens' ModifyTargetGroup (Lude.Maybe Lude.Bool)
mtgHealthCheckEnabled = Lens.lens (healthCheckEnabled :: ModifyTargetGroup -> Lude.Maybe Lude.Bool) (\s a -> s {healthCheckEnabled = a} :: ModifyTargetGroup)
{-# DEPRECATED mtgHealthCheckEnabled "Use generic-lens or generic-optics with 'healthCheckEnabled' instead." #-}

-- | The number of consecutive health check failures required before considering the target unhealthy. For target groups with a protocol of TCP or TLS, this value must be the same as the healthy threshold count.
--
-- /Note:/ Consider using 'unhealthyThresholdCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgUnhealthyThresholdCount :: Lens.Lens' ModifyTargetGroup (Lude.Maybe Lude.Natural)
mtgUnhealthyThresholdCount = Lens.lens (unhealthyThresholdCount :: ModifyTargetGroup -> Lude.Maybe Lude.Natural) (\s a -> s {unhealthyThresholdCount = a} :: ModifyTargetGroup)
{-# DEPRECATED mtgUnhealthyThresholdCount "Use generic-lens or generic-optics with 'unhealthyThresholdCount' instead." #-}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgTargetGroupARN :: Lens.Lens' ModifyTargetGroup Lude.Text
mtgTargetGroupARN = Lens.lens (targetGroupARN :: ModifyTargetGroup -> Lude.Text) (\s a -> s {targetGroupARN = a} :: ModifyTargetGroup)
{-# DEPRECATED mtgTargetGroupARN "Use generic-lens or generic-optics with 'targetGroupARN' instead." #-}

-- | The approximate amount of time, in seconds, between health checks of an individual target. For TCP health checks, the supported values are 10 or 30 seconds.
--
-- With Network Load Balancers, you can't modify this setting.
--
-- /Note:/ Consider using 'healthCheckIntervalSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckIntervalSeconds :: Lens.Lens' ModifyTargetGroup (Lude.Maybe Lude.Natural)
mtgHealthCheckIntervalSeconds = Lens.lens (healthCheckIntervalSeconds :: ModifyTargetGroup -> Lude.Maybe Lude.Natural) (\s a -> s {healthCheckIntervalSeconds = a} :: ModifyTargetGroup)
{-# DEPRECATED mtgHealthCheckIntervalSeconds "Use generic-lens or generic-optics with 'healthCheckIntervalSeconds' instead." #-}

-- | The number of consecutive health checks successes required before considering an unhealthy target healthy.
--
-- /Note:/ Consider using 'healthyThresholdCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthyThresholdCount :: Lens.Lens' ModifyTargetGroup (Lude.Maybe Lude.Natural)
mtgHealthyThresholdCount = Lens.lens (healthyThresholdCount :: ModifyTargetGroup -> Lude.Maybe Lude.Natural) (\s a -> s {healthyThresholdCount = a} :: ModifyTargetGroup)
{-# DEPRECATED mtgHealthyThresholdCount "Use generic-lens or generic-optics with 'healthyThresholdCount' instead." #-}

-- | The protocol the load balancer uses when performing health checks on targets. The TCP protocol is supported for health checks only if the protocol of the target group is TCP, TLS, UDP, or TCP_UDP. The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health checks.
--
-- With Network Load Balancers, you can't modify this setting.
--
-- /Note:/ Consider using 'healthCheckProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckProtocol :: Lens.Lens' ModifyTargetGroup (Lude.Maybe ProtocolEnum)
mtgHealthCheckProtocol = Lens.lens (healthCheckProtocol :: ModifyTargetGroup -> Lude.Maybe ProtocolEnum) (\s a -> s {healthCheckProtocol = a} :: ModifyTargetGroup)
{-# DEPRECATED mtgHealthCheckProtocol "Use generic-lens or generic-optics with 'healthCheckProtocol' instead." #-}

-- | [HTTP/HTTPS health checks] The amount of time, in seconds, during which no response means a failed health check.
--
-- With Network Load Balancers, you can't modify this setting.
--
-- /Note:/ Consider using 'healthCheckTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckTimeoutSeconds :: Lens.Lens' ModifyTargetGroup (Lude.Maybe Lude.Natural)
mtgHealthCheckTimeoutSeconds = Lens.lens (healthCheckTimeoutSeconds :: ModifyTargetGroup -> Lude.Maybe Lude.Natural) (\s a -> s {healthCheckTimeoutSeconds = a} :: ModifyTargetGroup)
{-# DEPRECATED mtgHealthCheckTimeoutSeconds "Use generic-lens or generic-optics with 'healthCheckTimeoutSeconds' instead." #-}

-- | The port the load balancer uses when performing health checks on targets.
--
-- /Note:/ Consider using 'healthCheckPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgHealthCheckPort :: Lens.Lens' ModifyTargetGroup (Lude.Maybe Lude.Text)
mtgHealthCheckPort = Lens.lens (healthCheckPort :: ModifyTargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {healthCheckPort = a} :: ModifyTargetGroup)
{-# DEPRECATED mtgHealthCheckPort "Use generic-lens or generic-optics with 'healthCheckPort' instead." #-}

instance Lude.AWSRequest ModifyTargetGroup where
  type Rs ModifyTargetGroup = ModifyTargetGroupResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "ModifyTargetGroupResult"
      ( \s h x ->
          ModifyTargetGroupResponse'
            Lude.<$> ( x Lude..@? "TargetGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyTargetGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyTargetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyTargetGroup where
  toQuery ModifyTargetGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyTargetGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "Matcher" Lude.=: matcher,
        "HealthCheckPath" Lude.=: healthCheckPath,
        "HealthCheckEnabled" Lude.=: healthCheckEnabled,
        "UnhealthyThresholdCount" Lude.=: unhealthyThresholdCount,
        "TargetGroupArn" Lude.=: targetGroupARN,
        "HealthCheckIntervalSeconds" Lude.=: healthCheckIntervalSeconds,
        "HealthyThresholdCount" Lude.=: healthyThresholdCount,
        "HealthCheckProtocol" Lude.=: healthCheckProtocol,
        "HealthCheckTimeoutSeconds" Lude.=: healthCheckTimeoutSeconds,
        "HealthCheckPort" Lude.=: healthCheckPort
      ]

-- | /See:/ 'mkModifyTargetGroupResponse' smart constructor.
data ModifyTargetGroupResponse = ModifyTargetGroupResponse'
  { -- | Information about the modified target group.
    targetGroups :: Lude.Maybe [TargetGroup],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyTargetGroupResponse' with the minimum fields required to make a request.
--
-- * 'targetGroups' - Information about the modified target group.
-- * 'responseStatus' - The response status code.
mkModifyTargetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyTargetGroupResponse
mkModifyTargetGroupResponse pResponseStatus_ =
  ModifyTargetGroupResponse'
    { targetGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the modified target group.
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgrsTargetGroups :: Lens.Lens' ModifyTargetGroupResponse (Lude.Maybe [TargetGroup])
mtgrsTargetGroups = Lens.lens (targetGroups :: ModifyTargetGroupResponse -> Lude.Maybe [TargetGroup]) (\s a -> s {targetGroups = a} :: ModifyTargetGroupResponse)
{-# DEPRECATED mtgrsTargetGroups "Use generic-lens or generic-optics with 'targetGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgrsResponseStatus :: Lens.Lens' ModifyTargetGroupResponse Lude.Int
mtgrsResponseStatus = Lens.lens (responseStatus :: ModifyTargetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyTargetGroupResponse)
{-# DEPRECATED mtgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
