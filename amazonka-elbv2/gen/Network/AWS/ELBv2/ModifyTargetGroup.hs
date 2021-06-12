{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.ModifyTargetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the health checks used when evaluating the health state of the
-- targets in the specified target group.
module Network.AWS.ELBv2.ModifyTargetGroup
  ( -- * Creating a Request
    ModifyTargetGroup (..),
    newModifyTargetGroup,

    -- * Request Lenses
    modifyTargetGroup_healthCheckEnabled,
    modifyTargetGroup_healthCheckProtocol,
    modifyTargetGroup_healthCheckPort,
    modifyTargetGroup_healthCheckTimeoutSeconds,
    modifyTargetGroup_healthCheckPath,
    modifyTargetGroup_matcher,
    modifyTargetGroup_healthyThresholdCount,
    modifyTargetGroup_healthCheckIntervalSeconds,
    modifyTargetGroup_unhealthyThresholdCount,
    modifyTargetGroup_targetGroupArn,

    -- * Destructuring the Response
    ModifyTargetGroupResponse (..),
    newModifyTargetGroupResponse,

    -- * Response Lenses
    modifyTargetGroupResponse_targetGroups,
    modifyTargetGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyTargetGroup' smart constructor.
data ModifyTargetGroup = ModifyTargetGroup'
  { -- | Indicates whether health checks are enabled.
    healthCheckEnabled :: Core.Maybe Core.Bool,
    -- | The protocol the load balancer uses when performing health checks on
    -- targets. The TCP protocol is supported for health checks only if the
    -- protocol of the target group is TCP, TLS, UDP, or TCP_UDP. The GENEVE,
    -- TLS, UDP, and TCP_UDP protocols are not supported for health checks.
    --
    -- With Network Load Balancers, you can\'t modify this setting.
    healthCheckProtocol :: Core.Maybe ProtocolEnum,
    -- | The port the load balancer uses when performing health checks on
    -- targets.
    healthCheckPort :: Core.Maybe Core.Text,
    -- | [HTTP\/HTTPS health checks] The amount of time, in seconds, during which
    -- no response means a failed health check.
    --
    -- With Network Load Balancers, you can\'t modify this setting.
    healthCheckTimeoutSeconds :: Core.Maybe Core.Natural,
    -- | [HTTP\/HTTPS health checks] The destination for health checks on the
    -- targets.
    --
    -- [HTTP1 or HTTP2 protocol version] The ping path. The default is \/.
    --
    -- [GRPC protocol version] The path of a custom health check method with
    -- the format \/package.service\/method. The default is
    -- \/AWS.ALB\/healthcheck.
    healthCheckPath :: Core.Maybe Core.Text,
    -- | [HTTP\/HTTPS health checks] The HTTP or gRPC codes to use when checking
    -- for a successful response from a target.
    --
    -- With Network Load Balancers, you can\'t modify this setting.
    matcher :: Core.Maybe Matcher,
    -- | The number of consecutive health checks successes required before
    -- considering an unhealthy target healthy.
    healthyThresholdCount :: Core.Maybe Core.Natural,
    -- | The approximate amount of time, in seconds, between health checks of an
    -- individual target. For TCP health checks, the supported values are 10 or
    -- 30 seconds.
    --
    -- With Network Load Balancers, you can\'t modify this setting.
    healthCheckIntervalSeconds :: Core.Maybe Core.Natural,
    -- | The number of consecutive health check failures required before
    -- considering the target unhealthy. For target groups with a protocol of
    -- TCP or TLS, this value must be the same as the healthy threshold count.
    unhealthyThresholdCount :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyTargetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheckEnabled', 'modifyTargetGroup_healthCheckEnabled' - Indicates whether health checks are enabled.
--
-- 'healthCheckProtocol', 'modifyTargetGroup_healthCheckProtocol' - The protocol the load balancer uses when performing health checks on
-- targets. The TCP protocol is supported for health checks only if the
-- protocol of the target group is TCP, TLS, UDP, or TCP_UDP. The GENEVE,
-- TLS, UDP, and TCP_UDP protocols are not supported for health checks.
--
-- With Network Load Balancers, you can\'t modify this setting.
--
-- 'healthCheckPort', 'modifyTargetGroup_healthCheckPort' - The port the load balancer uses when performing health checks on
-- targets.
--
-- 'healthCheckTimeoutSeconds', 'modifyTargetGroup_healthCheckTimeoutSeconds' - [HTTP\/HTTPS health checks] The amount of time, in seconds, during which
-- no response means a failed health check.
--
-- With Network Load Balancers, you can\'t modify this setting.
--
-- 'healthCheckPath', 'modifyTargetGroup_healthCheckPath' - [HTTP\/HTTPS health checks] The destination for health checks on the
-- targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is \/.
--
-- [GRPC protocol version] The path of a custom health check method with
-- the format \/package.service\/method. The default is
-- \/AWS.ALB\/healthcheck.
--
-- 'matcher', 'modifyTargetGroup_matcher' - [HTTP\/HTTPS health checks] The HTTP or gRPC codes to use when checking
-- for a successful response from a target.
--
-- With Network Load Balancers, you can\'t modify this setting.
--
-- 'healthyThresholdCount', 'modifyTargetGroup_healthyThresholdCount' - The number of consecutive health checks successes required before
-- considering an unhealthy target healthy.
--
-- 'healthCheckIntervalSeconds', 'modifyTargetGroup_healthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an
-- individual target. For TCP health checks, the supported values are 10 or
-- 30 seconds.
--
-- With Network Load Balancers, you can\'t modify this setting.
--
-- 'unhealthyThresholdCount', 'modifyTargetGroup_unhealthyThresholdCount' - The number of consecutive health check failures required before
-- considering the target unhealthy. For target groups with a protocol of
-- TCP or TLS, this value must be the same as the healthy threshold count.
--
-- 'targetGroupArn', 'modifyTargetGroup_targetGroupArn' - The Amazon Resource Name (ARN) of the target group.
newModifyTargetGroup ::
  -- | 'targetGroupArn'
  Core.Text ->
  ModifyTargetGroup
newModifyTargetGroup pTargetGroupArn_ =
  ModifyTargetGroup'
    { healthCheckEnabled =
        Core.Nothing,
      healthCheckProtocol = Core.Nothing,
      healthCheckPort = Core.Nothing,
      healthCheckTimeoutSeconds = Core.Nothing,
      healthCheckPath = Core.Nothing,
      matcher = Core.Nothing,
      healthyThresholdCount = Core.Nothing,
      healthCheckIntervalSeconds = Core.Nothing,
      unhealthyThresholdCount = Core.Nothing,
      targetGroupArn = pTargetGroupArn_
    }

-- | Indicates whether health checks are enabled.
modifyTargetGroup_healthCheckEnabled :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Bool)
modifyTargetGroup_healthCheckEnabled = Lens.lens (\ModifyTargetGroup' {healthCheckEnabled} -> healthCheckEnabled) (\s@ModifyTargetGroup' {} a -> s {healthCheckEnabled = a} :: ModifyTargetGroup)

-- | The protocol the load balancer uses when performing health checks on
-- targets. The TCP protocol is supported for health checks only if the
-- protocol of the target group is TCP, TLS, UDP, or TCP_UDP. The GENEVE,
-- TLS, UDP, and TCP_UDP protocols are not supported for health checks.
--
-- With Network Load Balancers, you can\'t modify this setting.
modifyTargetGroup_healthCheckProtocol :: Lens.Lens' ModifyTargetGroup (Core.Maybe ProtocolEnum)
modifyTargetGroup_healthCheckProtocol = Lens.lens (\ModifyTargetGroup' {healthCheckProtocol} -> healthCheckProtocol) (\s@ModifyTargetGroup' {} a -> s {healthCheckProtocol = a} :: ModifyTargetGroup)

-- | The port the load balancer uses when performing health checks on
-- targets.
modifyTargetGroup_healthCheckPort :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Text)
modifyTargetGroup_healthCheckPort = Lens.lens (\ModifyTargetGroup' {healthCheckPort} -> healthCheckPort) (\s@ModifyTargetGroup' {} a -> s {healthCheckPort = a} :: ModifyTargetGroup)

-- | [HTTP\/HTTPS health checks] The amount of time, in seconds, during which
-- no response means a failed health check.
--
-- With Network Load Balancers, you can\'t modify this setting.
modifyTargetGroup_healthCheckTimeoutSeconds :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Natural)
modifyTargetGroup_healthCheckTimeoutSeconds = Lens.lens (\ModifyTargetGroup' {healthCheckTimeoutSeconds} -> healthCheckTimeoutSeconds) (\s@ModifyTargetGroup' {} a -> s {healthCheckTimeoutSeconds = a} :: ModifyTargetGroup)

-- | [HTTP\/HTTPS health checks] The destination for health checks on the
-- targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is \/.
--
-- [GRPC protocol version] The path of a custom health check method with
-- the format \/package.service\/method. The default is
-- \/AWS.ALB\/healthcheck.
modifyTargetGroup_healthCheckPath :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Text)
modifyTargetGroup_healthCheckPath = Lens.lens (\ModifyTargetGroup' {healthCheckPath} -> healthCheckPath) (\s@ModifyTargetGroup' {} a -> s {healthCheckPath = a} :: ModifyTargetGroup)

-- | [HTTP\/HTTPS health checks] The HTTP or gRPC codes to use when checking
-- for a successful response from a target.
--
-- With Network Load Balancers, you can\'t modify this setting.
modifyTargetGroup_matcher :: Lens.Lens' ModifyTargetGroup (Core.Maybe Matcher)
modifyTargetGroup_matcher = Lens.lens (\ModifyTargetGroup' {matcher} -> matcher) (\s@ModifyTargetGroup' {} a -> s {matcher = a} :: ModifyTargetGroup)

-- | The number of consecutive health checks successes required before
-- considering an unhealthy target healthy.
modifyTargetGroup_healthyThresholdCount :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Natural)
modifyTargetGroup_healthyThresholdCount = Lens.lens (\ModifyTargetGroup' {healthyThresholdCount} -> healthyThresholdCount) (\s@ModifyTargetGroup' {} a -> s {healthyThresholdCount = a} :: ModifyTargetGroup)

-- | The approximate amount of time, in seconds, between health checks of an
-- individual target. For TCP health checks, the supported values are 10 or
-- 30 seconds.
--
-- With Network Load Balancers, you can\'t modify this setting.
modifyTargetGroup_healthCheckIntervalSeconds :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Natural)
modifyTargetGroup_healthCheckIntervalSeconds = Lens.lens (\ModifyTargetGroup' {healthCheckIntervalSeconds} -> healthCheckIntervalSeconds) (\s@ModifyTargetGroup' {} a -> s {healthCheckIntervalSeconds = a} :: ModifyTargetGroup)

-- | The number of consecutive health check failures required before
-- considering the target unhealthy. For target groups with a protocol of
-- TCP or TLS, this value must be the same as the healthy threshold count.
modifyTargetGroup_unhealthyThresholdCount :: Lens.Lens' ModifyTargetGroup (Core.Maybe Core.Natural)
modifyTargetGroup_unhealthyThresholdCount = Lens.lens (\ModifyTargetGroup' {unhealthyThresholdCount} -> unhealthyThresholdCount) (\s@ModifyTargetGroup' {} a -> s {unhealthyThresholdCount = a} :: ModifyTargetGroup)

-- | The Amazon Resource Name (ARN) of the target group.
modifyTargetGroup_targetGroupArn :: Lens.Lens' ModifyTargetGroup Core.Text
modifyTargetGroup_targetGroupArn = Lens.lens (\ModifyTargetGroup' {targetGroupArn} -> targetGroupArn) (\s@ModifyTargetGroup' {} a -> s {targetGroupArn = a} :: ModifyTargetGroup)

instance Core.AWSRequest ModifyTargetGroup where
  type
    AWSResponse ModifyTargetGroup =
      ModifyTargetGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyTargetGroupResult"
      ( \s h x ->
          ModifyTargetGroupResponse'
            Core.<$> ( x Core..@? "TargetGroups" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyTargetGroup

instance Core.NFData ModifyTargetGroup

instance Core.ToHeaders ModifyTargetGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyTargetGroup where
  toPath = Core.const "/"

instance Core.ToQuery ModifyTargetGroup where
  toQuery ModifyTargetGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyTargetGroup" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "HealthCheckEnabled" Core.=: healthCheckEnabled,
        "HealthCheckProtocol" Core.=: healthCheckProtocol,
        "HealthCheckPort" Core.=: healthCheckPort,
        "HealthCheckTimeoutSeconds"
          Core.=: healthCheckTimeoutSeconds,
        "HealthCheckPath" Core.=: healthCheckPath,
        "Matcher" Core.=: matcher,
        "HealthyThresholdCount"
          Core.=: healthyThresholdCount,
        "HealthCheckIntervalSeconds"
          Core.=: healthCheckIntervalSeconds,
        "UnhealthyThresholdCount"
          Core.=: unhealthyThresholdCount,
        "TargetGroupArn" Core.=: targetGroupArn
      ]

-- | /See:/ 'newModifyTargetGroupResponse' smart constructor.
data ModifyTargetGroupResponse = ModifyTargetGroupResponse'
  { -- | Information about the modified target group.
    targetGroups :: Core.Maybe [TargetGroup],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyTargetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroups', 'modifyTargetGroupResponse_targetGroups' - Information about the modified target group.
--
-- 'httpStatus', 'modifyTargetGroupResponse_httpStatus' - The response's http status code.
newModifyTargetGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyTargetGroupResponse
newModifyTargetGroupResponse pHttpStatus_ =
  ModifyTargetGroupResponse'
    { targetGroups =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the modified target group.
modifyTargetGroupResponse_targetGroups :: Lens.Lens' ModifyTargetGroupResponse (Core.Maybe [TargetGroup])
modifyTargetGroupResponse_targetGroups = Lens.lens (\ModifyTargetGroupResponse' {targetGroups} -> targetGroups) (\s@ModifyTargetGroupResponse' {} a -> s {targetGroups = a} :: ModifyTargetGroupResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
modifyTargetGroupResponse_httpStatus :: Lens.Lens' ModifyTargetGroupResponse Core.Int
modifyTargetGroupResponse_httpStatus = Lens.lens (\ModifyTargetGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyTargetGroupResponse' {} a -> s {httpStatus = a} :: ModifyTargetGroupResponse)

instance Core.NFData ModifyTargetGroupResponse
