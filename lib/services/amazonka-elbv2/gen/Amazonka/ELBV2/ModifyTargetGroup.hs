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
-- Module      : Amazonka.ELBV2.ModifyTargetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the health checks used when evaluating the health state of the
-- targets in the specified target group.
module Amazonka.ELBV2.ModifyTargetGroup
  ( -- * Creating a Request
    ModifyTargetGroup (..),
    newModifyTargetGroup,

    -- * Request Lenses
    modifyTargetGroup_healthCheckEnabled,
    modifyTargetGroup_healthCheckIntervalSeconds,
    modifyTargetGroup_healthCheckPath,
    modifyTargetGroup_healthCheckPort,
    modifyTargetGroup_healthCheckProtocol,
    modifyTargetGroup_healthCheckTimeoutSeconds,
    modifyTargetGroup_healthyThresholdCount,
    modifyTargetGroup_matcher,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyTargetGroup' smart constructor.
data ModifyTargetGroup = ModifyTargetGroup'
  { -- | Indicates whether health checks are enabled.
    healthCheckEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The approximate amount of time, in seconds, between health checks of an
    -- individual target.
    healthCheckIntervalSeconds :: Prelude.Maybe Prelude.Natural,
    -- | [HTTP\/HTTPS health checks] The destination for health checks on the
    -- targets.
    --
    -- [HTTP1 or HTTP2 protocol version] The ping path. The default is \/.
    --
    -- [GRPC protocol version] The path of a custom health check method with
    -- the format \/package.service\/method. The default is \/Amazon Web
    -- Services.ALB\/healthcheck.
    healthCheckPath :: Prelude.Maybe Prelude.Text,
    -- | The port the load balancer uses when performing health checks on
    -- targets.
    healthCheckPort :: Prelude.Maybe Prelude.Text,
    -- | The protocol the load balancer uses when performing health checks on
    -- targets. For Application Load Balancers, the default is HTTP. For
    -- Network Load Balancers and Gateway Load Balancers, the default is TCP.
    -- The TCP protocol is not supported for health checks if the protocol of
    -- the target group is HTTP or HTTPS. It is supported for health checks
    -- only if the protocol of the target group is TCP, TLS, UDP, or TCP_UDP.
    -- The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health
    -- checks.
    healthCheckProtocol :: Prelude.Maybe ProtocolEnum,
    -- | [HTTP\/HTTPS health checks] The amount of time, in seconds, during which
    -- no response means a failed health check.
    healthCheckTimeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The number of consecutive health checks successes required before
    -- considering an unhealthy target healthy.
    healthyThresholdCount :: Prelude.Maybe Prelude.Natural,
    -- | [HTTP\/HTTPS health checks] The HTTP or gRPC codes to use when checking
    -- for a successful response from a target. For target groups with a
    -- protocol of TCP, TCP_UDP, UDP or TLS the range is 200-599. For target
    -- groups with a protocol of HTTP or HTTPS, the range is 200-499. For
    -- target groups with a protocol of GENEVE, the range is 200-399.
    matcher :: Prelude.Maybe Matcher,
    -- | The number of consecutive health check failures required before
    -- considering the target unhealthy.
    unhealthyThresholdCount :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'healthCheckIntervalSeconds', 'modifyTargetGroup_healthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an
-- individual target.
--
-- 'healthCheckPath', 'modifyTargetGroup_healthCheckPath' - [HTTP\/HTTPS health checks] The destination for health checks on the
-- targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is \/.
--
-- [GRPC protocol version] The path of a custom health check method with
-- the format \/package.service\/method. The default is \/Amazon Web
-- Services.ALB\/healthcheck.
--
-- 'healthCheckPort', 'modifyTargetGroup_healthCheckPort' - The port the load balancer uses when performing health checks on
-- targets.
--
-- 'healthCheckProtocol', 'modifyTargetGroup_healthCheckProtocol' - The protocol the load balancer uses when performing health checks on
-- targets. For Application Load Balancers, the default is HTTP. For
-- Network Load Balancers and Gateway Load Balancers, the default is TCP.
-- The TCP protocol is not supported for health checks if the protocol of
-- the target group is HTTP or HTTPS. It is supported for health checks
-- only if the protocol of the target group is TCP, TLS, UDP, or TCP_UDP.
-- The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health
-- checks.
--
-- 'healthCheckTimeoutSeconds', 'modifyTargetGroup_healthCheckTimeoutSeconds' - [HTTP\/HTTPS health checks] The amount of time, in seconds, during which
-- no response means a failed health check.
--
-- 'healthyThresholdCount', 'modifyTargetGroup_healthyThresholdCount' - The number of consecutive health checks successes required before
-- considering an unhealthy target healthy.
--
-- 'matcher', 'modifyTargetGroup_matcher' - [HTTP\/HTTPS health checks] The HTTP or gRPC codes to use when checking
-- for a successful response from a target. For target groups with a
-- protocol of TCP, TCP_UDP, UDP or TLS the range is 200-599. For target
-- groups with a protocol of HTTP or HTTPS, the range is 200-499. For
-- target groups with a protocol of GENEVE, the range is 200-399.
--
-- 'unhealthyThresholdCount', 'modifyTargetGroup_unhealthyThresholdCount' - The number of consecutive health check failures required before
-- considering the target unhealthy.
--
-- 'targetGroupArn', 'modifyTargetGroup_targetGroupArn' - The Amazon Resource Name (ARN) of the target group.
newModifyTargetGroup ::
  -- | 'targetGroupArn'
  Prelude.Text ->
  ModifyTargetGroup
newModifyTargetGroup pTargetGroupArn_ =
  ModifyTargetGroup'
    { healthCheckEnabled =
        Prelude.Nothing,
      healthCheckIntervalSeconds = Prelude.Nothing,
      healthCheckPath = Prelude.Nothing,
      healthCheckPort = Prelude.Nothing,
      healthCheckProtocol = Prelude.Nothing,
      healthCheckTimeoutSeconds = Prelude.Nothing,
      healthyThresholdCount = Prelude.Nothing,
      matcher = Prelude.Nothing,
      unhealthyThresholdCount = Prelude.Nothing,
      targetGroupArn = pTargetGroupArn_
    }

-- | Indicates whether health checks are enabled.
modifyTargetGroup_healthCheckEnabled :: Lens.Lens' ModifyTargetGroup (Prelude.Maybe Prelude.Bool)
modifyTargetGroup_healthCheckEnabled = Lens.lens (\ModifyTargetGroup' {healthCheckEnabled} -> healthCheckEnabled) (\s@ModifyTargetGroup' {} a -> s {healthCheckEnabled = a} :: ModifyTargetGroup)

-- | The approximate amount of time, in seconds, between health checks of an
-- individual target.
modifyTargetGroup_healthCheckIntervalSeconds :: Lens.Lens' ModifyTargetGroup (Prelude.Maybe Prelude.Natural)
modifyTargetGroup_healthCheckIntervalSeconds = Lens.lens (\ModifyTargetGroup' {healthCheckIntervalSeconds} -> healthCheckIntervalSeconds) (\s@ModifyTargetGroup' {} a -> s {healthCheckIntervalSeconds = a} :: ModifyTargetGroup)

-- | [HTTP\/HTTPS health checks] The destination for health checks on the
-- targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is \/.
--
-- [GRPC protocol version] The path of a custom health check method with
-- the format \/package.service\/method. The default is \/Amazon Web
-- Services.ALB\/healthcheck.
modifyTargetGroup_healthCheckPath :: Lens.Lens' ModifyTargetGroup (Prelude.Maybe Prelude.Text)
modifyTargetGroup_healthCheckPath = Lens.lens (\ModifyTargetGroup' {healthCheckPath} -> healthCheckPath) (\s@ModifyTargetGroup' {} a -> s {healthCheckPath = a} :: ModifyTargetGroup)

-- | The port the load balancer uses when performing health checks on
-- targets.
modifyTargetGroup_healthCheckPort :: Lens.Lens' ModifyTargetGroup (Prelude.Maybe Prelude.Text)
modifyTargetGroup_healthCheckPort = Lens.lens (\ModifyTargetGroup' {healthCheckPort} -> healthCheckPort) (\s@ModifyTargetGroup' {} a -> s {healthCheckPort = a} :: ModifyTargetGroup)

-- | The protocol the load balancer uses when performing health checks on
-- targets. For Application Load Balancers, the default is HTTP. For
-- Network Load Balancers and Gateway Load Balancers, the default is TCP.
-- The TCP protocol is not supported for health checks if the protocol of
-- the target group is HTTP or HTTPS. It is supported for health checks
-- only if the protocol of the target group is TCP, TLS, UDP, or TCP_UDP.
-- The GENEVE, TLS, UDP, and TCP_UDP protocols are not supported for health
-- checks.
modifyTargetGroup_healthCheckProtocol :: Lens.Lens' ModifyTargetGroup (Prelude.Maybe ProtocolEnum)
modifyTargetGroup_healthCheckProtocol = Lens.lens (\ModifyTargetGroup' {healthCheckProtocol} -> healthCheckProtocol) (\s@ModifyTargetGroup' {} a -> s {healthCheckProtocol = a} :: ModifyTargetGroup)

-- | [HTTP\/HTTPS health checks] The amount of time, in seconds, during which
-- no response means a failed health check.
modifyTargetGroup_healthCheckTimeoutSeconds :: Lens.Lens' ModifyTargetGroup (Prelude.Maybe Prelude.Natural)
modifyTargetGroup_healthCheckTimeoutSeconds = Lens.lens (\ModifyTargetGroup' {healthCheckTimeoutSeconds} -> healthCheckTimeoutSeconds) (\s@ModifyTargetGroup' {} a -> s {healthCheckTimeoutSeconds = a} :: ModifyTargetGroup)

-- | The number of consecutive health checks successes required before
-- considering an unhealthy target healthy.
modifyTargetGroup_healthyThresholdCount :: Lens.Lens' ModifyTargetGroup (Prelude.Maybe Prelude.Natural)
modifyTargetGroup_healthyThresholdCount = Lens.lens (\ModifyTargetGroup' {healthyThresholdCount} -> healthyThresholdCount) (\s@ModifyTargetGroup' {} a -> s {healthyThresholdCount = a} :: ModifyTargetGroup)

-- | [HTTP\/HTTPS health checks] The HTTP or gRPC codes to use when checking
-- for a successful response from a target. For target groups with a
-- protocol of TCP, TCP_UDP, UDP or TLS the range is 200-599. For target
-- groups with a protocol of HTTP or HTTPS, the range is 200-499. For
-- target groups with a protocol of GENEVE, the range is 200-399.
modifyTargetGroup_matcher :: Lens.Lens' ModifyTargetGroup (Prelude.Maybe Matcher)
modifyTargetGroup_matcher = Lens.lens (\ModifyTargetGroup' {matcher} -> matcher) (\s@ModifyTargetGroup' {} a -> s {matcher = a} :: ModifyTargetGroup)

-- | The number of consecutive health check failures required before
-- considering the target unhealthy.
modifyTargetGroup_unhealthyThresholdCount :: Lens.Lens' ModifyTargetGroup (Prelude.Maybe Prelude.Natural)
modifyTargetGroup_unhealthyThresholdCount = Lens.lens (\ModifyTargetGroup' {unhealthyThresholdCount} -> unhealthyThresholdCount) (\s@ModifyTargetGroup' {} a -> s {unhealthyThresholdCount = a} :: ModifyTargetGroup)

-- | The Amazon Resource Name (ARN) of the target group.
modifyTargetGroup_targetGroupArn :: Lens.Lens' ModifyTargetGroup Prelude.Text
modifyTargetGroup_targetGroupArn = Lens.lens (\ModifyTargetGroup' {targetGroupArn} -> targetGroupArn) (\s@ModifyTargetGroup' {} a -> s {targetGroupArn = a} :: ModifyTargetGroup)

instance Core.AWSRequest ModifyTargetGroup where
  type
    AWSResponse ModifyTargetGroup =
      ModifyTargetGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyTargetGroupResult"
      ( \s h x ->
          ModifyTargetGroupResponse'
            Prelude.<$> ( x
                            Data..@? "TargetGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyTargetGroup where
  hashWithSalt _salt ModifyTargetGroup' {..} =
    _salt
      `Prelude.hashWithSalt` healthCheckEnabled
      `Prelude.hashWithSalt` healthCheckIntervalSeconds
      `Prelude.hashWithSalt` healthCheckPath
      `Prelude.hashWithSalt` healthCheckPort
      `Prelude.hashWithSalt` healthCheckProtocol
      `Prelude.hashWithSalt` healthCheckTimeoutSeconds
      `Prelude.hashWithSalt` healthyThresholdCount
      `Prelude.hashWithSalt` matcher
      `Prelude.hashWithSalt` unhealthyThresholdCount
      `Prelude.hashWithSalt` targetGroupArn

instance Prelude.NFData ModifyTargetGroup where
  rnf ModifyTargetGroup' {..} =
    Prelude.rnf healthCheckEnabled
      `Prelude.seq` Prelude.rnf healthCheckIntervalSeconds
      `Prelude.seq` Prelude.rnf healthCheckPath
      `Prelude.seq` Prelude.rnf healthCheckPort
      `Prelude.seq` Prelude.rnf healthCheckProtocol
      `Prelude.seq` Prelude.rnf healthCheckTimeoutSeconds
      `Prelude.seq` Prelude.rnf healthyThresholdCount
      `Prelude.seq` Prelude.rnf matcher
      `Prelude.seq` Prelude.rnf unhealthyThresholdCount
      `Prelude.seq` Prelude.rnf targetGroupArn

instance Data.ToHeaders ModifyTargetGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyTargetGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyTargetGroup where
  toQuery ModifyTargetGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyTargetGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-12-01" :: Prelude.ByteString),
        "HealthCheckEnabled" Data.=: healthCheckEnabled,
        "HealthCheckIntervalSeconds"
          Data.=: healthCheckIntervalSeconds,
        "HealthCheckPath" Data.=: healthCheckPath,
        "HealthCheckPort" Data.=: healthCheckPort,
        "HealthCheckProtocol" Data.=: healthCheckProtocol,
        "HealthCheckTimeoutSeconds"
          Data.=: healthCheckTimeoutSeconds,
        "HealthyThresholdCount"
          Data.=: healthyThresholdCount,
        "Matcher" Data.=: matcher,
        "UnhealthyThresholdCount"
          Data.=: unhealthyThresholdCount,
        "TargetGroupArn" Data.=: targetGroupArn
      ]

-- | /See:/ 'newModifyTargetGroupResponse' smart constructor.
data ModifyTargetGroupResponse = ModifyTargetGroupResponse'
  { -- | Information about the modified target group.
    targetGroups :: Prelude.Maybe [TargetGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyTargetGroupResponse
newModifyTargetGroupResponse pHttpStatus_ =
  ModifyTargetGroupResponse'
    { targetGroups =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the modified target group.
modifyTargetGroupResponse_targetGroups :: Lens.Lens' ModifyTargetGroupResponse (Prelude.Maybe [TargetGroup])
modifyTargetGroupResponse_targetGroups = Lens.lens (\ModifyTargetGroupResponse' {targetGroups} -> targetGroups) (\s@ModifyTargetGroupResponse' {} a -> s {targetGroups = a} :: ModifyTargetGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
modifyTargetGroupResponse_httpStatus :: Lens.Lens' ModifyTargetGroupResponse Prelude.Int
modifyTargetGroupResponse_httpStatus = Lens.lens (\ModifyTargetGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyTargetGroupResponse' {} a -> s {httpStatus = a} :: ModifyTargetGroupResponse)

instance Prelude.NFData ModifyTargetGroupResponse where
  rnf ModifyTargetGroupResponse' {..} =
    Prelude.rnf targetGroups
      `Prelude.seq` Prelude.rnf httpStatus
