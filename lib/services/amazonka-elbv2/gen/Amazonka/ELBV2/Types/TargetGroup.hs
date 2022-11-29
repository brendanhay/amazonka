{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ELBV2.Types.TargetGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.TargetGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELBV2.Types.Matcher
import Amazonka.ELBV2.Types.ProtocolEnum
import Amazonka.ELBV2.Types.TargetGroupIpAddressTypeEnum
import Amazonka.ELBV2.Types.TargetTypeEnum
import qualified Amazonka.Prelude as Prelude

-- | Information about a target group.
--
-- /See:/ 'newTargetGroup' smart constructor.
data TargetGroup = TargetGroup'
  { -- | The protocol to use to connect with the target. The GENEVE, TLS, UDP,
    -- and TCP_UDP protocols are not supported for health checks.
    healthCheckProtocol :: Prelude.Maybe ProtocolEnum,
    -- | The port on which the targets are listening. Not used if the target is a
    -- Lambda function.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The amount of time, in seconds, during which no response means a failed
    -- health check.
    healthCheckTimeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Names (ARN) of the load balancers that route traffic
    -- to this target group.
    loadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The destination for health checks on the targets.
    healthCheckPath :: Prelude.Maybe Prelude.Text,
    -- | The number of consecutive health check failures required before
    -- considering the target unhealthy.
    unhealthyThresholdCount :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether health checks are enabled.
    healthCheckEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the target group.
    targetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The approximate amount of time, in seconds, between health checks of an
    -- individual target.
    healthCheckIntervalSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The number of consecutive health checks successes required before
    -- considering an unhealthy target healthy.
    healthyThresholdCount :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The type of target that you must specify when registering targets with
    -- this target group. The possible values are @instance@ (register targets
    -- by instance ID), @ip@ (register targets by IP address), @lambda@
    -- (register a single Lambda function as a target), or @alb@ (register a
    -- single Application Load Balancer as a target).
    targetType :: Prelude.Maybe TargetTypeEnum,
    -- | [HTTP\/HTTPS protocol] The protocol version. The possible values are
    -- @GRPC@, @HTTP1@, and @HTTP2@.
    protocolVersion :: Prelude.Maybe Prelude.Text,
    -- | The port to use to connect with the target.
    healthCheckPort :: Prelude.Maybe Prelude.Text,
    -- | The protocol to use for routing traffic to the targets.
    protocol :: Prelude.Maybe ProtocolEnum,
    -- | The ID of the VPC for the targets.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The type of IP address used for this target group. The possible values
    -- are @ipv4@ and @ipv6@. This is an optional parameter. If not specified,
    -- the IP address type defaults to @ipv4@.
    ipAddressType :: Prelude.Maybe TargetGroupIpAddressTypeEnum,
    -- | The HTTP or gRPC codes to use when checking for a successful response
    -- from a target.
    matcher :: Prelude.Maybe Matcher
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheckProtocol', 'targetGroup_healthCheckProtocol' - The protocol to use to connect with the target. The GENEVE, TLS, UDP,
-- and TCP_UDP protocols are not supported for health checks.
--
-- 'port', 'targetGroup_port' - The port on which the targets are listening. Not used if the target is a
-- Lambda function.
--
-- 'healthCheckTimeoutSeconds', 'targetGroup_healthCheckTimeoutSeconds' - The amount of time, in seconds, during which no response means a failed
-- health check.
--
-- 'loadBalancerArns', 'targetGroup_loadBalancerArns' - The Amazon Resource Names (ARN) of the load balancers that route traffic
-- to this target group.
--
-- 'healthCheckPath', 'targetGroup_healthCheckPath' - The destination for health checks on the targets.
--
-- 'unhealthyThresholdCount', 'targetGroup_unhealthyThresholdCount' - The number of consecutive health check failures required before
-- considering the target unhealthy.
--
-- 'healthCheckEnabled', 'targetGroup_healthCheckEnabled' - Indicates whether health checks are enabled.
--
-- 'targetGroupName', 'targetGroup_targetGroupName' - The name of the target group.
--
-- 'healthCheckIntervalSeconds', 'targetGroup_healthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an
-- individual target.
--
-- 'healthyThresholdCount', 'targetGroup_healthyThresholdCount' - The number of consecutive health checks successes required before
-- considering an unhealthy target healthy.
--
-- 'targetGroupArn', 'targetGroup_targetGroupArn' - The Amazon Resource Name (ARN) of the target group.
--
-- 'targetType', 'targetGroup_targetType' - The type of target that you must specify when registering targets with
-- this target group. The possible values are @instance@ (register targets
-- by instance ID), @ip@ (register targets by IP address), @lambda@
-- (register a single Lambda function as a target), or @alb@ (register a
-- single Application Load Balancer as a target).
--
-- 'protocolVersion', 'targetGroup_protocolVersion' - [HTTP\/HTTPS protocol] The protocol version. The possible values are
-- @GRPC@, @HTTP1@, and @HTTP2@.
--
-- 'healthCheckPort', 'targetGroup_healthCheckPort' - The port to use to connect with the target.
--
-- 'protocol', 'targetGroup_protocol' - The protocol to use for routing traffic to the targets.
--
-- 'vpcId', 'targetGroup_vpcId' - The ID of the VPC for the targets.
--
-- 'ipAddressType', 'targetGroup_ipAddressType' - The type of IP address used for this target group. The possible values
-- are @ipv4@ and @ipv6@. This is an optional parameter. If not specified,
-- the IP address type defaults to @ipv4@.
--
-- 'matcher', 'targetGroup_matcher' - The HTTP or gRPC codes to use when checking for a successful response
-- from a target.
newTargetGroup ::
  TargetGroup
newTargetGroup =
  TargetGroup'
    { healthCheckProtocol = Prelude.Nothing,
      port = Prelude.Nothing,
      healthCheckTimeoutSeconds = Prelude.Nothing,
      loadBalancerArns = Prelude.Nothing,
      healthCheckPath = Prelude.Nothing,
      unhealthyThresholdCount = Prelude.Nothing,
      healthCheckEnabled = Prelude.Nothing,
      targetGroupName = Prelude.Nothing,
      healthCheckIntervalSeconds = Prelude.Nothing,
      healthyThresholdCount = Prelude.Nothing,
      targetGroupArn = Prelude.Nothing,
      targetType = Prelude.Nothing,
      protocolVersion = Prelude.Nothing,
      healthCheckPort = Prelude.Nothing,
      protocol = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      matcher = Prelude.Nothing
    }

-- | The protocol to use to connect with the target. The GENEVE, TLS, UDP,
-- and TCP_UDP protocols are not supported for health checks.
targetGroup_healthCheckProtocol :: Lens.Lens' TargetGroup (Prelude.Maybe ProtocolEnum)
targetGroup_healthCheckProtocol = Lens.lens (\TargetGroup' {healthCheckProtocol} -> healthCheckProtocol) (\s@TargetGroup' {} a -> s {healthCheckProtocol = a} :: TargetGroup)

-- | The port on which the targets are listening. Not used if the target is a
-- Lambda function.
targetGroup_port :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Natural)
targetGroup_port = Lens.lens (\TargetGroup' {port} -> port) (\s@TargetGroup' {} a -> s {port = a} :: TargetGroup)

-- | The amount of time, in seconds, during which no response means a failed
-- health check.
targetGroup_healthCheckTimeoutSeconds :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Natural)
targetGroup_healthCheckTimeoutSeconds = Lens.lens (\TargetGroup' {healthCheckTimeoutSeconds} -> healthCheckTimeoutSeconds) (\s@TargetGroup' {} a -> s {healthCheckTimeoutSeconds = a} :: TargetGroup)

-- | The Amazon Resource Names (ARN) of the load balancers that route traffic
-- to this target group.
targetGroup_loadBalancerArns :: Lens.Lens' TargetGroup (Prelude.Maybe [Prelude.Text])
targetGroup_loadBalancerArns = Lens.lens (\TargetGroup' {loadBalancerArns} -> loadBalancerArns) (\s@TargetGroup' {} a -> s {loadBalancerArns = a} :: TargetGroup) Prelude.. Lens.mapping Lens.coerced

-- | The destination for health checks on the targets.
targetGroup_healthCheckPath :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Text)
targetGroup_healthCheckPath = Lens.lens (\TargetGroup' {healthCheckPath} -> healthCheckPath) (\s@TargetGroup' {} a -> s {healthCheckPath = a} :: TargetGroup)

-- | The number of consecutive health check failures required before
-- considering the target unhealthy.
targetGroup_unhealthyThresholdCount :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Natural)
targetGroup_unhealthyThresholdCount = Lens.lens (\TargetGroup' {unhealthyThresholdCount} -> unhealthyThresholdCount) (\s@TargetGroup' {} a -> s {unhealthyThresholdCount = a} :: TargetGroup)

-- | Indicates whether health checks are enabled.
targetGroup_healthCheckEnabled :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Bool)
targetGroup_healthCheckEnabled = Lens.lens (\TargetGroup' {healthCheckEnabled} -> healthCheckEnabled) (\s@TargetGroup' {} a -> s {healthCheckEnabled = a} :: TargetGroup)

-- | The name of the target group.
targetGroup_targetGroupName :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Text)
targetGroup_targetGroupName = Lens.lens (\TargetGroup' {targetGroupName} -> targetGroupName) (\s@TargetGroup' {} a -> s {targetGroupName = a} :: TargetGroup)

-- | The approximate amount of time, in seconds, between health checks of an
-- individual target.
targetGroup_healthCheckIntervalSeconds :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Natural)
targetGroup_healthCheckIntervalSeconds = Lens.lens (\TargetGroup' {healthCheckIntervalSeconds} -> healthCheckIntervalSeconds) (\s@TargetGroup' {} a -> s {healthCheckIntervalSeconds = a} :: TargetGroup)

-- | The number of consecutive health checks successes required before
-- considering an unhealthy target healthy.
targetGroup_healthyThresholdCount :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Natural)
targetGroup_healthyThresholdCount = Lens.lens (\TargetGroup' {healthyThresholdCount} -> healthyThresholdCount) (\s@TargetGroup' {} a -> s {healthyThresholdCount = a} :: TargetGroup)

-- | The Amazon Resource Name (ARN) of the target group.
targetGroup_targetGroupArn :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Text)
targetGroup_targetGroupArn = Lens.lens (\TargetGroup' {targetGroupArn} -> targetGroupArn) (\s@TargetGroup' {} a -> s {targetGroupArn = a} :: TargetGroup)

-- | The type of target that you must specify when registering targets with
-- this target group. The possible values are @instance@ (register targets
-- by instance ID), @ip@ (register targets by IP address), @lambda@
-- (register a single Lambda function as a target), or @alb@ (register a
-- single Application Load Balancer as a target).
targetGroup_targetType :: Lens.Lens' TargetGroup (Prelude.Maybe TargetTypeEnum)
targetGroup_targetType = Lens.lens (\TargetGroup' {targetType} -> targetType) (\s@TargetGroup' {} a -> s {targetType = a} :: TargetGroup)

-- | [HTTP\/HTTPS protocol] The protocol version. The possible values are
-- @GRPC@, @HTTP1@, and @HTTP2@.
targetGroup_protocolVersion :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Text)
targetGroup_protocolVersion = Lens.lens (\TargetGroup' {protocolVersion} -> protocolVersion) (\s@TargetGroup' {} a -> s {protocolVersion = a} :: TargetGroup)

-- | The port to use to connect with the target.
targetGroup_healthCheckPort :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Text)
targetGroup_healthCheckPort = Lens.lens (\TargetGroup' {healthCheckPort} -> healthCheckPort) (\s@TargetGroup' {} a -> s {healthCheckPort = a} :: TargetGroup)

-- | The protocol to use for routing traffic to the targets.
targetGroup_protocol :: Lens.Lens' TargetGroup (Prelude.Maybe ProtocolEnum)
targetGroup_protocol = Lens.lens (\TargetGroup' {protocol} -> protocol) (\s@TargetGroup' {} a -> s {protocol = a} :: TargetGroup)

-- | The ID of the VPC for the targets.
targetGroup_vpcId :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Text)
targetGroup_vpcId = Lens.lens (\TargetGroup' {vpcId} -> vpcId) (\s@TargetGroup' {} a -> s {vpcId = a} :: TargetGroup)

-- | The type of IP address used for this target group. The possible values
-- are @ipv4@ and @ipv6@. This is an optional parameter. If not specified,
-- the IP address type defaults to @ipv4@.
targetGroup_ipAddressType :: Lens.Lens' TargetGroup (Prelude.Maybe TargetGroupIpAddressTypeEnum)
targetGroup_ipAddressType = Lens.lens (\TargetGroup' {ipAddressType} -> ipAddressType) (\s@TargetGroup' {} a -> s {ipAddressType = a} :: TargetGroup)

-- | The HTTP or gRPC codes to use when checking for a successful response
-- from a target.
targetGroup_matcher :: Lens.Lens' TargetGroup (Prelude.Maybe Matcher)
targetGroup_matcher = Lens.lens (\TargetGroup' {matcher} -> matcher) (\s@TargetGroup' {} a -> s {matcher = a} :: TargetGroup)

instance Core.FromXML TargetGroup where
  parseXML x =
    TargetGroup'
      Prelude.<$> (x Core..@? "HealthCheckProtocol")
      Prelude.<*> (x Core..@? "Port")
      Prelude.<*> (x Core..@? "HealthCheckTimeoutSeconds")
      Prelude.<*> ( x Core..@? "LoadBalancerArns"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "HealthCheckPath")
      Prelude.<*> (x Core..@? "UnhealthyThresholdCount")
      Prelude.<*> (x Core..@? "HealthCheckEnabled")
      Prelude.<*> (x Core..@? "TargetGroupName")
      Prelude.<*> (x Core..@? "HealthCheckIntervalSeconds")
      Prelude.<*> (x Core..@? "HealthyThresholdCount")
      Prelude.<*> (x Core..@? "TargetGroupArn")
      Prelude.<*> (x Core..@? "TargetType")
      Prelude.<*> (x Core..@? "ProtocolVersion")
      Prelude.<*> (x Core..@? "HealthCheckPort")
      Prelude.<*> (x Core..@? "Protocol")
      Prelude.<*> (x Core..@? "VpcId")
      Prelude.<*> (x Core..@? "IpAddressType")
      Prelude.<*> (x Core..@? "Matcher")

instance Prelude.Hashable TargetGroup where
  hashWithSalt _salt TargetGroup' {..} =
    _salt `Prelude.hashWithSalt` healthCheckProtocol
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` healthCheckTimeoutSeconds
      `Prelude.hashWithSalt` loadBalancerArns
      `Prelude.hashWithSalt` healthCheckPath
      `Prelude.hashWithSalt` unhealthyThresholdCount
      `Prelude.hashWithSalt` healthCheckEnabled
      `Prelude.hashWithSalt` targetGroupName
      `Prelude.hashWithSalt` healthCheckIntervalSeconds
      `Prelude.hashWithSalt` healthyThresholdCount
      `Prelude.hashWithSalt` targetGroupArn
      `Prelude.hashWithSalt` targetType
      `Prelude.hashWithSalt` protocolVersion
      `Prelude.hashWithSalt` healthCheckPort
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` matcher

instance Prelude.NFData TargetGroup where
  rnf TargetGroup' {..} =
    Prelude.rnf healthCheckProtocol
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf healthCheckTimeoutSeconds
      `Prelude.seq` Prelude.rnf loadBalancerArns
      `Prelude.seq` Prelude.rnf healthCheckPath
      `Prelude.seq` Prelude.rnf unhealthyThresholdCount
      `Prelude.seq` Prelude.rnf healthCheckEnabled
      `Prelude.seq` Prelude.rnf targetGroupName
      `Prelude.seq` Prelude.rnf healthCheckIntervalSeconds
      `Prelude.seq` Prelude.rnf healthyThresholdCount
      `Prelude.seq` Prelude.rnf targetGroupArn
      `Prelude.seq` Prelude.rnf targetType
      `Prelude.seq` Prelude.rnf protocolVersion
      `Prelude.seq` Prelude.rnf healthCheckPort
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf matcher
