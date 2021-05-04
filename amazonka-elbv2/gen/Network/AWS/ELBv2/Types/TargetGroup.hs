{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ELBv2.Types.TargetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetGroup where

import Network.AWS.ELBv2.Types.Matcher
import Network.AWS.ELBv2.Types.ProtocolEnum
import Network.AWS.ELBv2.Types.TargetTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a target group.
--
-- /See:/ 'newTargetGroup' smart constructor.
data TargetGroup = TargetGroup'
  { -- | Indicates whether health checks are enabled.
    healthCheckEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The protocol to use to connect with the target. The GENEVE, TLS, UDP,
    -- and TCP_UDP protocols are not supported for health checks.
    healthCheckProtocol :: Prelude.Maybe ProtocolEnum,
    -- | The name of the target group.
    targetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The type of target that you must specify when registering targets with
    -- this target group. The possible values are @instance@ (register targets
    -- by instance ID), @ip@ (register targets by IP address), or @lambda@
    -- (register a single Lambda function as a target).
    targetType :: Prelude.Maybe TargetTypeEnum,
    -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The port to use to connect with the target.
    healthCheckPort :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, during which no response means a failed
    -- health check.
    healthCheckTimeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The destination for health checks on the targets.
    healthCheckPath :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARN) of the load balancers that route traffic
    -- to this target group.
    loadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The HTTP or gRPC codes to use when checking for a successful response
    -- from a target.
    matcher :: Prelude.Maybe Matcher,
    -- | [HTTP\/HTTPS protocol] The protocol version. The possible values are
    -- @GRPC@, @HTTP1@, and @HTTP2@.
    protocolVersion :: Prelude.Maybe Prelude.Text,
    -- | The number of consecutive health checks successes required before
    -- considering an unhealthy target healthy.
    healthyThresholdCount :: Prelude.Maybe Prelude.Natural,
    -- | The port on which the targets are listening. Not used if the target is a
    -- Lambda function.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The approximate amount of time, in seconds, between health checks of an
    -- individual target.
    healthCheckIntervalSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The protocol to use for routing traffic to the targets.
    protocol :: Prelude.Maybe ProtocolEnum,
    -- | The ID of the VPC for the targets.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The number of consecutive health check failures required before
    -- considering the target unhealthy.
    unhealthyThresholdCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TargetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheckEnabled', 'targetGroup_healthCheckEnabled' - Indicates whether health checks are enabled.
--
-- 'healthCheckProtocol', 'targetGroup_healthCheckProtocol' - The protocol to use to connect with the target. The GENEVE, TLS, UDP,
-- and TCP_UDP protocols are not supported for health checks.
--
-- 'targetGroupName', 'targetGroup_targetGroupName' - The name of the target group.
--
-- 'targetType', 'targetGroup_targetType' - The type of target that you must specify when registering targets with
-- this target group. The possible values are @instance@ (register targets
-- by instance ID), @ip@ (register targets by IP address), or @lambda@
-- (register a single Lambda function as a target).
--
-- 'targetGroupArn', 'targetGroup_targetGroupArn' - The Amazon Resource Name (ARN) of the target group.
--
-- 'healthCheckPort', 'targetGroup_healthCheckPort' - The port to use to connect with the target.
--
-- 'healthCheckTimeoutSeconds', 'targetGroup_healthCheckTimeoutSeconds' - The amount of time, in seconds, during which no response means a failed
-- health check.
--
-- 'healthCheckPath', 'targetGroup_healthCheckPath' - The destination for health checks on the targets.
--
-- 'loadBalancerArns', 'targetGroup_loadBalancerArns' - The Amazon Resource Names (ARN) of the load balancers that route traffic
-- to this target group.
--
-- 'matcher', 'targetGroup_matcher' - The HTTP or gRPC codes to use when checking for a successful response
-- from a target.
--
-- 'protocolVersion', 'targetGroup_protocolVersion' - [HTTP\/HTTPS protocol] The protocol version. The possible values are
-- @GRPC@, @HTTP1@, and @HTTP2@.
--
-- 'healthyThresholdCount', 'targetGroup_healthyThresholdCount' - The number of consecutive health checks successes required before
-- considering an unhealthy target healthy.
--
-- 'port', 'targetGroup_port' - The port on which the targets are listening. Not used if the target is a
-- Lambda function.
--
-- 'healthCheckIntervalSeconds', 'targetGroup_healthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an
-- individual target.
--
-- 'protocol', 'targetGroup_protocol' - The protocol to use for routing traffic to the targets.
--
-- 'vpcId', 'targetGroup_vpcId' - The ID of the VPC for the targets.
--
-- 'unhealthyThresholdCount', 'targetGroup_unhealthyThresholdCount' - The number of consecutive health check failures required before
-- considering the target unhealthy.
newTargetGroup ::
  TargetGroup
newTargetGroup =
  TargetGroup'
    { healthCheckEnabled = Prelude.Nothing,
      healthCheckProtocol = Prelude.Nothing,
      targetGroupName = Prelude.Nothing,
      targetType = Prelude.Nothing,
      targetGroupArn = Prelude.Nothing,
      healthCheckPort = Prelude.Nothing,
      healthCheckTimeoutSeconds = Prelude.Nothing,
      healthCheckPath = Prelude.Nothing,
      loadBalancerArns = Prelude.Nothing,
      matcher = Prelude.Nothing,
      protocolVersion = Prelude.Nothing,
      healthyThresholdCount = Prelude.Nothing,
      port = Prelude.Nothing,
      healthCheckIntervalSeconds = Prelude.Nothing,
      protocol = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      unhealthyThresholdCount = Prelude.Nothing
    }

-- | Indicates whether health checks are enabled.
targetGroup_healthCheckEnabled :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Bool)
targetGroup_healthCheckEnabled = Lens.lens (\TargetGroup' {healthCheckEnabled} -> healthCheckEnabled) (\s@TargetGroup' {} a -> s {healthCheckEnabled = a} :: TargetGroup)

-- | The protocol to use to connect with the target. The GENEVE, TLS, UDP,
-- and TCP_UDP protocols are not supported for health checks.
targetGroup_healthCheckProtocol :: Lens.Lens' TargetGroup (Prelude.Maybe ProtocolEnum)
targetGroup_healthCheckProtocol = Lens.lens (\TargetGroup' {healthCheckProtocol} -> healthCheckProtocol) (\s@TargetGroup' {} a -> s {healthCheckProtocol = a} :: TargetGroup)

-- | The name of the target group.
targetGroup_targetGroupName :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Text)
targetGroup_targetGroupName = Lens.lens (\TargetGroup' {targetGroupName} -> targetGroupName) (\s@TargetGroup' {} a -> s {targetGroupName = a} :: TargetGroup)

-- | The type of target that you must specify when registering targets with
-- this target group. The possible values are @instance@ (register targets
-- by instance ID), @ip@ (register targets by IP address), or @lambda@
-- (register a single Lambda function as a target).
targetGroup_targetType :: Lens.Lens' TargetGroup (Prelude.Maybe TargetTypeEnum)
targetGroup_targetType = Lens.lens (\TargetGroup' {targetType} -> targetType) (\s@TargetGroup' {} a -> s {targetType = a} :: TargetGroup)

-- | The Amazon Resource Name (ARN) of the target group.
targetGroup_targetGroupArn :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Text)
targetGroup_targetGroupArn = Lens.lens (\TargetGroup' {targetGroupArn} -> targetGroupArn) (\s@TargetGroup' {} a -> s {targetGroupArn = a} :: TargetGroup)

-- | The port to use to connect with the target.
targetGroup_healthCheckPort :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Text)
targetGroup_healthCheckPort = Lens.lens (\TargetGroup' {healthCheckPort} -> healthCheckPort) (\s@TargetGroup' {} a -> s {healthCheckPort = a} :: TargetGroup)

-- | The amount of time, in seconds, during which no response means a failed
-- health check.
targetGroup_healthCheckTimeoutSeconds :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Natural)
targetGroup_healthCheckTimeoutSeconds = Lens.lens (\TargetGroup' {healthCheckTimeoutSeconds} -> healthCheckTimeoutSeconds) (\s@TargetGroup' {} a -> s {healthCheckTimeoutSeconds = a} :: TargetGroup)

-- | The destination for health checks on the targets.
targetGroup_healthCheckPath :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Text)
targetGroup_healthCheckPath = Lens.lens (\TargetGroup' {healthCheckPath} -> healthCheckPath) (\s@TargetGroup' {} a -> s {healthCheckPath = a} :: TargetGroup)

-- | The Amazon Resource Names (ARN) of the load balancers that route traffic
-- to this target group.
targetGroup_loadBalancerArns :: Lens.Lens' TargetGroup (Prelude.Maybe [Prelude.Text])
targetGroup_loadBalancerArns = Lens.lens (\TargetGroup' {loadBalancerArns} -> loadBalancerArns) (\s@TargetGroup' {} a -> s {loadBalancerArns = a} :: TargetGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The HTTP or gRPC codes to use when checking for a successful response
-- from a target.
targetGroup_matcher :: Lens.Lens' TargetGroup (Prelude.Maybe Matcher)
targetGroup_matcher = Lens.lens (\TargetGroup' {matcher} -> matcher) (\s@TargetGroup' {} a -> s {matcher = a} :: TargetGroup)

-- | [HTTP\/HTTPS protocol] The protocol version. The possible values are
-- @GRPC@, @HTTP1@, and @HTTP2@.
targetGroup_protocolVersion :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Text)
targetGroup_protocolVersion = Lens.lens (\TargetGroup' {protocolVersion} -> protocolVersion) (\s@TargetGroup' {} a -> s {protocolVersion = a} :: TargetGroup)

-- | The number of consecutive health checks successes required before
-- considering an unhealthy target healthy.
targetGroup_healthyThresholdCount :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Natural)
targetGroup_healthyThresholdCount = Lens.lens (\TargetGroup' {healthyThresholdCount} -> healthyThresholdCount) (\s@TargetGroup' {} a -> s {healthyThresholdCount = a} :: TargetGroup)

-- | The port on which the targets are listening. Not used if the target is a
-- Lambda function.
targetGroup_port :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Natural)
targetGroup_port = Lens.lens (\TargetGroup' {port} -> port) (\s@TargetGroup' {} a -> s {port = a} :: TargetGroup)

-- | The approximate amount of time, in seconds, between health checks of an
-- individual target.
targetGroup_healthCheckIntervalSeconds :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Natural)
targetGroup_healthCheckIntervalSeconds = Lens.lens (\TargetGroup' {healthCheckIntervalSeconds} -> healthCheckIntervalSeconds) (\s@TargetGroup' {} a -> s {healthCheckIntervalSeconds = a} :: TargetGroup)

-- | The protocol to use for routing traffic to the targets.
targetGroup_protocol :: Lens.Lens' TargetGroup (Prelude.Maybe ProtocolEnum)
targetGroup_protocol = Lens.lens (\TargetGroup' {protocol} -> protocol) (\s@TargetGroup' {} a -> s {protocol = a} :: TargetGroup)

-- | The ID of the VPC for the targets.
targetGroup_vpcId :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Text)
targetGroup_vpcId = Lens.lens (\TargetGroup' {vpcId} -> vpcId) (\s@TargetGroup' {} a -> s {vpcId = a} :: TargetGroup)

-- | The number of consecutive health check failures required before
-- considering the target unhealthy.
targetGroup_unhealthyThresholdCount :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Natural)
targetGroup_unhealthyThresholdCount = Lens.lens (\TargetGroup' {unhealthyThresholdCount} -> unhealthyThresholdCount) (\s@TargetGroup' {} a -> s {unhealthyThresholdCount = a} :: TargetGroup)

instance Prelude.FromXML TargetGroup where
  parseXML x =
    TargetGroup'
      Prelude.<$> (x Prelude..@? "HealthCheckEnabled")
      Prelude.<*> (x Prelude..@? "HealthCheckProtocol")
      Prelude.<*> (x Prelude..@? "TargetGroupName")
      Prelude.<*> (x Prelude..@? "TargetType")
      Prelude.<*> (x Prelude..@? "TargetGroupArn")
      Prelude.<*> (x Prelude..@? "HealthCheckPort")
      Prelude.<*> (x Prelude..@? "HealthCheckTimeoutSeconds")
      Prelude.<*> (x Prelude..@? "HealthCheckPath")
      Prelude.<*> ( x Prelude..@? "LoadBalancerArns"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "Matcher")
      Prelude.<*> (x Prelude..@? "ProtocolVersion")
      Prelude.<*> (x Prelude..@? "HealthyThresholdCount")
      Prelude.<*> (x Prelude..@? "Port")
      Prelude.<*> (x Prelude..@? "HealthCheckIntervalSeconds")
      Prelude.<*> (x Prelude..@? "Protocol")
      Prelude.<*> (x Prelude..@? "VpcId")
      Prelude.<*> (x Prelude..@? "UnhealthyThresholdCount")

instance Prelude.Hashable TargetGroup

instance Prelude.NFData TargetGroup
