{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ELBv2.CreateTargetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a target group.
--
-- For more information, see the following:
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-target-groups.html Target groups for your Application Load Balancers>
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-target-groups.html Target groups for your Network Load Balancers>
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/target-groups.html Target groups for your Gateway Load Balancers>
--
-- This operation is idempotent, which means that it completes at most one
-- time. If you attempt to create multiple target groups with the same
-- settings, each call succeeds.
module Network.AWS.ELBv2.CreateTargetGroup
  ( -- * Creating a Request
    CreateTargetGroup (..),
    newCreateTargetGroup,

    -- * Request Lenses
    createTargetGroup_healthCheckEnabled,
    createTargetGroup_healthCheckProtocol,
    createTargetGroup_targetType,
    createTargetGroup_healthCheckPort,
    createTargetGroup_healthCheckTimeoutSeconds,
    createTargetGroup_healthCheckPath,
    createTargetGroup_matcher,
    createTargetGroup_protocolVersion,
    createTargetGroup_healthyThresholdCount,
    createTargetGroup_tags,
    createTargetGroup_port,
    createTargetGroup_healthCheckIntervalSeconds,
    createTargetGroup_protocol,
    createTargetGroup_vpcId,
    createTargetGroup_unhealthyThresholdCount,
    createTargetGroup_name,

    -- * Destructuring the Response
    CreateTargetGroupResponse (..),
    newCreateTargetGroupResponse,

    -- * Response Lenses
    createTargetGroupResponse_targetGroups,
    createTargetGroupResponse_httpStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTargetGroup' smart constructor.
data CreateTargetGroup = CreateTargetGroup'
  { -- | Indicates whether health checks are enabled. If the target type is
    -- @lambda@, health checks are disabled by default but can be enabled. If
    -- the target type is @instance@ or @ip@, health checks are always enabled
    -- and cannot be disabled.
    healthCheckEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The protocol the load balancer uses when performing health checks on
    -- targets. For Application Load Balancers, the default is HTTP. For
    -- Network Load Balancers and Gateway Load Balancers, the default is TCP.
    -- The TCP protocol is not supported for health checks if the protocol of
    -- the target group is HTTP or HTTPS. The GENEVE, TLS, UDP, and TCP_UDP
    -- protocols are not supported for health checks.
    healthCheckProtocol :: Prelude.Maybe ProtocolEnum,
    -- | The type of target that you must specify when registering targets with
    -- this target group. You can\'t specify targets for a target group using
    -- more than one target type.
    --
    -- -   @instance@ - Register targets by instance ID. This is the default
    --     value.
    --
    -- -   @ip@ - Register targets by IP address. You can specify IP addresses
    --     from the subnets of the virtual private cloud (VPC) for the target
    --     group, the RFC 1918 range (10.0.0.0\/8, 172.16.0.0\/12, and
    --     192.168.0.0\/16), and the RFC 6598 range (100.64.0.0\/10). You
    --     can\'t specify publicly routable IP addresses.
    --
    -- -   @lambda@ - Register a single Lambda function as a target.
    targetType :: Prelude.Maybe TargetTypeEnum,
    -- | The port the load balancer uses when performing health checks on
    -- targets. If the protocol is HTTP, HTTPS, TCP, TLS, UDP, or TCP_UDP, the
    -- default is @traffic-port@, which is the port on which each target
    -- receives traffic from the load balancer. If the protocol is GENEVE, the
    -- default is port 80.
    healthCheckPort :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, during which no response from a target
    -- means a failed health check. For target groups with a protocol of HTTP,
    -- HTTPS, or GENEVE, the default is 5 seconds. For target groups with a
    -- protocol of TCP or TLS, this value must be 6 seconds for HTTP health
    -- checks and 10 seconds for TCP and HTTPS health checks. If the target
    -- type is @lambda@, the default is 30 seconds.
    healthCheckTimeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | [HTTP\/HTTPS health checks] The destination for health checks on the
    -- targets.
    --
    -- [HTTP1 or HTTP2 protocol version] The ping path. The default is \/.
    --
    -- [GRPC protocol version] The path of a custom health check method with
    -- the format \/package.service\/method. The default is
    -- \/AWS.ALB\/healthcheck.
    healthCheckPath :: Prelude.Maybe Prelude.Text,
    -- | [HTTP\/HTTPS health checks] The HTTP or gRPC codes to use when checking
    -- for a successful response from a target.
    matcher :: Prelude.Maybe Matcher,
    -- | [HTTP\/HTTPS protocol] The protocol version. Specify @GRPC@ to send
    -- requests to targets using gRPC. Specify @HTTP2@ to send requests to
    -- targets using HTTP\/2. The default is @HTTP1@, which sends requests to
    -- targets using HTTP\/1.1.
    protocolVersion :: Prelude.Maybe Prelude.Text,
    -- | The number of consecutive health checks successes required before
    -- considering an unhealthy target healthy. For target groups with a
    -- protocol of HTTP or HTTPS, the default is 5. For target groups with a
    -- protocol of TCP, TLS, or GENEVE, the default is 3. If the target type is
    -- @lambda@, the default is 5.
    healthyThresholdCount :: Prelude.Maybe Prelude.Natural,
    -- | The tags to assign to the target group.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The port on which the targets receive traffic. This port is used unless
    -- you specify a port override when registering the target. If the target
    -- is a Lambda function, this parameter does not apply. If the protocol is
    -- GENEVE, the supported port is 6081.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The approximate amount of time, in seconds, between health checks of an
    -- individual target. If the target group protocol is TCP, TLS, UDP, or
    -- TCP_UDP, the supported values are 10 and 30 seconds. If the target group
    -- protocol is HTTP or HTTPS, the default is 30 seconds. If the target
    -- group protocol is GENEVE, the default is 10 seconds. If the target type
    -- is @lambda@, the default is 35 seconds.
    healthCheckIntervalSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The protocol to use for routing traffic to the targets. For Application
    -- Load Balancers, the supported protocols are HTTP and HTTPS. For Network
    -- Load Balancers, the supported protocols are TCP, TLS, UDP, or TCP_UDP.
    -- For Gateway Load Balancers, the supported protocol is GENEVE. A TCP_UDP
    -- listener must be associated with a TCP_UDP target group. If the target
    -- is a Lambda function, this parameter does not apply.
    protocol :: Prelude.Maybe ProtocolEnum,
    -- | The identifier of the virtual private cloud (VPC). If the target is a
    -- Lambda function, this parameter does not apply. Otherwise, this
    -- parameter is required.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The number of consecutive health check failures required before
    -- considering a target unhealthy. If the target group protocol is HTTP or
    -- HTTPS, the default is 2. If the target group protocol is TCP or TLS,
    -- this value must be the same as the healthy threshold count. If the
    -- target group protocol is GENEVE, the default is 3. If the target type is
    -- @lambda@, the default is 2.
    unhealthyThresholdCount :: Prelude.Maybe Prelude.Natural,
    -- | The name of the target group.
    --
    -- This name must be unique per region per account, can have a maximum of
    -- 32 characters, must contain only alphanumeric characters or hyphens, and
    -- must not begin or end with a hyphen.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateTargetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheckEnabled', 'createTargetGroup_healthCheckEnabled' - Indicates whether health checks are enabled. If the target type is
-- @lambda@, health checks are disabled by default but can be enabled. If
-- the target type is @instance@ or @ip@, health checks are always enabled
-- and cannot be disabled.
--
-- 'healthCheckProtocol', 'createTargetGroup_healthCheckProtocol' - The protocol the load balancer uses when performing health checks on
-- targets. For Application Load Balancers, the default is HTTP. For
-- Network Load Balancers and Gateway Load Balancers, the default is TCP.
-- The TCP protocol is not supported for health checks if the protocol of
-- the target group is HTTP or HTTPS. The GENEVE, TLS, UDP, and TCP_UDP
-- protocols are not supported for health checks.
--
-- 'targetType', 'createTargetGroup_targetType' - The type of target that you must specify when registering targets with
-- this target group. You can\'t specify targets for a target group using
-- more than one target type.
--
-- -   @instance@ - Register targets by instance ID. This is the default
--     value.
--
-- -   @ip@ - Register targets by IP address. You can specify IP addresses
--     from the subnets of the virtual private cloud (VPC) for the target
--     group, the RFC 1918 range (10.0.0.0\/8, 172.16.0.0\/12, and
--     192.168.0.0\/16), and the RFC 6598 range (100.64.0.0\/10). You
--     can\'t specify publicly routable IP addresses.
--
-- -   @lambda@ - Register a single Lambda function as a target.
--
-- 'healthCheckPort', 'createTargetGroup_healthCheckPort' - The port the load balancer uses when performing health checks on
-- targets. If the protocol is HTTP, HTTPS, TCP, TLS, UDP, or TCP_UDP, the
-- default is @traffic-port@, which is the port on which each target
-- receives traffic from the load balancer. If the protocol is GENEVE, the
-- default is port 80.
--
-- 'healthCheckTimeoutSeconds', 'createTargetGroup_healthCheckTimeoutSeconds' - The amount of time, in seconds, during which no response from a target
-- means a failed health check. For target groups with a protocol of HTTP,
-- HTTPS, or GENEVE, the default is 5 seconds. For target groups with a
-- protocol of TCP or TLS, this value must be 6 seconds for HTTP health
-- checks and 10 seconds for TCP and HTTPS health checks. If the target
-- type is @lambda@, the default is 30 seconds.
--
-- 'healthCheckPath', 'createTargetGroup_healthCheckPath' - [HTTP\/HTTPS health checks] The destination for health checks on the
-- targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is \/.
--
-- [GRPC protocol version] The path of a custom health check method with
-- the format \/package.service\/method. The default is
-- \/AWS.ALB\/healthcheck.
--
-- 'matcher', 'createTargetGroup_matcher' - [HTTP\/HTTPS health checks] The HTTP or gRPC codes to use when checking
-- for a successful response from a target.
--
-- 'protocolVersion', 'createTargetGroup_protocolVersion' - [HTTP\/HTTPS protocol] The protocol version. Specify @GRPC@ to send
-- requests to targets using gRPC. Specify @HTTP2@ to send requests to
-- targets using HTTP\/2. The default is @HTTP1@, which sends requests to
-- targets using HTTP\/1.1.
--
-- 'healthyThresholdCount', 'createTargetGroup_healthyThresholdCount' - The number of consecutive health checks successes required before
-- considering an unhealthy target healthy. For target groups with a
-- protocol of HTTP or HTTPS, the default is 5. For target groups with a
-- protocol of TCP, TLS, or GENEVE, the default is 3. If the target type is
-- @lambda@, the default is 5.
--
-- 'tags', 'createTargetGroup_tags' - The tags to assign to the target group.
--
-- 'port', 'createTargetGroup_port' - The port on which the targets receive traffic. This port is used unless
-- you specify a port override when registering the target. If the target
-- is a Lambda function, this parameter does not apply. If the protocol is
-- GENEVE, the supported port is 6081.
--
-- 'healthCheckIntervalSeconds', 'createTargetGroup_healthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an
-- individual target. If the target group protocol is TCP, TLS, UDP, or
-- TCP_UDP, the supported values are 10 and 30 seconds. If the target group
-- protocol is HTTP or HTTPS, the default is 30 seconds. If the target
-- group protocol is GENEVE, the default is 10 seconds. If the target type
-- is @lambda@, the default is 35 seconds.
--
-- 'protocol', 'createTargetGroup_protocol' - The protocol to use for routing traffic to the targets. For Application
-- Load Balancers, the supported protocols are HTTP and HTTPS. For Network
-- Load Balancers, the supported protocols are TCP, TLS, UDP, or TCP_UDP.
-- For Gateway Load Balancers, the supported protocol is GENEVE. A TCP_UDP
-- listener must be associated with a TCP_UDP target group. If the target
-- is a Lambda function, this parameter does not apply.
--
-- 'vpcId', 'createTargetGroup_vpcId' - The identifier of the virtual private cloud (VPC). If the target is a
-- Lambda function, this parameter does not apply. Otherwise, this
-- parameter is required.
--
-- 'unhealthyThresholdCount', 'createTargetGroup_unhealthyThresholdCount' - The number of consecutive health check failures required before
-- considering a target unhealthy. If the target group protocol is HTTP or
-- HTTPS, the default is 2. If the target group protocol is TCP or TLS,
-- this value must be the same as the healthy threshold count. If the
-- target group protocol is GENEVE, the default is 3. If the target type is
-- @lambda@, the default is 2.
--
-- 'name', 'createTargetGroup_name' - The name of the target group.
--
-- This name must be unique per region per account, can have a maximum of
-- 32 characters, must contain only alphanumeric characters or hyphens, and
-- must not begin or end with a hyphen.
newCreateTargetGroup ::
  -- | 'name'
  Prelude.Text ->
  CreateTargetGroup
newCreateTargetGroup pName_ =
  CreateTargetGroup'
    { healthCheckEnabled =
        Prelude.Nothing,
      healthCheckProtocol = Prelude.Nothing,
      targetType = Prelude.Nothing,
      healthCheckPort = Prelude.Nothing,
      healthCheckTimeoutSeconds = Prelude.Nothing,
      healthCheckPath = Prelude.Nothing,
      matcher = Prelude.Nothing,
      protocolVersion = Prelude.Nothing,
      healthyThresholdCount = Prelude.Nothing,
      tags = Prelude.Nothing,
      port = Prelude.Nothing,
      healthCheckIntervalSeconds = Prelude.Nothing,
      protocol = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      unhealthyThresholdCount = Prelude.Nothing,
      name = pName_
    }

-- | Indicates whether health checks are enabled. If the target type is
-- @lambda@, health checks are disabled by default but can be enabled. If
-- the target type is @instance@ or @ip@, health checks are always enabled
-- and cannot be disabled.
createTargetGroup_healthCheckEnabled :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Bool)
createTargetGroup_healthCheckEnabled = Lens.lens (\CreateTargetGroup' {healthCheckEnabled} -> healthCheckEnabled) (\s@CreateTargetGroup' {} a -> s {healthCheckEnabled = a} :: CreateTargetGroup)

-- | The protocol the load balancer uses when performing health checks on
-- targets. For Application Load Balancers, the default is HTTP. For
-- Network Load Balancers and Gateway Load Balancers, the default is TCP.
-- The TCP protocol is not supported for health checks if the protocol of
-- the target group is HTTP or HTTPS. The GENEVE, TLS, UDP, and TCP_UDP
-- protocols are not supported for health checks.
createTargetGroup_healthCheckProtocol :: Lens.Lens' CreateTargetGroup (Prelude.Maybe ProtocolEnum)
createTargetGroup_healthCheckProtocol = Lens.lens (\CreateTargetGroup' {healthCheckProtocol} -> healthCheckProtocol) (\s@CreateTargetGroup' {} a -> s {healthCheckProtocol = a} :: CreateTargetGroup)

-- | The type of target that you must specify when registering targets with
-- this target group. You can\'t specify targets for a target group using
-- more than one target type.
--
-- -   @instance@ - Register targets by instance ID. This is the default
--     value.
--
-- -   @ip@ - Register targets by IP address. You can specify IP addresses
--     from the subnets of the virtual private cloud (VPC) for the target
--     group, the RFC 1918 range (10.0.0.0\/8, 172.16.0.0\/12, and
--     192.168.0.0\/16), and the RFC 6598 range (100.64.0.0\/10). You
--     can\'t specify publicly routable IP addresses.
--
-- -   @lambda@ - Register a single Lambda function as a target.
createTargetGroup_targetType :: Lens.Lens' CreateTargetGroup (Prelude.Maybe TargetTypeEnum)
createTargetGroup_targetType = Lens.lens (\CreateTargetGroup' {targetType} -> targetType) (\s@CreateTargetGroup' {} a -> s {targetType = a} :: CreateTargetGroup)

-- | The port the load balancer uses when performing health checks on
-- targets. If the protocol is HTTP, HTTPS, TCP, TLS, UDP, or TCP_UDP, the
-- default is @traffic-port@, which is the port on which each target
-- receives traffic from the load balancer. If the protocol is GENEVE, the
-- default is port 80.
createTargetGroup_healthCheckPort :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Text)
createTargetGroup_healthCheckPort = Lens.lens (\CreateTargetGroup' {healthCheckPort} -> healthCheckPort) (\s@CreateTargetGroup' {} a -> s {healthCheckPort = a} :: CreateTargetGroup)

-- | The amount of time, in seconds, during which no response from a target
-- means a failed health check. For target groups with a protocol of HTTP,
-- HTTPS, or GENEVE, the default is 5 seconds. For target groups with a
-- protocol of TCP or TLS, this value must be 6 seconds for HTTP health
-- checks and 10 seconds for TCP and HTTPS health checks. If the target
-- type is @lambda@, the default is 30 seconds.
createTargetGroup_healthCheckTimeoutSeconds :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Natural)
createTargetGroup_healthCheckTimeoutSeconds = Lens.lens (\CreateTargetGroup' {healthCheckTimeoutSeconds} -> healthCheckTimeoutSeconds) (\s@CreateTargetGroup' {} a -> s {healthCheckTimeoutSeconds = a} :: CreateTargetGroup)

-- | [HTTP\/HTTPS health checks] The destination for health checks on the
-- targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is \/.
--
-- [GRPC protocol version] The path of a custom health check method with
-- the format \/package.service\/method. The default is
-- \/AWS.ALB\/healthcheck.
createTargetGroup_healthCheckPath :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Text)
createTargetGroup_healthCheckPath = Lens.lens (\CreateTargetGroup' {healthCheckPath} -> healthCheckPath) (\s@CreateTargetGroup' {} a -> s {healthCheckPath = a} :: CreateTargetGroup)

-- | [HTTP\/HTTPS health checks] The HTTP or gRPC codes to use when checking
-- for a successful response from a target.
createTargetGroup_matcher :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Matcher)
createTargetGroup_matcher = Lens.lens (\CreateTargetGroup' {matcher} -> matcher) (\s@CreateTargetGroup' {} a -> s {matcher = a} :: CreateTargetGroup)

-- | [HTTP\/HTTPS protocol] The protocol version. Specify @GRPC@ to send
-- requests to targets using gRPC. Specify @HTTP2@ to send requests to
-- targets using HTTP\/2. The default is @HTTP1@, which sends requests to
-- targets using HTTP\/1.1.
createTargetGroup_protocolVersion :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Text)
createTargetGroup_protocolVersion = Lens.lens (\CreateTargetGroup' {protocolVersion} -> protocolVersion) (\s@CreateTargetGroup' {} a -> s {protocolVersion = a} :: CreateTargetGroup)

-- | The number of consecutive health checks successes required before
-- considering an unhealthy target healthy. For target groups with a
-- protocol of HTTP or HTTPS, the default is 5. For target groups with a
-- protocol of TCP, TLS, or GENEVE, the default is 3. If the target type is
-- @lambda@, the default is 5.
createTargetGroup_healthyThresholdCount :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Natural)
createTargetGroup_healthyThresholdCount = Lens.lens (\CreateTargetGroup' {healthyThresholdCount} -> healthyThresholdCount) (\s@CreateTargetGroup' {} a -> s {healthyThresholdCount = a} :: CreateTargetGroup)

-- | The tags to assign to the target group.
createTargetGroup_tags :: Lens.Lens' CreateTargetGroup (Prelude.Maybe (Prelude.NonEmpty Tag))
createTargetGroup_tags = Lens.lens (\CreateTargetGroup' {tags} -> tags) (\s@CreateTargetGroup' {} a -> s {tags = a} :: CreateTargetGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The port on which the targets receive traffic. This port is used unless
-- you specify a port override when registering the target. If the target
-- is a Lambda function, this parameter does not apply. If the protocol is
-- GENEVE, the supported port is 6081.
createTargetGroup_port :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Natural)
createTargetGroup_port = Lens.lens (\CreateTargetGroup' {port} -> port) (\s@CreateTargetGroup' {} a -> s {port = a} :: CreateTargetGroup)

-- | The approximate amount of time, in seconds, between health checks of an
-- individual target. If the target group protocol is TCP, TLS, UDP, or
-- TCP_UDP, the supported values are 10 and 30 seconds. If the target group
-- protocol is HTTP or HTTPS, the default is 30 seconds. If the target
-- group protocol is GENEVE, the default is 10 seconds. If the target type
-- is @lambda@, the default is 35 seconds.
createTargetGroup_healthCheckIntervalSeconds :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Natural)
createTargetGroup_healthCheckIntervalSeconds = Lens.lens (\CreateTargetGroup' {healthCheckIntervalSeconds} -> healthCheckIntervalSeconds) (\s@CreateTargetGroup' {} a -> s {healthCheckIntervalSeconds = a} :: CreateTargetGroup)

-- | The protocol to use for routing traffic to the targets. For Application
-- Load Balancers, the supported protocols are HTTP and HTTPS. For Network
-- Load Balancers, the supported protocols are TCP, TLS, UDP, or TCP_UDP.
-- For Gateway Load Balancers, the supported protocol is GENEVE. A TCP_UDP
-- listener must be associated with a TCP_UDP target group. If the target
-- is a Lambda function, this parameter does not apply.
createTargetGroup_protocol :: Lens.Lens' CreateTargetGroup (Prelude.Maybe ProtocolEnum)
createTargetGroup_protocol = Lens.lens (\CreateTargetGroup' {protocol} -> protocol) (\s@CreateTargetGroup' {} a -> s {protocol = a} :: CreateTargetGroup)

-- | The identifier of the virtual private cloud (VPC). If the target is a
-- Lambda function, this parameter does not apply. Otherwise, this
-- parameter is required.
createTargetGroup_vpcId :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Text)
createTargetGroup_vpcId = Lens.lens (\CreateTargetGroup' {vpcId} -> vpcId) (\s@CreateTargetGroup' {} a -> s {vpcId = a} :: CreateTargetGroup)

-- | The number of consecutive health check failures required before
-- considering a target unhealthy. If the target group protocol is HTTP or
-- HTTPS, the default is 2. If the target group protocol is TCP or TLS,
-- this value must be the same as the healthy threshold count. If the
-- target group protocol is GENEVE, the default is 3. If the target type is
-- @lambda@, the default is 2.
createTargetGroup_unhealthyThresholdCount :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Natural)
createTargetGroup_unhealthyThresholdCount = Lens.lens (\CreateTargetGroup' {unhealthyThresholdCount} -> unhealthyThresholdCount) (\s@CreateTargetGroup' {} a -> s {unhealthyThresholdCount = a} :: CreateTargetGroup)

-- | The name of the target group.
--
-- This name must be unique per region per account, can have a maximum of
-- 32 characters, must contain only alphanumeric characters or hyphens, and
-- must not begin or end with a hyphen.
createTargetGroup_name :: Lens.Lens' CreateTargetGroup Prelude.Text
createTargetGroup_name = Lens.lens (\CreateTargetGroup' {name} -> name) (\s@CreateTargetGroup' {} a -> s {name = a} :: CreateTargetGroup)

instance Prelude.AWSRequest CreateTargetGroup where
  type Rs CreateTargetGroup = CreateTargetGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateTargetGroupResult"
      ( \s h x ->
          CreateTargetGroupResponse'
            Prelude.<$> ( x Prelude..@? "TargetGroups"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTargetGroup

instance Prelude.NFData CreateTargetGroup

instance Prelude.ToHeaders CreateTargetGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateTargetGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateTargetGroup where
  toQuery CreateTargetGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateTargetGroup" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-12-01" :: Prelude.ByteString),
        "HealthCheckEnabled" Prelude.=: healthCheckEnabled,
        "HealthCheckProtocol" Prelude.=: healthCheckProtocol,
        "TargetType" Prelude.=: targetType,
        "HealthCheckPort" Prelude.=: healthCheckPort,
        "HealthCheckTimeoutSeconds"
          Prelude.=: healthCheckTimeoutSeconds,
        "HealthCheckPath" Prelude.=: healthCheckPath,
        "Matcher" Prelude.=: matcher,
        "ProtocolVersion" Prelude.=: protocolVersion,
        "HealthyThresholdCount"
          Prelude.=: healthyThresholdCount,
        "Tags"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> tags),
        "Port" Prelude.=: port,
        "HealthCheckIntervalSeconds"
          Prelude.=: healthCheckIntervalSeconds,
        "Protocol" Prelude.=: protocol,
        "VpcId" Prelude.=: vpcId,
        "UnhealthyThresholdCount"
          Prelude.=: unhealthyThresholdCount,
        "Name" Prelude.=: name
      ]

-- | /See:/ 'newCreateTargetGroupResponse' smart constructor.
data CreateTargetGroupResponse = CreateTargetGroupResponse'
  { -- | Information about the target group.
    targetGroups :: Prelude.Maybe [TargetGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateTargetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroups', 'createTargetGroupResponse_targetGroups' - Information about the target group.
--
-- 'httpStatus', 'createTargetGroupResponse_httpStatus' - The response's http status code.
newCreateTargetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTargetGroupResponse
newCreateTargetGroupResponse pHttpStatus_ =
  CreateTargetGroupResponse'
    { targetGroups =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the target group.
createTargetGroupResponse_targetGroups :: Lens.Lens' CreateTargetGroupResponse (Prelude.Maybe [TargetGroup])
createTargetGroupResponse_targetGroups = Lens.lens (\CreateTargetGroupResponse' {targetGroups} -> targetGroups) (\s@CreateTargetGroupResponse' {} a -> s {targetGroups = a} :: CreateTargetGroupResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
createTargetGroupResponse_httpStatus :: Lens.Lens' CreateTargetGroupResponse Prelude.Int
createTargetGroupResponse_httpStatus = Lens.lens (\CreateTargetGroupResponse' {httpStatus} -> httpStatus) (\s@CreateTargetGroupResponse' {} a -> s {httpStatus = a} :: CreateTargetGroupResponse)

instance Prelude.NFData CreateTargetGroupResponse
