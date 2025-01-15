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
-- Module      : Amazonka.ELBV2.CreateTargetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.ELBV2.CreateTargetGroup
  ( -- * Creating a Request
    CreateTargetGroup (..),
    newCreateTargetGroup,

    -- * Request Lenses
    createTargetGroup_healthCheckEnabled,
    createTargetGroup_healthCheckIntervalSeconds,
    createTargetGroup_healthCheckPath,
    createTargetGroup_healthCheckPort,
    createTargetGroup_healthCheckProtocol,
    createTargetGroup_healthCheckTimeoutSeconds,
    createTargetGroup_healthyThresholdCount,
    createTargetGroup_ipAddressType,
    createTargetGroup_matcher,
    createTargetGroup_port,
    createTargetGroup_protocol,
    createTargetGroup_protocolVersion,
    createTargetGroup_tags,
    createTargetGroup_targetType,
    createTargetGroup_unhealthyThresholdCount,
    createTargetGroup_vpcId,
    createTargetGroup_name,

    -- * Destructuring the Response
    CreateTargetGroupResponse (..),
    newCreateTargetGroupResponse,

    -- * Response Lenses
    createTargetGroupResponse_targetGroups,
    createTargetGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTargetGroup' smart constructor.
data CreateTargetGroup = CreateTargetGroup'
  { -- | Indicates whether health checks are enabled. If the target type is
    -- @lambda@, health checks are disabled by default but can be enabled. If
    -- the target type is @instance@, @ip@, or @alb@, health checks are always
    -- enabled and cannot be disabled.
    healthCheckEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The approximate amount of time, in seconds, between health checks of an
    -- individual target. The range is 5-300. If the target group protocol is
    -- TCP, TLS, UDP, TCP_UDP, HTTP or HTTPS, the default is 30 seconds. If the
    -- target group protocol is GENEVE, the default is 10 seconds. If the
    -- target type is @lambda@, the default is 35 seconds.
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
    -- targets. If the protocol is HTTP, HTTPS, TCP, TLS, UDP, or TCP_UDP, the
    -- default is @traffic-port@, which is the port on which each target
    -- receives traffic from the load balancer. If the protocol is GENEVE, the
    -- default is port 80.
    healthCheckPort :: Prelude.Maybe Prelude.Text,
    -- | The protocol the load balancer uses when performing health checks on
    -- targets. For Application Load Balancers, the default is HTTP. For
    -- Network Load Balancers and Gateway Load Balancers, the default is TCP.
    -- The TCP protocol is not supported for health checks if the protocol of
    -- the target group is HTTP or HTTPS. The GENEVE, TLS, UDP, and TCP_UDP
    -- protocols are not supported for health checks.
    healthCheckProtocol :: Prelude.Maybe ProtocolEnum,
    -- | The amount of time, in seconds, during which no response from a target
    -- means a failed health check. The range is 2–120 seconds. For target
    -- groups with a protocol of HTTP, the default is 6 seconds. For target
    -- groups with a protocol of TCP, TLS or HTTPS, the default is 10 seconds.
    -- For target groups with a protocol of GENEVE, the default is 5 seconds.
    -- If the target type is @lambda@, the default is 30 seconds.
    healthCheckTimeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The number of consecutive health check successes required before
    -- considering a target healthy. The range is 2-10. If the target group
    -- protocol is TCP, TCP_UDP, UDP, TLS, HTTP or HTTPS, the default is 5. For
    -- target groups with a protocol of GENEVE, the default is 3. If the target
    -- type is @lambda@, the default is 5.
    healthyThresholdCount :: Prelude.Maybe Prelude.Natural,
    -- | The type of IP address used for this target group. The possible values
    -- are @ipv4@ and @ipv6@. This is an optional parameter. If not specified,
    -- the IP address type defaults to @ipv4@.
    ipAddressType :: Prelude.Maybe TargetGroupIpAddressTypeEnum,
    -- | [HTTP\/HTTPS health checks] The HTTP or gRPC codes to use when checking
    -- for a successful response from a target. For target groups with a
    -- protocol of TCP, TCP_UDP, UDP or TLS the range is 200-599. For target
    -- groups with a protocol of HTTP or HTTPS, the range is 200-499. For
    -- target groups with a protocol of GENEVE, the range is 200-399.
    matcher :: Prelude.Maybe Matcher,
    -- | The port on which the targets receive traffic. This port is used unless
    -- you specify a port override when registering the target. If the target
    -- is a Lambda function, this parameter does not apply. If the protocol is
    -- GENEVE, the supported port is 6081.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The protocol to use for routing traffic to the targets. For Application
    -- Load Balancers, the supported protocols are HTTP and HTTPS. For Network
    -- Load Balancers, the supported protocols are TCP, TLS, UDP, or TCP_UDP.
    -- For Gateway Load Balancers, the supported protocol is GENEVE. A TCP_UDP
    -- listener must be associated with a TCP_UDP target group. If the target
    -- is a Lambda function, this parameter does not apply.
    protocol :: Prelude.Maybe ProtocolEnum,
    -- | [HTTP\/HTTPS protocol] The protocol version. Specify @GRPC@ to send
    -- requests to targets using gRPC. Specify @HTTP2@ to send requests to
    -- targets using HTTP\/2. The default is @HTTP1@, which sends requests to
    -- targets using HTTP\/1.1.
    protocolVersion :: Prelude.Maybe Prelude.Text,
    -- | The tags to assign to the target group.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
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
    --
    -- -   @alb@ - Register a single Application Load Balancer as a target.
    targetType :: Prelude.Maybe TargetTypeEnum,
    -- | The number of consecutive health check failures required before
    -- considering a target unhealthy. The range is 2-10. If the target group
    -- protocol is TCP, TCP_UDP, UDP, TLS, HTTP or HTTPS, the default is 2. For
    -- target groups with a protocol of GENEVE, the default is 3. If the target
    -- type is @lambda@, the default is 5.
    unhealthyThresholdCount :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the virtual private cloud (VPC). If the target is a
    -- Lambda function, this parameter does not apply. Otherwise, this
    -- parameter is required.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The name of the target group.
    --
    -- This name must be unique per region per account, can have a maximum of
    -- 32 characters, must contain only alphanumeric characters or hyphens, and
    -- must not begin or end with a hyphen.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- the target type is @instance@, @ip@, or @alb@, health checks are always
-- enabled and cannot be disabled.
--
-- 'healthCheckIntervalSeconds', 'createTargetGroup_healthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an
-- individual target. The range is 5-300. If the target group protocol is
-- TCP, TLS, UDP, TCP_UDP, HTTP or HTTPS, the default is 30 seconds. If the
-- target group protocol is GENEVE, the default is 10 seconds. If the
-- target type is @lambda@, the default is 35 seconds.
--
-- 'healthCheckPath', 'createTargetGroup_healthCheckPath' - [HTTP\/HTTPS health checks] The destination for health checks on the
-- targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is \/.
--
-- [GRPC protocol version] The path of a custom health check method with
-- the format \/package.service\/method. The default is \/Amazon Web
-- Services.ALB\/healthcheck.
--
-- 'healthCheckPort', 'createTargetGroup_healthCheckPort' - The port the load balancer uses when performing health checks on
-- targets. If the protocol is HTTP, HTTPS, TCP, TLS, UDP, or TCP_UDP, the
-- default is @traffic-port@, which is the port on which each target
-- receives traffic from the load balancer. If the protocol is GENEVE, the
-- default is port 80.
--
-- 'healthCheckProtocol', 'createTargetGroup_healthCheckProtocol' - The protocol the load balancer uses when performing health checks on
-- targets. For Application Load Balancers, the default is HTTP. For
-- Network Load Balancers and Gateway Load Balancers, the default is TCP.
-- The TCP protocol is not supported for health checks if the protocol of
-- the target group is HTTP or HTTPS. The GENEVE, TLS, UDP, and TCP_UDP
-- protocols are not supported for health checks.
--
-- 'healthCheckTimeoutSeconds', 'createTargetGroup_healthCheckTimeoutSeconds' - The amount of time, in seconds, during which no response from a target
-- means a failed health check. The range is 2–120 seconds. For target
-- groups with a protocol of HTTP, the default is 6 seconds. For target
-- groups with a protocol of TCP, TLS or HTTPS, the default is 10 seconds.
-- For target groups with a protocol of GENEVE, the default is 5 seconds.
-- If the target type is @lambda@, the default is 30 seconds.
--
-- 'healthyThresholdCount', 'createTargetGroup_healthyThresholdCount' - The number of consecutive health check successes required before
-- considering a target healthy. The range is 2-10. If the target group
-- protocol is TCP, TCP_UDP, UDP, TLS, HTTP or HTTPS, the default is 5. For
-- target groups with a protocol of GENEVE, the default is 3. If the target
-- type is @lambda@, the default is 5.
--
-- 'ipAddressType', 'createTargetGroup_ipAddressType' - The type of IP address used for this target group. The possible values
-- are @ipv4@ and @ipv6@. This is an optional parameter. If not specified,
-- the IP address type defaults to @ipv4@.
--
-- 'matcher', 'createTargetGroup_matcher' - [HTTP\/HTTPS health checks] The HTTP or gRPC codes to use when checking
-- for a successful response from a target. For target groups with a
-- protocol of TCP, TCP_UDP, UDP or TLS the range is 200-599. For target
-- groups with a protocol of HTTP or HTTPS, the range is 200-499. For
-- target groups with a protocol of GENEVE, the range is 200-399.
--
-- 'port', 'createTargetGroup_port' - The port on which the targets receive traffic. This port is used unless
-- you specify a port override when registering the target. If the target
-- is a Lambda function, this parameter does not apply. If the protocol is
-- GENEVE, the supported port is 6081.
--
-- 'protocol', 'createTargetGroup_protocol' - The protocol to use for routing traffic to the targets. For Application
-- Load Balancers, the supported protocols are HTTP and HTTPS. For Network
-- Load Balancers, the supported protocols are TCP, TLS, UDP, or TCP_UDP.
-- For Gateway Load Balancers, the supported protocol is GENEVE. A TCP_UDP
-- listener must be associated with a TCP_UDP target group. If the target
-- is a Lambda function, this parameter does not apply.
--
-- 'protocolVersion', 'createTargetGroup_protocolVersion' - [HTTP\/HTTPS protocol] The protocol version. Specify @GRPC@ to send
-- requests to targets using gRPC. Specify @HTTP2@ to send requests to
-- targets using HTTP\/2. The default is @HTTP1@, which sends requests to
-- targets using HTTP\/1.1.
--
-- 'tags', 'createTargetGroup_tags' - The tags to assign to the target group.
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
-- -   @alb@ - Register a single Application Load Balancer as a target.
--
-- 'unhealthyThresholdCount', 'createTargetGroup_unhealthyThresholdCount' - The number of consecutive health check failures required before
-- considering a target unhealthy. The range is 2-10. If the target group
-- protocol is TCP, TCP_UDP, UDP, TLS, HTTP or HTTPS, the default is 2. For
-- target groups with a protocol of GENEVE, the default is 3. If the target
-- type is @lambda@, the default is 5.
--
-- 'vpcId', 'createTargetGroup_vpcId' - The identifier of the virtual private cloud (VPC). If the target is a
-- Lambda function, this parameter does not apply. Otherwise, this
-- parameter is required.
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
      healthCheckIntervalSeconds = Prelude.Nothing,
      healthCheckPath = Prelude.Nothing,
      healthCheckPort = Prelude.Nothing,
      healthCheckProtocol = Prelude.Nothing,
      healthCheckTimeoutSeconds = Prelude.Nothing,
      healthyThresholdCount = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      matcher = Prelude.Nothing,
      port = Prelude.Nothing,
      protocol = Prelude.Nothing,
      protocolVersion = Prelude.Nothing,
      tags = Prelude.Nothing,
      targetType = Prelude.Nothing,
      unhealthyThresholdCount = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      name = pName_
    }

-- | Indicates whether health checks are enabled. If the target type is
-- @lambda@, health checks are disabled by default but can be enabled. If
-- the target type is @instance@, @ip@, or @alb@, health checks are always
-- enabled and cannot be disabled.
createTargetGroup_healthCheckEnabled :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Bool)
createTargetGroup_healthCheckEnabled = Lens.lens (\CreateTargetGroup' {healthCheckEnabled} -> healthCheckEnabled) (\s@CreateTargetGroup' {} a -> s {healthCheckEnabled = a} :: CreateTargetGroup)

-- | The approximate amount of time, in seconds, between health checks of an
-- individual target. The range is 5-300. If the target group protocol is
-- TCP, TLS, UDP, TCP_UDP, HTTP or HTTPS, the default is 30 seconds. If the
-- target group protocol is GENEVE, the default is 10 seconds. If the
-- target type is @lambda@, the default is 35 seconds.
createTargetGroup_healthCheckIntervalSeconds :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Natural)
createTargetGroup_healthCheckIntervalSeconds = Lens.lens (\CreateTargetGroup' {healthCheckIntervalSeconds} -> healthCheckIntervalSeconds) (\s@CreateTargetGroup' {} a -> s {healthCheckIntervalSeconds = a} :: CreateTargetGroup)

-- | [HTTP\/HTTPS health checks] The destination for health checks on the
-- targets.
--
-- [HTTP1 or HTTP2 protocol version] The ping path. The default is \/.
--
-- [GRPC protocol version] The path of a custom health check method with
-- the format \/package.service\/method. The default is \/Amazon Web
-- Services.ALB\/healthcheck.
createTargetGroup_healthCheckPath :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Text)
createTargetGroup_healthCheckPath = Lens.lens (\CreateTargetGroup' {healthCheckPath} -> healthCheckPath) (\s@CreateTargetGroup' {} a -> s {healthCheckPath = a} :: CreateTargetGroup)

-- | The port the load balancer uses when performing health checks on
-- targets. If the protocol is HTTP, HTTPS, TCP, TLS, UDP, or TCP_UDP, the
-- default is @traffic-port@, which is the port on which each target
-- receives traffic from the load balancer. If the protocol is GENEVE, the
-- default is port 80.
createTargetGroup_healthCheckPort :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Text)
createTargetGroup_healthCheckPort = Lens.lens (\CreateTargetGroup' {healthCheckPort} -> healthCheckPort) (\s@CreateTargetGroup' {} a -> s {healthCheckPort = a} :: CreateTargetGroup)

-- | The protocol the load balancer uses when performing health checks on
-- targets. For Application Load Balancers, the default is HTTP. For
-- Network Load Balancers and Gateway Load Balancers, the default is TCP.
-- The TCP protocol is not supported for health checks if the protocol of
-- the target group is HTTP or HTTPS. The GENEVE, TLS, UDP, and TCP_UDP
-- protocols are not supported for health checks.
createTargetGroup_healthCheckProtocol :: Lens.Lens' CreateTargetGroup (Prelude.Maybe ProtocolEnum)
createTargetGroup_healthCheckProtocol = Lens.lens (\CreateTargetGroup' {healthCheckProtocol} -> healthCheckProtocol) (\s@CreateTargetGroup' {} a -> s {healthCheckProtocol = a} :: CreateTargetGroup)

-- | The amount of time, in seconds, during which no response from a target
-- means a failed health check. The range is 2–120 seconds. For target
-- groups with a protocol of HTTP, the default is 6 seconds. For target
-- groups with a protocol of TCP, TLS or HTTPS, the default is 10 seconds.
-- For target groups with a protocol of GENEVE, the default is 5 seconds.
-- If the target type is @lambda@, the default is 30 seconds.
createTargetGroup_healthCheckTimeoutSeconds :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Natural)
createTargetGroup_healthCheckTimeoutSeconds = Lens.lens (\CreateTargetGroup' {healthCheckTimeoutSeconds} -> healthCheckTimeoutSeconds) (\s@CreateTargetGroup' {} a -> s {healthCheckTimeoutSeconds = a} :: CreateTargetGroup)

-- | The number of consecutive health check successes required before
-- considering a target healthy. The range is 2-10. If the target group
-- protocol is TCP, TCP_UDP, UDP, TLS, HTTP or HTTPS, the default is 5. For
-- target groups with a protocol of GENEVE, the default is 3. If the target
-- type is @lambda@, the default is 5.
createTargetGroup_healthyThresholdCount :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Natural)
createTargetGroup_healthyThresholdCount = Lens.lens (\CreateTargetGroup' {healthyThresholdCount} -> healthyThresholdCount) (\s@CreateTargetGroup' {} a -> s {healthyThresholdCount = a} :: CreateTargetGroup)

-- | The type of IP address used for this target group. The possible values
-- are @ipv4@ and @ipv6@. This is an optional parameter. If not specified,
-- the IP address type defaults to @ipv4@.
createTargetGroup_ipAddressType :: Lens.Lens' CreateTargetGroup (Prelude.Maybe TargetGroupIpAddressTypeEnum)
createTargetGroup_ipAddressType = Lens.lens (\CreateTargetGroup' {ipAddressType} -> ipAddressType) (\s@CreateTargetGroup' {} a -> s {ipAddressType = a} :: CreateTargetGroup)

-- | [HTTP\/HTTPS health checks] The HTTP or gRPC codes to use when checking
-- for a successful response from a target. For target groups with a
-- protocol of TCP, TCP_UDP, UDP or TLS the range is 200-599. For target
-- groups with a protocol of HTTP or HTTPS, the range is 200-499. For
-- target groups with a protocol of GENEVE, the range is 200-399.
createTargetGroup_matcher :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Matcher)
createTargetGroup_matcher = Lens.lens (\CreateTargetGroup' {matcher} -> matcher) (\s@CreateTargetGroup' {} a -> s {matcher = a} :: CreateTargetGroup)

-- | The port on which the targets receive traffic. This port is used unless
-- you specify a port override when registering the target. If the target
-- is a Lambda function, this parameter does not apply. If the protocol is
-- GENEVE, the supported port is 6081.
createTargetGroup_port :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Natural)
createTargetGroup_port = Lens.lens (\CreateTargetGroup' {port} -> port) (\s@CreateTargetGroup' {} a -> s {port = a} :: CreateTargetGroup)

-- | The protocol to use for routing traffic to the targets. For Application
-- Load Balancers, the supported protocols are HTTP and HTTPS. For Network
-- Load Balancers, the supported protocols are TCP, TLS, UDP, or TCP_UDP.
-- For Gateway Load Balancers, the supported protocol is GENEVE. A TCP_UDP
-- listener must be associated with a TCP_UDP target group. If the target
-- is a Lambda function, this parameter does not apply.
createTargetGroup_protocol :: Lens.Lens' CreateTargetGroup (Prelude.Maybe ProtocolEnum)
createTargetGroup_protocol = Lens.lens (\CreateTargetGroup' {protocol} -> protocol) (\s@CreateTargetGroup' {} a -> s {protocol = a} :: CreateTargetGroup)

-- | [HTTP\/HTTPS protocol] The protocol version. Specify @GRPC@ to send
-- requests to targets using gRPC. Specify @HTTP2@ to send requests to
-- targets using HTTP\/2. The default is @HTTP1@, which sends requests to
-- targets using HTTP\/1.1.
createTargetGroup_protocolVersion :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Text)
createTargetGroup_protocolVersion = Lens.lens (\CreateTargetGroup' {protocolVersion} -> protocolVersion) (\s@CreateTargetGroup' {} a -> s {protocolVersion = a} :: CreateTargetGroup)

-- | The tags to assign to the target group.
createTargetGroup_tags :: Lens.Lens' CreateTargetGroup (Prelude.Maybe (Prelude.NonEmpty Tag))
createTargetGroup_tags = Lens.lens (\CreateTargetGroup' {tags} -> tags) (\s@CreateTargetGroup' {} a -> s {tags = a} :: CreateTargetGroup) Prelude.. Lens.mapping Lens.coerced

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
--
-- -   @alb@ - Register a single Application Load Balancer as a target.
createTargetGroup_targetType :: Lens.Lens' CreateTargetGroup (Prelude.Maybe TargetTypeEnum)
createTargetGroup_targetType = Lens.lens (\CreateTargetGroup' {targetType} -> targetType) (\s@CreateTargetGroup' {} a -> s {targetType = a} :: CreateTargetGroup)

-- | The number of consecutive health check failures required before
-- considering a target unhealthy. The range is 2-10. If the target group
-- protocol is TCP, TCP_UDP, UDP, TLS, HTTP or HTTPS, the default is 2. For
-- target groups with a protocol of GENEVE, the default is 3. If the target
-- type is @lambda@, the default is 5.
createTargetGroup_unhealthyThresholdCount :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Natural)
createTargetGroup_unhealthyThresholdCount = Lens.lens (\CreateTargetGroup' {unhealthyThresholdCount} -> unhealthyThresholdCount) (\s@CreateTargetGroup' {} a -> s {unhealthyThresholdCount = a} :: CreateTargetGroup)

-- | The identifier of the virtual private cloud (VPC). If the target is a
-- Lambda function, this parameter does not apply. Otherwise, this
-- parameter is required.
createTargetGroup_vpcId :: Lens.Lens' CreateTargetGroup (Prelude.Maybe Prelude.Text)
createTargetGroup_vpcId = Lens.lens (\CreateTargetGroup' {vpcId} -> vpcId) (\s@CreateTargetGroup' {} a -> s {vpcId = a} :: CreateTargetGroup)

-- | The name of the target group.
--
-- This name must be unique per region per account, can have a maximum of
-- 32 characters, must contain only alphanumeric characters or hyphens, and
-- must not begin or end with a hyphen.
createTargetGroup_name :: Lens.Lens' CreateTargetGroup Prelude.Text
createTargetGroup_name = Lens.lens (\CreateTargetGroup' {name} -> name) (\s@CreateTargetGroup' {} a -> s {name = a} :: CreateTargetGroup)

instance Core.AWSRequest CreateTargetGroup where
  type
    AWSResponse CreateTargetGroup =
      CreateTargetGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateTargetGroupResult"
      ( \s h x ->
          CreateTargetGroupResponse'
            Prelude.<$> ( x Data..@? "TargetGroups" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTargetGroup where
  hashWithSalt _salt CreateTargetGroup' {..} =
    _salt
      `Prelude.hashWithSalt` healthCheckEnabled
      `Prelude.hashWithSalt` healthCheckIntervalSeconds
      `Prelude.hashWithSalt` healthCheckPath
      `Prelude.hashWithSalt` healthCheckPort
      `Prelude.hashWithSalt` healthCheckProtocol
      `Prelude.hashWithSalt` healthCheckTimeoutSeconds
      `Prelude.hashWithSalt` healthyThresholdCount
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` matcher
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` protocolVersion
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetType
      `Prelude.hashWithSalt` unhealthyThresholdCount
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateTargetGroup where
  rnf CreateTargetGroup' {..} =
    Prelude.rnf healthCheckEnabled `Prelude.seq`
      Prelude.rnf healthCheckIntervalSeconds `Prelude.seq`
        Prelude.rnf healthCheckPath `Prelude.seq`
          Prelude.rnf healthCheckPort `Prelude.seq`
            Prelude.rnf healthCheckProtocol `Prelude.seq`
              Prelude.rnf healthCheckTimeoutSeconds `Prelude.seq`
                Prelude.rnf healthyThresholdCount `Prelude.seq`
                  Prelude.rnf ipAddressType `Prelude.seq`
                    Prelude.rnf matcher `Prelude.seq`
                      Prelude.rnf port `Prelude.seq`
                        Prelude.rnf protocol `Prelude.seq`
                          Prelude.rnf protocolVersion `Prelude.seq`
                            Prelude.rnf tags `Prelude.seq`
                              Prelude.rnf targetType `Prelude.seq`
                                Prelude.rnf unhealthyThresholdCount `Prelude.seq`
                                  Prelude.rnf vpcId `Prelude.seq`
                                    Prelude.rnf name

instance Data.ToHeaders CreateTargetGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateTargetGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTargetGroup where
  toQuery CreateTargetGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateTargetGroup" :: Prelude.ByteString),
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
        "IpAddressType" Data.=: ipAddressType,
        "Matcher" Data.=: matcher,
        "Port" Data.=: port,
        "Protocol" Data.=: protocol,
        "ProtocolVersion" Data.=: protocolVersion,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tags),
        "TargetType" Data.=: targetType,
        "UnhealthyThresholdCount"
          Data.=: unhealthyThresholdCount,
        "VpcId" Data.=: vpcId,
        "Name" Data.=: name
      ]

-- | /See:/ 'newCreateTargetGroupResponse' smart constructor.
data CreateTargetGroupResponse = CreateTargetGroupResponse'
  { -- | Information about the target group.
    targetGroups :: Prelude.Maybe [TargetGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
createTargetGroupResponse_targetGroups = Lens.lens (\CreateTargetGroupResponse' {targetGroups} -> targetGroups) (\s@CreateTargetGroupResponse' {} a -> s {targetGroups = a} :: CreateTargetGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createTargetGroupResponse_httpStatus :: Lens.Lens' CreateTargetGroupResponse Prelude.Int
createTargetGroupResponse_httpStatus = Lens.lens (\CreateTargetGroupResponse' {httpStatus} -> httpStatus) (\s@CreateTargetGroupResponse' {} a -> s {httpStatus = a} :: CreateTargetGroupResponse)

instance Prelude.NFData CreateTargetGroupResponse where
  rnf CreateTargetGroupResponse' {..} =
    Prelude.rnf targetGroups `Prelude.seq`
      Prelude.rnf httpStatus
