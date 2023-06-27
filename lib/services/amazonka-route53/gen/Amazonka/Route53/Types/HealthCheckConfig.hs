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
-- Module      : Amazonka.Route53.Types.HealthCheckConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.HealthCheckConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal
import Amazonka.Route53.Types.AlarmIdentifier
import Amazonka.Route53.Types.HealthCheckRegion
import Amazonka.Route53.Types.HealthCheckType
import Amazonka.Route53.Types.InsufficientDataHealthStatus

-- | A complex type that contains information about the health check.
--
-- /See:/ 'newHealthCheckConfig' smart constructor.
data HealthCheckConfig = HealthCheckConfig'
  { -- | A complex type that identifies the CloudWatch alarm that you want Amazon
    -- Route 53 health checkers to use to determine whether the specified
    -- health check is healthy.
    alarmIdentifier :: Prelude.Maybe AlarmIdentifier,
    -- | (CALCULATED Health Checks Only) A complex type that contains one
    -- @ChildHealthCheck@ element for each health check that you want to
    -- associate with a @CALCULATED@ health check.
    childHealthChecks :: Prelude.Maybe [Prelude.Text],
    -- | Stops Route 53 from performing health checks. When you disable a health
    -- check, here\'s what happens:
    --
    -- -   __Health checks that check the health of endpoints:__ Route 53 stops
    --     submitting requests to your application, server, or other resource.
    --
    -- -   __Calculated health checks:__ Route 53 stops aggregating the status
    --     of the referenced health checks.
    --
    -- -   __Health checks that monitor CloudWatch alarms:__ Route 53 stops
    --     monitoring the corresponding CloudWatch metrics.
    --
    -- After you disable a health check, Route 53 considers the status of the
    -- health check to always be healthy. If you configured DNS failover, Route
    -- 53 continues to route traffic to the corresponding resources. If you
    -- want to stop routing traffic to a resource, change the value of
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-Inverted Inverted>.
    --
    -- Charges for a health check still apply when the health check is
    -- disabled. For more information, see
    -- <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing>.
    disabled :: Prelude.Maybe Prelude.Bool,
    -- | Specify whether you want Amazon Route 53 to send the value of
    -- @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message
    -- during TLS negotiation. This allows the endpoint to respond to @HTTPS@
    -- health check requests with the applicable SSL\/TLS certificate.
    --
    -- Some endpoints require that @HTTPS@ requests include the host name in
    -- the @client_hello@ message. If you don\'t enable SNI, the status of the
    -- health check will be @SSL alert handshake_failure@. A health check can
    -- also have that status for other reasons. If SNI is enabled and you\'re
    -- still getting the error, check the SSL\/TLS configuration on your
    -- endpoint and confirm that your certificate is valid.
    --
    -- The SSL\/TLS certificate on your endpoint includes a domain name in the
    -- @Common Name@ field and possibly several more in the
    -- @Subject Alternative Names@ field. One of the domain names in the
    -- certificate should match the value that you specify for
    -- @FullyQualifiedDomainName@. If the endpoint responds to the
    -- @client_hello@ message with a certificate that does not include the
    -- domain name that you specified in @FullyQualifiedDomainName@, a health
    -- checker will retry the handshake. In the second attempt, the health
    -- checker will omit @FullyQualifiedDomainName@ from the @client_hello@
    -- message.
    enableSNI :: Prelude.Maybe Prelude.Bool,
    -- | The number of consecutive health checks that an endpoint must pass or
    -- fail for Amazon Route 53 to change the current status of the endpoint
    -- from unhealthy to healthy or vice versa. For more information, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy>
    -- in the /Amazon Route 53 Developer Guide/.
    --
    -- If you don\'t specify a value for @FailureThreshold@, the default value
    -- is three health checks.
    failureThreshold :: Prelude.Maybe Prelude.Natural,
    -- | Amazon Route 53 behavior depends on whether you specify a value for
    -- @IPAddress@.
    --
    -- __If you specify a value for__ @IPAddress@:
    --
    -- Amazon Route 53 sends health check requests to the specified IPv4 or
    -- IPv6 address and passes the value of @FullyQualifiedDomainName@ in the
    -- @Host@ header for all health checks except TCP health checks. This is
    -- typically the fully qualified DNS name of the endpoint on which you want
    -- Route 53 to perform health checks.
    --
    -- When Route 53 checks the health of an endpoint, here is how it
    -- constructs the @Host@ header:
    --
    -- -   If you specify a value of @80@ for @Port@ and @HTTP@ or
    --     @HTTP_STR_MATCH@ for @Type@, Route 53 passes the value of
    --     @FullyQualifiedDomainName@ to the endpoint in the Host header.
    --
    -- -   If you specify a value of @443@ for @Port@ and @HTTPS@ or
    --     @HTTPS_STR_MATCH@ for @Type@, Route 53 passes the value of
    --     @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
    --
    -- -   If you specify another value for @Port@ and any value except @TCP@
    --     for @Type@, Route 53 passes @FullyQualifiedDomainName:Port@ to the
    --     endpoint in the @Host@ header.
    --
    -- If you don\'t specify a value for @FullyQualifiedDomainName@, Route 53
    -- substitutes the value of @IPAddress@ in the @Host@ header in each of the
    -- preceding cases.
    --
    -- __If you don\'t specify a value for__ @IPAddress@:
    --
    -- Route 53 sends a DNS request to the domain that you specify for
    -- @FullyQualifiedDomainName@ at the interval that you specify for
    -- @RequestInterval@. Using an IPv4 address that DNS returns, Route 53 then
    -- checks the health of the endpoint.
    --
    -- If you don\'t specify a value for @IPAddress@, Route 53 uses only IPv4
    -- to send health checks to the endpoint. If there\'s no resource record
    -- set with a type of A for the name that you specify for
    -- @FullyQualifiedDomainName@, the health check fails with a \"DNS
    -- resolution failed\" error.
    --
    -- If you want to check the health of weighted, latency, or failover
    -- resource record sets and you choose to specify the endpoint only by
    -- @FullyQualifiedDomainName@, we recommend that you create a separate
    -- health check for each endpoint. For example, create a health check for
    -- each HTTP server that is serving content for www.example.com. For the
    -- value of @FullyQualifiedDomainName@, specify the domain name of the
    -- server (such as us-east-2-www.example.com), not the name of the resource
    -- record sets (www.example.com).
    --
    -- In this configuration, if you create a health check for which the value
    -- of @FullyQualifiedDomainName@ matches the name of the resource record
    -- sets and you then associate the health check with those resource record
    -- sets, health check results will be unpredictable.
    --
    -- In addition, if the value that you specify for @Type@ is @HTTP@,
    -- @HTTPS@, @HTTP_STR_MATCH@, or @HTTPS_STR_MATCH@, Route 53 passes the
    -- value of @FullyQualifiedDomainName@ in the @Host@ header, as it does
    -- when you specify a value for @IPAddress@. If the value of @Type@ is
    -- @TCP@, Route 53 doesn\'t pass a @Host@ header.
    fullyQualifiedDomainName :: Prelude.Maybe Prelude.Text,
    -- | The number of child health checks that are associated with a
    -- @CALCULATED@ health check that Amazon Route 53 must consider healthy for
    -- the @CALCULATED@ health check to be considered healthy. To specify the
    -- child health checks that you want to associate with a @CALCULATED@
    -- health check, use the
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-ChildHealthChecks ChildHealthChecks>
    -- element.
    --
    -- Note the following:
    --
    -- -   If you specify a number greater than the number of child health
    --     checks, Route 53 always considers this health check to be unhealthy.
    --
    -- -   If you specify @0@, Route 53 always considers this health check to
    --     be healthy.
    healthThreshold :: Prelude.Maybe Prelude.Natural,
    -- | The IPv4 or IPv6 IP address of the endpoint that you want Amazon Route
    -- 53 to perform health checks on. If you don\'t specify a value for
    -- @IPAddress@, Route 53 sends a DNS request to resolve the domain name
    -- that you specify in @FullyQualifiedDomainName@ at the interval that you
    -- specify in @RequestInterval@. Using an IP address returned by DNS, Route
    -- 53 then checks the health of the endpoint.
    --
    -- Use one of the following formats for the value of @IPAddress@:
    --
    -- -   __IPv4 address__: four values between 0 and 255, separated by
    --     periods (.), for example, @192.0.2.44@.
    --
    -- -   __IPv6 address__: eight groups of four hexadecimal values, separated
    --     by colons (:), for example,
    --     @2001:0db8:85a3:0000:0000:abcd:0001:2345@. You can also shorten IPv6
    --     addresses as described in RFC 5952, for example,
    --     @2001:db8:85a3::abcd:1:2345@.
    --
    -- If the endpoint is an EC2 instance, we recommend that you create an
    -- Elastic IP address, associate it with your EC2 instance, and specify the
    -- Elastic IP address for @IPAddress@. This ensures that the IP address of
    -- your instance will never change.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-FullyQualifiedDomainName FullyQualifiedDomainName>.
    --
    -- Constraints: Route 53 can\'t check the health of endpoints for which the
    -- IP address is in local, private, non-routable, or multicast ranges. For
    -- more information about IP addresses for which you can\'t create health
    -- checks, see the following documents:
    --
    -- -   <https://tools.ietf.org/html/rfc5735 RFC 5735, Special Use IPv4 Addresses>
    --
    -- -   <https://tools.ietf.org/html/rfc6598 RFC 6598, IANA-Reserved IPv4 Prefix for Shared Address Space>
    --
    -- -   <https://tools.ietf.org/html/rfc5156 RFC 5156, Special-Use IPv6 Addresses>
    --
    -- When the value of @Type@ is @CALCULATED@ or @CLOUDWATCH_METRIC@, omit
    -- @IPAddress@.
    iPAddress :: Prelude.Maybe Prelude.Text,
    -- | When CloudWatch has insufficient data about the metric to determine the
    -- alarm state, the status that you want Amazon Route 53 to assign to the
    -- health check:
    --
    -- -   @Healthy@: Route 53 considers the health check to be healthy.
    --
    -- -   @Unhealthy@: Route 53 considers the health check to be unhealthy.
    --
    -- -   @LastKnownStatus@: Route 53 uses the status of the health check from
    --     the last time that CloudWatch had sufficient data to determine the
    --     alarm state. For new health checks that have no last known status,
    --     the default status for the health check is healthy.
    insufficientDataHealthStatus :: Prelude.Maybe InsufficientDataHealthStatus,
    -- | Specify whether you want Amazon Route 53 to invert the status of a
    -- health check, for example, to consider a health check unhealthy when it
    -- otherwise would be considered healthy.
    inverted :: Prelude.Maybe Prelude.Bool,
    -- | Specify whether you want Amazon Route 53 to measure the latency between
    -- health checkers in multiple Amazon Web Services regions and your
    -- endpoint, and to display CloudWatch latency graphs on the __Health
    -- Checks__ page in the Route 53 console.
    --
    -- You can\'t change the value of @MeasureLatency@ after you create a
    -- health check.
    measureLatency :: Prelude.Maybe Prelude.Bool,
    -- | The port on the endpoint that you want Amazon Route 53 to perform health
    -- checks on.
    --
    -- Don\'t specify a value for @Port@ when you specify a value for @Type@ of
    -- @CLOUDWATCH_METRIC@ or @CALCULATED@.
    port :: Prelude.Maybe Prelude.Natural,
    -- | A complex type that contains one @Region@ element for each region from
    -- which you want Amazon Route 53 health checkers to check the specified
    -- endpoint.
    --
    -- If you don\'t specify any regions, Route 53 health checkers
    -- automatically performs checks from all of the regions that are listed
    -- under __Valid Values__.
    --
    -- If you update a health check to remove a region that has been performing
    -- health checks, Route 53 will briefly continue to perform checks from
    -- that region to ensure that some health checkers are always checking the
    -- endpoint (for example, if you replace three regions with four different
    -- regions).
    regions :: Prelude.Maybe (Prelude.NonEmpty HealthCheckRegion),
    -- | The number of seconds between the time that Amazon Route 53 gets a
    -- response from your endpoint and the time that it sends the next health
    -- check request. Each Route 53 health checker makes requests at this
    -- interval.
    --
    -- You can\'t change the value of @RequestInterval@ after you create a
    -- health check.
    --
    -- If you don\'t specify a value for @RequestInterval@, the default value
    -- is @30@ seconds.
    requestInterval :: Prelude.Maybe Prelude.Natural,
    -- | The path, if any, that you want Amazon Route 53 to request when
    -- performing health checks. The path can be any value for which your
    -- endpoint will return an HTTP status code of 2xx or 3xx when the endpoint
    -- is healthy, for example, the file \/docs\/route53-health-check.html. You
    -- can also include query string parameters, for example,
    -- @\/welcome.html?language=jp&login=y@.
    resourcePath :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the Route 53 Application Recovery
    -- Controller routing control.
    --
    -- For more information about Route 53 Application Recovery Controller, see
    -- <https://docs.aws.amazon.com/r53recovery/latest/dg/what-is-route-53-recovery.html Route 53 Application Recovery Controller Developer Guide.>.
    routingControlArn :: Prelude.Maybe Prelude.Text,
    -- | If the value of Type is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@, the
    -- string that you want Amazon Route 53 to search for in the response body
    -- from the specified resource. If the string appears in the response body,
    -- Route 53 considers the resource healthy.
    --
    -- Route 53 considers case when searching for @SearchString@ in the
    -- response body.
    searchString :: Prelude.Maybe Prelude.Text,
    -- | The type of health check that you want to create, which indicates how
    -- Amazon Route 53 determines whether an endpoint is healthy.
    --
    -- You can\'t change the value of @Type@ after you create a health check.
    --
    -- You can create the following types of health checks:
    --
    -- -   __HTTP__: Route 53 tries to establish a TCP connection. If
    --     successful, Route 53 submits an HTTP request and waits for an HTTP
    --     status code of 200 or greater and less than 400.
    --
    -- -   __HTTPS__: Route 53 tries to establish a TCP connection. If
    --     successful, Route 53 submits an HTTPS request and waits for an HTTP
    --     status code of 200 or greater and less than 400.
    --
    --     If you specify @HTTPS@ for the value of @Type@, the endpoint must
    --     support TLS v1.0 or later.
    --
    -- -   __HTTP_STR_MATCH__: Route 53 tries to establish a TCP connection. If
    --     successful, Route 53 submits an HTTP request and searches the first
    --     5,120 bytes of the response body for the string that you specify in
    --     @SearchString@.
    --
    -- -   __HTTPS_STR_MATCH__: Route 53 tries to establish a TCP connection.
    --     If successful, Route 53 submits an @HTTPS@ request and searches the
    --     first 5,120 bytes of the response body for the string that you
    --     specify in @SearchString@.
    --
    -- -   __TCP__: Route 53 tries to establish a TCP connection.
    --
    -- -   __CLOUDWATCH_METRIC__: The health check is associated with a
    --     CloudWatch alarm. If the state of the alarm is @OK@, the health
    --     check is considered healthy. If the state is @ALARM@, the health
    --     check is considered unhealthy. If CloudWatch doesn\'t have
    --     sufficient data to determine whether the state is @OK@ or @ALARM@,
    --     the health check status depends on the setting for
    --     @InsufficientDataHealthStatus@: @Healthy@, @Unhealthy@, or
    --     @LastKnownStatus@.
    --
    -- -   __CALCULATED__: For health checks that monitor the status of other
    --     health checks, Route 53 adds up the number of health checks that
    --     Route 53 health checkers consider to be healthy and compares that
    --     number with the value of @HealthThreshold@.
    --
    -- -   __RECOVERY_CONTROL__: The health check is assocated with a Route53
    --     Application Recovery Controller routing control. If the routing
    --     control state is @ON@, the health check is considered healthy. If
    --     the state is @OFF@, the health check is considered unhealthy.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy>
    -- in the /Amazon Route 53 Developer Guide/.
    type' :: HealthCheckType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HealthCheckConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmIdentifier', 'healthCheckConfig_alarmIdentifier' - A complex type that identifies the CloudWatch alarm that you want Amazon
-- Route 53 health checkers to use to determine whether the specified
-- health check is healthy.
--
-- 'childHealthChecks', 'healthCheckConfig_childHealthChecks' - (CALCULATED Health Checks Only) A complex type that contains one
-- @ChildHealthCheck@ element for each health check that you want to
-- associate with a @CALCULATED@ health check.
--
-- 'disabled', 'healthCheckConfig_disabled' - Stops Route 53 from performing health checks. When you disable a health
-- check, here\'s what happens:
--
-- -   __Health checks that check the health of endpoints:__ Route 53 stops
--     submitting requests to your application, server, or other resource.
--
-- -   __Calculated health checks:__ Route 53 stops aggregating the status
--     of the referenced health checks.
--
-- -   __Health checks that monitor CloudWatch alarms:__ Route 53 stops
--     monitoring the corresponding CloudWatch metrics.
--
-- After you disable a health check, Route 53 considers the status of the
-- health check to always be healthy. If you configured DNS failover, Route
-- 53 continues to route traffic to the corresponding resources. If you
-- want to stop routing traffic to a resource, change the value of
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-Inverted Inverted>.
--
-- Charges for a health check still apply when the health check is
-- disabled. For more information, see
-- <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing>.
--
-- 'enableSNI', 'healthCheckConfig_enableSNI' - Specify whether you want Amazon Route 53 to send the value of
-- @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message
-- during TLS negotiation. This allows the endpoint to respond to @HTTPS@
-- health check requests with the applicable SSL\/TLS certificate.
--
-- Some endpoints require that @HTTPS@ requests include the host name in
-- the @client_hello@ message. If you don\'t enable SNI, the status of the
-- health check will be @SSL alert handshake_failure@. A health check can
-- also have that status for other reasons. If SNI is enabled and you\'re
-- still getting the error, check the SSL\/TLS configuration on your
-- endpoint and confirm that your certificate is valid.
--
-- The SSL\/TLS certificate on your endpoint includes a domain name in the
-- @Common Name@ field and possibly several more in the
-- @Subject Alternative Names@ field. One of the domain names in the
-- certificate should match the value that you specify for
-- @FullyQualifiedDomainName@. If the endpoint responds to the
-- @client_hello@ message with a certificate that does not include the
-- domain name that you specified in @FullyQualifiedDomainName@, a health
-- checker will retry the handshake. In the second attempt, the health
-- checker will omit @FullyQualifiedDomainName@ from the @client_hello@
-- message.
--
-- 'failureThreshold', 'healthCheckConfig_failureThreshold' - The number of consecutive health checks that an endpoint must pass or
-- fail for Amazon Route 53 to change the current status of the endpoint
-- from unhealthy to healthy or vice versa. For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy>
-- in the /Amazon Route 53 Developer Guide/.
--
-- If you don\'t specify a value for @FailureThreshold@, the default value
-- is three health checks.
--
-- 'fullyQualifiedDomainName', 'healthCheckConfig_fullyQualifiedDomainName' - Amazon Route 53 behavior depends on whether you specify a value for
-- @IPAddress@.
--
-- __If you specify a value for__ @IPAddress@:
--
-- Amazon Route 53 sends health check requests to the specified IPv4 or
-- IPv6 address and passes the value of @FullyQualifiedDomainName@ in the
-- @Host@ header for all health checks except TCP health checks. This is
-- typically the fully qualified DNS name of the endpoint on which you want
-- Route 53 to perform health checks.
--
-- When Route 53 checks the health of an endpoint, here is how it
-- constructs the @Host@ header:
--
-- -   If you specify a value of @80@ for @Port@ and @HTTP@ or
--     @HTTP_STR_MATCH@ for @Type@, Route 53 passes the value of
--     @FullyQualifiedDomainName@ to the endpoint in the Host header.
--
-- -   If you specify a value of @443@ for @Port@ and @HTTPS@ or
--     @HTTPS_STR_MATCH@ for @Type@, Route 53 passes the value of
--     @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
--
-- -   If you specify another value for @Port@ and any value except @TCP@
--     for @Type@, Route 53 passes @FullyQualifiedDomainName:Port@ to the
--     endpoint in the @Host@ header.
--
-- If you don\'t specify a value for @FullyQualifiedDomainName@, Route 53
-- substitutes the value of @IPAddress@ in the @Host@ header in each of the
-- preceding cases.
--
-- __If you don\'t specify a value for__ @IPAddress@:
--
-- Route 53 sends a DNS request to the domain that you specify for
-- @FullyQualifiedDomainName@ at the interval that you specify for
-- @RequestInterval@. Using an IPv4 address that DNS returns, Route 53 then
-- checks the health of the endpoint.
--
-- If you don\'t specify a value for @IPAddress@, Route 53 uses only IPv4
-- to send health checks to the endpoint. If there\'s no resource record
-- set with a type of A for the name that you specify for
-- @FullyQualifiedDomainName@, the health check fails with a \"DNS
-- resolution failed\" error.
--
-- If you want to check the health of weighted, latency, or failover
-- resource record sets and you choose to specify the endpoint only by
-- @FullyQualifiedDomainName@, we recommend that you create a separate
-- health check for each endpoint. For example, create a health check for
-- each HTTP server that is serving content for www.example.com. For the
-- value of @FullyQualifiedDomainName@, specify the domain name of the
-- server (such as us-east-2-www.example.com), not the name of the resource
-- record sets (www.example.com).
--
-- In this configuration, if you create a health check for which the value
-- of @FullyQualifiedDomainName@ matches the name of the resource record
-- sets and you then associate the health check with those resource record
-- sets, health check results will be unpredictable.
--
-- In addition, if the value that you specify for @Type@ is @HTTP@,
-- @HTTPS@, @HTTP_STR_MATCH@, or @HTTPS_STR_MATCH@, Route 53 passes the
-- value of @FullyQualifiedDomainName@ in the @Host@ header, as it does
-- when you specify a value for @IPAddress@. If the value of @Type@ is
-- @TCP@, Route 53 doesn\'t pass a @Host@ header.
--
-- 'healthThreshold', 'healthCheckConfig_healthThreshold' - The number of child health checks that are associated with a
-- @CALCULATED@ health check that Amazon Route 53 must consider healthy for
-- the @CALCULATED@ health check to be considered healthy. To specify the
-- child health checks that you want to associate with a @CALCULATED@
-- health check, use the
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-ChildHealthChecks ChildHealthChecks>
-- element.
--
-- Note the following:
--
-- -   If you specify a number greater than the number of child health
--     checks, Route 53 always considers this health check to be unhealthy.
--
-- -   If you specify @0@, Route 53 always considers this health check to
--     be healthy.
--
-- 'iPAddress', 'healthCheckConfig_iPAddress' - The IPv4 or IPv6 IP address of the endpoint that you want Amazon Route
-- 53 to perform health checks on. If you don\'t specify a value for
-- @IPAddress@, Route 53 sends a DNS request to resolve the domain name
-- that you specify in @FullyQualifiedDomainName@ at the interval that you
-- specify in @RequestInterval@. Using an IP address returned by DNS, Route
-- 53 then checks the health of the endpoint.
--
-- Use one of the following formats for the value of @IPAddress@:
--
-- -   __IPv4 address__: four values between 0 and 255, separated by
--     periods (.), for example, @192.0.2.44@.
--
-- -   __IPv6 address__: eight groups of four hexadecimal values, separated
--     by colons (:), for example,
--     @2001:0db8:85a3:0000:0000:abcd:0001:2345@. You can also shorten IPv6
--     addresses as described in RFC 5952, for example,
--     @2001:db8:85a3::abcd:1:2345@.
--
-- If the endpoint is an EC2 instance, we recommend that you create an
-- Elastic IP address, associate it with your EC2 instance, and specify the
-- Elastic IP address for @IPAddress@. This ensures that the IP address of
-- your instance will never change.
--
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-FullyQualifiedDomainName FullyQualifiedDomainName>.
--
-- Constraints: Route 53 can\'t check the health of endpoints for which the
-- IP address is in local, private, non-routable, or multicast ranges. For
-- more information about IP addresses for which you can\'t create health
-- checks, see the following documents:
--
-- -   <https://tools.ietf.org/html/rfc5735 RFC 5735, Special Use IPv4 Addresses>
--
-- -   <https://tools.ietf.org/html/rfc6598 RFC 6598, IANA-Reserved IPv4 Prefix for Shared Address Space>
--
-- -   <https://tools.ietf.org/html/rfc5156 RFC 5156, Special-Use IPv6 Addresses>
--
-- When the value of @Type@ is @CALCULATED@ or @CLOUDWATCH_METRIC@, omit
-- @IPAddress@.
--
-- 'insufficientDataHealthStatus', 'healthCheckConfig_insufficientDataHealthStatus' - When CloudWatch has insufficient data about the metric to determine the
-- alarm state, the status that you want Amazon Route 53 to assign to the
-- health check:
--
-- -   @Healthy@: Route 53 considers the health check to be healthy.
--
-- -   @Unhealthy@: Route 53 considers the health check to be unhealthy.
--
-- -   @LastKnownStatus@: Route 53 uses the status of the health check from
--     the last time that CloudWatch had sufficient data to determine the
--     alarm state. For new health checks that have no last known status,
--     the default status for the health check is healthy.
--
-- 'inverted', 'healthCheckConfig_inverted' - Specify whether you want Amazon Route 53 to invert the status of a
-- health check, for example, to consider a health check unhealthy when it
-- otherwise would be considered healthy.
--
-- 'measureLatency', 'healthCheckConfig_measureLatency' - Specify whether you want Amazon Route 53 to measure the latency between
-- health checkers in multiple Amazon Web Services regions and your
-- endpoint, and to display CloudWatch latency graphs on the __Health
-- Checks__ page in the Route 53 console.
--
-- You can\'t change the value of @MeasureLatency@ after you create a
-- health check.
--
-- 'port', 'healthCheckConfig_port' - The port on the endpoint that you want Amazon Route 53 to perform health
-- checks on.
--
-- Don\'t specify a value for @Port@ when you specify a value for @Type@ of
-- @CLOUDWATCH_METRIC@ or @CALCULATED@.
--
-- 'regions', 'healthCheckConfig_regions' - A complex type that contains one @Region@ element for each region from
-- which you want Amazon Route 53 health checkers to check the specified
-- endpoint.
--
-- If you don\'t specify any regions, Route 53 health checkers
-- automatically performs checks from all of the regions that are listed
-- under __Valid Values__.
--
-- If you update a health check to remove a region that has been performing
-- health checks, Route 53 will briefly continue to perform checks from
-- that region to ensure that some health checkers are always checking the
-- endpoint (for example, if you replace three regions with four different
-- regions).
--
-- 'requestInterval', 'healthCheckConfig_requestInterval' - The number of seconds between the time that Amazon Route 53 gets a
-- response from your endpoint and the time that it sends the next health
-- check request. Each Route 53 health checker makes requests at this
-- interval.
--
-- You can\'t change the value of @RequestInterval@ after you create a
-- health check.
--
-- If you don\'t specify a value for @RequestInterval@, the default value
-- is @30@ seconds.
--
-- 'resourcePath', 'healthCheckConfig_resourcePath' - The path, if any, that you want Amazon Route 53 to request when
-- performing health checks. The path can be any value for which your
-- endpoint will return an HTTP status code of 2xx or 3xx when the endpoint
-- is healthy, for example, the file \/docs\/route53-health-check.html. You
-- can also include query string parameters, for example,
-- @\/welcome.html?language=jp&login=y@.
--
-- 'routingControlArn', 'healthCheckConfig_routingControlArn' - The Amazon Resource Name (ARN) for the Route 53 Application Recovery
-- Controller routing control.
--
-- For more information about Route 53 Application Recovery Controller, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/what-is-route-53-recovery.html Route 53 Application Recovery Controller Developer Guide.>.
--
-- 'searchString', 'healthCheckConfig_searchString' - If the value of Type is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@, the
-- string that you want Amazon Route 53 to search for in the response body
-- from the specified resource. If the string appears in the response body,
-- Route 53 considers the resource healthy.
--
-- Route 53 considers case when searching for @SearchString@ in the
-- response body.
--
-- 'type'', 'healthCheckConfig_type' - The type of health check that you want to create, which indicates how
-- Amazon Route 53 determines whether an endpoint is healthy.
--
-- You can\'t change the value of @Type@ after you create a health check.
--
-- You can create the following types of health checks:
--
-- -   __HTTP__: Route 53 tries to establish a TCP connection. If
--     successful, Route 53 submits an HTTP request and waits for an HTTP
--     status code of 200 or greater and less than 400.
--
-- -   __HTTPS__: Route 53 tries to establish a TCP connection. If
--     successful, Route 53 submits an HTTPS request and waits for an HTTP
--     status code of 200 or greater and less than 400.
--
--     If you specify @HTTPS@ for the value of @Type@, the endpoint must
--     support TLS v1.0 or later.
--
-- -   __HTTP_STR_MATCH__: Route 53 tries to establish a TCP connection. If
--     successful, Route 53 submits an HTTP request and searches the first
--     5,120 bytes of the response body for the string that you specify in
--     @SearchString@.
--
-- -   __HTTPS_STR_MATCH__: Route 53 tries to establish a TCP connection.
--     If successful, Route 53 submits an @HTTPS@ request and searches the
--     first 5,120 bytes of the response body for the string that you
--     specify in @SearchString@.
--
-- -   __TCP__: Route 53 tries to establish a TCP connection.
--
-- -   __CLOUDWATCH_METRIC__: The health check is associated with a
--     CloudWatch alarm. If the state of the alarm is @OK@, the health
--     check is considered healthy. If the state is @ALARM@, the health
--     check is considered unhealthy. If CloudWatch doesn\'t have
--     sufficient data to determine whether the state is @OK@ or @ALARM@,
--     the health check status depends on the setting for
--     @InsufficientDataHealthStatus@: @Healthy@, @Unhealthy@, or
--     @LastKnownStatus@.
--
-- -   __CALCULATED__: For health checks that monitor the status of other
--     health checks, Route 53 adds up the number of health checks that
--     Route 53 health checkers consider to be healthy and compares that
--     number with the value of @HealthThreshold@.
--
-- -   __RECOVERY_CONTROL__: The health check is assocated with a Route53
--     Application Recovery Controller routing control. If the routing
--     control state is @ON@, the health check is considered healthy. If
--     the state is @OFF@, the health check is considered unhealthy.
--
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy>
-- in the /Amazon Route 53 Developer Guide/.
newHealthCheckConfig ::
  -- | 'type''
  HealthCheckType ->
  HealthCheckConfig
newHealthCheckConfig pType_ =
  HealthCheckConfig'
    { alarmIdentifier =
        Prelude.Nothing,
      childHealthChecks = Prelude.Nothing,
      disabled = Prelude.Nothing,
      enableSNI = Prelude.Nothing,
      failureThreshold = Prelude.Nothing,
      fullyQualifiedDomainName = Prelude.Nothing,
      healthThreshold = Prelude.Nothing,
      iPAddress = Prelude.Nothing,
      insufficientDataHealthStatus = Prelude.Nothing,
      inverted = Prelude.Nothing,
      measureLatency = Prelude.Nothing,
      port = Prelude.Nothing,
      regions = Prelude.Nothing,
      requestInterval = Prelude.Nothing,
      resourcePath = Prelude.Nothing,
      routingControlArn = Prelude.Nothing,
      searchString = Prelude.Nothing,
      type' = pType_
    }

-- | A complex type that identifies the CloudWatch alarm that you want Amazon
-- Route 53 health checkers to use to determine whether the specified
-- health check is healthy.
healthCheckConfig_alarmIdentifier :: Lens.Lens' HealthCheckConfig (Prelude.Maybe AlarmIdentifier)
healthCheckConfig_alarmIdentifier = Lens.lens (\HealthCheckConfig' {alarmIdentifier} -> alarmIdentifier) (\s@HealthCheckConfig' {} a -> s {alarmIdentifier = a} :: HealthCheckConfig)

-- | (CALCULATED Health Checks Only) A complex type that contains one
-- @ChildHealthCheck@ element for each health check that you want to
-- associate with a @CALCULATED@ health check.
healthCheckConfig_childHealthChecks :: Lens.Lens' HealthCheckConfig (Prelude.Maybe [Prelude.Text])
healthCheckConfig_childHealthChecks = Lens.lens (\HealthCheckConfig' {childHealthChecks} -> childHealthChecks) (\s@HealthCheckConfig' {} a -> s {childHealthChecks = a} :: HealthCheckConfig) Prelude.. Lens.mapping Lens.coerced

-- | Stops Route 53 from performing health checks. When you disable a health
-- check, here\'s what happens:
--
-- -   __Health checks that check the health of endpoints:__ Route 53 stops
--     submitting requests to your application, server, or other resource.
--
-- -   __Calculated health checks:__ Route 53 stops aggregating the status
--     of the referenced health checks.
--
-- -   __Health checks that monitor CloudWatch alarms:__ Route 53 stops
--     monitoring the corresponding CloudWatch metrics.
--
-- After you disable a health check, Route 53 considers the status of the
-- health check to always be healthy. If you configured DNS failover, Route
-- 53 continues to route traffic to the corresponding resources. If you
-- want to stop routing traffic to a resource, change the value of
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-Inverted Inverted>.
--
-- Charges for a health check still apply when the health check is
-- disabled. For more information, see
-- <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing>.
healthCheckConfig_disabled :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Bool)
healthCheckConfig_disabled = Lens.lens (\HealthCheckConfig' {disabled} -> disabled) (\s@HealthCheckConfig' {} a -> s {disabled = a} :: HealthCheckConfig)

-- | Specify whether you want Amazon Route 53 to send the value of
-- @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message
-- during TLS negotiation. This allows the endpoint to respond to @HTTPS@
-- health check requests with the applicable SSL\/TLS certificate.
--
-- Some endpoints require that @HTTPS@ requests include the host name in
-- the @client_hello@ message. If you don\'t enable SNI, the status of the
-- health check will be @SSL alert handshake_failure@. A health check can
-- also have that status for other reasons. If SNI is enabled and you\'re
-- still getting the error, check the SSL\/TLS configuration on your
-- endpoint and confirm that your certificate is valid.
--
-- The SSL\/TLS certificate on your endpoint includes a domain name in the
-- @Common Name@ field and possibly several more in the
-- @Subject Alternative Names@ field. One of the domain names in the
-- certificate should match the value that you specify for
-- @FullyQualifiedDomainName@. If the endpoint responds to the
-- @client_hello@ message with a certificate that does not include the
-- domain name that you specified in @FullyQualifiedDomainName@, a health
-- checker will retry the handshake. In the second attempt, the health
-- checker will omit @FullyQualifiedDomainName@ from the @client_hello@
-- message.
healthCheckConfig_enableSNI :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Bool)
healthCheckConfig_enableSNI = Lens.lens (\HealthCheckConfig' {enableSNI} -> enableSNI) (\s@HealthCheckConfig' {} a -> s {enableSNI = a} :: HealthCheckConfig)

-- | The number of consecutive health checks that an endpoint must pass or
-- fail for Amazon Route 53 to change the current status of the endpoint
-- from unhealthy to healthy or vice versa. For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy>
-- in the /Amazon Route 53 Developer Guide/.
--
-- If you don\'t specify a value for @FailureThreshold@, the default value
-- is three health checks.
healthCheckConfig_failureThreshold :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Natural)
healthCheckConfig_failureThreshold = Lens.lens (\HealthCheckConfig' {failureThreshold} -> failureThreshold) (\s@HealthCheckConfig' {} a -> s {failureThreshold = a} :: HealthCheckConfig)

-- | Amazon Route 53 behavior depends on whether you specify a value for
-- @IPAddress@.
--
-- __If you specify a value for__ @IPAddress@:
--
-- Amazon Route 53 sends health check requests to the specified IPv4 or
-- IPv6 address and passes the value of @FullyQualifiedDomainName@ in the
-- @Host@ header for all health checks except TCP health checks. This is
-- typically the fully qualified DNS name of the endpoint on which you want
-- Route 53 to perform health checks.
--
-- When Route 53 checks the health of an endpoint, here is how it
-- constructs the @Host@ header:
--
-- -   If you specify a value of @80@ for @Port@ and @HTTP@ or
--     @HTTP_STR_MATCH@ for @Type@, Route 53 passes the value of
--     @FullyQualifiedDomainName@ to the endpoint in the Host header.
--
-- -   If you specify a value of @443@ for @Port@ and @HTTPS@ or
--     @HTTPS_STR_MATCH@ for @Type@, Route 53 passes the value of
--     @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
--
-- -   If you specify another value for @Port@ and any value except @TCP@
--     for @Type@, Route 53 passes @FullyQualifiedDomainName:Port@ to the
--     endpoint in the @Host@ header.
--
-- If you don\'t specify a value for @FullyQualifiedDomainName@, Route 53
-- substitutes the value of @IPAddress@ in the @Host@ header in each of the
-- preceding cases.
--
-- __If you don\'t specify a value for__ @IPAddress@:
--
-- Route 53 sends a DNS request to the domain that you specify for
-- @FullyQualifiedDomainName@ at the interval that you specify for
-- @RequestInterval@. Using an IPv4 address that DNS returns, Route 53 then
-- checks the health of the endpoint.
--
-- If you don\'t specify a value for @IPAddress@, Route 53 uses only IPv4
-- to send health checks to the endpoint. If there\'s no resource record
-- set with a type of A for the name that you specify for
-- @FullyQualifiedDomainName@, the health check fails with a \"DNS
-- resolution failed\" error.
--
-- If you want to check the health of weighted, latency, or failover
-- resource record sets and you choose to specify the endpoint only by
-- @FullyQualifiedDomainName@, we recommend that you create a separate
-- health check for each endpoint. For example, create a health check for
-- each HTTP server that is serving content for www.example.com. For the
-- value of @FullyQualifiedDomainName@, specify the domain name of the
-- server (such as us-east-2-www.example.com), not the name of the resource
-- record sets (www.example.com).
--
-- In this configuration, if you create a health check for which the value
-- of @FullyQualifiedDomainName@ matches the name of the resource record
-- sets and you then associate the health check with those resource record
-- sets, health check results will be unpredictable.
--
-- In addition, if the value that you specify for @Type@ is @HTTP@,
-- @HTTPS@, @HTTP_STR_MATCH@, or @HTTPS_STR_MATCH@, Route 53 passes the
-- value of @FullyQualifiedDomainName@ in the @Host@ header, as it does
-- when you specify a value for @IPAddress@. If the value of @Type@ is
-- @TCP@, Route 53 doesn\'t pass a @Host@ header.
healthCheckConfig_fullyQualifiedDomainName :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Text)
healthCheckConfig_fullyQualifiedDomainName = Lens.lens (\HealthCheckConfig' {fullyQualifiedDomainName} -> fullyQualifiedDomainName) (\s@HealthCheckConfig' {} a -> s {fullyQualifiedDomainName = a} :: HealthCheckConfig)

-- | The number of child health checks that are associated with a
-- @CALCULATED@ health check that Amazon Route 53 must consider healthy for
-- the @CALCULATED@ health check to be considered healthy. To specify the
-- child health checks that you want to associate with a @CALCULATED@
-- health check, use the
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-ChildHealthChecks ChildHealthChecks>
-- element.
--
-- Note the following:
--
-- -   If you specify a number greater than the number of child health
--     checks, Route 53 always considers this health check to be unhealthy.
--
-- -   If you specify @0@, Route 53 always considers this health check to
--     be healthy.
healthCheckConfig_healthThreshold :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Natural)
healthCheckConfig_healthThreshold = Lens.lens (\HealthCheckConfig' {healthThreshold} -> healthThreshold) (\s@HealthCheckConfig' {} a -> s {healthThreshold = a} :: HealthCheckConfig)

-- | The IPv4 or IPv6 IP address of the endpoint that you want Amazon Route
-- 53 to perform health checks on. If you don\'t specify a value for
-- @IPAddress@, Route 53 sends a DNS request to resolve the domain name
-- that you specify in @FullyQualifiedDomainName@ at the interval that you
-- specify in @RequestInterval@. Using an IP address returned by DNS, Route
-- 53 then checks the health of the endpoint.
--
-- Use one of the following formats for the value of @IPAddress@:
--
-- -   __IPv4 address__: four values between 0 and 255, separated by
--     periods (.), for example, @192.0.2.44@.
--
-- -   __IPv6 address__: eight groups of four hexadecimal values, separated
--     by colons (:), for example,
--     @2001:0db8:85a3:0000:0000:abcd:0001:2345@. You can also shorten IPv6
--     addresses as described in RFC 5952, for example,
--     @2001:db8:85a3::abcd:1:2345@.
--
-- If the endpoint is an EC2 instance, we recommend that you create an
-- Elastic IP address, associate it with your EC2 instance, and specify the
-- Elastic IP address for @IPAddress@. This ensures that the IP address of
-- your instance will never change.
--
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-FullyQualifiedDomainName FullyQualifiedDomainName>.
--
-- Constraints: Route 53 can\'t check the health of endpoints for which the
-- IP address is in local, private, non-routable, or multicast ranges. For
-- more information about IP addresses for which you can\'t create health
-- checks, see the following documents:
--
-- -   <https://tools.ietf.org/html/rfc5735 RFC 5735, Special Use IPv4 Addresses>
--
-- -   <https://tools.ietf.org/html/rfc6598 RFC 6598, IANA-Reserved IPv4 Prefix for Shared Address Space>
--
-- -   <https://tools.ietf.org/html/rfc5156 RFC 5156, Special-Use IPv6 Addresses>
--
-- When the value of @Type@ is @CALCULATED@ or @CLOUDWATCH_METRIC@, omit
-- @IPAddress@.
healthCheckConfig_iPAddress :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Text)
healthCheckConfig_iPAddress = Lens.lens (\HealthCheckConfig' {iPAddress} -> iPAddress) (\s@HealthCheckConfig' {} a -> s {iPAddress = a} :: HealthCheckConfig)

-- | When CloudWatch has insufficient data about the metric to determine the
-- alarm state, the status that you want Amazon Route 53 to assign to the
-- health check:
--
-- -   @Healthy@: Route 53 considers the health check to be healthy.
--
-- -   @Unhealthy@: Route 53 considers the health check to be unhealthy.
--
-- -   @LastKnownStatus@: Route 53 uses the status of the health check from
--     the last time that CloudWatch had sufficient data to determine the
--     alarm state. For new health checks that have no last known status,
--     the default status for the health check is healthy.
healthCheckConfig_insufficientDataHealthStatus :: Lens.Lens' HealthCheckConfig (Prelude.Maybe InsufficientDataHealthStatus)
healthCheckConfig_insufficientDataHealthStatus = Lens.lens (\HealthCheckConfig' {insufficientDataHealthStatus} -> insufficientDataHealthStatus) (\s@HealthCheckConfig' {} a -> s {insufficientDataHealthStatus = a} :: HealthCheckConfig)

-- | Specify whether you want Amazon Route 53 to invert the status of a
-- health check, for example, to consider a health check unhealthy when it
-- otherwise would be considered healthy.
healthCheckConfig_inverted :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Bool)
healthCheckConfig_inverted = Lens.lens (\HealthCheckConfig' {inverted} -> inverted) (\s@HealthCheckConfig' {} a -> s {inverted = a} :: HealthCheckConfig)

-- | Specify whether you want Amazon Route 53 to measure the latency between
-- health checkers in multiple Amazon Web Services regions and your
-- endpoint, and to display CloudWatch latency graphs on the __Health
-- Checks__ page in the Route 53 console.
--
-- You can\'t change the value of @MeasureLatency@ after you create a
-- health check.
healthCheckConfig_measureLatency :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Bool)
healthCheckConfig_measureLatency = Lens.lens (\HealthCheckConfig' {measureLatency} -> measureLatency) (\s@HealthCheckConfig' {} a -> s {measureLatency = a} :: HealthCheckConfig)

-- | The port on the endpoint that you want Amazon Route 53 to perform health
-- checks on.
--
-- Don\'t specify a value for @Port@ when you specify a value for @Type@ of
-- @CLOUDWATCH_METRIC@ or @CALCULATED@.
healthCheckConfig_port :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Natural)
healthCheckConfig_port = Lens.lens (\HealthCheckConfig' {port} -> port) (\s@HealthCheckConfig' {} a -> s {port = a} :: HealthCheckConfig)

-- | A complex type that contains one @Region@ element for each region from
-- which you want Amazon Route 53 health checkers to check the specified
-- endpoint.
--
-- If you don\'t specify any regions, Route 53 health checkers
-- automatically performs checks from all of the regions that are listed
-- under __Valid Values__.
--
-- If you update a health check to remove a region that has been performing
-- health checks, Route 53 will briefly continue to perform checks from
-- that region to ensure that some health checkers are always checking the
-- endpoint (for example, if you replace three regions with four different
-- regions).
healthCheckConfig_regions :: Lens.Lens' HealthCheckConfig (Prelude.Maybe (Prelude.NonEmpty HealthCheckRegion))
healthCheckConfig_regions = Lens.lens (\HealthCheckConfig' {regions} -> regions) (\s@HealthCheckConfig' {} a -> s {regions = a} :: HealthCheckConfig) Prelude.. Lens.mapping Lens.coerced

-- | The number of seconds between the time that Amazon Route 53 gets a
-- response from your endpoint and the time that it sends the next health
-- check request. Each Route 53 health checker makes requests at this
-- interval.
--
-- You can\'t change the value of @RequestInterval@ after you create a
-- health check.
--
-- If you don\'t specify a value for @RequestInterval@, the default value
-- is @30@ seconds.
healthCheckConfig_requestInterval :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Natural)
healthCheckConfig_requestInterval = Lens.lens (\HealthCheckConfig' {requestInterval} -> requestInterval) (\s@HealthCheckConfig' {} a -> s {requestInterval = a} :: HealthCheckConfig)

-- | The path, if any, that you want Amazon Route 53 to request when
-- performing health checks. The path can be any value for which your
-- endpoint will return an HTTP status code of 2xx or 3xx when the endpoint
-- is healthy, for example, the file \/docs\/route53-health-check.html. You
-- can also include query string parameters, for example,
-- @\/welcome.html?language=jp&login=y@.
healthCheckConfig_resourcePath :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Text)
healthCheckConfig_resourcePath = Lens.lens (\HealthCheckConfig' {resourcePath} -> resourcePath) (\s@HealthCheckConfig' {} a -> s {resourcePath = a} :: HealthCheckConfig)

-- | The Amazon Resource Name (ARN) for the Route 53 Application Recovery
-- Controller routing control.
--
-- For more information about Route 53 Application Recovery Controller, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/what-is-route-53-recovery.html Route 53 Application Recovery Controller Developer Guide.>.
healthCheckConfig_routingControlArn :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Text)
healthCheckConfig_routingControlArn = Lens.lens (\HealthCheckConfig' {routingControlArn} -> routingControlArn) (\s@HealthCheckConfig' {} a -> s {routingControlArn = a} :: HealthCheckConfig)

-- | If the value of Type is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@, the
-- string that you want Amazon Route 53 to search for in the response body
-- from the specified resource. If the string appears in the response body,
-- Route 53 considers the resource healthy.
--
-- Route 53 considers case when searching for @SearchString@ in the
-- response body.
healthCheckConfig_searchString :: Lens.Lens' HealthCheckConfig (Prelude.Maybe Prelude.Text)
healthCheckConfig_searchString = Lens.lens (\HealthCheckConfig' {searchString} -> searchString) (\s@HealthCheckConfig' {} a -> s {searchString = a} :: HealthCheckConfig)

-- | The type of health check that you want to create, which indicates how
-- Amazon Route 53 determines whether an endpoint is healthy.
--
-- You can\'t change the value of @Type@ after you create a health check.
--
-- You can create the following types of health checks:
--
-- -   __HTTP__: Route 53 tries to establish a TCP connection. If
--     successful, Route 53 submits an HTTP request and waits for an HTTP
--     status code of 200 or greater and less than 400.
--
-- -   __HTTPS__: Route 53 tries to establish a TCP connection. If
--     successful, Route 53 submits an HTTPS request and waits for an HTTP
--     status code of 200 or greater and less than 400.
--
--     If you specify @HTTPS@ for the value of @Type@, the endpoint must
--     support TLS v1.0 or later.
--
-- -   __HTTP_STR_MATCH__: Route 53 tries to establish a TCP connection. If
--     successful, Route 53 submits an HTTP request and searches the first
--     5,120 bytes of the response body for the string that you specify in
--     @SearchString@.
--
-- -   __HTTPS_STR_MATCH__: Route 53 tries to establish a TCP connection.
--     If successful, Route 53 submits an @HTTPS@ request and searches the
--     first 5,120 bytes of the response body for the string that you
--     specify in @SearchString@.
--
-- -   __TCP__: Route 53 tries to establish a TCP connection.
--
-- -   __CLOUDWATCH_METRIC__: The health check is associated with a
--     CloudWatch alarm. If the state of the alarm is @OK@, the health
--     check is considered healthy. If the state is @ALARM@, the health
--     check is considered unhealthy. If CloudWatch doesn\'t have
--     sufficient data to determine whether the state is @OK@ or @ALARM@,
--     the health check status depends on the setting for
--     @InsufficientDataHealthStatus@: @Healthy@, @Unhealthy@, or
--     @LastKnownStatus@.
--
-- -   __CALCULATED__: For health checks that monitor the status of other
--     health checks, Route 53 adds up the number of health checks that
--     Route 53 health checkers consider to be healthy and compares that
--     number with the value of @HealthThreshold@.
--
-- -   __RECOVERY_CONTROL__: The health check is assocated with a Route53
--     Application Recovery Controller routing control. If the routing
--     control state is @ON@, the health check is considered healthy. If
--     the state is @OFF@, the health check is considered unhealthy.
--
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy>
-- in the /Amazon Route 53 Developer Guide/.
healthCheckConfig_type :: Lens.Lens' HealthCheckConfig HealthCheckType
healthCheckConfig_type = Lens.lens (\HealthCheckConfig' {type'} -> type') (\s@HealthCheckConfig' {} a -> s {type' = a} :: HealthCheckConfig)

instance Data.FromXML HealthCheckConfig where
  parseXML x =
    HealthCheckConfig'
      Prelude.<$> (x Data..@? "AlarmIdentifier")
      Prelude.<*> ( x
                      Data..@? "ChildHealthChecks"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "ChildHealthCheck")
                  )
      Prelude.<*> (x Data..@? "Disabled")
      Prelude.<*> (x Data..@? "EnableSNI")
      Prelude.<*> (x Data..@? "FailureThreshold")
      Prelude.<*> (x Data..@? "FullyQualifiedDomainName")
      Prelude.<*> (x Data..@? "HealthThreshold")
      Prelude.<*> (x Data..@? "IPAddress")
      Prelude.<*> (x Data..@? "InsufficientDataHealthStatus")
      Prelude.<*> (x Data..@? "Inverted")
      Prelude.<*> (x Data..@? "MeasureLatency")
      Prelude.<*> (x Data..@? "Port")
      Prelude.<*> ( x
                      Data..@? "Regions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList1 "Region")
                  )
      Prelude.<*> (x Data..@? "RequestInterval")
      Prelude.<*> (x Data..@? "ResourcePath")
      Prelude.<*> (x Data..@? "RoutingControlArn")
      Prelude.<*> (x Data..@? "SearchString")
      Prelude.<*> (x Data..@ "Type")

instance Prelude.Hashable HealthCheckConfig where
  hashWithSalt _salt HealthCheckConfig' {..} =
    _salt
      `Prelude.hashWithSalt` alarmIdentifier
      `Prelude.hashWithSalt` childHealthChecks
      `Prelude.hashWithSalt` disabled
      `Prelude.hashWithSalt` enableSNI
      `Prelude.hashWithSalt` failureThreshold
      `Prelude.hashWithSalt` fullyQualifiedDomainName
      `Prelude.hashWithSalt` healthThreshold
      `Prelude.hashWithSalt` iPAddress
      `Prelude.hashWithSalt` insufficientDataHealthStatus
      `Prelude.hashWithSalt` inverted
      `Prelude.hashWithSalt` measureLatency
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` regions
      `Prelude.hashWithSalt` requestInterval
      `Prelude.hashWithSalt` resourcePath
      `Prelude.hashWithSalt` routingControlArn
      `Prelude.hashWithSalt` searchString
      `Prelude.hashWithSalt` type'

instance Prelude.NFData HealthCheckConfig where
  rnf HealthCheckConfig' {..} =
    Prelude.rnf alarmIdentifier
      `Prelude.seq` Prelude.rnf childHealthChecks
      `Prelude.seq` Prelude.rnf disabled
      `Prelude.seq` Prelude.rnf enableSNI
      `Prelude.seq` Prelude.rnf failureThreshold
      `Prelude.seq` Prelude.rnf fullyQualifiedDomainName
      `Prelude.seq` Prelude.rnf healthThreshold
      `Prelude.seq` Prelude.rnf iPAddress
      `Prelude.seq` Prelude.rnf insufficientDataHealthStatus
      `Prelude.seq` Prelude.rnf inverted
      `Prelude.seq` Prelude.rnf measureLatency
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf requestInterval
      `Prelude.seq` Prelude.rnf resourcePath
      `Prelude.seq` Prelude.rnf routingControlArn
      `Prelude.seq` Prelude.rnf searchString
      `Prelude.seq` Prelude.rnf type'

instance Data.ToXML HealthCheckConfig where
  toXML HealthCheckConfig' {..} =
    Prelude.mconcat
      [ "AlarmIdentifier" Data.@= alarmIdentifier,
        "ChildHealthChecks"
          Data.@= Data.toXML
            ( Data.toXMLList "ChildHealthCheck"
                Prelude.<$> childHealthChecks
            ),
        "Disabled" Data.@= disabled,
        "EnableSNI" Data.@= enableSNI,
        "FailureThreshold" Data.@= failureThreshold,
        "FullyQualifiedDomainName"
          Data.@= fullyQualifiedDomainName,
        "HealthThreshold" Data.@= healthThreshold,
        "IPAddress" Data.@= iPAddress,
        "InsufficientDataHealthStatus"
          Data.@= insufficientDataHealthStatus,
        "Inverted" Data.@= inverted,
        "MeasureLatency" Data.@= measureLatency,
        "Port" Data.@= port,
        "Regions"
          Data.@= Data.toXML
            (Data.toXMLList "Region" Prelude.<$> regions),
        "RequestInterval" Data.@= requestInterval,
        "ResourcePath" Data.@= resourcePath,
        "RoutingControlArn" Data.@= routingControlArn,
        "SearchString" Data.@= searchString,
        "Type" Data.@= type'
      ]
