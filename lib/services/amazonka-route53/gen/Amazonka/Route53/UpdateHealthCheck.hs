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
-- Module      : Amazonka.Route53.UpdateHealthCheck
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing health check. Note that some values can\'t be
-- updated.
--
-- For more information about updating health checks, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/health-checks-creating-deleting.html Creating, Updating, and Deleting Health Checks>
-- in the /Amazon Route 53 Developer Guide/.
module Amazonka.Route53.UpdateHealthCheck
  ( -- * Creating a Request
    UpdateHealthCheck (..),
    newUpdateHealthCheck,

    -- * Request Lenses
    updateHealthCheck_port,
    updateHealthCheck_healthThreshold,
    updateHealthCheck_healthCheckVersion,
    updateHealthCheck_failureThreshold,
    updateHealthCheck_alarmIdentifier,
    updateHealthCheck_regions,
    updateHealthCheck_resourcePath,
    updateHealthCheck_childHealthChecks,
    updateHealthCheck_searchString,
    updateHealthCheck_resetElements,
    updateHealthCheck_disabled,
    updateHealthCheck_inverted,
    updateHealthCheck_fullyQualifiedDomainName,
    updateHealthCheck_insufficientDataHealthStatus,
    updateHealthCheck_enableSNI,
    updateHealthCheck_iPAddress,
    updateHealthCheck_healthCheckId,

    -- * Destructuring the Response
    UpdateHealthCheckResponse (..),
    newUpdateHealthCheckResponse,

    -- * Response Lenses
    updateHealthCheckResponse_httpStatus,
    updateHealthCheckResponse_healthCheck,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A complex type that contains information about a request to update a
-- health check.
--
-- /See:/ 'newUpdateHealthCheck' smart constructor.
data UpdateHealthCheck = UpdateHealthCheck'
  { -- | The port on the endpoint that you want Amazon Route 53 to perform health
    -- checks on.
    --
    -- Don\'t specify a value for @Port@ when you specify a value for @Type@ of
    -- @CLOUDWATCH_METRIC@ or @CALCULATED@.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The number of child health checks that are associated with a
    -- @CALCULATED@ health that Amazon Route 53 must consider healthy for the
    -- @CALCULATED@ health check to be considered healthy. To specify the child
    -- health checks that you want to associate with a @CALCULATED@ health
    -- check, use the @ChildHealthChecks@ and @ChildHealthCheck@ elements.
    --
    -- Note the following:
    --
    -- -   If you specify a number greater than the number of child health
    --     checks, Route 53 always considers this health check to be unhealthy.
    --
    -- -   If you specify @0@, Route 53 always considers this health check to
    --     be healthy.
    healthThreshold :: Prelude.Maybe Prelude.Natural,
    -- | A sequential counter that Amazon Route 53 sets to @1@ when you create a
    -- health check and increments by 1 each time you update settings for the
    -- health check.
    --
    -- We recommend that you use @GetHealthCheck@ or @ListHealthChecks@ to get
    -- the current value of @HealthCheckVersion@ for the health check that you
    -- want to update, and that you include that value in your
    -- @UpdateHealthCheck@ request. This prevents Route 53 from overwriting an
    -- intervening update:
    --
    -- -   If the value in the @UpdateHealthCheck@ request matches the value of
    --     @HealthCheckVersion@ in the health check, Route 53 updates the
    --     health check with the new settings.
    --
    -- -   If the value of @HealthCheckVersion@ in the health check is greater,
    --     the health check was changed after you got the version number. Route
    --     53 does not update the health check, and it returns a
    --     @HealthCheckVersionMismatch@ error.
    healthCheckVersion :: Prelude.Maybe Prelude.Natural,
    -- | The number of consecutive health checks that an endpoint must pass or
    -- fail for Amazon Route 53 to change the current status of the endpoint
    -- from unhealthy to healthy or vice versa. For more information, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy>
    -- in the /Amazon Route 53 Developer Guide/.
    --
    -- If you don\'t specify a value for @FailureThreshold@, the default value
    -- is three health checks.
    failureThreshold :: Prelude.Maybe Prelude.Natural,
    -- | A complex type that identifies the CloudWatch alarm that you want Amazon
    -- Route 53 health checkers to use to determine whether the specified
    -- health check is healthy.
    alarmIdentifier :: Prelude.Maybe AlarmIdentifier,
    -- | A complex type that contains one @Region@ element for each region that
    -- you want Amazon Route 53 health checkers to check the specified endpoint
    -- from.
    regions :: Prelude.Maybe (Prelude.NonEmpty HealthCheckRegion),
    -- | The path that you want Amazon Route 53 to request when performing health
    -- checks. The path can be any value for which your endpoint will return an
    -- HTTP status code of 2xx or 3xx when the endpoint is healthy, for example
    -- the file \/docs\/route53-health-check.html. You can also include query
    -- string parameters, for example, @\/welcome.html?language=jp&login=y@.
    --
    -- Specify this value only if you want to change it.
    resourcePath :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains one @ChildHealthCheck@ element for each
    -- health check that you want to associate with a @CALCULATED@ health
    -- check.
    childHealthChecks :: Prelude.Maybe [Prelude.Text],
    -- | If the value of @Type@ is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@, the
    -- string that you want Amazon Route 53 to search for in the response body
    -- from the specified resource. If the string appears in the response body,
    -- Route 53 considers the resource healthy. (You can\'t change the value of
    -- @Type@ when you update a health check.)
    searchString :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains one @ResettableElementName@ element for
    -- each element that you want to reset to the default value. Valid values
    -- for @ResettableElementName@ include the following:
    --
    -- -   @ChildHealthChecks@: Amazon Route 53 resets
    --     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-ChildHealthChecks ChildHealthChecks>
    --     to null.
    --
    -- -   @FullyQualifiedDomainName@: Route 53 resets
    --     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-FullyQualifiedDomainName FullyQualifiedDomainName>.
    --     to null.
    --
    -- -   @Regions@: Route 53 resets the
    --     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-Regions Regions>
    --     list to the default set of regions.
    --
    -- -   @ResourcePath@: Route 53 resets
    --     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-ResourcePath ResourcePath>
    --     to null.
    resetElements :: Prelude.Maybe [ResettableElementName],
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
    -- | Specify whether you want Amazon Route 53 to invert the status of a
    -- health check, for example, to consider a health check unhealthy when it
    -- otherwise would be considered healthy.
    inverted :: Prelude.Maybe Prelude.Bool,
    -- | Amazon Route 53 behavior depends on whether you specify a value for
    -- @IPAddress@.
    --
    -- If a health check already has a value for @IPAddress@, you can change
    -- the value. However, you can\'t update an existing health check to add or
    -- remove the value of @IPAddress@.
    --
    -- __If you specify a value for__ @IPAddress@:
    --
    -- Route 53 sends health check requests to the specified IPv4 or IPv6
    -- address and passes the value of @FullyQualifiedDomainName@ in the @Host@
    -- header for all health checks except TCP health checks. This is typically
    -- the fully qualified DNS name of the endpoint on which you want Route 53
    -- to perform health checks.
    --
    -- When Route 53 checks the health of an endpoint, here is how it
    -- constructs the @Host@ header:
    --
    -- -   If you specify a value of @80@ for @Port@ and @HTTP@ or
    --     @HTTP_STR_MATCH@ for @Type@, Route 53 passes the value of
    --     @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
    --
    -- -   If you specify a value of @443@ for @Port@ and @HTTPS@ or
    --     @HTTPS_STR_MATCH@ for @Type@, Route 53 passes the value of
    --     @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
    --
    -- -   If you specify another value for @Port@ and any value except @TCP@
    --     for @Type@, Route 53 passes /@FullyQualifiedDomainName@:@Port@/ to
    --     the endpoint in the @Host@ header.
    --
    -- If you don\'t specify a value for @FullyQualifiedDomainName@, Route 53
    -- substitutes the value of @IPAddress@ in the @Host@ header in each of the
    -- above cases.
    --
    -- __If you don\'t specify a value for__ @IPAddress@:
    --
    -- If you don\'t specify a value for @IPAddress@, Route 53 sends a DNS
    -- request to the domain that you specify in @FullyQualifiedDomainName@ at
    -- the interval you specify in @RequestInterval@. Using an IPv4 address
    -- that is returned by DNS, Route 53 then checks the health of the
    -- endpoint.
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
    -- server (such as @us-east-2-www.example.com@), not the name of the
    -- resource record sets (www.example.com).
    --
    -- In this configuration, if the value of @FullyQualifiedDomainName@
    -- matches the name of the resource record sets and you then associate the
    -- health check with those resource record sets, health check results will
    -- be unpredictable.
    --
    -- In addition, if the value of @Type@ is @HTTP@, @HTTPS@,
    -- @HTTP_STR_MATCH@, or @HTTPS_STR_MATCH@, Route 53 passes the value of
    -- @FullyQualifiedDomainName@ in the @Host@ header, as it does when you
    -- specify a value for @IPAddress@. If the value of @Type@ is @TCP@, Route
    -- 53 doesn\'t pass a @Host@ header.
    fullyQualifiedDomainName :: Prelude.Maybe Prelude.Text,
    -- | When CloudWatch has insufficient data about the metric to determine the
    -- alarm state, the status that you want Amazon Route 53 to assign to the
    -- health check:
    --
    -- -   @Healthy@: Route 53 considers the health check to be healthy.
    --
    -- -   @Unhealthy@: Route 53 considers the health check to be unhealthy.
    --
    -- -   @LastKnownStatus@: By default, Route 53 uses the status of the
    --     health check from the last time CloudWatch had sufficient data to
    --     determine the alarm state. For new health checks that have no last
    --     known status, the status for the health check is healthy.
    insufficientDataHealthStatus :: Prelude.Maybe InsufficientDataHealthStatus,
    -- | Specify whether you want Amazon Route 53 to send the value of
    -- @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message
    -- during @TLS@ negotiation. This allows the endpoint to respond to @HTTPS@
    -- health check requests with the applicable SSL\/TLS certificate.
    --
    -- Some endpoints require that HTTPS requests include the host name in the
    -- @client_hello@ message. If you don\'t enable SNI, the status of the
    -- health check will be SSL alert @handshake_failure@. A health check can
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
    -- | The IPv4 or IPv6 IP address for the endpoint that you want Amazon Route
    -- 53 to perform health checks on. If you don\'t specify a value for
    -- @IPAddress@, Route 53 sends a DNS request to resolve the domain name
    -- that you specify in @FullyQualifiedDomainName@ at the interval that you
    -- specify in @RequestInterval@. Using an IP address that is returned by
    -- DNS, Route 53 then checks the health of the endpoint.
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
    -- your instance never changes. For more information, see the applicable
    -- documentation:
    --
    -- -   Linux:
    --     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses (EIP)>
    --     in the /Amazon EC2 User Guide for Linux Instances/
    --
    -- -   Windows:
    --     <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-ip-addresses-eip.html Elastic IP Addresses (EIP)>
    --     in the /Amazon EC2 User Guide for Windows Instances/
    --
    -- If a health check already has a value for @IPAddress@, you can change
    -- the value. However, you can\'t update an existing health check to add or
    -- remove the value of @IPAddress@.
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
    iPAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID for the health check for which you want detailed information.
    -- When you created the health check, @CreateHealthCheck@ returned the ID
    -- in the response, in the @HealthCheckId@ element.
    healthCheckId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateHealthCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'updateHealthCheck_port' - The port on the endpoint that you want Amazon Route 53 to perform health
-- checks on.
--
-- Don\'t specify a value for @Port@ when you specify a value for @Type@ of
-- @CLOUDWATCH_METRIC@ or @CALCULATED@.
--
-- 'healthThreshold', 'updateHealthCheck_healthThreshold' - The number of child health checks that are associated with a
-- @CALCULATED@ health that Amazon Route 53 must consider healthy for the
-- @CALCULATED@ health check to be considered healthy. To specify the child
-- health checks that you want to associate with a @CALCULATED@ health
-- check, use the @ChildHealthChecks@ and @ChildHealthCheck@ elements.
--
-- Note the following:
--
-- -   If you specify a number greater than the number of child health
--     checks, Route 53 always considers this health check to be unhealthy.
--
-- -   If you specify @0@, Route 53 always considers this health check to
--     be healthy.
--
-- 'healthCheckVersion', 'updateHealthCheck_healthCheckVersion' - A sequential counter that Amazon Route 53 sets to @1@ when you create a
-- health check and increments by 1 each time you update settings for the
-- health check.
--
-- We recommend that you use @GetHealthCheck@ or @ListHealthChecks@ to get
-- the current value of @HealthCheckVersion@ for the health check that you
-- want to update, and that you include that value in your
-- @UpdateHealthCheck@ request. This prevents Route 53 from overwriting an
-- intervening update:
--
-- -   If the value in the @UpdateHealthCheck@ request matches the value of
--     @HealthCheckVersion@ in the health check, Route 53 updates the
--     health check with the new settings.
--
-- -   If the value of @HealthCheckVersion@ in the health check is greater,
--     the health check was changed after you got the version number. Route
--     53 does not update the health check, and it returns a
--     @HealthCheckVersionMismatch@ error.
--
-- 'failureThreshold', 'updateHealthCheck_failureThreshold' - The number of consecutive health checks that an endpoint must pass or
-- fail for Amazon Route 53 to change the current status of the endpoint
-- from unhealthy to healthy or vice versa. For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy>
-- in the /Amazon Route 53 Developer Guide/.
--
-- If you don\'t specify a value for @FailureThreshold@, the default value
-- is three health checks.
--
-- 'alarmIdentifier', 'updateHealthCheck_alarmIdentifier' - A complex type that identifies the CloudWatch alarm that you want Amazon
-- Route 53 health checkers to use to determine whether the specified
-- health check is healthy.
--
-- 'regions', 'updateHealthCheck_regions' - A complex type that contains one @Region@ element for each region that
-- you want Amazon Route 53 health checkers to check the specified endpoint
-- from.
--
-- 'resourcePath', 'updateHealthCheck_resourcePath' - The path that you want Amazon Route 53 to request when performing health
-- checks. The path can be any value for which your endpoint will return an
-- HTTP status code of 2xx or 3xx when the endpoint is healthy, for example
-- the file \/docs\/route53-health-check.html. You can also include query
-- string parameters, for example, @\/welcome.html?language=jp&login=y@.
--
-- Specify this value only if you want to change it.
--
-- 'childHealthChecks', 'updateHealthCheck_childHealthChecks' - A complex type that contains one @ChildHealthCheck@ element for each
-- health check that you want to associate with a @CALCULATED@ health
-- check.
--
-- 'searchString', 'updateHealthCheck_searchString' - If the value of @Type@ is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@, the
-- string that you want Amazon Route 53 to search for in the response body
-- from the specified resource. If the string appears in the response body,
-- Route 53 considers the resource healthy. (You can\'t change the value of
-- @Type@ when you update a health check.)
--
-- 'resetElements', 'updateHealthCheck_resetElements' - A complex type that contains one @ResettableElementName@ element for
-- each element that you want to reset to the default value. Valid values
-- for @ResettableElementName@ include the following:
--
-- -   @ChildHealthChecks@: Amazon Route 53 resets
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-ChildHealthChecks ChildHealthChecks>
--     to null.
--
-- -   @FullyQualifiedDomainName@: Route 53 resets
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-FullyQualifiedDomainName FullyQualifiedDomainName>.
--     to null.
--
-- -   @Regions@: Route 53 resets the
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-Regions Regions>
--     list to the default set of regions.
--
-- -   @ResourcePath@: Route 53 resets
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-ResourcePath ResourcePath>
--     to null.
--
-- 'disabled', 'updateHealthCheck_disabled' - Stops Route 53 from performing health checks. When you disable a health
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
-- 'inverted', 'updateHealthCheck_inverted' - Specify whether you want Amazon Route 53 to invert the status of a
-- health check, for example, to consider a health check unhealthy when it
-- otherwise would be considered healthy.
--
-- 'fullyQualifiedDomainName', 'updateHealthCheck_fullyQualifiedDomainName' - Amazon Route 53 behavior depends on whether you specify a value for
-- @IPAddress@.
--
-- If a health check already has a value for @IPAddress@, you can change
-- the value. However, you can\'t update an existing health check to add or
-- remove the value of @IPAddress@.
--
-- __If you specify a value for__ @IPAddress@:
--
-- Route 53 sends health check requests to the specified IPv4 or IPv6
-- address and passes the value of @FullyQualifiedDomainName@ in the @Host@
-- header for all health checks except TCP health checks. This is typically
-- the fully qualified DNS name of the endpoint on which you want Route 53
-- to perform health checks.
--
-- When Route 53 checks the health of an endpoint, here is how it
-- constructs the @Host@ header:
--
-- -   If you specify a value of @80@ for @Port@ and @HTTP@ or
--     @HTTP_STR_MATCH@ for @Type@, Route 53 passes the value of
--     @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
--
-- -   If you specify a value of @443@ for @Port@ and @HTTPS@ or
--     @HTTPS_STR_MATCH@ for @Type@, Route 53 passes the value of
--     @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
--
-- -   If you specify another value for @Port@ and any value except @TCP@
--     for @Type@, Route 53 passes /@FullyQualifiedDomainName@:@Port@/ to
--     the endpoint in the @Host@ header.
--
-- If you don\'t specify a value for @FullyQualifiedDomainName@, Route 53
-- substitutes the value of @IPAddress@ in the @Host@ header in each of the
-- above cases.
--
-- __If you don\'t specify a value for__ @IPAddress@:
--
-- If you don\'t specify a value for @IPAddress@, Route 53 sends a DNS
-- request to the domain that you specify in @FullyQualifiedDomainName@ at
-- the interval you specify in @RequestInterval@. Using an IPv4 address
-- that is returned by DNS, Route 53 then checks the health of the
-- endpoint.
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
-- server (such as @us-east-2-www.example.com@), not the name of the
-- resource record sets (www.example.com).
--
-- In this configuration, if the value of @FullyQualifiedDomainName@
-- matches the name of the resource record sets and you then associate the
-- health check with those resource record sets, health check results will
-- be unpredictable.
--
-- In addition, if the value of @Type@ is @HTTP@, @HTTPS@,
-- @HTTP_STR_MATCH@, or @HTTPS_STR_MATCH@, Route 53 passes the value of
-- @FullyQualifiedDomainName@ in the @Host@ header, as it does when you
-- specify a value for @IPAddress@. If the value of @Type@ is @TCP@, Route
-- 53 doesn\'t pass a @Host@ header.
--
-- 'insufficientDataHealthStatus', 'updateHealthCheck_insufficientDataHealthStatus' - When CloudWatch has insufficient data about the metric to determine the
-- alarm state, the status that you want Amazon Route 53 to assign to the
-- health check:
--
-- -   @Healthy@: Route 53 considers the health check to be healthy.
--
-- -   @Unhealthy@: Route 53 considers the health check to be unhealthy.
--
-- -   @LastKnownStatus@: By default, Route 53 uses the status of the
--     health check from the last time CloudWatch had sufficient data to
--     determine the alarm state. For new health checks that have no last
--     known status, the status for the health check is healthy.
--
-- 'enableSNI', 'updateHealthCheck_enableSNI' - Specify whether you want Amazon Route 53 to send the value of
-- @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message
-- during @TLS@ negotiation. This allows the endpoint to respond to @HTTPS@
-- health check requests with the applicable SSL\/TLS certificate.
--
-- Some endpoints require that HTTPS requests include the host name in the
-- @client_hello@ message. If you don\'t enable SNI, the status of the
-- health check will be SSL alert @handshake_failure@. A health check can
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
-- 'iPAddress', 'updateHealthCheck_iPAddress' - The IPv4 or IPv6 IP address for the endpoint that you want Amazon Route
-- 53 to perform health checks on. If you don\'t specify a value for
-- @IPAddress@, Route 53 sends a DNS request to resolve the domain name
-- that you specify in @FullyQualifiedDomainName@ at the interval that you
-- specify in @RequestInterval@. Using an IP address that is returned by
-- DNS, Route 53 then checks the health of the endpoint.
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
-- your instance never changes. For more information, see the applicable
-- documentation:
--
-- -   Linux:
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses (EIP)>
--     in the /Amazon EC2 User Guide for Linux Instances/
--
-- -   Windows:
--     <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-ip-addresses-eip.html Elastic IP Addresses (EIP)>
--     in the /Amazon EC2 User Guide for Windows Instances/
--
-- If a health check already has a value for @IPAddress@, you can change
-- the value. However, you can\'t update an existing health check to add or
-- remove the value of @IPAddress@.
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
-- 'healthCheckId', 'updateHealthCheck_healthCheckId' - The ID for the health check for which you want detailed information.
-- When you created the health check, @CreateHealthCheck@ returned the ID
-- in the response, in the @HealthCheckId@ element.
newUpdateHealthCheck ::
  -- | 'healthCheckId'
  Prelude.Text ->
  UpdateHealthCheck
newUpdateHealthCheck pHealthCheckId_ =
  UpdateHealthCheck'
    { port = Prelude.Nothing,
      healthThreshold = Prelude.Nothing,
      healthCheckVersion = Prelude.Nothing,
      failureThreshold = Prelude.Nothing,
      alarmIdentifier = Prelude.Nothing,
      regions = Prelude.Nothing,
      resourcePath = Prelude.Nothing,
      childHealthChecks = Prelude.Nothing,
      searchString = Prelude.Nothing,
      resetElements = Prelude.Nothing,
      disabled = Prelude.Nothing,
      inverted = Prelude.Nothing,
      fullyQualifiedDomainName = Prelude.Nothing,
      insufficientDataHealthStatus = Prelude.Nothing,
      enableSNI = Prelude.Nothing,
      iPAddress = Prelude.Nothing,
      healthCheckId = pHealthCheckId_
    }

-- | The port on the endpoint that you want Amazon Route 53 to perform health
-- checks on.
--
-- Don\'t specify a value for @Port@ when you specify a value for @Type@ of
-- @CLOUDWATCH_METRIC@ or @CALCULATED@.
updateHealthCheck_port :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe Prelude.Natural)
updateHealthCheck_port = Lens.lens (\UpdateHealthCheck' {port} -> port) (\s@UpdateHealthCheck' {} a -> s {port = a} :: UpdateHealthCheck)

-- | The number of child health checks that are associated with a
-- @CALCULATED@ health that Amazon Route 53 must consider healthy for the
-- @CALCULATED@ health check to be considered healthy. To specify the child
-- health checks that you want to associate with a @CALCULATED@ health
-- check, use the @ChildHealthChecks@ and @ChildHealthCheck@ elements.
--
-- Note the following:
--
-- -   If you specify a number greater than the number of child health
--     checks, Route 53 always considers this health check to be unhealthy.
--
-- -   If you specify @0@, Route 53 always considers this health check to
--     be healthy.
updateHealthCheck_healthThreshold :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe Prelude.Natural)
updateHealthCheck_healthThreshold = Lens.lens (\UpdateHealthCheck' {healthThreshold} -> healthThreshold) (\s@UpdateHealthCheck' {} a -> s {healthThreshold = a} :: UpdateHealthCheck)

-- | A sequential counter that Amazon Route 53 sets to @1@ when you create a
-- health check and increments by 1 each time you update settings for the
-- health check.
--
-- We recommend that you use @GetHealthCheck@ or @ListHealthChecks@ to get
-- the current value of @HealthCheckVersion@ for the health check that you
-- want to update, and that you include that value in your
-- @UpdateHealthCheck@ request. This prevents Route 53 from overwriting an
-- intervening update:
--
-- -   If the value in the @UpdateHealthCheck@ request matches the value of
--     @HealthCheckVersion@ in the health check, Route 53 updates the
--     health check with the new settings.
--
-- -   If the value of @HealthCheckVersion@ in the health check is greater,
--     the health check was changed after you got the version number. Route
--     53 does not update the health check, and it returns a
--     @HealthCheckVersionMismatch@ error.
updateHealthCheck_healthCheckVersion :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe Prelude.Natural)
updateHealthCheck_healthCheckVersion = Lens.lens (\UpdateHealthCheck' {healthCheckVersion} -> healthCheckVersion) (\s@UpdateHealthCheck' {} a -> s {healthCheckVersion = a} :: UpdateHealthCheck)

-- | The number of consecutive health checks that an endpoint must pass or
-- fail for Amazon Route 53 to change the current status of the endpoint
-- from unhealthy to healthy or vice versa. For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy>
-- in the /Amazon Route 53 Developer Guide/.
--
-- If you don\'t specify a value for @FailureThreshold@, the default value
-- is three health checks.
updateHealthCheck_failureThreshold :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe Prelude.Natural)
updateHealthCheck_failureThreshold = Lens.lens (\UpdateHealthCheck' {failureThreshold} -> failureThreshold) (\s@UpdateHealthCheck' {} a -> s {failureThreshold = a} :: UpdateHealthCheck)

-- | A complex type that identifies the CloudWatch alarm that you want Amazon
-- Route 53 health checkers to use to determine whether the specified
-- health check is healthy.
updateHealthCheck_alarmIdentifier :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe AlarmIdentifier)
updateHealthCheck_alarmIdentifier = Lens.lens (\UpdateHealthCheck' {alarmIdentifier} -> alarmIdentifier) (\s@UpdateHealthCheck' {} a -> s {alarmIdentifier = a} :: UpdateHealthCheck)

-- | A complex type that contains one @Region@ element for each region that
-- you want Amazon Route 53 health checkers to check the specified endpoint
-- from.
updateHealthCheck_regions :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe (Prelude.NonEmpty HealthCheckRegion))
updateHealthCheck_regions = Lens.lens (\UpdateHealthCheck' {regions} -> regions) (\s@UpdateHealthCheck' {} a -> s {regions = a} :: UpdateHealthCheck) Prelude.. Lens.mapping Lens.coerced

-- | The path that you want Amazon Route 53 to request when performing health
-- checks. The path can be any value for which your endpoint will return an
-- HTTP status code of 2xx or 3xx when the endpoint is healthy, for example
-- the file \/docs\/route53-health-check.html. You can also include query
-- string parameters, for example, @\/welcome.html?language=jp&login=y@.
--
-- Specify this value only if you want to change it.
updateHealthCheck_resourcePath :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe Prelude.Text)
updateHealthCheck_resourcePath = Lens.lens (\UpdateHealthCheck' {resourcePath} -> resourcePath) (\s@UpdateHealthCheck' {} a -> s {resourcePath = a} :: UpdateHealthCheck)

-- | A complex type that contains one @ChildHealthCheck@ element for each
-- health check that you want to associate with a @CALCULATED@ health
-- check.
updateHealthCheck_childHealthChecks :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe [Prelude.Text])
updateHealthCheck_childHealthChecks = Lens.lens (\UpdateHealthCheck' {childHealthChecks} -> childHealthChecks) (\s@UpdateHealthCheck' {} a -> s {childHealthChecks = a} :: UpdateHealthCheck) Prelude.. Lens.mapping Lens.coerced

-- | If the value of @Type@ is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@, the
-- string that you want Amazon Route 53 to search for in the response body
-- from the specified resource. If the string appears in the response body,
-- Route 53 considers the resource healthy. (You can\'t change the value of
-- @Type@ when you update a health check.)
updateHealthCheck_searchString :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe Prelude.Text)
updateHealthCheck_searchString = Lens.lens (\UpdateHealthCheck' {searchString} -> searchString) (\s@UpdateHealthCheck' {} a -> s {searchString = a} :: UpdateHealthCheck)

-- | A complex type that contains one @ResettableElementName@ element for
-- each element that you want to reset to the default value. Valid values
-- for @ResettableElementName@ include the following:
--
-- -   @ChildHealthChecks@: Amazon Route 53 resets
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-ChildHealthChecks ChildHealthChecks>
--     to null.
--
-- -   @FullyQualifiedDomainName@: Route 53 resets
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-FullyQualifiedDomainName FullyQualifiedDomainName>.
--     to null.
--
-- -   @Regions@: Route 53 resets the
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-Regions Regions>
--     list to the default set of regions.
--
-- -   @ResourcePath@: Route 53 resets
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-ResourcePath ResourcePath>
--     to null.
updateHealthCheck_resetElements :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe [ResettableElementName])
updateHealthCheck_resetElements = Lens.lens (\UpdateHealthCheck' {resetElements} -> resetElements) (\s@UpdateHealthCheck' {} a -> s {resetElements = a} :: UpdateHealthCheck) Prelude.. Lens.mapping Lens.coerced

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
updateHealthCheck_disabled :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe Prelude.Bool)
updateHealthCheck_disabled = Lens.lens (\UpdateHealthCheck' {disabled} -> disabled) (\s@UpdateHealthCheck' {} a -> s {disabled = a} :: UpdateHealthCheck)

-- | Specify whether you want Amazon Route 53 to invert the status of a
-- health check, for example, to consider a health check unhealthy when it
-- otherwise would be considered healthy.
updateHealthCheck_inverted :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe Prelude.Bool)
updateHealthCheck_inverted = Lens.lens (\UpdateHealthCheck' {inverted} -> inverted) (\s@UpdateHealthCheck' {} a -> s {inverted = a} :: UpdateHealthCheck)

-- | Amazon Route 53 behavior depends on whether you specify a value for
-- @IPAddress@.
--
-- If a health check already has a value for @IPAddress@, you can change
-- the value. However, you can\'t update an existing health check to add or
-- remove the value of @IPAddress@.
--
-- __If you specify a value for__ @IPAddress@:
--
-- Route 53 sends health check requests to the specified IPv4 or IPv6
-- address and passes the value of @FullyQualifiedDomainName@ in the @Host@
-- header for all health checks except TCP health checks. This is typically
-- the fully qualified DNS name of the endpoint on which you want Route 53
-- to perform health checks.
--
-- When Route 53 checks the health of an endpoint, here is how it
-- constructs the @Host@ header:
--
-- -   If you specify a value of @80@ for @Port@ and @HTTP@ or
--     @HTTP_STR_MATCH@ for @Type@, Route 53 passes the value of
--     @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
--
-- -   If you specify a value of @443@ for @Port@ and @HTTPS@ or
--     @HTTPS_STR_MATCH@ for @Type@, Route 53 passes the value of
--     @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
--
-- -   If you specify another value for @Port@ and any value except @TCP@
--     for @Type@, Route 53 passes /@FullyQualifiedDomainName@:@Port@/ to
--     the endpoint in the @Host@ header.
--
-- If you don\'t specify a value for @FullyQualifiedDomainName@, Route 53
-- substitutes the value of @IPAddress@ in the @Host@ header in each of the
-- above cases.
--
-- __If you don\'t specify a value for__ @IPAddress@:
--
-- If you don\'t specify a value for @IPAddress@, Route 53 sends a DNS
-- request to the domain that you specify in @FullyQualifiedDomainName@ at
-- the interval you specify in @RequestInterval@. Using an IPv4 address
-- that is returned by DNS, Route 53 then checks the health of the
-- endpoint.
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
-- server (such as @us-east-2-www.example.com@), not the name of the
-- resource record sets (www.example.com).
--
-- In this configuration, if the value of @FullyQualifiedDomainName@
-- matches the name of the resource record sets and you then associate the
-- health check with those resource record sets, health check results will
-- be unpredictable.
--
-- In addition, if the value of @Type@ is @HTTP@, @HTTPS@,
-- @HTTP_STR_MATCH@, or @HTTPS_STR_MATCH@, Route 53 passes the value of
-- @FullyQualifiedDomainName@ in the @Host@ header, as it does when you
-- specify a value for @IPAddress@. If the value of @Type@ is @TCP@, Route
-- 53 doesn\'t pass a @Host@ header.
updateHealthCheck_fullyQualifiedDomainName :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe Prelude.Text)
updateHealthCheck_fullyQualifiedDomainName = Lens.lens (\UpdateHealthCheck' {fullyQualifiedDomainName} -> fullyQualifiedDomainName) (\s@UpdateHealthCheck' {} a -> s {fullyQualifiedDomainName = a} :: UpdateHealthCheck)

-- | When CloudWatch has insufficient data about the metric to determine the
-- alarm state, the status that you want Amazon Route 53 to assign to the
-- health check:
--
-- -   @Healthy@: Route 53 considers the health check to be healthy.
--
-- -   @Unhealthy@: Route 53 considers the health check to be unhealthy.
--
-- -   @LastKnownStatus@: By default, Route 53 uses the status of the
--     health check from the last time CloudWatch had sufficient data to
--     determine the alarm state. For new health checks that have no last
--     known status, the status for the health check is healthy.
updateHealthCheck_insufficientDataHealthStatus :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe InsufficientDataHealthStatus)
updateHealthCheck_insufficientDataHealthStatus = Lens.lens (\UpdateHealthCheck' {insufficientDataHealthStatus} -> insufficientDataHealthStatus) (\s@UpdateHealthCheck' {} a -> s {insufficientDataHealthStatus = a} :: UpdateHealthCheck)

-- | Specify whether you want Amazon Route 53 to send the value of
-- @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message
-- during @TLS@ negotiation. This allows the endpoint to respond to @HTTPS@
-- health check requests with the applicable SSL\/TLS certificate.
--
-- Some endpoints require that HTTPS requests include the host name in the
-- @client_hello@ message. If you don\'t enable SNI, the status of the
-- health check will be SSL alert @handshake_failure@. A health check can
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
updateHealthCheck_enableSNI :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe Prelude.Bool)
updateHealthCheck_enableSNI = Lens.lens (\UpdateHealthCheck' {enableSNI} -> enableSNI) (\s@UpdateHealthCheck' {} a -> s {enableSNI = a} :: UpdateHealthCheck)

-- | The IPv4 or IPv6 IP address for the endpoint that you want Amazon Route
-- 53 to perform health checks on. If you don\'t specify a value for
-- @IPAddress@, Route 53 sends a DNS request to resolve the domain name
-- that you specify in @FullyQualifiedDomainName@ at the interval that you
-- specify in @RequestInterval@. Using an IP address that is returned by
-- DNS, Route 53 then checks the health of the endpoint.
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
-- your instance never changes. For more information, see the applicable
-- documentation:
--
-- -   Linux:
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses (EIP)>
--     in the /Amazon EC2 User Guide for Linux Instances/
--
-- -   Windows:
--     <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-ip-addresses-eip.html Elastic IP Addresses (EIP)>
--     in the /Amazon EC2 User Guide for Windows Instances/
--
-- If a health check already has a value for @IPAddress@, you can change
-- the value. However, you can\'t update an existing health check to add or
-- remove the value of @IPAddress@.
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
updateHealthCheck_iPAddress :: Lens.Lens' UpdateHealthCheck (Prelude.Maybe Prelude.Text)
updateHealthCheck_iPAddress = Lens.lens (\UpdateHealthCheck' {iPAddress} -> iPAddress) (\s@UpdateHealthCheck' {} a -> s {iPAddress = a} :: UpdateHealthCheck)

-- | The ID for the health check for which you want detailed information.
-- When you created the health check, @CreateHealthCheck@ returned the ID
-- in the response, in the @HealthCheckId@ element.
updateHealthCheck_healthCheckId :: Lens.Lens' UpdateHealthCheck Prelude.Text
updateHealthCheck_healthCheckId = Lens.lens (\UpdateHealthCheck' {healthCheckId} -> healthCheckId) (\s@UpdateHealthCheck' {} a -> s {healthCheckId = a} :: UpdateHealthCheck)

instance Core.AWSRequest UpdateHealthCheck where
  type
    AWSResponse UpdateHealthCheck =
      UpdateHealthCheckResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateHealthCheckResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "HealthCheck")
      )

instance Prelude.Hashable UpdateHealthCheck where
  hashWithSalt _salt UpdateHealthCheck' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` healthThreshold
      `Prelude.hashWithSalt` healthCheckVersion
      `Prelude.hashWithSalt` failureThreshold
      `Prelude.hashWithSalt` alarmIdentifier
      `Prelude.hashWithSalt` regions
      `Prelude.hashWithSalt` resourcePath
      `Prelude.hashWithSalt` childHealthChecks
      `Prelude.hashWithSalt` searchString
      `Prelude.hashWithSalt` resetElements
      `Prelude.hashWithSalt` disabled
      `Prelude.hashWithSalt` inverted
      `Prelude.hashWithSalt` fullyQualifiedDomainName
      `Prelude.hashWithSalt` insufficientDataHealthStatus
      `Prelude.hashWithSalt` enableSNI
      `Prelude.hashWithSalt` iPAddress
      `Prelude.hashWithSalt` healthCheckId

instance Prelude.NFData UpdateHealthCheck where
  rnf UpdateHealthCheck' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf healthThreshold
      `Prelude.seq` Prelude.rnf healthCheckVersion
      `Prelude.seq` Prelude.rnf failureThreshold
      `Prelude.seq` Prelude.rnf alarmIdentifier
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf resourcePath
      `Prelude.seq` Prelude.rnf childHealthChecks
      `Prelude.seq` Prelude.rnf searchString
      `Prelude.seq` Prelude.rnf resetElements
      `Prelude.seq` Prelude.rnf disabled
      `Prelude.seq` Prelude.rnf inverted
      `Prelude.seq` Prelude.rnf fullyQualifiedDomainName
      `Prelude.seq` Prelude.rnf insufficientDataHealthStatus
      `Prelude.seq` Prelude.rnf enableSNI
      `Prelude.seq` Prelude.rnf iPAddress
      `Prelude.seq` Prelude.rnf healthCheckId

instance Data.ToElement UpdateHealthCheck where
  toElement =
    Data.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateHealthCheckRequest"

instance Data.ToHeaders UpdateHealthCheck where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateHealthCheck where
  toPath UpdateHealthCheck' {..} =
    Prelude.mconcat
      ["/2013-04-01/healthcheck/", Data.toBS healthCheckId]

instance Data.ToQuery UpdateHealthCheck where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML UpdateHealthCheck where
  toXML UpdateHealthCheck' {..} =
    Prelude.mconcat
      [ "Port" Data.@= port,
        "HealthThreshold" Data.@= healthThreshold,
        "HealthCheckVersion" Data.@= healthCheckVersion,
        "FailureThreshold" Data.@= failureThreshold,
        "AlarmIdentifier" Data.@= alarmIdentifier,
        "Regions"
          Data.@= Data.toXML
            (Data.toXMLList "Region" Prelude.<$> regions),
        "ResourcePath" Data.@= resourcePath,
        "ChildHealthChecks"
          Data.@= Data.toXML
            ( Data.toXMLList "ChildHealthCheck"
                Prelude.<$> childHealthChecks
            ),
        "SearchString" Data.@= searchString,
        "ResetElements"
          Data.@= Data.toXML
            ( Data.toXMLList "ResettableElementName"
                Prelude.<$> resetElements
            ),
        "Disabled" Data.@= disabled,
        "Inverted" Data.@= inverted,
        "FullyQualifiedDomainName"
          Data.@= fullyQualifiedDomainName,
        "InsufficientDataHealthStatus"
          Data.@= insufficientDataHealthStatus,
        "EnableSNI" Data.@= enableSNI,
        "IPAddress" Data.@= iPAddress
      ]

-- | A complex type that contains the response to the @UpdateHealthCheck@
-- request.
--
-- /See:/ 'newUpdateHealthCheckResponse' smart constructor.
data UpdateHealthCheckResponse = UpdateHealthCheckResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains the response to an @UpdateHealthCheck@
    -- request.
    healthCheck :: HealthCheck
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateHealthCheckResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateHealthCheckResponse_httpStatus' - The response's http status code.
--
-- 'healthCheck', 'updateHealthCheckResponse_healthCheck' - A complex type that contains the response to an @UpdateHealthCheck@
-- request.
newUpdateHealthCheckResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'healthCheck'
  HealthCheck ->
  UpdateHealthCheckResponse
newUpdateHealthCheckResponse
  pHttpStatus_
  pHealthCheck_ =
    UpdateHealthCheckResponse'
      { httpStatus =
          pHttpStatus_,
        healthCheck = pHealthCheck_
      }

-- | The response's http status code.
updateHealthCheckResponse_httpStatus :: Lens.Lens' UpdateHealthCheckResponse Prelude.Int
updateHealthCheckResponse_httpStatus = Lens.lens (\UpdateHealthCheckResponse' {httpStatus} -> httpStatus) (\s@UpdateHealthCheckResponse' {} a -> s {httpStatus = a} :: UpdateHealthCheckResponse)

-- | A complex type that contains the response to an @UpdateHealthCheck@
-- request.
updateHealthCheckResponse_healthCheck :: Lens.Lens' UpdateHealthCheckResponse HealthCheck
updateHealthCheckResponse_healthCheck = Lens.lens (\UpdateHealthCheckResponse' {healthCheck} -> healthCheck) (\s@UpdateHealthCheckResponse' {} a -> s {healthCheck = a} :: UpdateHealthCheckResponse)

instance Prelude.NFData UpdateHealthCheckResponse where
  rnf UpdateHealthCheckResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf healthCheck
