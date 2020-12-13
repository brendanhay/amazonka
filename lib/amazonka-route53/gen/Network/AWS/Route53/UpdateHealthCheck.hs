{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.UpdateHealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing health check. Note that some values can't be updated.
--
-- For more information about updating health checks, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/health-checks-creating-deleting.html Creating, Updating, and Deleting Health Checks> in the /Amazon Route 53 Developer Guide/ .
module Network.AWS.Route53.UpdateHealthCheck
  ( -- * Creating a request
    UpdateHealthCheck (..),
    mkUpdateHealthCheck,

    -- ** Request lenses
    uhcFailureThreshold,
    uhcIPAddress,
    uhcEnableSNI,
    uhcDisabled,
    uhcResetElements,
    uhcSearchString,
    uhcHealthThreshold,
    uhcRegions,
    uhcResourcePath,
    uhcHealthCheckId,
    uhcInsufficientDataHealthStatus,
    uhcHealthCheckVersion,
    uhcAlarmIdentifier,
    uhcInverted,
    uhcFullyQualifiedDomainName,
    uhcChildHealthChecks,
    uhcPort,

    -- * Destructuring the response
    UpdateHealthCheckResponse (..),
    mkUpdateHealthCheckResponse,

    -- ** Response lenses
    uhcrsHealthCheck,
    uhcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about a request to update a health check.
--
-- /See:/ 'mkUpdateHealthCheck' smart constructor.
data UpdateHealthCheck = UpdateHealthCheck'
  { -- | The number of consecutive health checks that an endpoint must pass or fail for Amazon Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
    --
    -- If you don't specify a value for @FailureThreshold@ , the default value is three health checks.
    failureThreshold :: Lude.Maybe Lude.Natural,
    -- | The IPv4 or IPv6 IP address for the endpoint that you want Amazon Route 53 to perform health checks on. If you don't specify a value for @IPAddress@ , Route 53 sends a DNS request to resolve the domain name that you specify in @FullyQualifiedDomainName@ at the interval that you specify in @RequestInterval@ . Using an IP address that is returned by DNS, Route 53 then checks the health of the endpoint.
    --
    -- Use one of the following formats for the value of @IPAddress@ :
    --
    --     * __IPv4 address__ : four values between 0 and 255, separated by periods (.), for example, @192.0.2.44@ .
    --
    --
    --     * __IPv6 address__ : eight groups of four hexadecimal values, separated by colons (:), for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ . You can also shorten IPv6 addresses as described in RFC 5952, for example, @2001:db8:85a3::abcd:1:2345@ .
    --
    --
    -- If the endpoint is an EC2 instance, we recommend that you create an Elastic IP address, associate it with your EC2 instance, and specify the Elastic IP address for @IPAddress@ . This ensures that the IP address of your instance never changes. For more information, see the applicable documentation:
    --
    --     * Linux: <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses (EIP)> in the /Amazon EC2 User Guide for Linux Instances/
    --
    --
    --     * Windows: <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-ip-addresses-eip.html Elastic IP Addresses (EIP)> in the /Amazon EC2 User Guide for Windows Instances/
    --
    --
    -- For more information, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-FullyQualifiedDomainName FullyQualifiedDomainName> .
    -- Constraints: Route 53 can't check the health of endpoints for which the IP address is in local, private, non-routable, or multicast ranges. For more information about IP addresses for which you can't create health checks, see the following documents:
    --
    --     * <https://tools.ietf.org/html/rfc5735 RFC 5735, Special Use IPv4 Addresses>
    --
    --
    --     * <https://tools.ietf.org/html/rfc6598 RFC 6598, IANA-Reserved IPv4 Prefix for Shared Address Space>
    --
    --
    --     * <https://tools.ietf.org/html/rfc5156 RFC 5156, Special-Use IPv6 Addresses>
    ipAddress :: Lude.Maybe Lude.Text,
    -- | Specify whether you want Amazon Route 53 to send the value of @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message during @TLS@ negotiation. This allows the endpoint to respond to @HTTPS@ health check requests with the applicable SSL/TLS certificate.
    --
    -- Some endpoints require that HTTPS requests include the host name in the @client_hello@ message. If you don't enable SNI, the status of the health check will be SSL alert @handshake_failure@ . A health check can also have that status for other reasons. If SNI is enabled and you're still getting the error, check the SSL/TLS configuration on your endpoint and confirm that your certificate is valid.
    -- The SSL/TLS certificate on your endpoint includes a domain name in the @Common Name@ field and possibly several more in the @Subject Alternative Names@ field. One of the domain names in the certificate should match the value that you specify for @FullyQualifiedDomainName@ . If the endpoint responds to the @client_hello@ message with a certificate that does not include the domain name that you specified in @FullyQualifiedDomainName@ , a health checker will retry the handshake. In the second attempt, the health checker will omit @FullyQualifiedDomainName@ from the @client_hello@ message.
    enableSNI :: Lude.Maybe Lude.Bool,
    -- | Stops Route 53 from performing health checks. When you disable a health check, here's what happens:
    --
    --
    --     * __Health checks that check the health of endpoints:__ Route 53 stops submitting requests to your application, server, or other resource.
    --
    --
    --     * __Calculated health checks:__ Route 53 stops aggregating the status of the referenced health checks.
    --
    --
    --     * __Health checks that monitor CloudWatch alarms:__ Route 53 stops monitoring the corresponding CloudWatch metrics.
    --
    --
    -- After you disable a health check, Route 53 considers the status of the health check to always be healthy. If you configured DNS failover, Route 53 continues to route traffic to the corresponding resources. If you want to stop routing traffic to a resource, change the value of <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-Inverted Inverted> .
    -- Charges for a health check still apply when the health check is disabled. For more information, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
    disabled :: Lude.Maybe Lude.Bool,
    -- | A complex type that contains one @ResettableElementName@ element for each element that you want to reset to the default value. Valid values for @ResettableElementName@ include the following:
    --
    --
    --     * @ChildHealthChecks@ : Amazon Route 53 resets <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-ChildHealthChecks ChildHealthChecks> to null.
    --
    --
    --     * @FullyQualifiedDomainName@ : Route 53 resets <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-FullyQualifiedDomainName FullyQualifiedDomainName> . to null.
    --
    --
    --     * @Regions@ : Route 53 resets the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-Regions Regions> list to the default set of regions.
    --
    --
    --     * @ResourcePath@ : Route 53 resets <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-ResourcePath ResourcePath> to null.
    resetElements :: Lude.Maybe [ResettableElementName],
    -- | If the value of @Type@ is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@ , the string that you want Amazon Route 53 to search for in the response body from the specified resource. If the string appears in the response body, Route 53 considers the resource healthy. (You can't change the value of @Type@ when you update a health check.)
    searchString :: Lude.Maybe Lude.Text,
    -- | The number of child health checks that are associated with a @CALCULATED@ health that Amazon Route 53 must consider healthy for the @CALCULATED@ health check to be considered healthy. To specify the child health checks that you want to associate with a @CALCULATED@ health check, use the @ChildHealthChecks@ and @ChildHealthCheck@ elements.
    --
    -- Note the following:
    --
    --     * If you specify a number greater than the number of child health checks, Route 53 always considers this health check to be unhealthy.
    --
    --
    --     * If you specify @0@ , Route 53 always considers this health check to be healthy.
    healthThreshold :: Lude.Maybe Lude.Natural,
    -- | A complex type that contains one @Region@ element for each region that you want Amazon Route 53 health checkers to check the specified endpoint from.
    regions :: Lude.Maybe (Lude.NonEmpty HealthCheckRegion),
    -- | The path that you want Amazon Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, for example the file /docs/route53-health-check.html. You can also include query string parameters, for example, @/welcome.html?language=jp&login=y@ .
    --
    -- Specify this value only if you want to change it.
    resourcePath :: Lude.Maybe Lude.Text,
    -- | The ID for the health check for which you want detailed information. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
    healthCheckId :: Lude.Text,
    -- | When CloudWatch has insufficient data about the metric to determine the alarm state, the status that you want Amazon Route 53 to assign to the health check:
    --
    --
    --     * @Healthy@ : Route 53 considers the health check to be healthy.
    --
    --
    --     * @Unhealthy@ : Route 53 considers the health check to be unhealthy.
    --
    --
    --     * @LastKnownStatus@ : Route 53 uses the status of the health check from the last time CloudWatch had sufficient data to determine the alarm state. For new health checks that have no last known status, the default status for the health check is healthy.
    insufficientDataHealthStatus :: Lude.Maybe InsufficientDataHealthStatus,
    -- | A sequential counter that Amazon Route 53 sets to @1@ when you create a health check and increments by 1 each time you update settings for the health check.
    --
    -- We recommend that you use @GetHealthCheck@ or @ListHealthChecks@ to get the current value of @HealthCheckVersion@ for the health check that you want to update, and that you include that value in your @UpdateHealthCheck@ request. This prevents Route 53 from overwriting an intervening update:
    --
    --     * If the value in the @UpdateHealthCheck@ request matches the value of @HealthCheckVersion@ in the health check, Route 53 updates the health check with the new settings.
    --
    --
    --     * If the value of @HealthCheckVersion@ in the health check is greater, the health check was changed after you got the version number. Route 53 does not update the health check, and it returns a @HealthCheckVersionMismatch@ error.
    healthCheckVersion :: Lude.Maybe Lude.Natural,
    -- | A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether the specified health check is healthy.
    alarmIdentifier :: Lude.Maybe AlarmIdentifier,
    -- | Specify whether you want Amazon Route 53 to invert the status of a health check, for example, to consider a health check unhealthy when it otherwise would be considered healthy.
    inverted :: Lude.Maybe Lude.Bool,
    -- | Amazon Route 53 behavior depends on whether you specify a value for @IPAddress@ .
    --
    -- __If you specify a value for__ @IPAddress@ :
    -- Route 53 sends health check requests to the specified IPv4 or IPv6 address and passes the value of @FullyQualifiedDomainName@ in the @Host@ header for all health checks except TCP health checks. This is typically the fully qualified DNS name of the endpoint on which you want Route 53 to perform health checks.
    -- When Route 53 checks the health of an endpoint, here is how it constructs the @Host@ header:
    --
    --     * If you specify a value of @80@ for @Port@ and @HTTP@ or @HTTP_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
    --
    --
    --     * If you specify a value of @443@ for @Port@ and @HTTPS@ or @HTTPS_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
    --
    --
    --     * If you specify another value for @Port@ and any value except @TCP@ for @Type@ , Route 53 passes /@FullyQualifiedDomainName@ :@Port@ / to the endpoint in the @Host@ header.
    --
    --
    -- If you don't specify a value for @FullyQualifiedDomainName@ , Route 53 substitutes the value of @IPAddress@ in the @Host@ header in each of the above cases.
    -- __If you don't specify a value for__ @IPAddress@ :
    -- If you don't specify a value for @IPAddress@ , Route 53 sends a DNS request to the domain that you specify in @FullyQualifiedDomainName@ at the interval you specify in @RequestInterval@ . Using an IPv4 address that is returned by DNS, Route 53 then checks the health of the endpoint.
    -- If you want to check the health of weighted, latency, or failover resource record sets and you choose to specify the endpoint only by @FullyQualifiedDomainName@ , we recommend that you create a separate health check for each endpoint. For example, create a health check for each HTTP server that is serving content for www.example.com. For the value of @FullyQualifiedDomainName@ , specify the domain name of the server (such as @us-east-2-www.example.com@ ), not the name of the resource record sets (www.example.com).
    -- /Important:/ In this configuration, if the value of @FullyQualifiedDomainName@ matches the name of the resource record sets and you then associate the health check with those resource record sets, health check results will be unpredictable.
    -- In addition, if the value of @Type@ is @HTTP@ , @HTTPS@ , @HTTP_STR_MATCH@ , or @HTTPS_STR_MATCH@ , Route 53 passes the value of @FullyQualifiedDomainName@ in the @Host@ header, as it does when you specify a value for @IPAddress@ . If the value of @Type@ is @TCP@ , Route 53 doesn't pass a @Host@ header.
    fullyQualifiedDomainName :: Lude.Maybe Lude.Text,
    -- | A complex type that contains one @ChildHealthCheck@ element for each health check that you want to associate with a @CALCULATED@ health check.
    childHealthChecks :: Lude.Maybe [Lude.Text],
    -- | The port on the endpoint that you want Amazon Route 53 to perform health checks on.
    port :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateHealthCheck' with the minimum fields required to make a request.
--
-- * 'failureThreshold' - The number of consecutive health checks that an endpoint must pass or fail for Amazon Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
--
-- If you don't specify a value for @FailureThreshold@ , the default value is three health checks.
-- * 'ipAddress' - The IPv4 or IPv6 IP address for the endpoint that you want Amazon Route 53 to perform health checks on. If you don't specify a value for @IPAddress@ , Route 53 sends a DNS request to resolve the domain name that you specify in @FullyQualifiedDomainName@ at the interval that you specify in @RequestInterval@ . Using an IP address that is returned by DNS, Route 53 then checks the health of the endpoint.
--
-- Use one of the following formats for the value of @IPAddress@ :
--
--     * __IPv4 address__ : four values between 0 and 255, separated by periods (.), for example, @192.0.2.44@ .
--
--
--     * __IPv6 address__ : eight groups of four hexadecimal values, separated by colons (:), for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ . You can also shorten IPv6 addresses as described in RFC 5952, for example, @2001:db8:85a3::abcd:1:2345@ .
--
--
-- If the endpoint is an EC2 instance, we recommend that you create an Elastic IP address, associate it with your EC2 instance, and specify the Elastic IP address for @IPAddress@ . This ensures that the IP address of your instance never changes. For more information, see the applicable documentation:
--
--     * Linux: <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses (EIP)> in the /Amazon EC2 User Guide for Linux Instances/
--
--
--     * Windows: <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-ip-addresses-eip.html Elastic IP Addresses (EIP)> in the /Amazon EC2 User Guide for Windows Instances/
--
--
-- For more information, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-FullyQualifiedDomainName FullyQualifiedDomainName> .
-- Constraints: Route 53 can't check the health of endpoints for which the IP address is in local, private, non-routable, or multicast ranges. For more information about IP addresses for which you can't create health checks, see the following documents:
--
--     * <https://tools.ietf.org/html/rfc5735 RFC 5735, Special Use IPv4 Addresses>
--
--
--     * <https://tools.ietf.org/html/rfc6598 RFC 6598, IANA-Reserved IPv4 Prefix for Shared Address Space>
--
--
--     * <https://tools.ietf.org/html/rfc5156 RFC 5156, Special-Use IPv6 Addresses>
--
--
-- * 'enableSNI' - Specify whether you want Amazon Route 53 to send the value of @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message during @TLS@ negotiation. This allows the endpoint to respond to @HTTPS@ health check requests with the applicable SSL/TLS certificate.
--
-- Some endpoints require that HTTPS requests include the host name in the @client_hello@ message. If you don't enable SNI, the status of the health check will be SSL alert @handshake_failure@ . A health check can also have that status for other reasons. If SNI is enabled and you're still getting the error, check the SSL/TLS configuration on your endpoint and confirm that your certificate is valid.
-- The SSL/TLS certificate on your endpoint includes a domain name in the @Common Name@ field and possibly several more in the @Subject Alternative Names@ field. One of the domain names in the certificate should match the value that you specify for @FullyQualifiedDomainName@ . If the endpoint responds to the @client_hello@ message with a certificate that does not include the domain name that you specified in @FullyQualifiedDomainName@ , a health checker will retry the handshake. In the second attempt, the health checker will omit @FullyQualifiedDomainName@ from the @client_hello@ message.
-- * 'disabled' - Stops Route 53 from performing health checks. When you disable a health check, here's what happens:
--
--
--     * __Health checks that check the health of endpoints:__ Route 53 stops submitting requests to your application, server, or other resource.
--
--
--     * __Calculated health checks:__ Route 53 stops aggregating the status of the referenced health checks.
--
--
--     * __Health checks that monitor CloudWatch alarms:__ Route 53 stops monitoring the corresponding CloudWatch metrics.
--
--
-- After you disable a health check, Route 53 considers the status of the health check to always be healthy. If you configured DNS failover, Route 53 continues to route traffic to the corresponding resources. If you want to stop routing traffic to a resource, change the value of <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-Inverted Inverted> .
-- Charges for a health check still apply when the health check is disabled. For more information, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
-- * 'resetElements' - A complex type that contains one @ResettableElementName@ element for each element that you want to reset to the default value. Valid values for @ResettableElementName@ include the following:
--
--
--     * @ChildHealthChecks@ : Amazon Route 53 resets <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-ChildHealthChecks ChildHealthChecks> to null.
--
--
--     * @FullyQualifiedDomainName@ : Route 53 resets <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-FullyQualifiedDomainName FullyQualifiedDomainName> . to null.
--
--
--     * @Regions@ : Route 53 resets the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-Regions Regions> list to the default set of regions.
--
--
--     * @ResourcePath@ : Route 53 resets <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-ResourcePath ResourcePath> to null.
--
--
-- * 'searchString' - If the value of @Type@ is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@ , the string that you want Amazon Route 53 to search for in the response body from the specified resource. If the string appears in the response body, Route 53 considers the resource healthy. (You can't change the value of @Type@ when you update a health check.)
-- * 'healthThreshold' - The number of child health checks that are associated with a @CALCULATED@ health that Amazon Route 53 must consider healthy for the @CALCULATED@ health check to be considered healthy. To specify the child health checks that you want to associate with a @CALCULATED@ health check, use the @ChildHealthChecks@ and @ChildHealthCheck@ elements.
--
-- Note the following:
--
--     * If you specify a number greater than the number of child health checks, Route 53 always considers this health check to be unhealthy.
--
--
--     * If you specify @0@ , Route 53 always considers this health check to be healthy.
--
--
-- * 'regions' - A complex type that contains one @Region@ element for each region that you want Amazon Route 53 health checkers to check the specified endpoint from.
-- * 'resourcePath' - The path that you want Amazon Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, for example the file /docs/route53-health-check.html. You can also include query string parameters, for example, @/welcome.html?language=jp&login=y@ .
--
-- Specify this value only if you want to change it.
-- * 'healthCheckId' - The ID for the health check for which you want detailed information. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
-- * 'insufficientDataHealthStatus' - When CloudWatch has insufficient data about the metric to determine the alarm state, the status that you want Amazon Route 53 to assign to the health check:
--
--
--     * @Healthy@ : Route 53 considers the health check to be healthy.
--
--
--     * @Unhealthy@ : Route 53 considers the health check to be unhealthy.
--
--
--     * @LastKnownStatus@ : Route 53 uses the status of the health check from the last time CloudWatch had sufficient data to determine the alarm state. For new health checks that have no last known status, the default status for the health check is healthy.
--
--
-- * 'healthCheckVersion' - A sequential counter that Amazon Route 53 sets to @1@ when you create a health check and increments by 1 each time you update settings for the health check.
--
-- We recommend that you use @GetHealthCheck@ or @ListHealthChecks@ to get the current value of @HealthCheckVersion@ for the health check that you want to update, and that you include that value in your @UpdateHealthCheck@ request. This prevents Route 53 from overwriting an intervening update:
--
--     * If the value in the @UpdateHealthCheck@ request matches the value of @HealthCheckVersion@ in the health check, Route 53 updates the health check with the new settings.
--
--
--     * If the value of @HealthCheckVersion@ in the health check is greater, the health check was changed after you got the version number. Route 53 does not update the health check, and it returns a @HealthCheckVersionMismatch@ error.
--
--
-- * 'alarmIdentifier' - A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether the specified health check is healthy.
-- * 'inverted' - Specify whether you want Amazon Route 53 to invert the status of a health check, for example, to consider a health check unhealthy when it otherwise would be considered healthy.
-- * 'fullyQualifiedDomainName' - Amazon Route 53 behavior depends on whether you specify a value for @IPAddress@ .
--
-- __If you specify a value for__ @IPAddress@ :
-- Route 53 sends health check requests to the specified IPv4 or IPv6 address and passes the value of @FullyQualifiedDomainName@ in the @Host@ header for all health checks except TCP health checks. This is typically the fully qualified DNS name of the endpoint on which you want Route 53 to perform health checks.
-- When Route 53 checks the health of an endpoint, here is how it constructs the @Host@ header:
--
--     * If you specify a value of @80@ for @Port@ and @HTTP@ or @HTTP_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
--
--
--     * If you specify a value of @443@ for @Port@ and @HTTPS@ or @HTTPS_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
--
--
--     * If you specify another value for @Port@ and any value except @TCP@ for @Type@ , Route 53 passes /@FullyQualifiedDomainName@ :@Port@ / to the endpoint in the @Host@ header.
--
--
-- If you don't specify a value for @FullyQualifiedDomainName@ , Route 53 substitutes the value of @IPAddress@ in the @Host@ header in each of the above cases.
-- __If you don't specify a value for__ @IPAddress@ :
-- If you don't specify a value for @IPAddress@ , Route 53 sends a DNS request to the domain that you specify in @FullyQualifiedDomainName@ at the interval you specify in @RequestInterval@ . Using an IPv4 address that is returned by DNS, Route 53 then checks the health of the endpoint.
-- If you want to check the health of weighted, latency, or failover resource record sets and you choose to specify the endpoint only by @FullyQualifiedDomainName@ , we recommend that you create a separate health check for each endpoint. For example, create a health check for each HTTP server that is serving content for www.example.com. For the value of @FullyQualifiedDomainName@ , specify the domain name of the server (such as @us-east-2-www.example.com@ ), not the name of the resource record sets (www.example.com).
-- /Important:/ In this configuration, if the value of @FullyQualifiedDomainName@ matches the name of the resource record sets and you then associate the health check with those resource record sets, health check results will be unpredictable.
-- In addition, if the value of @Type@ is @HTTP@ , @HTTPS@ , @HTTP_STR_MATCH@ , or @HTTPS_STR_MATCH@ , Route 53 passes the value of @FullyQualifiedDomainName@ in the @Host@ header, as it does when you specify a value for @IPAddress@ . If the value of @Type@ is @TCP@ , Route 53 doesn't pass a @Host@ header.
-- * 'childHealthChecks' - A complex type that contains one @ChildHealthCheck@ element for each health check that you want to associate with a @CALCULATED@ health check.
-- * 'port' - The port on the endpoint that you want Amazon Route 53 to perform health checks on.
mkUpdateHealthCheck ::
  -- | 'healthCheckId'
  Lude.Text ->
  UpdateHealthCheck
mkUpdateHealthCheck pHealthCheckId_ =
  UpdateHealthCheck'
    { failureThreshold = Lude.Nothing,
      ipAddress = Lude.Nothing,
      enableSNI = Lude.Nothing,
      disabled = Lude.Nothing,
      resetElements = Lude.Nothing,
      searchString = Lude.Nothing,
      healthThreshold = Lude.Nothing,
      regions = Lude.Nothing,
      resourcePath = Lude.Nothing,
      healthCheckId = pHealthCheckId_,
      insufficientDataHealthStatus = Lude.Nothing,
      healthCheckVersion = Lude.Nothing,
      alarmIdentifier = Lude.Nothing,
      inverted = Lude.Nothing,
      fullyQualifiedDomainName = Lude.Nothing,
      childHealthChecks = Lude.Nothing,
      port = Lude.Nothing
    }

-- | The number of consecutive health checks that an endpoint must pass or fail for Amazon Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
--
-- If you don't specify a value for @FailureThreshold@ , the default value is three health checks.
--
-- /Note:/ Consider using 'failureThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcFailureThreshold :: Lens.Lens' UpdateHealthCheck (Lude.Maybe Lude.Natural)
uhcFailureThreshold = Lens.lens (failureThreshold :: UpdateHealthCheck -> Lude.Maybe Lude.Natural) (\s a -> s {failureThreshold = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcFailureThreshold "Use generic-lens or generic-optics with 'failureThreshold' instead." #-}

-- | The IPv4 or IPv6 IP address for the endpoint that you want Amazon Route 53 to perform health checks on. If you don't specify a value for @IPAddress@ , Route 53 sends a DNS request to resolve the domain name that you specify in @FullyQualifiedDomainName@ at the interval that you specify in @RequestInterval@ . Using an IP address that is returned by DNS, Route 53 then checks the health of the endpoint.
--
-- Use one of the following formats for the value of @IPAddress@ :
--
--     * __IPv4 address__ : four values between 0 and 255, separated by periods (.), for example, @192.0.2.44@ .
--
--
--     * __IPv6 address__ : eight groups of four hexadecimal values, separated by colons (:), for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ . You can also shorten IPv6 addresses as described in RFC 5952, for example, @2001:db8:85a3::abcd:1:2345@ .
--
--
-- If the endpoint is an EC2 instance, we recommend that you create an Elastic IP address, associate it with your EC2 instance, and specify the Elastic IP address for @IPAddress@ . This ensures that the IP address of your instance never changes. For more information, see the applicable documentation:
--
--     * Linux: <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses (EIP)> in the /Amazon EC2 User Guide for Linux Instances/
--
--
--     * Windows: <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-ip-addresses-eip.html Elastic IP Addresses (EIP)> in the /Amazon EC2 User Guide for Windows Instances/
--
--
-- For more information, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-FullyQualifiedDomainName FullyQualifiedDomainName> .
-- Constraints: Route 53 can't check the health of endpoints for which the IP address is in local, private, non-routable, or multicast ranges. For more information about IP addresses for which you can't create health checks, see the following documents:
--
--     * <https://tools.ietf.org/html/rfc5735 RFC 5735, Special Use IPv4 Addresses>
--
--
--     * <https://tools.ietf.org/html/rfc6598 RFC 6598, IANA-Reserved IPv4 Prefix for Shared Address Space>
--
--
--     * <https://tools.ietf.org/html/rfc5156 RFC 5156, Special-Use IPv6 Addresses>
--
--
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcIPAddress :: Lens.Lens' UpdateHealthCheck (Lude.Maybe Lude.Text)
uhcIPAddress = Lens.lens (ipAddress :: UpdateHealthCheck -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | Specify whether you want Amazon Route 53 to send the value of @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message during @TLS@ negotiation. This allows the endpoint to respond to @HTTPS@ health check requests with the applicable SSL/TLS certificate.
--
-- Some endpoints require that HTTPS requests include the host name in the @client_hello@ message. If you don't enable SNI, the status of the health check will be SSL alert @handshake_failure@ . A health check can also have that status for other reasons. If SNI is enabled and you're still getting the error, check the SSL/TLS configuration on your endpoint and confirm that your certificate is valid.
-- The SSL/TLS certificate on your endpoint includes a domain name in the @Common Name@ field and possibly several more in the @Subject Alternative Names@ field. One of the domain names in the certificate should match the value that you specify for @FullyQualifiedDomainName@ . If the endpoint responds to the @client_hello@ message with a certificate that does not include the domain name that you specified in @FullyQualifiedDomainName@ , a health checker will retry the handshake. In the second attempt, the health checker will omit @FullyQualifiedDomainName@ from the @client_hello@ message.
--
-- /Note:/ Consider using 'enableSNI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcEnableSNI :: Lens.Lens' UpdateHealthCheck (Lude.Maybe Lude.Bool)
uhcEnableSNI = Lens.lens (enableSNI :: UpdateHealthCheck -> Lude.Maybe Lude.Bool) (\s a -> s {enableSNI = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcEnableSNI "Use generic-lens or generic-optics with 'enableSNI' instead." #-}

-- | Stops Route 53 from performing health checks. When you disable a health check, here's what happens:
--
--
--     * __Health checks that check the health of endpoints:__ Route 53 stops submitting requests to your application, server, or other resource.
--
--
--     * __Calculated health checks:__ Route 53 stops aggregating the status of the referenced health checks.
--
--
--     * __Health checks that monitor CloudWatch alarms:__ Route 53 stops monitoring the corresponding CloudWatch metrics.
--
--
-- After you disable a health check, Route 53 considers the status of the health check to always be healthy. If you configured DNS failover, Route 53 continues to route traffic to the corresponding resources. If you want to stop routing traffic to a resource, change the value of <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-Inverted Inverted> .
-- Charges for a health check still apply when the health check is disabled. For more information, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
--
-- /Note:/ Consider using 'disabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcDisabled :: Lens.Lens' UpdateHealthCheck (Lude.Maybe Lude.Bool)
uhcDisabled = Lens.lens (disabled :: UpdateHealthCheck -> Lude.Maybe Lude.Bool) (\s a -> s {disabled = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcDisabled "Use generic-lens or generic-optics with 'disabled' instead." #-}

-- | A complex type that contains one @ResettableElementName@ element for each element that you want to reset to the default value. Valid values for @ResettableElementName@ include the following:
--
--
--     * @ChildHealthChecks@ : Amazon Route 53 resets <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-ChildHealthChecks ChildHealthChecks> to null.
--
--
--     * @FullyQualifiedDomainName@ : Route 53 resets <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-FullyQualifiedDomainName FullyQualifiedDomainName> . to null.
--
--
--     * @Regions@ : Route 53 resets the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-Regions Regions> list to the default set of regions.
--
--
--     * @ResourcePath@ : Route 53 resets <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-ResourcePath ResourcePath> to null.
--
--
--
-- /Note:/ Consider using 'resetElements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcResetElements :: Lens.Lens' UpdateHealthCheck (Lude.Maybe [ResettableElementName])
uhcResetElements = Lens.lens (resetElements :: UpdateHealthCheck -> Lude.Maybe [ResettableElementName]) (\s a -> s {resetElements = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcResetElements "Use generic-lens or generic-optics with 'resetElements' instead." #-}

-- | If the value of @Type@ is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@ , the string that you want Amazon Route 53 to search for in the response body from the specified resource. If the string appears in the response body, Route 53 considers the resource healthy. (You can't change the value of @Type@ when you update a health check.)
--
-- /Note:/ Consider using 'searchString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcSearchString :: Lens.Lens' UpdateHealthCheck (Lude.Maybe Lude.Text)
uhcSearchString = Lens.lens (searchString :: UpdateHealthCheck -> Lude.Maybe Lude.Text) (\s a -> s {searchString = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcSearchString "Use generic-lens or generic-optics with 'searchString' instead." #-}

-- | The number of child health checks that are associated with a @CALCULATED@ health that Amazon Route 53 must consider healthy for the @CALCULATED@ health check to be considered healthy. To specify the child health checks that you want to associate with a @CALCULATED@ health check, use the @ChildHealthChecks@ and @ChildHealthCheck@ elements.
--
-- Note the following:
--
--     * If you specify a number greater than the number of child health checks, Route 53 always considers this health check to be unhealthy.
--
--
--     * If you specify @0@ , Route 53 always considers this health check to be healthy.
--
--
--
-- /Note:/ Consider using 'healthThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcHealthThreshold :: Lens.Lens' UpdateHealthCheck (Lude.Maybe Lude.Natural)
uhcHealthThreshold = Lens.lens (healthThreshold :: UpdateHealthCheck -> Lude.Maybe Lude.Natural) (\s a -> s {healthThreshold = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcHealthThreshold "Use generic-lens or generic-optics with 'healthThreshold' instead." #-}

-- | A complex type that contains one @Region@ element for each region that you want Amazon Route 53 health checkers to check the specified endpoint from.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcRegions :: Lens.Lens' UpdateHealthCheck (Lude.Maybe (Lude.NonEmpty HealthCheckRegion))
uhcRegions = Lens.lens (regions :: UpdateHealthCheck -> Lude.Maybe (Lude.NonEmpty HealthCheckRegion)) (\s a -> s {regions = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | The path that you want Amazon Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, for example the file /docs/route53-health-check.html. You can also include query string parameters, for example, @/welcome.html?language=jp&login=y@ .
--
-- Specify this value only if you want to change it.
--
-- /Note:/ Consider using 'resourcePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcResourcePath :: Lens.Lens' UpdateHealthCheck (Lude.Maybe Lude.Text)
uhcResourcePath = Lens.lens (resourcePath :: UpdateHealthCheck -> Lude.Maybe Lude.Text) (\s a -> s {resourcePath = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcResourcePath "Use generic-lens or generic-optics with 'resourcePath' instead." #-}

-- | The ID for the health check for which you want detailed information. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
--
-- /Note:/ Consider using 'healthCheckId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcHealthCheckId :: Lens.Lens' UpdateHealthCheck Lude.Text
uhcHealthCheckId = Lens.lens (healthCheckId :: UpdateHealthCheck -> Lude.Text) (\s a -> s {healthCheckId = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcHealthCheckId "Use generic-lens or generic-optics with 'healthCheckId' instead." #-}

-- | When CloudWatch has insufficient data about the metric to determine the alarm state, the status that you want Amazon Route 53 to assign to the health check:
--
--
--     * @Healthy@ : Route 53 considers the health check to be healthy.
--
--
--     * @Unhealthy@ : Route 53 considers the health check to be unhealthy.
--
--
--     * @LastKnownStatus@ : Route 53 uses the status of the health check from the last time CloudWatch had sufficient data to determine the alarm state. For new health checks that have no last known status, the default status for the health check is healthy.
--
--
--
-- /Note:/ Consider using 'insufficientDataHealthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcInsufficientDataHealthStatus :: Lens.Lens' UpdateHealthCheck (Lude.Maybe InsufficientDataHealthStatus)
uhcInsufficientDataHealthStatus = Lens.lens (insufficientDataHealthStatus :: UpdateHealthCheck -> Lude.Maybe InsufficientDataHealthStatus) (\s a -> s {insufficientDataHealthStatus = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcInsufficientDataHealthStatus "Use generic-lens or generic-optics with 'insufficientDataHealthStatus' instead." #-}

-- | A sequential counter that Amazon Route 53 sets to @1@ when you create a health check and increments by 1 each time you update settings for the health check.
--
-- We recommend that you use @GetHealthCheck@ or @ListHealthChecks@ to get the current value of @HealthCheckVersion@ for the health check that you want to update, and that you include that value in your @UpdateHealthCheck@ request. This prevents Route 53 from overwriting an intervening update:
--
--     * If the value in the @UpdateHealthCheck@ request matches the value of @HealthCheckVersion@ in the health check, Route 53 updates the health check with the new settings.
--
--
--     * If the value of @HealthCheckVersion@ in the health check is greater, the health check was changed after you got the version number. Route 53 does not update the health check, and it returns a @HealthCheckVersionMismatch@ error.
--
--
--
-- /Note:/ Consider using 'healthCheckVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcHealthCheckVersion :: Lens.Lens' UpdateHealthCheck (Lude.Maybe Lude.Natural)
uhcHealthCheckVersion = Lens.lens (healthCheckVersion :: UpdateHealthCheck -> Lude.Maybe Lude.Natural) (\s a -> s {healthCheckVersion = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcHealthCheckVersion "Use generic-lens or generic-optics with 'healthCheckVersion' instead." #-}

-- | A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether the specified health check is healthy.
--
-- /Note:/ Consider using 'alarmIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcAlarmIdentifier :: Lens.Lens' UpdateHealthCheck (Lude.Maybe AlarmIdentifier)
uhcAlarmIdentifier = Lens.lens (alarmIdentifier :: UpdateHealthCheck -> Lude.Maybe AlarmIdentifier) (\s a -> s {alarmIdentifier = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcAlarmIdentifier "Use generic-lens or generic-optics with 'alarmIdentifier' instead." #-}

-- | Specify whether you want Amazon Route 53 to invert the status of a health check, for example, to consider a health check unhealthy when it otherwise would be considered healthy.
--
-- /Note:/ Consider using 'inverted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcInverted :: Lens.Lens' UpdateHealthCheck (Lude.Maybe Lude.Bool)
uhcInverted = Lens.lens (inverted :: UpdateHealthCheck -> Lude.Maybe Lude.Bool) (\s a -> s {inverted = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcInverted "Use generic-lens or generic-optics with 'inverted' instead." #-}

-- | Amazon Route 53 behavior depends on whether you specify a value for @IPAddress@ .
--
-- __If you specify a value for__ @IPAddress@ :
-- Route 53 sends health check requests to the specified IPv4 or IPv6 address and passes the value of @FullyQualifiedDomainName@ in the @Host@ header for all health checks except TCP health checks. This is typically the fully qualified DNS name of the endpoint on which you want Route 53 to perform health checks.
-- When Route 53 checks the health of an endpoint, here is how it constructs the @Host@ header:
--
--     * If you specify a value of @80@ for @Port@ and @HTTP@ or @HTTP_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
--
--
--     * If you specify a value of @443@ for @Port@ and @HTTPS@ or @HTTPS_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
--
--
--     * If you specify another value for @Port@ and any value except @TCP@ for @Type@ , Route 53 passes /@FullyQualifiedDomainName@ :@Port@ / to the endpoint in the @Host@ header.
--
--
-- If you don't specify a value for @FullyQualifiedDomainName@ , Route 53 substitutes the value of @IPAddress@ in the @Host@ header in each of the above cases.
-- __If you don't specify a value for__ @IPAddress@ :
-- If you don't specify a value for @IPAddress@ , Route 53 sends a DNS request to the domain that you specify in @FullyQualifiedDomainName@ at the interval you specify in @RequestInterval@ . Using an IPv4 address that is returned by DNS, Route 53 then checks the health of the endpoint.
-- If you want to check the health of weighted, latency, or failover resource record sets and you choose to specify the endpoint only by @FullyQualifiedDomainName@ , we recommend that you create a separate health check for each endpoint. For example, create a health check for each HTTP server that is serving content for www.example.com. For the value of @FullyQualifiedDomainName@ , specify the domain name of the server (such as @us-east-2-www.example.com@ ), not the name of the resource record sets (www.example.com).
-- /Important:/ In this configuration, if the value of @FullyQualifiedDomainName@ matches the name of the resource record sets and you then associate the health check with those resource record sets, health check results will be unpredictable.
-- In addition, if the value of @Type@ is @HTTP@ , @HTTPS@ , @HTTP_STR_MATCH@ , or @HTTPS_STR_MATCH@ , Route 53 passes the value of @FullyQualifiedDomainName@ in the @Host@ header, as it does when you specify a value for @IPAddress@ . If the value of @Type@ is @TCP@ , Route 53 doesn't pass a @Host@ header.
--
-- /Note:/ Consider using 'fullyQualifiedDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcFullyQualifiedDomainName :: Lens.Lens' UpdateHealthCheck (Lude.Maybe Lude.Text)
uhcFullyQualifiedDomainName = Lens.lens (fullyQualifiedDomainName :: UpdateHealthCheck -> Lude.Maybe Lude.Text) (\s a -> s {fullyQualifiedDomainName = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcFullyQualifiedDomainName "Use generic-lens or generic-optics with 'fullyQualifiedDomainName' instead." #-}

-- | A complex type that contains one @ChildHealthCheck@ element for each health check that you want to associate with a @CALCULATED@ health check.
--
-- /Note:/ Consider using 'childHealthChecks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcChildHealthChecks :: Lens.Lens' UpdateHealthCheck (Lude.Maybe [Lude.Text])
uhcChildHealthChecks = Lens.lens (childHealthChecks :: UpdateHealthCheck -> Lude.Maybe [Lude.Text]) (\s a -> s {childHealthChecks = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcChildHealthChecks "Use generic-lens or generic-optics with 'childHealthChecks' instead." #-}

-- | The port on the endpoint that you want Amazon Route 53 to perform health checks on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcPort :: Lens.Lens' UpdateHealthCheck (Lude.Maybe Lude.Natural)
uhcPort = Lens.lens (port :: UpdateHealthCheck -> Lude.Maybe Lude.Natural) (\s a -> s {port = a} :: UpdateHealthCheck)
{-# DEPRECATED uhcPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.AWSRequest UpdateHealthCheck where
  type Rs UpdateHealthCheck = UpdateHealthCheckResponse
  request = Req.postXML route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          UpdateHealthCheckResponse'
            Lude.<$> (x Lude..@ "HealthCheck") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement UpdateHealthCheck where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateHealthCheckRequest"

instance Lude.ToHeaders UpdateHealthCheck where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateHealthCheck where
  toPath UpdateHealthCheck' {..} =
    Lude.mconcat
      ["/2013-04-01/healthcheck/", Lude.toBS healthCheckId]

instance Lude.ToQuery UpdateHealthCheck where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML UpdateHealthCheck where
  toXML UpdateHealthCheck' {..} =
    Lude.mconcat
      [ "FailureThreshold" Lude.@= failureThreshold,
        "IPAddress" Lude.@= ipAddress,
        "EnableSNI" Lude.@= enableSNI,
        "Disabled" Lude.@= disabled,
        "ResetElements"
          Lude.@= Lude.toXML
            (Lude.toXMLList "ResettableElementName" Lude.<$> resetElements),
        "SearchString" Lude.@= searchString,
        "HealthThreshold" Lude.@= healthThreshold,
        "Regions"
          Lude.@= Lude.toXML (Lude.toXMLList "Region" Lude.<$> regions),
        "ResourcePath" Lude.@= resourcePath,
        "InsufficientDataHealthStatus"
          Lude.@= insufficientDataHealthStatus,
        "HealthCheckVersion" Lude.@= healthCheckVersion,
        "AlarmIdentifier" Lude.@= alarmIdentifier,
        "Inverted" Lude.@= inverted,
        "FullyQualifiedDomainName" Lude.@= fullyQualifiedDomainName,
        "ChildHealthChecks"
          Lude.@= Lude.toXML
            (Lude.toXMLList "ChildHealthCheck" Lude.<$> childHealthChecks),
        "Port" Lude.@= port
      ]

-- | A complex type that contains the response to the @UpdateHealthCheck@ request.
--
-- /See:/ 'mkUpdateHealthCheckResponse' smart constructor.
data UpdateHealthCheckResponse = UpdateHealthCheckResponse'
  { -- | A complex type that contains the response to an @UpdateHealthCheck@ request.
    healthCheck :: HealthCheck,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateHealthCheckResponse' with the minimum fields required to make a request.
--
-- * 'healthCheck' - A complex type that contains the response to an @UpdateHealthCheck@ request.
-- * 'responseStatus' - The response status code.
mkUpdateHealthCheckResponse ::
  -- | 'healthCheck'
  HealthCheck ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateHealthCheckResponse
mkUpdateHealthCheckResponse pHealthCheck_ pResponseStatus_ =
  UpdateHealthCheckResponse'
    { healthCheck = pHealthCheck_,
      responseStatus = pResponseStatus_
    }

-- | A complex type that contains the response to an @UpdateHealthCheck@ request.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcrsHealthCheck :: Lens.Lens' UpdateHealthCheckResponse HealthCheck
uhcrsHealthCheck = Lens.lens (healthCheck :: UpdateHealthCheckResponse -> HealthCheck) (\s a -> s {healthCheck = a} :: UpdateHealthCheckResponse)
{-# DEPRECATED uhcrsHealthCheck "Use generic-lens or generic-optics with 'healthCheck' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcrsResponseStatus :: Lens.Lens' UpdateHealthCheckResponse Lude.Int
uhcrsResponseStatus = Lens.lens (responseStatus :: UpdateHealthCheckResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateHealthCheckResponse)
{-# DEPRECATED uhcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
