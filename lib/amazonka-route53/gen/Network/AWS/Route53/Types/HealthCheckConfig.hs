{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HealthCheckConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HealthCheckConfig
  ( HealthCheckConfig (..),

    -- * Smart constructor
    mkHealthCheckConfig,

    -- * Lenses
    hccFailureThreshold,
    hccIPAddress,
    hccEnableSNI,
    hccDisabled,
    hccSearchString,
    hccHealthThreshold,
    hccRegions,
    hccResourcePath,
    hccInsufficientDataHealthStatus,
    hccType,
    hccAlarmIdentifier,
    hccMeasureLatency,
    hccInverted,
    hccFullyQualifiedDomainName,
    hccChildHealthChecks,
    hccRequestInterval,
    hccPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.AlarmIdentifier
import Network.AWS.Route53.Types.HealthCheckRegion
import Network.AWS.Route53.Types.HealthCheckType
import Network.AWS.Route53.Types.InsufficientDataHealthStatus

-- | A complex type that contains information about the health check.
--
-- /See:/ 'mkHealthCheckConfig' smart constructor.
data HealthCheckConfig = HealthCheckConfig'
  { -- | The number of consecutive health checks that an endpoint must pass or fail for Amazon Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
    --
    -- If you don't specify a value for @FailureThreshold@ , the default value is three health checks.
    failureThreshold :: Lude.Maybe Lude.Natural,
    -- | The IPv4 or IPv6 IP address of the endpoint that you want Amazon Route 53 to perform health checks on. If you don't specify a value for @IPAddress@ , Route 53 sends a DNS request to resolve the domain name that you specify in @FullyQualifiedDomainName@ at the interval that you specify in @RequestInterval@ . Using an IP address returned by DNS, Route 53 then checks the health of the endpoint.
    --
    -- Use one of the following formats for the value of @IPAddress@ :
    --
    --     * __IPv4 address__ : four values between 0 and 255, separated by periods (.), for example, @192.0.2.44@ .
    --
    --
    --     * __IPv6 address__ : eight groups of four hexadecimal values, separated by colons (:), for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ . You can also shorten IPv6 addresses as described in RFC 5952, for example, @2001:db8:85a3::abcd:1:2345@ .
    --
    --
    -- If the endpoint is an EC2 instance, we recommend that you create an Elastic IP address, associate it with your EC2 instance, and specify the Elastic IP address for @IPAddress@ . This ensures that the IP address of your instance will never change.
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
    -- When the value of @Type@ is @CALCULATED@ or @CLOUDWATCH_METRIC@ , omit @IPAddress@ .
    ipAddress :: Lude.Maybe Lude.Text,
    -- | Specify whether you want Amazon Route 53 to send the value of @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message during TLS negotiation. This allows the endpoint to respond to @HTTPS@ health check requests with the applicable SSL/TLS certificate.
    --
    -- Some endpoints require that @HTTPS@ requests include the host name in the @client_hello@ message. If you don't enable SNI, the status of the health check will be @SSL alert handshake_failure@ . A health check can also have that status for other reasons. If SNI is enabled and you're still getting the error, check the SSL/TLS configuration on your endpoint and confirm that your certificate is valid.
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
    -- | If the value of Type is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@ , the string that you want Amazon Route 53 to search for in the response body from the specified resource. If the string appears in the response body, Route 53 considers the resource healthy.
    --
    -- Route 53 considers case when searching for @SearchString@ in the response body.
    searchString :: Lude.Maybe Lude.Text,
    -- | The number of child health checks that are associated with a @CALCULATED@ health check that Amazon Route 53 must consider healthy for the @CALCULATED@ health check to be considered healthy. To specify the child health checks that you want to associate with a @CALCULATED@ health check, use the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-ChildHealthChecks ChildHealthChecks> element.
    --
    -- Note the following:
    --
    --     * If you specify a number greater than the number of child health checks, Route 53 always considers this health check to be unhealthy.
    --
    --
    --     * If you specify @0@ , Route 53 always considers this health check to be healthy.
    healthThreshold :: Lude.Maybe Lude.Natural,
    -- | A complex type that contains one @Region@ element for each region from which you want Amazon Route 53 health checkers to check the specified endpoint.
    --
    -- If you don't specify any regions, Route 53 health checkers automatically performs checks from all of the regions that are listed under __Valid Values__ .
    -- If you update a health check to remove a region that has been performing health checks, Route 53 will briefly continue to perform checks from that region to ensure that some health checkers are always checking the endpoint (for example, if you replace three regions with four different regions).
    regions :: Lude.Maybe (Lude.NonEmpty HealthCheckRegion),
    -- | The path, if any, that you want Amazon Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, for example, the file /docs/route53-health-check.html. You can also include query string parameters, for example, @/welcome.html?language=jp&login=y@ .
    resourcePath :: Lude.Maybe Lude.Text,
    -- | When CloudWatch has insufficient data about the metric to determine the alarm state, the status that you want Amazon Route 53 to assign to the health check:
    --
    --
    --     * @Healthy@ : Route 53 considers the health check to be healthy.
    --
    --
    --     * @Unhealthy@ : Route 53 considers the health check to be unhealthy.
    --
    --
    --     * @LastKnownStatus@ : Route 53 uses the status of the health check from the last time that CloudWatch had sufficient data to determine the alarm state. For new health checks that have no last known status, the default status for the health check is healthy.
    insufficientDataHealthStatus :: Lude.Maybe InsufficientDataHealthStatus,
    -- | The type of health check that you want to create, which indicates how Amazon Route 53 determines whether an endpoint is healthy.
    --
    -- /Important:/ You can't change the value of @Type@ after you create a health check.
    -- You can create the following types of health checks:
    --
    --     * __HTTP__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTP request and waits for an HTTP status code of 200 or greater and less than 400.
    --
    --
    --     * __HTTPS__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTPS request and waits for an HTTP status code of 200 or greater and less than 400.
    -- /Important:/ If you specify @HTTPS@ for the value of @Type@ , the endpoint must support TLS v1.0 or later.
    --
    --
    --     * __HTTP_STR_MATCH__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTP request and searches the first 5,120 bytes of the response body for the string that you specify in @SearchString@ .
    --
    --
    --     * __HTTPS_STR_MATCH__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an @HTTPS@ request and searches the first 5,120 bytes of the response body for the string that you specify in @SearchString@ .
    --
    --
    --     * __TCP__ : Route 53 tries to establish a TCP connection.
    --
    --
    --     * __CLOUDWATCH_METRIC__ : The health check is associated with a CloudWatch alarm. If the state of the alarm is @OK@ , the health check is considered healthy. If the state is @ALARM@ , the health check is considered unhealthy. If CloudWatch doesn't have sufficient data to determine whether the state is @OK@ or @ALARM@ , the health check status depends on the setting for @InsufficientDataHealthStatus@ : @Healthy@ , @Unhealthy@ , or @LastKnownStatus@ .
    --
    --
    --     * __CALCULATED__ : For health checks that monitor the status of other health checks, Route 53 adds up the number of health checks that Route 53 health checkers consider to be healthy and compares that number with the value of @HealthThreshold@ .
    --
    --
    -- For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
    type' :: HealthCheckType,
    -- | A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether the specified health check is healthy.
    alarmIdentifier :: Lude.Maybe AlarmIdentifier,
    -- | Specify whether you want Amazon Route 53 to measure the latency between health checkers in multiple AWS regions and your endpoint, and to display CloudWatch latency graphs on the __Health Checks__ page in the Route 53 console.
    --
    -- /Important:/ You can't change the value of @MeasureLatency@ after you create a health check.
    measureLatency :: Lude.Maybe Lude.Bool,
    -- | Specify whether you want Amazon Route 53 to invert the status of a health check, for example, to consider a health check unhealthy when it otherwise would be considered healthy.
    inverted :: Lude.Maybe Lude.Bool,
    -- | Amazon Route 53 behavior depends on whether you specify a value for @IPAddress@ .
    --
    -- __If you specify a value for__ @IPAddress@ :
    -- Amazon Route 53 sends health check requests to the specified IPv4 or IPv6 address and passes the value of @FullyQualifiedDomainName@ in the @Host@ header for all health checks except TCP health checks. This is typically the fully qualified DNS name of the endpoint on which you want Route 53 to perform health checks.
    -- When Route 53 checks the health of an endpoint, here is how it constructs the @Host@ header:
    --
    --     * If you specify a value of @80@ for @Port@ and @HTTP@ or @HTTP_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the Host header.
    --
    --
    --     * If you specify a value of @443@ for @Port@ and @HTTPS@ or @HTTPS_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
    --
    --
    --     * If you specify another value for @Port@ and any value except @TCP@ for @Type@ , Route 53 passes @FullyQualifiedDomainName:Port@ to the endpoint in the @Host@ header.
    --
    --
    -- If you don't specify a value for @FullyQualifiedDomainName@ , Route 53 substitutes the value of @IPAddress@ in the @Host@ header in each of the preceding cases.
    -- __If you don't specify a value for @IPAddress@ __ :
    -- Route 53 sends a DNS request to the domain that you specify for @FullyQualifiedDomainName@ at the interval that you specify for @RequestInterval@ . Using an IPv4 address that DNS returns, Route 53 then checks the health of the endpoint.
    -- If you want to check the health of weighted, latency, or failover resource record sets and you choose to specify the endpoint only by @FullyQualifiedDomainName@ , we recommend that you create a separate health check for each endpoint. For example, create a health check for each HTTP server that is serving content for www.example.com. For the value of @FullyQualifiedDomainName@ , specify the domain name of the server (such as us-east-2-www.example.com), not the name of the resource record sets (www.example.com).
    -- /Important:/ In this configuration, if you create a health check for which the value of @FullyQualifiedDomainName@ matches the name of the resource record sets and you then associate the health check with those resource record sets, health check results will be unpredictable.
    -- In addition, if the value that you specify for @Type@ is @HTTP@ , @HTTPS@ , @HTTP_STR_MATCH@ , or @HTTPS_STR_MATCH@ , Route 53 passes the value of @FullyQualifiedDomainName@ in the @Host@ header, as it does when you specify a value for @IPAddress@ . If the value of @Type@ is @TCP@ , Route 53 doesn't pass a @Host@ header.
    fullyQualifiedDomainName :: Lude.Maybe Lude.Text,
    -- | (CALCULATED Health Checks Only) A complex type that contains one @ChildHealthCheck@ element for each health check that you want to associate with a @CALCULATED@ health check.
    childHealthChecks :: Lude.Maybe [Lude.Text],
    -- | The number of seconds between the time that Amazon Route 53 gets a response from your endpoint and the time that it sends the next health check request. Each Route 53 health checker makes requests at this interval.
    --
    -- /Important:/ You can't change the value of @RequestInterval@ after you create a health check.
    -- If you don't specify a value for @RequestInterval@ , the default value is @30@ seconds.
    requestInterval :: Lude.Maybe Lude.Natural,
    -- | The port on the endpoint that you want Amazon Route 53 to perform health checks on.
    port :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HealthCheckConfig' with the minimum fields required to make a request.
--
-- * 'failureThreshold' - The number of consecutive health checks that an endpoint must pass or fail for Amazon Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
--
-- If you don't specify a value for @FailureThreshold@ , the default value is three health checks.
-- * 'ipAddress' - The IPv4 or IPv6 IP address of the endpoint that you want Amazon Route 53 to perform health checks on. If you don't specify a value for @IPAddress@ , Route 53 sends a DNS request to resolve the domain name that you specify in @FullyQualifiedDomainName@ at the interval that you specify in @RequestInterval@ . Using an IP address returned by DNS, Route 53 then checks the health of the endpoint.
--
-- Use one of the following formats for the value of @IPAddress@ :
--
--     * __IPv4 address__ : four values between 0 and 255, separated by periods (.), for example, @192.0.2.44@ .
--
--
--     * __IPv6 address__ : eight groups of four hexadecimal values, separated by colons (:), for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ . You can also shorten IPv6 addresses as described in RFC 5952, for example, @2001:db8:85a3::abcd:1:2345@ .
--
--
-- If the endpoint is an EC2 instance, we recommend that you create an Elastic IP address, associate it with your EC2 instance, and specify the Elastic IP address for @IPAddress@ . This ensures that the IP address of your instance will never change.
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
-- When the value of @Type@ is @CALCULATED@ or @CLOUDWATCH_METRIC@ , omit @IPAddress@ .
-- * 'enableSNI' - Specify whether you want Amazon Route 53 to send the value of @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message during TLS negotiation. This allows the endpoint to respond to @HTTPS@ health check requests with the applicable SSL/TLS certificate.
--
-- Some endpoints require that @HTTPS@ requests include the host name in the @client_hello@ message. If you don't enable SNI, the status of the health check will be @SSL alert handshake_failure@ . A health check can also have that status for other reasons. If SNI is enabled and you're still getting the error, check the SSL/TLS configuration on your endpoint and confirm that your certificate is valid.
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
-- * 'searchString' - If the value of Type is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@ , the string that you want Amazon Route 53 to search for in the response body from the specified resource. If the string appears in the response body, Route 53 considers the resource healthy.
--
-- Route 53 considers case when searching for @SearchString@ in the response body.
-- * 'healthThreshold' - The number of child health checks that are associated with a @CALCULATED@ health check that Amazon Route 53 must consider healthy for the @CALCULATED@ health check to be considered healthy. To specify the child health checks that you want to associate with a @CALCULATED@ health check, use the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-ChildHealthChecks ChildHealthChecks> element.
--
-- Note the following:
--
--     * If you specify a number greater than the number of child health checks, Route 53 always considers this health check to be unhealthy.
--
--
--     * If you specify @0@ , Route 53 always considers this health check to be healthy.
--
--
-- * 'regions' - A complex type that contains one @Region@ element for each region from which you want Amazon Route 53 health checkers to check the specified endpoint.
--
-- If you don't specify any regions, Route 53 health checkers automatically performs checks from all of the regions that are listed under __Valid Values__ .
-- If you update a health check to remove a region that has been performing health checks, Route 53 will briefly continue to perform checks from that region to ensure that some health checkers are always checking the endpoint (for example, if you replace three regions with four different regions).
-- * 'resourcePath' - The path, if any, that you want Amazon Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, for example, the file /docs/route53-health-check.html. You can also include query string parameters, for example, @/welcome.html?language=jp&login=y@ .
-- * 'insufficientDataHealthStatus' - When CloudWatch has insufficient data about the metric to determine the alarm state, the status that you want Amazon Route 53 to assign to the health check:
--
--
--     * @Healthy@ : Route 53 considers the health check to be healthy.
--
--
--     * @Unhealthy@ : Route 53 considers the health check to be unhealthy.
--
--
--     * @LastKnownStatus@ : Route 53 uses the status of the health check from the last time that CloudWatch had sufficient data to determine the alarm state. For new health checks that have no last known status, the default status for the health check is healthy.
--
--
-- * 'type'' - The type of health check that you want to create, which indicates how Amazon Route 53 determines whether an endpoint is healthy.
--
-- /Important:/ You can't change the value of @Type@ after you create a health check.
-- You can create the following types of health checks:
--
--     * __HTTP__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTP request and waits for an HTTP status code of 200 or greater and less than 400.
--
--
--     * __HTTPS__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTPS request and waits for an HTTP status code of 200 or greater and less than 400.
-- /Important:/ If you specify @HTTPS@ for the value of @Type@ , the endpoint must support TLS v1.0 or later.
--
--
--     * __HTTP_STR_MATCH__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTP request and searches the first 5,120 bytes of the response body for the string that you specify in @SearchString@ .
--
--
--     * __HTTPS_STR_MATCH__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an @HTTPS@ request and searches the first 5,120 bytes of the response body for the string that you specify in @SearchString@ .
--
--
--     * __TCP__ : Route 53 tries to establish a TCP connection.
--
--
--     * __CLOUDWATCH_METRIC__ : The health check is associated with a CloudWatch alarm. If the state of the alarm is @OK@ , the health check is considered healthy. If the state is @ALARM@ , the health check is considered unhealthy. If CloudWatch doesn't have sufficient data to determine whether the state is @OK@ or @ALARM@ , the health check status depends on the setting for @InsufficientDataHealthStatus@ : @Healthy@ , @Unhealthy@ , or @LastKnownStatus@ .
--
--
--     * __CALCULATED__ : For health checks that monitor the status of other health checks, Route 53 adds up the number of health checks that Route 53 health checkers consider to be healthy and compares that number with the value of @HealthThreshold@ .
--
--
-- For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
-- * 'alarmIdentifier' - A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether the specified health check is healthy.
-- * 'measureLatency' - Specify whether you want Amazon Route 53 to measure the latency between health checkers in multiple AWS regions and your endpoint, and to display CloudWatch latency graphs on the __Health Checks__ page in the Route 53 console.
--
-- /Important:/ You can't change the value of @MeasureLatency@ after you create a health check.
-- * 'inverted' - Specify whether you want Amazon Route 53 to invert the status of a health check, for example, to consider a health check unhealthy when it otherwise would be considered healthy.
-- * 'fullyQualifiedDomainName' - Amazon Route 53 behavior depends on whether you specify a value for @IPAddress@ .
--
-- __If you specify a value for__ @IPAddress@ :
-- Amazon Route 53 sends health check requests to the specified IPv4 or IPv6 address and passes the value of @FullyQualifiedDomainName@ in the @Host@ header for all health checks except TCP health checks. This is typically the fully qualified DNS name of the endpoint on which you want Route 53 to perform health checks.
-- When Route 53 checks the health of an endpoint, here is how it constructs the @Host@ header:
--
--     * If you specify a value of @80@ for @Port@ and @HTTP@ or @HTTP_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the Host header.
--
--
--     * If you specify a value of @443@ for @Port@ and @HTTPS@ or @HTTPS_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
--
--
--     * If you specify another value for @Port@ and any value except @TCP@ for @Type@ , Route 53 passes @FullyQualifiedDomainName:Port@ to the endpoint in the @Host@ header.
--
--
-- If you don't specify a value for @FullyQualifiedDomainName@ , Route 53 substitutes the value of @IPAddress@ in the @Host@ header in each of the preceding cases.
-- __If you don't specify a value for @IPAddress@ __ :
-- Route 53 sends a DNS request to the domain that you specify for @FullyQualifiedDomainName@ at the interval that you specify for @RequestInterval@ . Using an IPv4 address that DNS returns, Route 53 then checks the health of the endpoint.
-- If you want to check the health of weighted, latency, or failover resource record sets and you choose to specify the endpoint only by @FullyQualifiedDomainName@ , we recommend that you create a separate health check for each endpoint. For example, create a health check for each HTTP server that is serving content for www.example.com. For the value of @FullyQualifiedDomainName@ , specify the domain name of the server (such as us-east-2-www.example.com), not the name of the resource record sets (www.example.com).
-- /Important:/ In this configuration, if you create a health check for which the value of @FullyQualifiedDomainName@ matches the name of the resource record sets and you then associate the health check with those resource record sets, health check results will be unpredictable.
-- In addition, if the value that you specify for @Type@ is @HTTP@ , @HTTPS@ , @HTTP_STR_MATCH@ , or @HTTPS_STR_MATCH@ , Route 53 passes the value of @FullyQualifiedDomainName@ in the @Host@ header, as it does when you specify a value for @IPAddress@ . If the value of @Type@ is @TCP@ , Route 53 doesn't pass a @Host@ header.
-- * 'childHealthChecks' - (CALCULATED Health Checks Only) A complex type that contains one @ChildHealthCheck@ element for each health check that you want to associate with a @CALCULATED@ health check.
-- * 'requestInterval' - The number of seconds between the time that Amazon Route 53 gets a response from your endpoint and the time that it sends the next health check request. Each Route 53 health checker makes requests at this interval.
--
-- /Important:/ You can't change the value of @RequestInterval@ after you create a health check.
-- If you don't specify a value for @RequestInterval@ , the default value is @30@ seconds.
-- * 'port' - The port on the endpoint that you want Amazon Route 53 to perform health checks on.
mkHealthCheckConfig ::
  -- | 'type''
  HealthCheckType ->
  HealthCheckConfig
mkHealthCheckConfig pType_ =
  HealthCheckConfig'
    { failureThreshold = Lude.Nothing,
      ipAddress = Lude.Nothing,
      enableSNI = Lude.Nothing,
      disabled = Lude.Nothing,
      searchString = Lude.Nothing,
      healthThreshold = Lude.Nothing,
      regions = Lude.Nothing,
      resourcePath = Lude.Nothing,
      insufficientDataHealthStatus = Lude.Nothing,
      type' = pType_,
      alarmIdentifier = Lude.Nothing,
      measureLatency = Lude.Nothing,
      inverted = Lude.Nothing,
      fullyQualifiedDomainName = Lude.Nothing,
      childHealthChecks = Lude.Nothing,
      requestInterval = Lude.Nothing,
      port = Lude.Nothing
    }

-- | The number of consecutive health checks that an endpoint must pass or fail for Amazon Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
--
-- If you don't specify a value for @FailureThreshold@ , the default value is three health checks.
--
-- /Note:/ Consider using 'failureThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccFailureThreshold :: Lens.Lens' HealthCheckConfig (Lude.Maybe Lude.Natural)
hccFailureThreshold = Lens.lens (failureThreshold :: HealthCheckConfig -> Lude.Maybe Lude.Natural) (\s a -> s {failureThreshold = a} :: HealthCheckConfig)
{-# DEPRECATED hccFailureThreshold "Use generic-lens or generic-optics with 'failureThreshold' instead." #-}

-- | The IPv4 or IPv6 IP address of the endpoint that you want Amazon Route 53 to perform health checks on. If you don't specify a value for @IPAddress@ , Route 53 sends a DNS request to resolve the domain name that you specify in @FullyQualifiedDomainName@ at the interval that you specify in @RequestInterval@ . Using an IP address returned by DNS, Route 53 then checks the health of the endpoint.
--
-- Use one of the following formats for the value of @IPAddress@ :
--
--     * __IPv4 address__ : four values between 0 and 255, separated by periods (.), for example, @192.0.2.44@ .
--
--
--     * __IPv6 address__ : eight groups of four hexadecimal values, separated by colons (:), for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ . You can also shorten IPv6 addresses as described in RFC 5952, for example, @2001:db8:85a3::abcd:1:2345@ .
--
--
-- If the endpoint is an EC2 instance, we recommend that you create an Elastic IP address, associate it with your EC2 instance, and specify the Elastic IP address for @IPAddress@ . This ensures that the IP address of your instance will never change.
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
-- When the value of @Type@ is @CALCULATED@ or @CLOUDWATCH_METRIC@ , omit @IPAddress@ .
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccIPAddress :: Lens.Lens' HealthCheckConfig (Lude.Maybe Lude.Text)
hccIPAddress = Lens.lens (ipAddress :: HealthCheckConfig -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: HealthCheckConfig)
{-# DEPRECATED hccIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | Specify whether you want Amazon Route 53 to send the value of @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message during TLS negotiation. This allows the endpoint to respond to @HTTPS@ health check requests with the applicable SSL/TLS certificate.
--
-- Some endpoints require that @HTTPS@ requests include the host name in the @client_hello@ message. If you don't enable SNI, the status of the health check will be @SSL alert handshake_failure@ . A health check can also have that status for other reasons. If SNI is enabled and you're still getting the error, check the SSL/TLS configuration on your endpoint and confirm that your certificate is valid.
-- The SSL/TLS certificate on your endpoint includes a domain name in the @Common Name@ field and possibly several more in the @Subject Alternative Names@ field. One of the domain names in the certificate should match the value that you specify for @FullyQualifiedDomainName@ . If the endpoint responds to the @client_hello@ message with a certificate that does not include the domain name that you specified in @FullyQualifiedDomainName@ , a health checker will retry the handshake. In the second attempt, the health checker will omit @FullyQualifiedDomainName@ from the @client_hello@ message.
--
-- /Note:/ Consider using 'enableSNI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccEnableSNI :: Lens.Lens' HealthCheckConfig (Lude.Maybe Lude.Bool)
hccEnableSNI = Lens.lens (enableSNI :: HealthCheckConfig -> Lude.Maybe Lude.Bool) (\s a -> s {enableSNI = a} :: HealthCheckConfig)
{-# DEPRECATED hccEnableSNI "Use generic-lens or generic-optics with 'enableSNI' instead." #-}

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
hccDisabled :: Lens.Lens' HealthCheckConfig (Lude.Maybe Lude.Bool)
hccDisabled = Lens.lens (disabled :: HealthCheckConfig -> Lude.Maybe Lude.Bool) (\s a -> s {disabled = a} :: HealthCheckConfig)
{-# DEPRECATED hccDisabled "Use generic-lens or generic-optics with 'disabled' instead." #-}

-- | If the value of Type is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@ , the string that you want Amazon Route 53 to search for in the response body from the specified resource. If the string appears in the response body, Route 53 considers the resource healthy.
--
-- Route 53 considers case when searching for @SearchString@ in the response body.
--
-- /Note:/ Consider using 'searchString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccSearchString :: Lens.Lens' HealthCheckConfig (Lude.Maybe Lude.Text)
hccSearchString = Lens.lens (searchString :: HealthCheckConfig -> Lude.Maybe Lude.Text) (\s a -> s {searchString = a} :: HealthCheckConfig)
{-# DEPRECATED hccSearchString "Use generic-lens or generic-optics with 'searchString' instead." #-}

-- | The number of child health checks that are associated with a @CALCULATED@ health check that Amazon Route 53 must consider healthy for the @CALCULATED@ health check to be considered healthy. To specify the child health checks that you want to associate with a @CALCULATED@ health check, use the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-ChildHealthChecks ChildHealthChecks> element.
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
hccHealthThreshold :: Lens.Lens' HealthCheckConfig (Lude.Maybe Lude.Natural)
hccHealthThreshold = Lens.lens (healthThreshold :: HealthCheckConfig -> Lude.Maybe Lude.Natural) (\s a -> s {healthThreshold = a} :: HealthCheckConfig)
{-# DEPRECATED hccHealthThreshold "Use generic-lens or generic-optics with 'healthThreshold' instead." #-}

-- | A complex type that contains one @Region@ element for each region from which you want Amazon Route 53 health checkers to check the specified endpoint.
--
-- If you don't specify any regions, Route 53 health checkers automatically performs checks from all of the regions that are listed under __Valid Values__ .
-- If you update a health check to remove a region that has been performing health checks, Route 53 will briefly continue to perform checks from that region to ensure that some health checkers are always checking the endpoint (for example, if you replace three regions with four different regions).
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccRegions :: Lens.Lens' HealthCheckConfig (Lude.Maybe (Lude.NonEmpty HealthCheckRegion))
hccRegions = Lens.lens (regions :: HealthCheckConfig -> Lude.Maybe (Lude.NonEmpty HealthCheckRegion)) (\s a -> s {regions = a} :: HealthCheckConfig)
{-# DEPRECATED hccRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | The path, if any, that you want Amazon Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, for example, the file /docs/route53-health-check.html. You can also include query string parameters, for example, @/welcome.html?language=jp&login=y@ .
--
-- /Note:/ Consider using 'resourcePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccResourcePath :: Lens.Lens' HealthCheckConfig (Lude.Maybe Lude.Text)
hccResourcePath = Lens.lens (resourcePath :: HealthCheckConfig -> Lude.Maybe Lude.Text) (\s a -> s {resourcePath = a} :: HealthCheckConfig)
{-# DEPRECATED hccResourcePath "Use generic-lens or generic-optics with 'resourcePath' instead." #-}

-- | When CloudWatch has insufficient data about the metric to determine the alarm state, the status that you want Amazon Route 53 to assign to the health check:
--
--
--     * @Healthy@ : Route 53 considers the health check to be healthy.
--
--
--     * @Unhealthy@ : Route 53 considers the health check to be unhealthy.
--
--
--     * @LastKnownStatus@ : Route 53 uses the status of the health check from the last time that CloudWatch had sufficient data to determine the alarm state. For new health checks that have no last known status, the default status for the health check is healthy.
--
--
--
-- /Note:/ Consider using 'insufficientDataHealthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccInsufficientDataHealthStatus :: Lens.Lens' HealthCheckConfig (Lude.Maybe InsufficientDataHealthStatus)
hccInsufficientDataHealthStatus = Lens.lens (insufficientDataHealthStatus :: HealthCheckConfig -> Lude.Maybe InsufficientDataHealthStatus) (\s a -> s {insufficientDataHealthStatus = a} :: HealthCheckConfig)
{-# DEPRECATED hccInsufficientDataHealthStatus "Use generic-lens or generic-optics with 'insufficientDataHealthStatus' instead." #-}

-- | The type of health check that you want to create, which indicates how Amazon Route 53 determines whether an endpoint is healthy.
--
-- /Important:/ You can't change the value of @Type@ after you create a health check.
-- You can create the following types of health checks:
--
--     * __HTTP__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTP request and waits for an HTTP status code of 200 or greater and less than 400.
--
--
--     * __HTTPS__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTPS request and waits for an HTTP status code of 200 or greater and less than 400.
-- /Important:/ If you specify @HTTPS@ for the value of @Type@ , the endpoint must support TLS v1.0 or later.
--
--
--     * __HTTP_STR_MATCH__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTP request and searches the first 5,120 bytes of the response body for the string that you specify in @SearchString@ .
--
--
--     * __HTTPS_STR_MATCH__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an @HTTPS@ request and searches the first 5,120 bytes of the response body for the string that you specify in @SearchString@ .
--
--
--     * __TCP__ : Route 53 tries to establish a TCP connection.
--
--
--     * __CLOUDWATCH_METRIC__ : The health check is associated with a CloudWatch alarm. If the state of the alarm is @OK@ , the health check is considered healthy. If the state is @ALARM@ , the health check is considered unhealthy. If CloudWatch doesn't have sufficient data to determine whether the state is @OK@ or @ALARM@ , the health check status depends on the setting for @InsufficientDataHealthStatus@ : @Healthy@ , @Unhealthy@ , or @LastKnownStatus@ .
--
--
--     * __CALCULATED__ : For health checks that monitor the status of other health checks, Route 53 adds up the number of health checks that Route 53 health checkers consider to be healthy and compares that number with the value of @HealthThreshold@ .
--
--
-- For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccType :: Lens.Lens' HealthCheckConfig HealthCheckType
hccType = Lens.lens (type' :: HealthCheckConfig -> HealthCheckType) (\s a -> s {type' = a} :: HealthCheckConfig)
{-# DEPRECATED hccType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether the specified health check is healthy.
--
-- /Note:/ Consider using 'alarmIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccAlarmIdentifier :: Lens.Lens' HealthCheckConfig (Lude.Maybe AlarmIdentifier)
hccAlarmIdentifier = Lens.lens (alarmIdentifier :: HealthCheckConfig -> Lude.Maybe AlarmIdentifier) (\s a -> s {alarmIdentifier = a} :: HealthCheckConfig)
{-# DEPRECATED hccAlarmIdentifier "Use generic-lens or generic-optics with 'alarmIdentifier' instead." #-}

-- | Specify whether you want Amazon Route 53 to measure the latency between health checkers in multiple AWS regions and your endpoint, and to display CloudWatch latency graphs on the __Health Checks__ page in the Route 53 console.
--
-- /Important:/ You can't change the value of @MeasureLatency@ after you create a health check.
--
-- /Note:/ Consider using 'measureLatency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccMeasureLatency :: Lens.Lens' HealthCheckConfig (Lude.Maybe Lude.Bool)
hccMeasureLatency = Lens.lens (measureLatency :: HealthCheckConfig -> Lude.Maybe Lude.Bool) (\s a -> s {measureLatency = a} :: HealthCheckConfig)
{-# DEPRECATED hccMeasureLatency "Use generic-lens or generic-optics with 'measureLatency' instead." #-}

-- | Specify whether you want Amazon Route 53 to invert the status of a health check, for example, to consider a health check unhealthy when it otherwise would be considered healthy.
--
-- /Note:/ Consider using 'inverted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccInverted :: Lens.Lens' HealthCheckConfig (Lude.Maybe Lude.Bool)
hccInverted = Lens.lens (inverted :: HealthCheckConfig -> Lude.Maybe Lude.Bool) (\s a -> s {inverted = a} :: HealthCheckConfig)
{-# DEPRECATED hccInverted "Use generic-lens or generic-optics with 'inverted' instead." #-}

-- | Amazon Route 53 behavior depends on whether you specify a value for @IPAddress@ .
--
-- __If you specify a value for__ @IPAddress@ :
-- Amazon Route 53 sends health check requests to the specified IPv4 or IPv6 address and passes the value of @FullyQualifiedDomainName@ in the @Host@ header for all health checks except TCP health checks. This is typically the fully qualified DNS name of the endpoint on which you want Route 53 to perform health checks.
-- When Route 53 checks the health of an endpoint, here is how it constructs the @Host@ header:
--
--     * If you specify a value of @80@ for @Port@ and @HTTP@ or @HTTP_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the Host header.
--
--
--     * If you specify a value of @443@ for @Port@ and @HTTPS@ or @HTTPS_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.
--
--
--     * If you specify another value for @Port@ and any value except @TCP@ for @Type@ , Route 53 passes @FullyQualifiedDomainName:Port@ to the endpoint in the @Host@ header.
--
--
-- If you don't specify a value for @FullyQualifiedDomainName@ , Route 53 substitutes the value of @IPAddress@ in the @Host@ header in each of the preceding cases.
-- __If you don't specify a value for @IPAddress@ __ :
-- Route 53 sends a DNS request to the domain that you specify for @FullyQualifiedDomainName@ at the interval that you specify for @RequestInterval@ . Using an IPv4 address that DNS returns, Route 53 then checks the health of the endpoint.
-- If you want to check the health of weighted, latency, or failover resource record sets and you choose to specify the endpoint only by @FullyQualifiedDomainName@ , we recommend that you create a separate health check for each endpoint. For example, create a health check for each HTTP server that is serving content for www.example.com. For the value of @FullyQualifiedDomainName@ , specify the domain name of the server (such as us-east-2-www.example.com), not the name of the resource record sets (www.example.com).
-- /Important:/ In this configuration, if you create a health check for which the value of @FullyQualifiedDomainName@ matches the name of the resource record sets and you then associate the health check with those resource record sets, health check results will be unpredictable.
-- In addition, if the value that you specify for @Type@ is @HTTP@ , @HTTPS@ , @HTTP_STR_MATCH@ , or @HTTPS_STR_MATCH@ , Route 53 passes the value of @FullyQualifiedDomainName@ in the @Host@ header, as it does when you specify a value for @IPAddress@ . If the value of @Type@ is @TCP@ , Route 53 doesn't pass a @Host@ header.
--
-- /Note:/ Consider using 'fullyQualifiedDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccFullyQualifiedDomainName :: Lens.Lens' HealthCheckConfig (Lude.Maybe Lude.Text)
hccFullyQualifiedDomainName = Lens.lens (fullyQualifiedDomainName :: HealthCheckConfig -> Lude.Maybe Lude.Text) (\s a -> s {fullyQualifiedDomainName = a} :: HealthCheckConfig)
{-# DEPRECATED hccFullyQualifiedDomainName "Use generic-lens or generic-optics with 'fullyQualifiedDomainName' instead." #-}

-- | (CALCULATED Health Checks Only) A complex type that contains one @ChildHealthCheck@ element for each health check that you want to associate with a @CALCULATED@ health check.
--
-- /Note:/ Consider using 'childHealthChecks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccChildHealthChecks :: Lens.Lens' HealthCheckConfig (Lude.Maybe [Lude.Text])
hccChildHealthChecks = Lens.lens (childHealthChecks :: HealthCheckConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {childHealthChecks = a} :: HealthCheckConfig)
{-# DEPRECATED hccChildHealthChecks "Use generic-lens or generic-optics with 'childHealthChecks' instead." #-}

-- | The number of seconds between the time that Amazon Route 53 gets a response from your endpoint and the time that it sends the next health check request. Each Route 53 health checker makes requests at this interval.
--
-- /Important:/ You can't change the value of @RequestInterval@ after you create a health check.
-- If you don't specify a value for @RequestInterval@ , the default value is @30@ seconds.
--
-- /Note:/ Consider using 'requestInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccRequestInterval :: Lens.Lens' HealthCheckConfig (Lude.Maybe Lude.Natural)
hccRequestInterval = Lens.lens (requestInterval :: HealthCheckConfig -> Lude.Maybe Lude.Natural) (\s a -> s {requestInterval = a} :: HealthCheckConfig)
{-# DEPRECATED hccRequestInterval "Use generic-lens or generic-optics with 'requestInterval' instead." #-}

-- | The port on the endpoint that you want Amazon Route 53 to perform health checks on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccPort :: Lens.Lens' HealthCheckConfig (Lude.Maybe Lude.Natural)
hccPort = Lens.lens (port :: HealthCheckConfig -> Lude.Maybe Lude.Natural) (\s a -> s {port = a} :: HealthCheckConfig)
{-# DEPRECATED hccPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromXML HealthCheckConfig where
  parseXML x =
    HealthCheckConfig'
      Lude.<$> (x Lude..@? "FailureThreshold")
      Lude.<*> (x Lude..@? "IPAddress")
      Lude.<*> (x Lude..@? "EnableSNI")
      Lude.<*> (x Lude..@? "Disabled")
      Lude.<*> (x Lude..@? "SearchString")
      Lude.<*> (x Lude..@? "HealthThreshold")
      Lude.<*> ( x Lude..@? "Regions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLNonEmpty "Region")
               )
      Lude.<*> (x Lude..@? "ResourcePath")
      Lude.<*> (x Lude..@? "InsufficientDataHealthStatus")
      Lude.<*> (x Lude..@ "Type")
      Lude.<*> (x Lude..@? "AlarmIdentifier")
      Lude.<*> (x Lude..@? "MeasureLatency")
      Lude.<*> (x Lude..@? "Inverted")
      Lude.<*> (x Lude..@? "FullyQualifiedDomainName")
      Lude.<*> ( x Lude..@? "ChildHealthChecks" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ChildHealthCheck")
               )
      Lude.<*> (x Lude..@? "RequestInterval")
      Lude.<*> (x Lude..@? "Port")

instance Lude.ToXML HealthCheckConfig where
  toXML HealthCheckConfig' {..} =
    Lude.mconcat
      [ "FailureThreshold" Lude.@= failureThreshold,
        "IPAddress" Lude.@= ipAddress,
        "EnableSNI" Lude.@= enableSNI,
        "Disabled" Lude.@= disabled,
        "SearchString" Lude.@= searchString,
        "HealthThreshold" Lude.@= healthThreshold,
        "Regions"
          Lude.@= Lude.toXML (Lude.toXMLList "Region" Lude.<$> regions),
        "ResourcePath" Lude.@= resourcePath,
        "InsufficientDataHealthStatus"
          Lude.@= insufficientDataHealthStatus,
        "Type" Lude.@= type',
        "AlarmIdentifier" Lude.@= alarmIdentifier,
        "MeasureLatency" Lude.@= measureLatency,
        "Inverted" Lude.@= inverted,
        "FullyQualifiedDomainName" Lude.@= fullyQualifiedDomainName,
        "ChildHealthChecks"
          Lude.@= Lude.toXML
            (Lude.toXMLList "ChildHealthCheck" Lude.<$> childHealthChecks),
        "RequestInterval" Lude.@= requestInterval,
        "Port" Lude.@= port
      ]
