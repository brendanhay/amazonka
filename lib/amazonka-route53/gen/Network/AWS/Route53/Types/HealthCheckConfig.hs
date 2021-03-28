{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HealthCheckConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.HealthCheckConfig
  ( HealthCheckConfig (..)
  -- * Smart constructor
  , mkHealthCheckConfig
  -- * Lenses
  , hccType
  , hccAlarmIdentifier
  , hccChildHealthChecks
  , hccDisabled
  , hccEnableSNI
  , hccFailureThreshold
  , hccFullyQualifiedDomainName
  , hccHealthThreshold
  , hccIPAddress
  , hccInsufficientDataHealthStatus
  , hccInverted
  , hccMeasureLatency
  , hccPort
  , hccRegions
  , hccRequestInterval
  , hccResourcePath
  , hccSearchString
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.AlarmIdentifier as Types
import qualified Network.AWS.Route53.Types.FullyQualifiedDomainName as Types
import qualified Network.AWS.Route53.Types.HealthCheckId as Types
import qualified Network.AWS.Route53.Types.HealthCheckRegion as Types
import qualified Network.AWS.Route53.Types.HealthCheckType as Types
import qualified Network.AWS.Route53.Types.IPAddress as Types
import qualified Network.AWS.Route53.Types.InsufficientDataHealthStatus as Types
import qualified Network.AWS.Route53.Types.ResourcePath as Types
import qualified Network.AWS.Route53.Types.SearchString as Types

-- | A complex type that contains information about the health check.
--
-- /See:/ 'mkHealthCheckConfig' smart constructor.
data HealthCheckConfig = HealthCheckConfig'
  { type' :: Types.HealthCheckType
    -- ^ The type of health check that you want to create, which indicates how Amazon Route 53 determines whether an endpoint is healthy.
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
  , alarmIdentifier :: Core.Maybe Types.AlarmIdentifier
    -- ^ A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether the specified health check is healthy.
  , childHealthChecks :: Core.Maybe [Types.HealthCheckId]
    -- ^ (CALCULATED Health Checks Only) A complex type that contains one @ChildHealthCheck@ element for each health check that you want to associate with a @CALCULATED@ health check.
  , disabled :: Core.Maybe Core.Bool
    -- ^ Stops Route 53 from performing health checks. When you disable a health check, here's what happens:
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
  , enableSNI :: Core.Maybe Core.Bool
    -- ^ Specify whether you want Amazon Route 53 to send the value of @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message during TLS negotiation. This allows the endpoint to respond to @HTTPS@ health check requests with the applicable SSL/TLS certificate.
--
-- Some endpoints require that @HTTPS@ requests include the host name in the @client_hello@ message. If you don't enable SNI, the status of the health check will be @SSL alert handshake_failure@ . A health check can also have that status for other reasons. If SNI is enabled and you're still getting the error, check the SSL/TLS configuration on your endpoint and confirm that your certificate is valid.
-- The SSL/TLS certificate on your endpoint includes a domain name in the @Common Name@ field and possibly several more in the @Subject Alternative Names@ field. One of the domain names in the certificate should match the value that you specify for @FullyQualifiedDomainName@ . If the endpoint responds to the @client_hello@ message with a certificate that does not include the domain name that you specified in @FullyQualifiedDomainName@ , a health checker will retry the handshake. In the second attempt, the health checker will omit @FullyQualifiedDomainName@ from the @client_hello@ message.
  , failureThreshold :: Core.Maybe Core.Natural
    -- ^ The number of consecutive health checks that an endpoint must pass or fail for Amazon Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
--
-- If you don't specify a value for @FailureThreshold@ , the default value is three health checks.
  , fullyQualifiedDomainName :: Core.Maybe Types.FullyQualifiedDomainName
    -- ^ Amazon Route 53 behavior depends on whether you specify a value for @IPAddress@ .
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
  , healthThreshold :: Core.Maybe Core.Natural
    -- ^ The number of child health checks that are associated with a @CALCULATED@ health check that Amazon Route 53 must consider healthy for the @CALCULATED@ health check to be considered healthy. To specify the child health checks that you want to associate with a @CALCULATED@ health check, use the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-ChildHealthChecks ChildHealthChecks> element.
--
-- Note the following:
--
--     * If you specify a number greater than the number of child health checks, Route 53 always considers this health check to be unhealthy.
--
--
--     * If you specify @0@ , Route 53 always considers this health check to be healthy.
--
--
  , iPAddress :: Core.Maybe Types.IPAddress
    -- ^ The IPv4 or IPv6 IP address of the endpoint that you want Amazon Route 53 to perform health checks on. If you don't specify a value for @IPAddress@ , Route 53 sends a DNS request to resolve the domain name that you specify in @FullyQualifiedDomainName@ at the interval that you specify in @RequestInterval@ . Using an IP address returned by DNS, Route 53 then checks the health of the endpoint.
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
  , insufficientDataHealthStatus :: Core.Maybe Types.InsufficientDataHealthStatus
    -- ^ When CloudWatch has insufficient data about the metric to determine the alarm state, the status that you want Amazon Route 53 to assign to the health check:
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
  , inverted :: Core.Maybe Core.Bool
    -- ^ Specify whether you want Amazon Route 53 to invert the status of a health check, for example, to consider a health check unhealthy when it otherwise would be considered healthy.
  , measureLatency :: Core.Maybe Core.Bool
    -- ^ Specify whether you want Amazon Route 53 to measure the latency between health checkers in multiple AWS regions and your endpoint, and to display CloudWatch latency graphs on the __Health Checks__ page in the Route 53 console.
--
-- /Important:/ You can't change the value of @MeasureLatency@ after you create a health check.
  , port :: Core.Maybe Core.Natural
    -- ^ The port on the endpoint that you want Amazon Route 53 to perform health checks on.
  , regions :: Core.Maybe (Core.NonEmpty Types.HealthCheckRegion)
    -- ^ A complex type that contains one @Region@ element for each region from which you want Amazon Route 53 health checkers to check the specified endpoint.
--
-- If you don't specify any regions, Route 53 health checkers automatically performs checks from all of the regions that are listed under __Valid Values__ .
-- If you update a health check to remove a region that has been performing health checks, Route 53 will briefly continue to perform checks from that region to ensure that some health checkers are always checking the endpoint (for example, if you replace three regions with four different regions). 
  , requestInterval :: Core.Maybe Core.Natural
    -- ^ The number of seconds between the time that Amazon Route 53 gets a response from your endpoint and the time that it sends the next health check request. Each Route 53 health checker makes requests at this interval.
--
-- /Important:/ You can't change the value of @RequestInterval@ after you create a health check.
-- If you don't specify a value for @RequestInterval@ , the default value is @30@ seconds.
  , resourcePath :: Core.Maybe Types.ResourcePath
    -- ^ The path, if any, that you want Amazon Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, for example, the file /docs/route53-health-check.html. You can also include query string parameters, for example, @/welcome.html?language=jp&login=y@ . 
  , searchString :: Core.Maybe Types.SearchString
    -- ^ If the value of Type is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@ , the string that you want Amazon Route 53 to search for in the response body from the specified resource. If the string appears in the response body, Route 53 considers the resource healthy.
--
-- Route 53 considers case when searching for @SearchString@ in the response body. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HealthCheckConfig' value with any optional fields omitted.
mkHealthCheckConfig
    :: Types.HealthCheckType -- ^ 'type\''
    -> HealthCheckConfig
mkHealthCheckConfig type'
  = HealthCheckConfig'{type', alarmIdentifier = Core.Nothing,
                       childHealthChecks = Core.Nothing, disabled = Core.Nothing,
                       enableSNI = Core.Nothing, failureThreshold = Core.Nothing,
                       fullyQualifiedDomainName = Core.Nothing,
                       healthThreshold = Core.Nothing, iPAddress = Core.Nothing,
                       insufficientDataHealthStatus = Core.Nothing,
                       inverted = Core.Nothing, measureLatency = Core.Nothing,
                       port = Core.Nothing, regions = Core.Nothing,
                       requestInterval = Core.Nothing, resourcePath = Core.Nothing,
                       searchString = Core.Nothing}

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
hccType :: Lens.Lens' HealthCheckConfig Types.HealthCheckType
hccType = Lens.field @"type'"
{-# INLINEABLE hccType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether the specified health check is healthy.
--
-- /Note:/ Consider using 'alarmIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccAlarmIdentifier :: Lens.Lens' HealthCheckConfig (Core.Maybe Types.AlarmIdentifier)
hccAlarmIdentifier = Lens.field @"alarmIdentifier"
{-# INLINEABLE hccAlarmIdentifier #-}
{-# DEPRECATED alarmIdentifier "Use generic-lens or generic-optics with 'alarmIdentifier' instead"  #-}

-- | (CALCULATED Health Checks Only) A complex type that contains one @ChildHealthCheck@ element for each health check that you want to associate with a @CALCULATED@ health check.
--
-- /Note:/ Consider using 'childHealthChecks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccChildHealthChecks :: Lens.Lens' HealthCheckConfig (Core.Maybe [Types.HealthCheckId])
hccChildHealthChecks = Lens.field @"childHealthChecks"
{-# INLINEABLE hccChildHealthChecks #-}
{-# DEPRECATED childHealthChecks "Use generic-lens or generic-optics with 'childHealthChecks' instead"  #-}

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
hccDisabled :: Lens.Lens' HealthCheckConfig (Core.Maybe Core.Bool)
hccDisabled = Lens.field @"disabled"
{-# INLINEABLE hccDisabled #-}
{-# DEPRECATED disabled "Use generic-lens or generic-optics with 'disabled' instead"  #-}

-- | Specify whether you want Amazon Route 53 to send the value of @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message during TLS negotiation. This allows the endpoint to respond to @HTTPS@ health check requests with the applicable SSL/TLS certificate.
--
-- Some endpoints require that @HTTPS@ requests include the host name in the @client_hello@ message. If you don't enable SNI, the status of the health check will be @SSL alert handshake_failure@ . A health check can also have that status for other reasons. If SNI is enabled and you're still getting the error, check the SSL/TLS configuration on your endpoint and confirm that your certificate is valid.
-- The SSL/TLS certificate on your endpoint includes a domain name in the @Common Name@ field and possibly several more in the @Subject Alternative Names@ field. One of the domain names in the certificate should match the value that you specify for @FullyQualifiedDomainName@ . If the endpoint responds to the @client_hello@ message with a certificate that does not include the domain name that you specified in @FullyQualifiedDomainName@ , a health checker will retry the handshake. In the second attempt, the health checker will omit @FullyQualifiedDomainName@ from the @client_hello@ message.
--
-- /Note:/ Consider using 'enableSNI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccEnableSNI :: Lens.Lens' HealthCheckConfig (Core.Maybe Core.Bool)
hccEnableSNI = Lens.field @"enableSNI"
{-# INLINEABLE hccEnableSNI #-}
{-# DEPRECATED enableSNI "Use generic-lens or generic-optics with 'enableSNI' instead"  #-}

-- | The number of consecutive health checks that an endpoint must pass or fail for Amazon Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
--
-- If you don't specify a value for @FailureThreshold@ , the default value is three health checks.
--
-- /Note:/ Consider using 'failureThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccFailureThreshold :: Lens.Lens' HealthCheckConfig (Core.Maybe Core.Natural)
hccFailureThreshold = Lens.field @"failureThreshold"
{-# INLINEABLE hccFailureThreshold #-}
{-# DEPRECATED failureThreshold "Use generic-lens or generic-optics with 'failureThreshold' instead"  #-}

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
hccFullyQualifiedDomainName :: Lens.Lens' HealthCheckConfig (Core.Maybe Types.FullyQualifiedDomainName)
hccFullyQualifiedDomainName = Lens.field @"fullyQualifiedDomainName"
{-# INLINEABLE hccFullyQualifiedDomainName #-}
{-# DEPRECATED fullyQualifiedDomainName "Use generic-lens or generic-optics with 'fullyQualifiedDomainName' instead"  #-}

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
hccHealthThreshold :: Lens.Lens' HealthCheckConfig (Core.Maybe Core.Natural)
hccHealthThreshold = Lens.field @"healthThreshold"
{-# INLINEABLE hccHealthThreshold #-}
{-# DEPRECATED healthThreshold "Use generic-lens or generic-optics with 'healthThreshold' instead"  #-}

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
-- /Note:/ Consider using 'iPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccIPAddress :: Lens.Lens' HealthCheckConfig (Core.Maybe Types.IPAddress)
hccIPAddress = Lens.field @"iPAddress"
{-# INLINEABLE hccIPAddress #-}
{-# DEPRECATED iPAddress "Use generic-lens or generic-optics with 'iPAddress' instead"  #-}

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
hccInsufficientDataHealthStatus :: Lens.Lens' HealthCheckConfig (Core.Maybe Types.InsufficientDataHealthStatus)
hccInsufficientDataHealthStatus = Lens.field @"insufficientDataHealthStatus"
{-# INLINEABLE hccInsufficientDataHealthStatus #-}
{-# DEPRECATED insufficientDataHealthStatus "Use generic-lens or generic-optics with 'insufficientDataHealthStatus' instead"  #-}

-- | Specify whether you want Amazon Route 53 to invert the status of a health check, for example, to consider a health check unhealthy when it otherwise would be considered healthy.
--
-- /Note:/ Consider using 'inverted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccInverted :: Lens.Lens' HealthCheckConfig (Core.Maybe Core.Bool)
hccInverted = Lens.field @"inverted"
{-# INLINEABLE hccInverted #-}
{-# DEPRECATED inverted "Use generic-lens or generic-optics with 'inverted' instead"  #-}

-- | Specify whether you want Amazon Route 53 to measure the latency between health checkers in multiple AWS regions and your endpoint, and to display CloudWatch latency graphs on the __Health Checks__ page in the Route 53 console.
--
-- /Important:/ You can't change the value of @MeasureLatency@ after you create a health check.
--
-- /Note:/ Consider using 'measureLatency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccMeasureLatency :: Lens.Lens' HealthCheckConfig (Core.Maybe Core.Bool)
hccMeasureLatency = Lens.field @"measureLatency"
{-# INLINEABLE hccMeasureLatency #-}
{-# DEPRECATED measureLatency "Use generic-lens or generic-optics with 'measureLatency' instead"  #-}

-- | The port on the endpoint that you want Amazon Route 53 to perform health checks on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccPort :: Lens.Lens' HealthCheckConfig (Core.Maybe Core.Natural)
hccPort = Lens.field @"port"
{-# INLINEABLE hccPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | A complex type that contains one @Region@ element for each region from which you want Amazon Route 53 health checkers to check the specified endpoint.
--
-- If you don't specify any regions, Route 53 health checkers automatically performs checks from all of the regions that are listed under __Valid Values__ .
-- If you update a health check to remove a region that has been performing health checks, Route 53 will briefly continue to perform checks from that region to ensure that some health checkers are always checking the endpoint (for example, if you replace three regions with four different regions). 
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccRegions :: Lens.Lens' HealthCheckConfig (Core.Maybe (Core.NonEmpty Types.HealthCheckRegion))
hccRegions = Lens.field @"regions"
{-# INLINEABLE hccRegions #-}
{-# DEPRECATED regions "Use generic-lens or generic-optics with 'regions' instead"  #-}

-- | The number of seconds between the time that Amazon Route 53 gets a response from your endpoint and the time that it sends the next health check request. Each Route 53 health checker makes requests at this interval.
--
-- /Important:/ You can't change the value of @RequestInterval@ after you create a health check.
-- If you don't specify a value for @RequestInterval@ , the default value is @30@ seconds.
--
-- /Note:/ Consider using 'requestInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccRequestInterval :: Lens.Lens' HealthCheckConfig (Core.Maybe Core.Natural)
hccRequestInterval = Lens.field @"requestInterval"
{-# INLINEABLE hccRequestInterval #-}
{-# DEPRECATED requestInterval "Use generic-lens or generic-optics with 'requestInterval' instead"  #-}

-- | The path, if any, that you want Amazon Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, for example, the file /docs/route53-health-check.html. You can also include query string parameters, for example, @/welcome.html?language=jp&login=y@ . 
--
-- /Note:/ Consider using 'resourcePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccResourcePath :: Lens.Lens' HealthCheckConfig (Core.Maybe Types.ResourcePath)
hccResourcePath = Lens.field @"resourcePath"
{-# INLINEABLE hccResourcePath #-}
{-# DEPRECATED resourcePath "Use generic-lens or generic-optics with 'resourcePath' instead"  #-}

-- | If the value of Type is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@ , the string that you want Amazon Route 53 to search for in the response body from the specified resource. If the string appears in the response body, Route 53 considers the resource healthy.
--
-- Route 53 considers case when searching for @SearchString@ in the response body. 
--
-- /Note:/ Consider using 'searchString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hccSearchString :: Lens.Lens' HealthCheckConfig (Core.Maybe Types.SearchString)
hccSearchString = Lens.field @"searchString"
{-# INLINEABLE hccSearchString #-}
{-# DEPRECATED searchString "Use generic-lens or generic-optics with 'searchString' instead"  #-}

instance Core.ToXML HealthCheckConfig where
        toXML HealthCheckConfig{..}
          = Core.toXMLElement "Type" type' Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "AlarmIdentifier")
                alarmIdentifier
              Core.<>
              Core.toXMLElement "ChildHealthChecks"
                (Core.maybe Core.mempty (Core.toXMLList "ChildHealthCheck")
                   childHealthChecks)
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Disabled") disabled
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "EnableSNI") enableSNI
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "FailureThreshold")
                failureThreshold
              Core.<>
              Core.maybe Core.mempty
                (Core.toXMLElement "FullyQualifiedDomainName")
                fullyQualifiedDomainName
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "HealthThreshold")
                healthThreshold
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "IPAddress") iPAddress
              Core.<>
              Core.maybe Core.mempty
                (Core.toXMLElement "InsufficientDataHealthStatus")
                insufficientDataHealthStatus
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Inverted") inverted
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "MeasureLatency")
                measureLatency
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "Port") port
              Core.<>
              Core.toXMLElement "Regions"
                (Core.maybe Core.mempty (Core.toXMLList "Region") regions)
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "RequestInterval")
                requestInterval
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "ResourcePath")
                resourcePath
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "SearchString")
                searchString

instance Core.FromXML HealthCheckConfig where
        parseXML x
          = HealthCheckConfig' Core.<$>
              (x Core..@ "Type") Core.<*> x Core..@? "AlarmIdentifier" Core.<*>
                x Core..@? "ChildHealthChecks" Core..<@>
                  Core.parseXMLList "ChildHealthCheck"
                Core.<*> x Core..@? "Disabled"
                Core.<*> x Core..@? "EnableSNI"
                Core.<*> x Core..@? "FailureThreshold"
                Core.<*> x Core..@? "FullyQualifiedDomainName"
                Core.<*> x Core..@? "HealthThreshold"
                Core.<*> x Core..@? "IPAddress"
                Core.<*> x Core..@? "InsufficientDataHealthStatus"
                Core.<*> x Core..@? "Inverted"
                Core.<*> x Core..@? "MeasureLatency"
                Core.<*> x Core..@? "Port"
                Core.<*>
                x Core..@? "Regions" Core..<@> Core.parseXMLNonEmpty "Region"
                Core.<*> x Core..@? "RequestInterval"
                Core.<*> x Core..@? "ResourcePath"
                Core.<*> x Core..@? "SearchString"
