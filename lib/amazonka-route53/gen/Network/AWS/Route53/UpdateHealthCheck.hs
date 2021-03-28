{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateHealthCheck (..)
    , mkUpdateHealthCheck
    -- ** Request lenses
    , uhcHealthCheckId
    , uhcAlarmIdentifier
    , uhcChildHealthChecks
    , uhcDisabled
    , uhcEnableSNI
    , uhcFailureThreshold
    , uhcFullyQualifiedDomainName
    , uhcHealthCheckVersion
    , uhcHealthThreshold
    , uhcIPAddress
    , uhcInsufficientDataHealthStatus
    , uhcInverted
    , uhcPort
    , uhcRegions
    , uhcResetElements
    , uhcResourcePath
    , uhcSearchString

    -- * Destructuring the response
    , UpdateHealthCheckResponse (..)
    , mkUpdateHealthCheckResponse
    -- ** Response lenses
    , uhcrrsHealthCheck
    , uhcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about a request to update a health check.
--
-- /See:/ 'mkUpdateHealthCheck' smart constructor.
data UpdateHealthCheck = UpdateHealthCheck'
  { healthCheckId :: Types.HealthCheckId
    -- ^ The ID for the health check for which you want detailed information. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
  , alarmIdentifier :: Core.Maybe Types.AlarmIdentifier
    -- ^ A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether the specified health check is healthy.
  , childHealthChecks :: Core.Maybe [Types.HealthCheckId]
    -- ^ A complex type that contains one @ChildHealthCheck@ element for each health check that you want to associate with a @CALCULATED@ health check.
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
    -- ^ Specify whether you want Amazon Route 53 to send the value of @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message during @TLS@ negotiation. This allows the endpoint to respond to @HTTPS@ health check requests with the applicable SSL/TLS certificate.
--
-- Some endpoints require that HTTPS requests include the host name in the @client_hello@ message. If you don't enable SNI, the status of the health check will be SSL alert @handshake_failure@ . A health check can also have that status for other reasons. If SNI is enabled and you're still getting the error, check the SSL/TLS configuration on your endpoint and confirm that your certificate is valid.
-- The SSL/TLS certificate on your endpoint includes a domain name in the @Common Name@ field and possibly several more in the @Subject Alternative Names@ field. One of the domain names in the certificate should match the value that you specify for @FullyQualifiedDomainName@ . If the endpoint responds to the @client_hello@ message with a certificate that does not include the domain name that you specified in @FullyQualifiedDomainName@ , a health checker will retry the handshake. In the second attempt, the health checker will omit @FullyQualifiedDomainName@ from the @client_hello@ message.
  , failureThreshold :: Core.Maybe Core.Natural
    -- ^ The number of consecutive health checks that an endpoint must pass or fail for Amazon Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
--
-- If you don't specify a value for @FailureThreshold@ , the default value is three health checks.
  , fullyQualifiedDomainName :: Core.Maybe Types.FullyQualifiedDomainName
    -- ^ Amazon Route 53 behavior depends on whether you specify a value for @IPAddress@ .
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
  , healthCheckVersion :: Core.Maybe Core.Natural
    -- ^ A sequential counter that Amazon Route 53 sets to @1@ when you create a health check and increments by 1 each time you update settings for the health check.
--
-- We recommend that you use @GetHealthCheck@ or @ListHealthChecks@ to get the current value of @HealthCheckVersion@ for the health check that you want to update, and that you include that value in your @UpdateHealthCheck@ request. This prevents Route 53 from overwriting an intervening update:
--
--     * If the value in the @UpdateHealthCheck@ request matches the value of @HealthCheckVersion@ in the health check, Route 53 updates the health check with the new settings.
--
--
--     * If the value of @HealthCheckVersion@ in the health check is greater, the health check was changed after you got the version number. Route 53 does not update the health check, and it returns a @HealthCheckVersionMismatch@ error.
--
--
  , healthThreshold :: Core.Maybe Core.Natural
    -- ^ The number of child health checks that are associated with a @CALCULATED@ health that Amazon Route 53 must consider healthy for the @CALCULATED@ health check to be considered healthy. To specify the child health checks that you want to associate with a @CALCULATED@ health check, use the @ChildHealthChecks@ and @ChildHealthCheck@ elements.
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
    -- ^ The IPv4 or IPv6 IP address for the endpoint that you want Amazon Route 53 to perform health checks on. If you don't specify a value for @IPAddress@ , Route 53 sends a DNS request to resolve the domain name that you specify in @FullyQualifiedDomainName@ at the interval that you specify in @RequestInterval@ . Using an IP address that is returned by DNS, Route 53 then checks the health of the endpoint.
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
--     * @LastKnownStatus@ : Route 53 uses the status of the health check from the last time CloudWatch had sufficient data to determine the alarm state. For new health checks that have no last known status, the default status for the health check is healthy.
--
--
  , inverted :: Core.Maybe Core.Bool
    -- ^ Specify whether you want Amazon Route 53 to invert the status of a health check, for example, to consider a health check unhealthy when it otherwise would be considered healthy.
  , port :: Core.Maybe Core.Natural
    -- ^ The port on the endpoint that you want Amazon Route 53 to perform health checks on.
  , regions :: Core.Maybe (Core.NonEmpty Types.HealthCheckRegion)
    -- ^ A complex type that contains one @Region@ element for each region that you want Amazon Route 53 health checkers to check the specified endpoint from.
  , resetElements :: Core.Maybe [Types.ResettableElementName]
    -- ^ A complex type that contains one @ResettableElementName@ element for each element that you want to reset to the default value. Valid values for @ResettableElementName@ include the following:
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
  , resourcePath :: Core.Maybe Types.ResourcePath
    -- ^ The path that you want Amazon Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, for example the file /docs/route53-health-check.html. You can also include query string parameters, for example, @/welcome.html?language=jp&login=y@ . 
--
-- Specify this value only if you want to change it.
  , searchString :: Core.Maybe Types.SearchString
    -- ^ If the value of @Type@ is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@ , the string that you want Amazon Route 53 to search for in the response body from the specified resource. If the string appears in the response body, Route 53 considers the resource healthy. (You can't change the value of @Type@ when you update a health check.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateHealthCheck' value with any optional fields omitted.
mkUpdateHealthCheck
    :: Types.HealthCheckId -- ^ 'healthCheckId'
    -> UpdateHealthCheck
mkUpdateHealthCheck healthCheckId
  = UpdateHealthCheck'{healthCheckId, alarmIdentifier = Core.Nothing,
                       childHealthChecks = Core.Nothing, disabled = Core.Nothing,
                       enableSNI = Core.Nothing, failureThreshold = Core.Nothing,
                       fullyQualifiedDomainName = Core.Nothing,
                       healthCheckVersion = Core.Nothing, healthThreshold = Core.Nothing,
                       iPAddress = Core.Nothing,
                       insufficientDataHealthStatus = Core.Nothing,
                       inverted = Core.Nothing, port = Core.Nothing,
                       regions = Core.Nothing, resetElements = Core.Nothing,
                       resourcePath = Core.Nothing, searchString = Core.Nothing}

-- | The ID for the health check for which you want detailed information. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
--
-- /Note:/ Consider using 'healthCheckId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcHealthCheckId :: Lens.Lens' UpdateHealthCheck Types.HealthCheckId
uhcHealthCheckId = Lens.field @"healthCheckId"
{-# INLINEABLE uhcHealthCheckId #-}
{-# DEPRECATED healthCheckId "Use generic-lens or generic-optics with 'healthCheckId' instead"  #-}

-- | A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether the specified health check is healthy.
--
-- /Note:/ Consider using 'alarmIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcAlarmIdentifier :: Lens.Lens' UpdateHealthCheck (Core.Maybe Types.AlarmIdentifier)
uhcAlarmIdentifier = Lens.field @"alarmIdentifier"
{-# INLINEABLE uhcAlarmIdentifier #-}
{-# DEPRECATED alarmIdentifier "Use generic-lens or generic-optics with 'alarmIdentifier' instead"  #-}

-- | A complex type that contains one @ChildHealthCheck@ element for each health check that you want to associate with a @CALCULATED@ health check.
--
-- /Note:/ Consider using 'childHealthChecks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcChildHealthChecks :: Lens.Lens' UpdateHealthCheck (Core.Maybe [Types.HealthCheckId])
uhcChildHealthChecks = Lens.field @"childHealthChecks"
{-# INLINEABLE uhcChildHealthChecks #-}
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
uhcDisabled :: Lens.Lens' UpdateHealthCheck (Core.Maybe Core.Bool)
uhcDisabled = Lens.field @"disabled"
{-# INLINEABLE uhcDisabled #-}
{-# DEPRECATED disabled "Use generic-lens or generic-optics with 'disabled' instead"  #-}

-- | Specify whether you want Amazon Route 53 to send the value of @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message during @TLS@ negotiation. This allows the endpoint to respond to @HTTPS@ health check requests with the applicable SSL/TLS certificate.
--
-- Some endpoints require that HTTPS requests include the host name in the @client_hello@ message. If you don't enable SNI, the status of the health check will be SSL alert @handshake_failure@ . A health check can also have that status for other reasons. If SNI is enabled and you're still getting the error, check the SSL/TLS configuration on your endpoint and confirm that your certificate is valid.
-- The SSL/TLS certificate on your endpoint includes a domain name in the @Common Name@ field and possibly several more in the @Subject Alternative Names@ field. One of the domain names in the certificate should match the value that you specify for @FullyQualifiedDomainName@ . If the endpoint responds to the @client_hello@ message with a certificate that does not include the domain name that you specified in @FullyQualifiedDomainName@ , a health checker will retry the handshake. In the second attempt, the health checker will omit @FullyQualifiedDomainName@ from the @client_hello@ message.
--
-- /Note:/ Consider using 'enableSNI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcEnableSNI :: Lens.Lens' UpdateHealthCheck (Core.Maybe Core.Bool)
uhcEnableSNI = Lens.field @"enableSNI"
{-# INLINEABLE uhcEnableSNI #-}
{-# DEPRECATED enableSNI "Use generic-lens or generic-optics with 'enableSNI' instead"  #-}

-- | The number of consecutive health checks that an endpoint must pass or fail for Amazon Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
--
-- If you don't specify a value for @FailureThreshold@ , the default value is three health checks.
--
-- /Note:/ Consider using 'failureThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcFailureThreshold :: Lens.Lens' UpdateHealthCheck (Core.Maybe Core.Natural)
uhcFailureThreshold = Lens.field @"failureThreshold"
{-# INLINEABLE uhcFailureThreshold #-}
{-# DEPRECATED failureThreshold "Use generic-lens or generic-optics with 'failureThreshold' instead"  #-}

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
uhcFullyQualifiedDomainName :: Lens.Lens' UpdateHealthCheck (Core.Maybe Types.FullyQualifiedDomainName)
uhcFullyQualifiedDomainName = Lens.field @"fullyQualifiedDomainName"
{-# INLINEABLE uhcFullyQualifiedDomainName #-}
{-# DEPRECATED fullyQualifiedDomainName "Use generic-lens or generic-optics with 'fullyQualifiedDomainName' instead"  #-}

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
uhcHealthCheckVersion :: Lens.Lens' UpdateHealthCheck (Core.Maybe Core.Natural)
uhcHealthCheckVersion = Lens.field @"healthCheckVersion"
{-# INLINEABLE uhcHealthCheckVersion #-}
{-# DEPRECATED healthCheckVersion "Use generic-lens or generic-optics with 'healthCheckVersion' instead"  #-}

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
uhcHealthThreshold :: Lens.Lens' UpdateHealthCheck (Core.Maybe Core.Natural)
uhcHealthThreshold = Lens.field @"healthThreshold"
{-# INLINEABLE uhcHealthThreshold #-}
{-# DEPRECATED healthThreshold "Use generic-lens or generic-optics with 'healthThreshold' instead"  #-}

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
-- /Note:/ Consider using 'iPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcIPAddress :: Lens.Lens' UpdateHealthCheck (Core.Maybe Types.IPAddress)
uhcIPAddress = Lens.field @"iPAddress"
{-# INLINEABLE uhcIPAddress #-}
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
--     * @LastKnownStatus@ : Route 53 uses the status of the health check from the last time CloudWatch had sufficient data to determine the alarm state. For new health checks that have no last known status, the default status for the health check is healthy.
--
--
--
-- /Note:/ Consider using 'insufficientDataHealthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcInsufficientDataHealthStatus :: Lens.Lens' UpdateHealthCheck (Core.Maybe Types.InsufficientDataHealthStatus)
uhcInsufficientDataHealthStatus = Lens.field @"insufficientDataHealthStatus"
{-# INLINEABLE uhcInsufficientDataHealthStatus #-}
{-# DEPRECATED insufficientDataHealthStatus "Use generic-lens or generic-optics with 'insufficientDataHealthStatus' instead"  #-}

-- | Specify whether you want Amazon Route 53 to invert the status of a health check, for example, to consider a health check unhealthy when it otherwise would be considered healthy.
--
-- /Note:/ Consider using 'inverted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcInverted :: Lens.Lens' UpdateHealthCheck (Core.Maybe Core.Bool)
uhcInverted = Lens.field @"inverted"
{-# INLINEABLE uhcInverted #-}
{-# DEPRECATED inverted "Use generic-lens or generic-optics with 'inverted' instead"  #-}

-- | The port on the endpoint that you want Amazon Route 53 to perform health checks on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcPort :: Lens.Lens' UpdateHealthCheck (Core.Maybe Core.Natural)
uhcPort = Lens.field @"port"
{-# INLINEABLE uhcPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | A complex type that contains one @Region@ element for each region that you want Amazon Route 53 health checkers to check the specified endpoint from.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcRegions :: Lens.Lens' UpdateHealthCheck (Core.Maybe (Core.NonEmpty Types.HealthCheckRegion))
uhcRegions = Lens.field @"regions"
{-# INLINEABLE uhcRegions #-}
{-# DEPRECATED regions "Use generic-lens or generic-optics with 'regions' instead"  #-}

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
uhcResetElements :: Lens.Lens' UpdateHealthCheck (Core.Maybe [Types.ResettableElementName])
uhcResetElements = Lens.field @"resetElements"
{-# INLINEABLE uhcResetElements #-}
{-# DEPRECATED resetElements "Use generic-lens or generic-optics with 'resetElements' instead"  #-}

-- | The path that you want Amazon Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, for example the file /docs/route53-health-check.html. You can also include query string parameters, for example, @/welcome.html?language=jp&login=y@ . 
--
-- Specify this value only if you want to change it.
--
-- /Note:/ Consider using 'resourcePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcResourcePath :: Lens.Lens' UpdateHealthCheck (Core.Maybe Types.ResourcePath)
uhcResourcePath = Lens.field @"resourcePath"
{-# INLINEABLE uhcResourcePath #-}
{-# DEPRECATED resourcePath "Use generic-lens or generic-optics with 'resourcePath' instead"  #-}

-- | If the value of @Type@ is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@ , the string that you want Amazon Route 53 to search for in the response body from the specified resource. If the string appears in the response body, Route 53 considers the resource healthy. (You can't change the value of @Type@ when you update a health check.)
--
-- /Note:/ Consider using 'searchString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcSearchString :: Lens.Lens' UpdateHealthCheck (Core.Maybe Types.SearchString)
uhcSearchString = Lens.field @"searchString"
{-# INLINEABLE uhcSearchString #-}
{-# DEPRECATED searchString "Use generic-lens or generic-optics with 'searchString' instead"  #-}

instance Core.ToQuery UpdateHealthCheck where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateHealthCheck where
        toHeaders _ = Core.pure Core.mempty

instance Core.ToXML UpdateHealthCheck where
        toXML UpdateHealthCheck{..}
          = Core.maybe Core.mempty (Core.toXMLElement "AlarmIdentifier")
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
              Core.maybe Core.mempty (Core.toXMLElement "HealthCheckVersion")
                healthCheckVersion
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
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "Port") port
              Core.<>
              Core.toXMLElement "Regions"
                (Core.maybe Core.mempty (Core.toXMLList "Region") regions)
              Core.<>
              Core.toXMLElement "ResetElements"
                (Core.maybe Core.mempty (Core.toXMLList "ResettableElementName")
                   resetElements)
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "ResourcePath")
                resourcePath
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "SearchString")
                searchString
        toXMLDocument
          = Core.newXMLDocument
              "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateHealthCheckRequest"

instance Core.AWSRequest UpdateHealthCheck where
        type Rs UpdateHealthCheck = UpdateHealthCheckResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/2013-04-01/healthcheck/" Core.<> Core.toText healthCheckId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 UpdateHealthCheckResponse' Core.<$>
                   (x Core..@ "HealthCheck") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the response to the @UpdateHealthCheck@ request.
--
-- /See:/ 'mkUpdateHealthCheckResponse' smart constructor.
data UpdateHealthCheckResponse = UpdateHealthCheckResponse'
  { healthCheck :: Types.HealthCheck
    -- ^ A complex type that contains the response to an @UpdateHealthCheck@ request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateHealthCheckResponse' value with any optional fields omitted.
mkUpdateHealthCheckResponse
    :: Types.HealthCheck -- ^ 'healthCheck'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateHealthCheckResponse
mkUpdateHealthCheckResponse healthCheck responseStatus
  = UpdateHealthCheckResponse'{healthCheck, responseStatus}

-- | A complex type that contains the response to an @UpdateHealthCheck@ request.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcrrsHealthCheck :: Lens.Lens' UpdateHealthCheckResponse Types.HealthCheck
uhcrrsHealthCheck = Lens.field @"healthCheck"
{-# INLINEABLE uhcrrsHealthCheck #-}
{-# DEPRECATED healthCheck "Use generic-lens or generic-optics with 'healthCheck' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhcrrsResponseStatus :: Lens.Lens' UpdateHealthCheckResponse Core.Int
uhcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uhcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
