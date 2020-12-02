{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HealthCheckConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HealthCheckConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.AlarmIdentifier
import Network.AWS.Route53.Types.HealthCheckRegion
import Network.AWS.Route53.Types.HealthCheckType
import Network.AWS.Route53.Types.InsufficientDataHealthStatus

-- | A complex type that contains information about the health check.
--
--
--
-- /See:/ 'healthCheckConfig' smart constructor.
data HealthCheckConfig = HealthCheckConfig'
  { _hccFailureThreshold ::
      !(Maybe Nat),
    _hccIPAddress :: !(Maybe Text),
    _hccEnableSNI :: !(Maybe Bool),
    _hccDisabled :: !(Maybe Bool),
    _hccSearchString :: !(Maybe Text),
    _hccHealthThreshold :: !(Maybe Nat),
    _hccRegions :: !(Maybe (List1 HealthCheckRegion)),
    _hccResourcePath :: !(Maybe Text),
    _hccInsufficientDataHealthStatus ::
      !(Maybe InsufficientDataHealthStatus),
    _hccAlarmIdentifier :: !(Maybe AlarmIdentifier),
    _hccMeasureLatency :: !(Maybe Bool),
    _hccInverted :: !(Maybe Bool),
    _hccFullyQualifiedDomainName :: !(Maybe Text),
    _hccChildHealthChecks :: !(Maybe [Text]),
    _hccRequestInterval :: !(Maybe Nat),
    _hccPort :: !(Maybe Nat),
    _hccType :: !HealthCheckType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HealthCheckConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hccFailureThreshold' - The number of consecutive health checks that an endpoint must pass or fail for Amazon Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ . If you don't specify a value for @FailureThreshold@ , the default value is three health checks.
--
-- * 'hccIPAddress' - The IPv4 or IPv6 IP address of the endpoint that you want Amazon Route 53 to perform health checks on. If you don't specify a value for @IPAddress@ , Route 53 sends a DNS request to resolve the domain name that you specify in @FullyQualifiedDomainName@ at the interval that you specify in @RequestInterval@ . Using an IP address returned by DNS, Route 53 then checks the health of the endpoint. Use one of the following formats for the value of @IPAddress@ :      * __IPv4 address__ : four values between 0 and 255, separated by periods (.), for example, @192.0.2.44@ .     * __IPv6 address__ : eight groups of four hexadecimal values, separated by colons (:), for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ . You can also shorten IPv6 addresses as described in RFC 5952, for example, @2001:db8:85a3::abcd:1:2345@ . If the endpoint is an EC2 instance, we recommend that you create an Elastic IP address, associate it with your EC2 instance, and specify the Elastic IP address for @IPAddress@ . This ensures that the IP address of your instance will never change. For more information, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-FullyQualifiedDomainName FullyQualifiedDomainName> .  Constraints: Route 53 can't check the health of endpoints for which the IP address is in local, private, non-routable, or multicast ranges. For more information about IP addresses for which you can't create health checks, see the following documents:     * <https://tools.ietf.org/html/rfc5735 RFC 5735, Special Use IPv4 Addresses>      * <https://tools.ietf.org/html/rfc6598 RFC 6598, IANA-Reserved IPv4 Prefix for Shared Address Space>      * <https://tools.ietf.org/html/rfc5156 RFC 5156, Special-Use IPv6 Addresses>  When the value of @Type@ is @CALCULATED@ or @CLOUDWATCH_METRIC@ , omit @IPAddress@ .
--
-- * 'hccEnableSNI' - Specify whether you want Amazon Route 53 to send the value of @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message during TLS negotiation. This allows the endpoint to respond to @HTTPS@ health check requests with the applicable SSL/TLS certificate. Some endpoints require that @HTTPS@ requests include the host name in the @client_hello@ message. If you don't enable SNI, the status of the health check will be @SSL alert handshake_failure@ . A health check can also have that status for other reasons. If SNI is enabled and you're still getting the error, check the SSL/TLS configuration on your endpoint and confirm that your certificate is valid. The SSL/TLS certificate on your endpoint includes a domain name in the @Common Name@ field and possibly several more in the @Subject Alternative Names@ field. One of the domain names in the certificate should match the value that you specify for @FullyQualifiedDomainName@ . If the endpoint responds to the @client_hello@ message with a certificate that does not include the domain name that you specified in @FullyQualifiedDomainName@ , a health checker will retry the handshake. In the second attempt, the health checker will omit @FullyQualifiedDomainName@ from the @client_hello@ message.
--
-- * 'hccDisabled' - Stops Route 53 from performing health checks. When you disable a health check, here's what happens:     * __Health checks that check the health of endpoints:__ Route 53 stops submitting requests to your application, server, or other resource.     * __Calculated health checks:__ Route 53 stops aggregating the status of the referenced health checks.     * __Health checks that monitor CloudWatch alarms:__ Route 53 stops monitoring the corresponding CloudWatch metrics. After you disable a health check, Route 53 considers the status of the health check to always be healthy. If you configured DNS failover, Route 53 continues to route traffic to the corresponding resources. If you want to stop routing traffic to a resource, change the value of <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-Inverted Inverted> .  Charges for a health check still apply when the health check is disabled. For more information, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
--
-- * 'hccSearchString' - If the value of Type is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@ , the string that you want Amazon Route 53 to search for in the response body from the specified resource. If the string appears in the response body, Route 53 considers the resource healthy. Route 53 considers case when searching for @SearchString@ in the response body.
--
-- * 'hccHealthThreshold' - The number of child health checks that are associated with a @CALCULATED@ health check that Amazon Route 53 must consider healthy for the @CALCULATED@ health check to be considered healthy. To specify the child health checks that you want to associate with a @CALCULATED@ health check, use the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-ChildHealthChecks ChildHealthChecks> element. Note the following:     * If you specify a number greater than the number of child health checks, Route 53 always considers this health check to be unhealthy.     * If you specify @0@ , Route 53 always considers this health check to be healthy.
--
-- * 'hccRegions' - A complex type that contains one @Region@ element for each region from which you want Amazon Route 53 health checkers to check the specified endpoint. If you don't specify any regions, Route 53 health checkers automatically performs checks from all of the regions that are listed under __Valid Values__ . If you update a health check to remove a region that has been performing health checks, Route 53 will briefly continue to perform checks from that region to ensure that some health checkers are always checking the endpoint (for example, if you replace three regions with four different regions).
--
-- * 'hccResourcePath' - The path, if any, that you want Amazon Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, for example, the file /docs/route53-health-check.html. You can also include query string parameters, for example, @/welcome.html?language=jp&login=y@ .
--
-- * 'hccInsufficientDataHealthStatus' - When CloudWatch has insufficient data about the metric to determine the alarm state, the status that you want Amazon Route 53 to assign to the health check:     * @Healthy@ : Route 53 considers the health check to be healthy.     * @Unhealthy@ : Route 53 considers the health check to be unhealthy.     * @LastKnownStatus@ : Route 53 uses the status of the health check from the last time that CloudWatch had sufficient data to determine the alarm state. For new health checks that have no last known status, the default status for the health check is healthy.
--
-- * 'hccAlarmIdentifier' - A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether the specified health check is healthy.
--
-- * 'hccMeasureLatency' - Specify whether you want Amazon Route 53 to measure the latency between health checkers in multiple AWS regions and your endpoint, and to display CloudWatch latency graphs on the __Health Checks__ page in the Route 53 console. /Important:/ You can't change the value of @MeasureLatency@ after you create a health check.
--
-- * 'hccInverted' - Specify whether you want Amazon Route 53 to invert the status of a health check, for example, to consider a health check unhealthy when it otherwise would be considered healthy.
--
-- * 'hccFullyQualifiedDomainName' - Amazon Route 53 behavior depends on whether you specify a value for @IPAddress@ . __If you specify a value for__ @IPAddress@ : Amazon Route 53 sends health check requests to the specified IPv4 or IPv6 address and passes the value of @FullyQualifiedDomainName@ in the @Host@ header for all health checks except TCP health checks. This is typically the fully qualified DNS name of the endpoint on which you want Route 53 to perform health checks. When Route 53 checks the health of an endpoint, here is how it constructs the @Host@ header:     * If you specify a value of @80@ for @Port@ and @HTTP@ or @HTTP_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the Host header.      * If you specify a value of @443@ for @Port@ and @HTTPS@ or @HTTPS_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.     * If you specify another value for @Port@ and any value except @TCP@ for @Type@ , Route 53 passes @FullyQualifiedDomainName:Port@ to the endpoint in the @Host@ header. If you don't specify a value for @FullyQualifiedDomainName@ , Route 53 substitutes the value of @IPAddress@ in the @Host@ header in each of the preceding cases. __If you don't specify a value for @IPAddress@ __ : Route 53 sends a DNS request to the domain that you specify for @FullyQualifiedDomainName@ at the interval that you specify for @RequestInterval@ . Using an IPv4 address that DNS returns, Route 53 then checks the health of the endpoint. If you want to check the health of weighted, latency, or failover resource record sets and you choose to specify the endpoint only by @FullyQualifiedDomainName@ , we recommend that you create a separate health check for each endpoint. For example, create a health check for each HTTP server that is serving content for www.example.com. For the value of @FullyQualifiedDomainName@ , specify the domain name of the server (such as us-east-2-www.example.com), not the name of the resource record sets (www.example.com). /Important:/ In this configuration, if you create a health check for which the value of @FullyQualifiedDomainName@ matches the name of the resource record sets and you then associate the health check with those resource record sets, health check results will be unpredictable. In addition, if the value that you specify for @Type@ is @HTTP@ , @HTTPS@ , @HTTP_STR_MATCH@ , or @HTTPS_STR_MATCH@ , Route 53 passes the value of @FullyQualifiedDomainName@ in the @Host@ header, as it does when you specify a value for @IPAddress@ . If the value of @Type@ is @TCP@ , Route 53 doesn't pass a @Host@ header.
--
-- * 'hccChildHealthChecks' - (CALCULATED Health Checks Only) A complex type that contains one @ChildHealthCheck@ element for each health check that you want to associate with a @CALCULATED@ health check.
--
-- * 'hccRequestInterval' - The number of seconds between the time that Amazon Route 53 gets a response from your endpoint and the time that it sends the next health check request. Each Route 53 health checker makes requests at this interval. /Important:/ You can't change the value of @RequestInterval@ after you create a health check. If you don't specify a value for @RequestInterval@ , the default value is @30@ seconds.
--
-- * 'hccPort' - The port on the endpoint that you want Amazon Route 53 to perform health checks on.
--
-- * 'hccType' - The type of health check that you want to create, which indicates how Amazon Route 53 determines whether an endpoint is healthy. /Important:/ You can't change the value of @Type@ after you create a health check. You can create the following types of health checks:     * __HTTP__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTP request and waits for an HTTP status code of 200 or greater and less than 400.     * __HTTPS__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTPS request and waits for an HTTP status code of 200 or greater and less than 400. /Important:/ If you specify @HTTPS@ for the value of @Type@ , the endpoint must support TLS v1.0 or later.     * __HTTP_STR_MATCH__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTP request and searches the first 5,120 bytes of the response body for the string that you specify in @SearchString@ .     * __HTTPS_STR_MATCH__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an @HTTPS@ request and searches the first 5,120 bytes of the response body for the string that you specify in @SearchString@ .     * __TCP__ : Route 53 tries to establish a TCP connection.     * __CLOUDWATCH_METRIC__ : The health check is associated with a CloudWatch alarm. If the state of the alarm is @OK@ , the health check is considered healthy. If the state is @ALARM@ , the health check is considered unhealthy. If CloudWatch doesn't have sufficient data to determine whether the state is @OK@ or @ALARM@ , the health check status depends on the setting for @InsufficientDataHealthStatus@ : @Healthy@ , @Unhealthy@ , or @LastKnownStatus@ .      * __CALCULATED__ : For health checks that monitor the status of other health checks, Route 53 adds up the number of health checks that Route 53 health checkers consider to be healthy and compares that number with the value of @HealthThreshold@ .  For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
healthCheckConfig ::
  -- | 'hccType'
  HealthCheckType ->
  HealthCheckConfig
healthCheckConfig pType_ =
  HealthCheckConfig'
    { _hccFailureThreshold = Nothing,
      _hccIPAddress = Nothing,
      _hccEnableSNI = Nothing,
      _hccDisabled = Nothing,
      _hccSearchString = Nothing,
      _hccHealthThreshold = Nothing,
      _hccRegions = Nothing,
      _hccResourcePath = Nothing,
      _hccInsufficientDataHealthStatus = Nothing,
      _hccAlarmIdentifier = Nothing,
      _hccMeasureLatency = Nothing,
      _hccInverted = Nothing,
      _hccFullyQualifiedDomainName = Nothing,
      _hccChildHealthChecks = Nothing,
      _hccRequestInterval = Nothing,
      _hccPort = Nothing,
      _hccType = pType_
    }

-- | The number of consecutive health checks that an endpoint must pass or fail for Amazon Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ . If you don't specify a value for @FailureThreshold@ , the default value is three health checks.
hccFailureThreshold :: Lens' HealthCheckConfig (Maybe Natural)
hccFailureThreshold = lens _hccFailureThreshold (\s a -> s {_hccFailureThreshold = a}) . mapping _Nat

-- | The IPv4 or IPv6 IP address of the endpoint that you want Amazon Route 53 to perform health checks on. If you don't specify a value for @IPAddress@ , Route 53 sends a DNS request to resolve the domain name that you specify in @FullyQualifiedDomainName@ at the interval that you specify in @RequestInterval@ . Using an IP address returned by DNS, Route 53 then checks the health of the endpoint. Use one of the following formats for the value of @IPAddress@ :      * __IPv4 address__ : four values between 0 and 255, separated by periods (.), for example, @192.0.2.44@ .     * __IPv6 address__ : eight groups of four hexadecimal values, separated by colons (:), for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ . You can also shorten IPv6 addresses as described in RFC 5952, for example, @2001:db8:85a3::abcd:1:2345@ . If the endpoint is an EC2 instance, we recommend that you create an Elastic IP address, associate it with your EC2 instance, and specify the Elastic IP address for @IPAddress@ . This ensures that the IP address of your instance will never change. For more information, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-FullyQualifiedDomainName FullyQualifiedDomainName> .  Constraints: Route 53 can't check the health of endpoints for which the IP address is in local, private, non-routable, or multicast ranges. For more information about IP addresses for which you can't create health checks, see the following documents:     * <https://tools.ietf.org/html/rfc5735 RFC 5735, Special Use IPv4 Addresses>      * <https://tools.ietf.org/html/rfc6598 RFC 6598, IANA-Reserved IPv4 Prefix for Shared Address Space>      * <https://tools.ietf.org/html/rfc5156 RFC 5156, Special-Use IPv6 Addresses>  When the value of @Type@ is @CALCULATED@ or @CLOUDWATCH_METRIC@ , omit @IPAddress@ .
hccIPAddress :: Lens' HealthCheckConfig (Maybe Text)
hccIPAddress = lens _hccIPAddress (\s a -> s {_hccIPAddress = a})

-- | Specify whether you want Amazon Route 53 to send the value of @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message during TLS negotiation. This allows the endpoint to respond to @HTTPS@ health check requests with the applicable SSL/TLS certificate. Some endpoints require that @HTTPS@ requests include the host name in the @client_hello@ message. If you don't enable SNI, the status of the health check will be @SSL alert handshake_failure@ . A health check can also have that status for other reasons. If SNI is enabled and you're still getting the error, check the SSL/TLS configuration on your endpoint and confirm that your certificate is valid. The SSL/TLS certificate on your endpoint includes a domain name in the @Common Name@ field and possibly several more in the @Subject Alternative Names@ field. One of the domain names in the certificate should match the value that you specify for @FullyQualifiedDomainName@ . If the endpoint responds to the @client_hello@ message with a certificate that does not include the domain name that you specified in @FullyQualifiedDomainName@ , a health checker will retry the handshake. In the second attempt, the health checker will omit @FullyQualifiedDomainName@ from the @client_hello@ message.
hccEnableSNI :: Lens' HealthCheckConfig (Maybe Bool)
hccEnableSNI = lens _hccEnableSNI (\s a -> s {_hccEnableSNI = a})

-- | Stops Route 53 from performing health checks. When you disable a health check, here's what happens:     * __Health checks that check the health of endpoints:__ Route 53 stops submitting requests to your application, server, or other resource.     * __Calculated health checks:__ Route 53 stops aggregating the status of the referenced health checks.     * __Health checks that monitor CloudWatch alarms:__ Route 53 stops monitoring the corresponding CloudWatch metrics. After you disable a health check, Route 53 considers the status of the health check to always be healthy. If you configured DNS failover, Route 53 continues to route traffic to the corresponding resources. If you want to stop routing traffic to a resource, change the value of <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-Inverted Inverted> .  Charges for a health check still apply when the health check is disabled. For more information, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
hccDisabled :: Lens' HealthCheckConfig (Maybe Bool)
hccDisabled = lens _hccDisabled (\s a -> s {_hccDisabled = a})

-- | If the value of Type is @HTTP_STR_MATCH@ or @HTTPS_STR_MATCH@ , the string that you want Amazon Route 53 to search for in the response body from the specified resource. If the string appears in the response body, Route 53 considers the resource healthy. Route 53 considers case when searching for @SearchString@ in the response body.
hccSearchString :: Lens' HealthCheckConfig (Maybe Text)
hccSearchString = lens _hccSearchString (\s a -> s {_hccSearchString = a})

-- | The number of child health checks that are associated with a @CALCULATED@ health check that Amazon Route 53 must consider healthy for the @CALCULATED@ health check to be considered healthy. To specify the child health checks that you want to associate with a @CALCULATED@ health check, use the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html#Route53-UpdateHealthCheck-request-ChildHealthChecks ChildHealthChecks> element. Note the following:     * If you specify a number greater than the number of child health checks, Route 53 always considers this health check to be unhealthy.     * If you specify @0@ , Route 53 always considers this health check to be healthy.
hccHealthThreshold :: Lens' HealthCheckConfig (Maybe Natural)
hccHealthThreshold = lens _hccHealthThreshold (\s a -> s {_hccHealthThreshold = a}) . mapping _Nat

-- | A complex type that contains one @Region@ element for each region from which you want Amazon Route 53 health checkers to check the specified endpoint. If you don't specify any regions, Route 53 health checkers automatically performs checks from all of the regions that are listed under __Valid Values__ . If you update a health check to remove a region that has been performing health checks, Route 53 will briefly continue to perform checks from that region to ensure that some health checkers are always checking the endpoint (for example, if you replace three regions with four different regions).
hccRegions :: Lens' HealthCheckConfig (Maybe (NonEmpty HealthCheckRegion))
hccRegions = lens _hccRegions (\s a -> s {_hccRegions = a}) . mapping _List1

-- | The path, if any, that you want Amazon Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, for example, the file /docs/route53-health-check.html. You can also include query string parameters, for example, @/welcome.html?language=jp&login=y@ .
hccResourcePath :: Lens' HealthCheckConfig (Maybe Text)
hccResourcePath = lens _hccResourcePath (\s a -> s {_hccResourcePath = a})

-- | When CloudWatch has insufficient data about the metric to determine the alarm state, the status that you want Amazon Route 53 to assign to the health check:     * @Healthy@ : Route 53 considers the health check to be healthy.     * @Unhealthy@ : Route 53 considers the health check to be unhealthy.     * @LastKnownStatus@ : Route 53 uses the status of the health check from the last time that CloudWatch had sufficient data to determine the alarm state. For new health checks that have no last known status, the default status for the health check is healthy.
hccInsufficientDataHealthStatus :: Lens' HealthCheckConfig (Maybe InsufficientDataHealthStatus)
hccInsufficientDataHealthStatus = lens _hccInsufficientDataHealthStatus (\s a -> s {_hccInsufficientDataHealthStatus = a})

-- | A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether the specified health check is healthy.
hccAlarmIdentifier :: Lens' HealthCheckConfig (Maybe AlarmIdentifier)
hccAlarmIdentifier = lens _hccAlarmIdentifier (\s a -> s {_hccAlarmIdentifier = a})

-- | Specify whether you want Amazon Route 53 to measure the latency between health checkers in multiple AWS regions and your endpoint, and to display CloudWatch latency graphs on the __Health Checks__ page in the Route 53 console. /Important:/ You can't change the value of @MeasureLatency@ after you create a health check.
hccMeasureLatency :: Lens' HealthCheckConfig (Maybe Bool)
hccMeasureLatency = lens _hccMeasureLatency (\s a -> s {_hccMeasureLatency = a})

-- | Specify whether you want Amazon Route 53 to invert the status of a health check, for example, to consider a health check unhealthy when it otherwise would be considered healthy.
hccInverted :: Lens' HealthCheckConfig (Maybe Bool)
hccInverted = lens _hccInverted (\s a -> s {_hccInverted = a})

-- | Amazon Route 53 behavior depends on whether you specify a value for @IPAddress@ . __If you specify a value for__ @IPAddress@ : Amazon Route 53 sends health check requests to the specified IPv4 or IPv6 address and passes the value of @FullyQualifiedDomainName@ in the @Host@ header for all health checks except TCP health checks. This is typically the fully qualified DNS name of the endpoint on which you want Route 53 to perform health checks. When Route 53 checks the health of an endpoint, here is how it constructs the @Host@ header:     * If you specify a value of @80@ for @Port@ and @HTTP@ or @HTTP_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the Host header.      * If you specify a value of @443@ for @Port@ and @HTTPS@ or @HTTPS_STR_MATCH@ for @Type@ , Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.     * If you specify another value for @Port@ and any value except @TCP@ for @Type@ , Route 53 passes @FullyQualifiedDomainName:Port@ to the endpoint in the @Host@ header. If you don't specify a value for @FullyQualifiedDomainName@ , Route 53 substitutes the value of @IPAddress@ in the @Host@ header in each of the preceding cases. __If you don't specify a value for @IPAddress@ __ : Route 53 sends a DNS request to the domain that you specify for @FullyQualifiedDomainName@ at the interval that you specify for @RequestInterval@ . Using an IPv4 address that DNS returns, Route 53 then checks the health of the endpoint. If you want to check the health of weighted, latency, or failover resource record sets and you choose to specify the endpoint only by @FullyQualifiedDomainName@ , we recommend that you create a separate health check for each endpoint. For example, create a health check for each HTTP server that is serving content for www.example.com. For the value of @FullyQualifiedDomainName@ , specify the domain name of the server (such as us-east-2-www.example.com), not the name of the resource record sets (www.example.com). /Important:/ In this configuration, if you create a health check for which the value of @FullyQualifiedDomainName@ matches the name of the resource record sets and you then associate the health check with those resource record sets, health check results will be unpredictable. In addition, if the value that you specify for @Type@ is @HTTP@ , @HTTPS@ , @HTTP_STR_MATCH@ , or @HTTPS_STR_MATCH@ , Route 53 passes the value of @FullyQualifiedDomainName@ in the @Host@ header, as it does when you specify a value for @IPAddress@ . If the value of @Type@ is @TCP@ , Route 53 doesn't pass a @Host@ header.
hccFullyQualifiedDomainName :: Lens' HealthCheckConfig (Maybe Text)
hccFullyQualifiedDomainName = lens _hccFullyQualifiedDomainName (\s a -> s {_hccFullyQualifiedDomainName = a})

-- | (CALCULATED Health Checks Only) A complex type that contains one @ChildHealthCheck@ element for each health check that you want to associate with a @CALCULATED@ health check.
hccChildHealthChecks :: Lens' HealthCheckConfig [Text]
hccChildHealthChecks = lens _hccChildHealthChecks (\s a -> s {_hccChildHealthChecks = a}) . _Default . _Coerce

-- | The number of seconds between the time that Amazon Route 53 gets a response from your endpoint and the time that it sends the next health check request. Each Route 53 health checker makes requests at this interval. /Important:/ You can't change the value of @RequestInterval@ after you create a health check. If you don't specify a value for @RequestInterval@ , the default value is @30@ seconds.
hccRequestInterval :: Lens' HealthCheckConfig (Maybe Natural)
hccRequestInterval = lens _hccRequestInterval (\s a -> s {_hccRequestInterval = a}) . mapping _Nat

-- | The port on the endpoint that you want Amazon Route 53 to perform health checks on.
hccPort :: Lens' HealthCheckConfig (Maybe Natural)
hccPort = lens _hccPort (\s a -> s {_hccPort = a}) . mapping _Nat

-- | The type of health check that you want to create, which indicates how Amazon Route 53 determines whether an endpoint is healthy. /Important:/ You can't change the value of @Type@ after you create a health check. You can create the following types of health checks:     * __HTTP__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTP request and waits for an HTTP status code of 200 or greater and less than 400.     * __HTTPS__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTPS request and waits for an HTTP status code of 200 or greater and less than 400. /Important:/ If you specify @HTTPS@ for the value of @Type@ , the endpoint must support TLS v1.0 or later.     * __HTTP_STR_MATCH__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTP request and searches the first 5,120 bytes of the response body for the string that you specify in @SearchString@ .     * __HTTPS_STR_MATCH__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an @HTTPS@ request and searches the first 5,120 bytes of the response body for the string that you specify in @SearchString@ .     * __TCP__ : Route 53 tries to establish a TCP connection.     * __CLOUDWATCH_METRIC__ : The health check is associated with a CloudWatch alarm. If the state of the alarm is @OK@ , the health check is considered healthy. If the state is @ALARM@ , the health check is considered unhealthy. If CloudWatch doesn't have sufficient data to determine whether the state is @OK@ or @ALARM@ , the health check status depends on the setting for @InsufficientDataHealthStatus@ : @Healthy@ , @Unhealthy@ , or @LastKnownStatus@ .      * __CALCULATED__ : For health checks that monitor the status of other health checks, Route 53 adds up the number of health checks that Route 53 health checkers consider to be healthy and compares that number with the value of @HealthThreshold@ .  For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
hccType :: Lens' HealthCheckConfig HealthCheckType
hccType = lens _hccType (\s a -> s {_hccType = a})

instance FromXML HealthCheckConfig where
  parseXML x =
    HealthCheckConfig'
      <$> (x .@? "FailureThreshold")
      <*> (x .@? "IPAddress")
      <*> (x .@? "EnableSNI")
      <*> (x .@? "Disabled")
      <*> (x .@? "SearchString")
      <*> (x .@? "HealthThreshold")
      <*> (x .@? "Regions" .!@ mempty >>= may (parseXMLList1 "Region"))
      <*> (x .@? "ResourcePath")
      <*> (x .@? "InsufficientDataHealthStatus")
      <*> (x .@? "AlarmIdentifier")
      <*> (x .@? "MeasureLatency")
      <*> (x .@? "Inverted")
      <*> (x .@? "FullyQualifiedDomainName")
      <*> ( x .@? "ChildHealthChecks" .!@ mempty
              >>= may (parseXMLList "ChildHealthCheck")
          )
      <*> (x .@? "RequestInterval")
      <*> (x .@? "Port")
      <*> (x .@ "Type")

instance Hashable HealthCheckConfig

instance NFData HealthCheckConfig

instance ToXML HealthCheckConfig where
  toXML HealthCheckConfig' {..} =
    mconcat
      [ "FailureThreshold" @= _hccFailureThreshold,
        "IPAddress" @= _hccIPAddress,
        "EnableSNI" @= _hccEnableSNI,
        "Disabled" @= _hccDisabled,
        "SearchString" @= _hccSearchString,
        "HealthThreshold" @= _hccHealthThreshold,
        "Regions" @= toXML (toXMLList "Region" <$> _hccRegions),
        "ResourcePath" @= _hccResourcePath,
        "InsufficientDataHealthStatus" @= _hccInsufficientDataHealthStatus,
        "AlarmIdentifier" @= _hccAlarmIdentifier,
        "MeasureLatency" @= _hccMeasureLatency,
        "Inverted" @= _hccInverted,
        "FullyQualifiedDomainName" @= _hccFullyQualifiedDomainName,
        "ChildHealthChecks"
          @= toXML (toXMLList "ChildHealthCheck" <$> _hccChildHealthChecks),
        "RequestInterval" @= _hccRequestInterval,
        "Port" @= _hccPort,
        "Type" @= _hccType
      ]
