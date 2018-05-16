{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.Sum

-- | A complex type that contains information about the records that you want Amazon Route 53 to create when you register an instance.
--
--
--
-- /See:/ 'dnsConfig' smart constructor.
data DNSConfig = DNSConfig'
  { _dcRoutingPolicy :: !(Maybe RoutingPolicy)
  , _dcNamespaceId   :: !Text
  , _dcDNSRecords    :: ![DNSRecord]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DNSConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcRoutingPolicy' - The routing policy that you want to apply to all records that Route 53 creates when you register an instance and specify this service. You can specify the following values: __MULTIVALUE__  If you define a health check for the service and the health check is healthy, Route 53 returns the applicable value for up to eight instances. For example, suppose the service includes configurations for one A record and a health check, and you use the service to register 10 instances. Route 53 responds to DNS queries with IP addresses for up to eight healthy instances. If fewer than eight instances are healthy, Route 53 responds to every DNS query with the IP addresses for all of the healthy instances. If you don't define a health check for the service, Route 53 assumes that all instances are healthy and returns the values for up to eight instances. For more information about the multivalue routing policy, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-multivalue Multivalue Answer Routing> in the /Route 53 Developer Guide/ . __WEIGHTED__  Route 53 returns the applicable value from one randomly selected instance from among the instances that you registered using the same service. Currently, all records have the same weight, so you can't route more or less traffic to any instances. For example, suppose the service includes configurations for one A record and a health check, and you use the service to register 10 instances. Route 53 responds to DNS queries with the IP address for one randomly selected instance from among the healthy instances. If no instances are healthy, Route 53 responds to DNS queries as if all of the instances were healthy. If you don't define a health check for the service, Route 53 assumes that all instances are healthy and returns the applicable value for one randomly selected instance. For more information about the weighted routing policy, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-weighted Weighted Routing> in the /Route 53 Developer Guide/ .
--
-- * 'dcNamespaceId' - The ID of the namespace to use for DNS configuration.
--
-- * 'dcDNSRecords' - An array that contains one @DnsRecord@ object for each record that you want Route 53 to create when you register an instance.
dnsConfig
    :: Text -- ^ 'dcNamespaceId'
    -> DNSConfig
dnsConfig pNamespaceId_ =
  DNSConfig'
    { _dcRoutingPolicy = Nothing
    , _dcNamespaceId = pNamespaceId_
    , _dcDNSRecords = mempty
    }


-- | The routing policy that you want to apply to all records that Route 53 creates when you register an instance and specify this service. You can specify the following values: __MULTIVALUE__  If you define a health check for the service and the health check is healthy, Route 53 returns the applicable value for up to eight instances. For example, suppose the service includes configurations for one A record and a health check, and you use the service to register 10 instances. Route 53 responds to DNS queries with IP addresses for up to eight healthy instances. If fewer than eight instances are healthy, Route 53 responds to every DNS query with the IP addresses for all of the healthy instances. If you don't define a health check for the service, Route 53 assumes that all instances are healthy and returns the values for up to eight instances. For more information about the multivalue routing policy, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-multivalue Multivalue Answer Routing> in the /Route 53 Developer Guide/ . __WEIGHTED__  Route 53 returns the applicable value from one randomly selected instance from among the instances that you registered using the same service. Currently, all records have the same weight, so you can't route more or less traffic to any instances. For example, suppose the service includes configurations for one A record and a health check, and you use the service to register 10 instances. Route 53 responds to DNS queries with the IP address for one randomly selected instance from among the healthy instances. If no instances are healthy, Route 53 responds to DNS queries as if all of the instances were healthy. If you don't define a health check for the service, Route 53 assumes that all instances are healthy and returns the applicable value for one randomly selected instance. For more information about the weighted routing policy, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-weighted Weighted Routing> in the /Route 53 Developer Guide/ .
dcRoutingPolicy :: Lens' DNSConfig (Maybe RoutingPolicy)
dcRoutingPolicy = lens _dcRoutingPolicy (\ s a -> s{_dcRoutingPolicy = a})

-- | The ID of the namespace to use for DNS configuration.
dcNamespaceId :: Lens' DNSConfig Text
dcNamespaceId = lens _dcNamespaceId (\ s a -> s{_dcNamespaceId = a})

-- | An array that contains one @DnsRecord@ object for each record that you want Route 53 to create when you register an instance.
dcDNSRecords :: Lens' DNSConfig [DNSRecord]
dcDNSRecords = lens _dcDNSRecords (\ s a -> s{_dcDNSRecords = a}) . _Coerce

instance FromJSON DNSConfig where
        parseJSON
          = withObject "DNSConfig"
              (\ x ->
                 DNSConfig' <$>
                   (x .:? "RoutingPolicy") <*> (x .: "NamespaceId") <*>
                     (x .:? "DnsRecords" .!= mempty))

instance Hashable DNSConfig where

instance NFData DNSConfig where

instance ToJSON DNSConfig where
        toJSON DNSConfig'{..}
          = object
              (catMaybes
                 [("RoutingPolicy" .=) <$> _dcRoutingPolicy,
                  Just ("NamespaceId" .= _dcNamespaceId),
                  Just ("DnsRecords" .= _dcDNSRecords)])

-- | A complex type that contains information about changes to the records that Route 53 creates when you register an instance.
--
--
--
-- /See:/ 'dnsConfigChange' smart constructor.
newtype DNSConfigChange = DNSConfigChange'
  { _dccDNSRecords :: [DNSRecord]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DNSConfigChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dccDNSRecords' - An array that contains one @DnsRecord@ object for each record that you want Route 53 to create when you register an instance.
dnsConfigChange
    :: DNSConfigChange
dnsConfigChange = DNSConfigChange' {_dccDNSRecords = mempty}


-- | An array that contains one @DnsRecord@ object for each record that you want Route 53 to create when you register an instance.
dccDNSRecords :: Lens' DNSConfigChange [DNSRecord]
dccDNSRecords = lens _dccDNSRecords (\ s a -> s{_dccDNSRecords = a}) . _Coerce

instance Hashable DNSConfigChange where

instance NFData DNSConfigChange where

instance ToJSON DNSConfigChange where
        toJSON DNSConfigChange'{..}
          = object
              (catMaybes [Just ("DnsRecords" .= _dccDNSRecords)])

-- | A complex type that contains the ID for the hosted zone that Route 53 creates when you create a namespace.
--
--
--
-- /See:/ 'dnsProperties' smart constructor.
newtype DNSProperties = DNSProperties'
  { _dpHostedZoneId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DNSProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpHostedZoneId' - The ID for the hosted zone that Route 53 creates when you create a namespace.
dnsProperties
    :: DNSProperties
dnsProperties = DNSProperties' {_dpHostedZoneId = Nothing}


-- | The ID for the hosted zone that Route 53 creates when you create a namespace.
dpHostedZoneId :: Lens' DNSProperties (Maybe Text)
dpHostedZoneId = lens _dpHostedZoneId (\ s a -> s{_dpHostedZoneId = a})

instance FromJSON DNSProperties where
        parseJSON
          = withObject "DNSProperties"
              (\ x -> DNSProperties' <$> (x .:? "HostedZoneId"))

instance Hashable DNSProperties where

instance NFData DNSProperties where

-- | A complex type that contains information about the records that you want Route 53 to create when you register an instance.
--
--
--
-- /See:/ 'dnsRecord' smart constructor.
data DNSRecord = DNSRecord'
  { _drType :: !RecordType
  , _drTTL  :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DNSRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drType' - The type of the resource, which indicates the type of value that Route 53 returns in response to DNS queries. Note the following:     * __A, AAAA, and SRV records: You can specify settings for a maximum of one A, one AAAA, and one SRV record. You can specify them in any combination.__      * __CNAME records:__ If you specify @CNAME@ for @Type@ , you can't define any other records. This is a limitation of DNS—you can't create a CNAME record and any other type of record that has the same name as a CNAME record.     * __Alias records:__ If you want Route 53 to create an alias record when you register an instance, specify @A@ or @AAAA@ for @Type@ .     * __All records:__ You specify settings other than @TTL@ and @Type@ when you register an instance. The following values are supported: __A__  Route 53 returns the IP address of the resource in IPv4 format, such as 192.0.2.44. __AAAA__  Route 53 returns the IP address of the resource in IPv6 format, such as 2001:0db8:85a3:0000:0000:abcd:0001:2345. __CNAME__  Route 53 returns the domain name of the resource, such as www.example.com. Note the following:     * You specify the domain name that you want to route traffic to when you register an instance. For more information, see 'RegisterInstanceRequest$Attributes' .     * You must specify @WEIGHTED@ for the value of @RoutingPolicy@ .     * You can't specify both @CNAME@ for @Type@ and settings for @HealthCheckConfig@ . If you do, the request will fail with an @InvalidInput@ error. __SRV__  Route 53 returns the value for an SRV record. The value for an SRV record uses the following values: @priority weight port service-hostname@  Note the following about the values:     * The values of @priority@ and @weight@ are both set to @1@ and can't be changed.      * The value of @port@ comes from the value that you specify for the @AWS_INSTANCE_PORT@ attribute when you submit a 'RegisterInstance' request.      * The value of @service-hostname@ is a concatenation of the following values:     * The value that you specify for @InstanceId@ when you register an instance.     * The name of the service.     * The name of the namespace.  For example, if the value of @InstanceId@ is @test@ , the name of the service is @backend@ , and the name of the namespace is @example.com@ , the value of @service-hostname@ is: @test.backend.example.com@  If you specify settings for an SRV record and if you specify values for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both in the @RegisterInstance@ request, Route 53 automatically creates @A@ and/or @AAAA@ records that have the same name as the value of @service-hostname@ in the SRV record. You can ignore these records.
--
-- * 'drTTL' - The amount of time, in seconds, that you want DNS resolvers to cache the settings for this record.
dnsRecord
    :: RecordType -- ^ 'drType'
    -> Natural -- ^ 'drTTL'
    -> DNSRecord
dnsRecord pType_ pTTL_ = DNSRecord' {_drType = pType_, _drTTL = _Nat # pTTL_}


-- | The type of the resource, which indicates the type of value that Route 53 returns in response to DNS queries. Note the following:     * __A, AAAA, and SRV records: You can specify settings for a maximum of one A, one AAAA, and one SRV record. You can specify them in any combination.__      * __CNAME records:__ If you specify @CNAME@ for @Type@ , you can't define any other records. This is a limitation of DNS—you can't create a CNAME record and any other type of record that has the same name as a CNAME record.     * __Alias records:__ If you want Route 53 to create an alias record when you register an instance, specify @A@ or @AAAA@ for @Type@ .     * __All records:__ You specify settings other than @TTL@ and @Type@ when you register an instance. The following values are supported: __A__  Route 53 returns the IP address of the resource in IPv4 format, such as 192.0.2.44. __AAAA__  Route 53 returns the IP address of the resource in IPv6 format, such as 2001:0db8:85a3:0000:0000:abcd:0001:2345. __CNAME__  Route 53 returns the domain name of the resource, such as www.example.com. Note the following:     * You specify the domain name that you want to route traffic to when you register an instance. For more information, see 'RegisterInstanceRequest$Attributes' .     * You must specify @WEIGHTED@ for the value of @RoutingPolicy@ .     * You can't specify both @CNAME@ for @Type@ and settings for @HealthCheckConfig@ . If you do, the request will fail with an @InvalidInput@ error. __SRV__  Route 53 returns the value for an SRV record. The value for an SRV record uses the following values: @priority weight port service-hostname@  Note the following about the values:     * The values of @priority@ and @weight@ are both set to @1@ and can't be changed.      * The value of @port@ comes from the value that you specify for the @AWS_INSTANCE_PORT@ attribute when you submit a 'RegisterInstance' request.      * The value of @service-hostname@ is a concatenation of the following values:     * The value that you specify for @InstanceId@ when you register an instance.     * The name of the service.     * The name of the namespace.  For example, if the value of @InstanceId@ is @test@ , the name of the service is @backend@ , and the name of the namespace is @example.com@ , the value of @service-hostname@ is: @test.backend.example.com@  If you specify settings for an SRV record and if you specify values for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both in the @RegisterInstance@ request, Route 53 automatically creates @A@ and/or @AAAA@ records that have the same name as the value of @service-hostname@ in the SRV record. You can ignore these records.
drType :: Lens' DNSRecord RecordType
drType = lens _drType (\ s a -> s{_drType = a})

-- | The amount of time, in seconds, that you want DNS resolvers to cache the settings for this record.
drTTL :: Lens' DNSRecord Natural
drTTL = lens _drTTL (\ s a -> s{_drTTL = a}) . _Nat

instance FromJSON DNSRecord where
        parseJSON
          = withObject "DNSRecord"
              (\ x ->
                 DNSRecord' <$> (x .: "Type") <*> (x .: "TTL"))

instance Hashable DNSRecord where

instance NFData DNSRecord where

instance ToJSON DNSRecord where
        toJSON DNSRecord'{..}
          = object
              (catMaybes
                 [Just ("Type" .= _drType), Just ("TTL" .= _drTTL)])

-- | /Public DNS namespaces only./ A complex type that contains settings for an optional health check. If you specify settings for a health check, Amazon Route 53 associates the health check with all the records that you specify in @DnsConfig@ .
--
--
-- __A and AAAA records__
--
-- If @DnsConfig@ includes configurations for both A and AAAA records, Route 53 creates a health check that uses the IPv4 address to check the health of the resource. If the endpoint that is specified by the IPv4 address is unhealthy, Route 53 considers both the A and AAAA records to be unhealthy.
--
-- __CNAME records__
--
-- You can't specify settings for @HealthCheckConfig@ when the @DNSConfig@ includes @CNAME@ for the value of @Type@ . If you do, the @CreateService@ request will fail with an @InvalidInput@ error.
--
-- __Request interval__
--
-- The health check uses 30 seconds as the request interval. This is the number of seconds between the time that each Route 53 health checker gets a response from your endpoint and the time that it sends the next health check request. A health checker in each data center around the world sends your endpoint a health check request every 30 seconds. On average, your endpoint receives a health check request about every two seconds. Health checkers in different data centers don't coordinate with one another, so you'll sometimes see several requests per second followed by a few seconds with no health checks at all.
--
-- __Health checking regions__
--
-- Health checkers perform checks from all Route 53 health-checking regions. For a list of the current regions, see <http://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-Regions Regions> .
--
-- __Alias records__
--
-- When you register an instance, if you include the @AWS_ALIAS_DNS_NAME@ attribute, Route 53 creates an alias record. Note the following:
--
--     * Route 53 automatically sets @EvaluateTargetHealth@ to true for alias records. When @EvaluateTargetHealth@ is true, the alias record inherits the health of the referenced AWS resource. such as an ELB load balancer. For more information, see <http://docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html#Route53-Type-AliasTarget-EvaluateTargetHealth EvaluateTargetHealth> .
--
--     * If you include @HealthCheckConfig@ and then use the service to register an instance that creates an alias record, Route 53 doesn't create the health check.
--
--
--
-- For information about the charges for health checks, see <http://aws.amazon.com/route53/pricing Route 53 Pricing> .
--
--
-- /See:/ 'healthCheckConfig' smart constructor.
data HealthCheckConfig = HealthCheckConfig'
  { _hccFailureThreshold :: !(Maybe Nat)
  , _hccResourcePath     :: !(Maybe Text)
  , _hccType             :: !(Maybe HealthCheckType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HealthCheckConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hccFailureThreshold' - The number of consecutive health checks that an endpoint must pass or fail for Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy> in the /Route 53 Developer Guide/ .
--
-- * 'hccResourcePath' - The path that you want Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, such as the file @/docs/route53-health-check.html@ . Route 53 automatically adds the DNS name for the service and a leading forward slash (@/@ ) character.
--
-- * 'hccType' - The type of health check that you want to create, which indicates how Route 53 determines whether an endpoint is healthy. /Important:/ You can't change the value of @Type@ after you create a health check. You can create the following types of health checks:     * __HTTP__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTP request and waits for an HTTP status code of 200 or greater and less than 400.     * __HTTPS__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTPS request and waits for an HTTP status code of 200 or greater and less than 400. /Important:/ If you specify HTTPS for the value of @Type@ , the endpoint must support TLS v1.0 or later.     * __TCP__ : Route 53 tries to establish a TCP connection. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy> in the /Route 53 Developer Guide/ .
healthCheckConfig
    :: HealthCheckConfig
healthCheckConfig =
  HealthCheckConfig'
    { _hccFailureThreshold = Nothing
    , _hccResourcePath = Nothing
    , _hccType = Nothing
    }


-- | The number of consecutive health checks that an endpoint must pass or fail for Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy> in the /Route 53 Developer Guide/ .
hccFailureThreshold :: Lens' HealthCheckConfig (Maybe Natural)
hccFailureThreshold = lens _hccFailureThreshold (\ s a -> s{_hccFailureThreshold = a}) . mapping _Nat

-- | The path that you want Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, such as the file @/docs/route53-health-check.html@ . Route 53 automatically adds the DNS name for the service and a leading forward slash (@/@ ) character.
hccResourcePath :: Lens' HealthCheckConfig (Maybe Text)
hccResourcePath = lens _hccResourcePath (\ s a -> s{_hccResourcePath = a})

-- | The type of health check that you want to create, which indicates how Route 53 determines whether an endpoint is healthy. /Important:/ You can't change the value of @Type@ after you create a health check. You can create the following types of health checks:     * __HTTP__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTP request and waits for an HTTP status code of 200 or greater and less than 400.     * __HTTPS__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTPS request and waits for an HTTP status code of 200 or greater and less than 400. /Important:/ If you specify HTTPS for the value of @Type@ , the endpoint must support TLS v1.0 or later.     * __TCP__ : Route 53 tries to establish a TCP connection. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy> in the /Route 53 Developer Guide/ .
hccType :: Lens' HealthCheckConfig (Maybe HealthCheckType)
hccType = lens _hccType (\ s a -> s{_hccType = a})

instance FromJSON HealthCheckConfig where
        parseJSON
          = withObject "HealthCheckConfig"
              (\ x ->
                 HealthCheckConfig' <$>
                   (x .:? "FailureThreshold") <*> (x .:? "ResourcePath")
                     <*> (x .:? "Type"))

instance Hashable HealthCheckConfig where

instance NFData HealthCheckConfig where

instance ToJSON HealthCheckConfig where
        toJSON HealthCheckConfig'{..}
          = object
              (catMaybes
                 [("FailureThreshold" .=) <$> _hccFailureThreshold,
                  ("ResourcePath" .=) <$> _hccResourcePath,
                  ("Type" .=) <$> _hccType])

-- | /See:/ 'healthCheckCustomConfig' smart constructor.
newtype HealthCheckCustomConfig = HealthCheckCustomConfig'
  { _hcccFailureThreshold :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HealthCheckCustomConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcccFailureThreshold' - Undocumented member.
healthCheckCustomConfig
    :: HealthCheckCustomConfig
healthCheckCustomConfig =
  HealthCheckCustomConfig' {_hcccFailureThreshold = Nothing}


-- | Undocumented member.
hcccFailureThreshold :: Lens' HealthCheckCustomConfig (Maybe Natural)
hcccFailureThreshold = lens _hcccFailureThreshold (\ s a -> s{_hcccFailureThreshold = a}) . mapping _Nat

instance FromJSON HealthCheckCustomConfig where
        parseJSON
          = withObject "HealthCheckCustomConfig"
              (\ x ->
                 HealthCheckCustomConfig' <$>
                   (x .:? "FailureThreshold"))

instance Hashable HealthCheckCustomConfig where

instance NFData HealthCheckCustomConfig where

instance ToJSON HealthCheckCustomConfig where
        toJSON HealthCheckCustomConfig'{..}
          = object
              (catMaybes
                 [("FailureThreshold" .=) <$> _hcccFailureThreshold])

-- | A complex type that contains information about an instance that Amazon Route 53 creates when you submit a @RegisterInstance@ request.
--
--
--
-- /See:/ 'instance'' smart constructor.
data Instance = Instance'
  { _iCreatorRequestId :: !(Maybe Text)
  , _iAttributes       :: !(Maybe (Map Text Text))
  , _iId               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iCreatorRequestId' - A unique string that identifies the request and that allows failed @RegisterInstance@ requests to be retried without the risk of executing the operation twice. You must use a unique @CreatorRequestId@ string every time you submit a @RegisterInstance@ request if you're registering additional instances for the same namespace and service. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- * 'iAttributes' - A string map that contains the following information for the service that you specify in @ServiceId@ :     * The attributes that apply to the records that are defined in the service.      * For each attribute, the applicable value. Supported attribute keys include the following: __AWS_ALIAS_DNS_NAME__  ____  If you want Route 53 to create an alias record that routes traffic to an Elastic Load Balancing load balancer, specify the DNS name that is associated with the load balancer. For information about how to get the DNS name, see "DNSName" in the topic <http://docs.aws.amazon.com/http:/docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html AliasTarget> . Note the following:     * The configuration for the service that is specified by @ServiceId@ must include settings for an A record, an AAAA record, or both.     * In the service that is specified by @ServiceId@ , the value of @RoutingPolicy@ must be @WEIGHTED@ .     * If the service that is specified by @ServiceId@ includes @HealthCheckConfig@ settings, Route 53 will create the health check, but it won't associate the health check with the alias record.     * Auto naming currently doesn't support creating alias records that route traffic to AWS resources other than ELB load balancers.     * If you specify a value for @AWS_ALIAS_DNS_NAME@ , don't specify values for any of the @AWS_INSTANCE@ attributes. __AWS_INSTANCE_CNAME__  If the service configuration includes a CNAME record, the domain name that you want Route 53 to return in response to DNS queries, for example, @example.com@ . This value is required if the service specified by @ServiceId@ includes settings for an CNAME record. __AWS_INSTANCE_IPV4__  If the service configuration includes an A record, the IPv4 address that you want Route 53 to return in response to DNS queries, for example, @192.0.2.44@ . This value is required if the service specified by @ServiceId@ includes settings for an A record. If the service includes settings for an SRV record, you must specify a value for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both. __AWS_INSTANCE_IPV6__  If the service configuration includes an AAAA record, the IPv6 address that you want Route 53 to return in response to DNS queries, for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ . This value is required if the service specified by @ServiceId@ includes settings for an AAAA record. If the service includes settings for an SRV record, you must specify a value for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both. __AWS_INSTANCE_PORT__  If the service includes an SRV record, the value that you want Route 53 to return for the port. If the service includes @HealthCheckConfig@ , the port on the endpoint that you want Route 53 to send requests to.  This value is required if you specified settings for an SRV record when you created the service.
--
-- * 'iId' - An identifier that you want to associate with the instance. Note the following:     * If the service that is specified by @ServiceId@ includes settings for an SRV record, the value of @InstanceId@ is automatically included as part of the value for the SRV record. For more information, see 'DnsRecord$Type' .     * You can use this value to update an existing instance.     * To register a new instance, you must specify a value that is unique among instances that you register by using the same service.      * If you specify an existing @InstanceId@ and @ServiceId@ , Route 53 updates the existing records. If there's also an existing health check, Route 53 deletes the old health check and creates a new one.
instance'
    :: Text -- ^ 'iId'
    -> Instance
instance' pId_ =
  Instance' {_iCreatorRequestId = Nothing, _iAttributes = Nothing, _iId = pId_}


-- | A unique string that identifies the request and that allows failed @RegisterInstance@ requests to be retried without the risk of executing the operation twice. You must use a unique @CreatorRequestId@ string every time you submit a @RegisterInstance@ request if you're registering additional instances for the same namespace and service. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
iCreatorRequestId :: Lens' Instance (Maybe Text)
iCreatorRequestId = lens _iCreatorRequestId (\ s a -> s{_iCreatorRequestId = a})

-- | A string map that contains the following information for the service that you specify in @ServiceId@ :     * The attributes that apply to the records that are defined in the service.      * For each attribute, the applicable value. Supported attribute keys include the following: __AWS_ALIAS_DNS_NAME__  ____  If you want Route 53 to create an alias record that routes traffic to an Elastic Load Balancing load balancer, specify the DNS name that is associated with the load balancer. For information about how to get the DNS name, see "DNSName" in the topic <http://docs.aws.amazon.com/http:/docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html AliasTarget> . Note the following:     * The configuration for the service that is specified by @ServiceId@ must include settings for an A record, an AAAA record, or both.     * In the service that is specified by @ServiceId@ , the value of @RoutingPolicy@ must be @WEIGHTED@ .     * If the service that is specified by @ServiceId@ includes @HealthCheckConfig@ settings, Route 53 will create the health check, but it won't associate the health check with the alias record.     * Auto naming currently doesn't support creating alias records that route traffic to AWS resources other than ELB load balancers.     * If you specify a value for @AWS_ALIAS_DNS_NAME@ , don't specify values for any of the @AWS_INSTANCE@ attributes. __AWS_INSTANCE_CNAME__  If the service configuration includes a CNAME record, the domain name that you want Route 53 to return in response to DNS queries, for example, @example.com@ . This value is required if the service specified by @ServiceId@ includes settings for an CNAME record. __AWS_INSTANCE_IPV4__  If the service configuration includes an A record, the IPv4 address that you want Route 53 to return in response to DNS queries, for example, @192.0.2.44@ . This value is required if the service specified by @ServiceId@ includes settings for an A record. If the service includes settings for an SRV record, you must specify a value for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both. __AWS_INSTANCE_IPV6__  If the service configuration includes an AAAA record, the IPv6 address that you want Route 53 to return in response to DNS queries, for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ . This value is required if the service specified by @ServiceId@ includes settings for an AAAA record. If the service includes settings for an SRV record, you must specify a value for @AWS_INSTANCE_IPV4@ , @AWS_INSTANCE_IPV6@ , or both. __AWS_INSTANCE_PORT__  If the service includes an SRV record, the value that you want Route 53 to return for the port. If the service includes @HealthCheckConfig@ , the port on the endpoint that you want Route 53 to send requests to.  This value is required if you specified settings for an SRV record when you created the service.
iAttributes :: Lens' Instance (HashMap Text Text)
iAttributes = lens _iAttributes (\ s a -> s{_iAttributes = a}) . _Default . _Map

-- | An identifier that you want to associate with the instance. Note the following:     * If the service that is specified by @ServiceId@ includes settings for an SRV record, the value of @InstanceId@ is automatically included as part of the value for the SRV record. For more information, see 'DnsRecord$Type' .     * You can use this value to update an existing instance.     * To register a new instance, you must specify a value that is unique among instances that you register by using the same service.      * If you specify an existing @InstanceId@ and @ServiceId@ , Route 53 updates the existing records. If there's also an existing health check, Route 53 deletes the old health check and creates a new one.
iId :: Lens' Instance Text
iId = lens _iId (\ s a -> s{_iId = a})

instance FromJSON Instance where
        parseJSON
          = withObject "Instance"
              (\ x ->
                 Instance' <$>
                   (x .:? "CreatorRequestId") <*>
                     (x .:? "Attributes" .!= mempty)
                     <*> (x .: "Id"))

instance Hashable Instance where

instance NFData Instance where

-- | A complex type that contains information about the instances that you registered by using a specified service.
--
--
--
-- /See:/ 'instanceSummary' smart constructor.
data InstanceSummary = InstanceSummary'
  { _isAttributes :: !(Maybe (Map Text Text))
  , _isId         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isAttributes' - A string map that contains the following information:     * The attributes that are associate with the instance.      * For each attribute, the applicable value. Supported attribute keys include the following:     * @AWS_ALIAS_DNS_NAME@ : For an alias record that routes traffic to an Elastic Load Balancing load balancer, the DNS name that is associated with the load balancer.      * @AWS_INSTANCE_CNAME@ : For a CNAME record, the domain name that Route 53 returns in response to DNS queries, for example, @example.com@ .     * @AWS_INSTANCE_IPV4@ : For an A record, the IPv4 address that Route 53 returns in response to DNS queries, for example, @192.0.2.44@ .     * @AWS_INSTANCE_IPV6@ : For an AAAA record, the IPv6 address that Route 53 returns in response to DNS queries, for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ .     * @AWS_INSTANCE_PORT@ : For an SRV record, the value that Route 53 returns for the port. In addition, if the service includes @HealthCheckConfig@ , the port on the endpoint that Route 53 sends requests to.
--
-- * 'isId' - The ID for an instance that you created by using a specified service.
instanceSummary
    :: InstanceSummary
instanceSummary = InstanceSummary' {_isAttributes = Nothing, _isId = Nothing}


-- | A string map that contains the following information:     * The attributes that are associate with the instance.      * For each attribute, the applicable value. Supported attribute keys include the following:     * @AWS_ALIAS_DNS_NAME@ : For an alias record that routes traffic to an Elastic Load Balancing load balancer, the DNS name that is associated with the load balancer.      * @AWS_INSTANCE_CNAME@ : For a CNAME record, the domain name that Route 53 returns in response to DNS queries, for example, @example.com@ .     * @AWS_INSTANCE_IPV4@ : For an A record, the IPv4 address that Route 53 returns in response to DNS queries, for example, @192.0.2.44@ .     * @AWS_INSTANCE_IPV6@ : For an AAAA record, the IPv6 address that Route 53 returns in response to DNS queries, for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ .     * @AWS_INSTANCE_PORT@ : For an SRV record, the value that Route 53 returns for the port. In addition, if the service includes @HealthCheckConfig@ , the port on the endpoint that Route 53 sends requests to.
isAttributes :: Lens' InstanceSummary (HashMap Text Text)
isAttributes = lens _isAttributes (\ s a -> s{_isAttributes = a}) . _Default . _Map

-- | The ID for an instance that you created by using a specified service.
isId :: Lens' InstanceSummary (Maybe Text)
isId = lens _isId (\ s a -> s{_isId = a})

instance FromJSON InstanceSummary where
        parseJSON
          = withObject "InstanceSummary"
              (\ x ->
                 InstanceSummary' <$>
                   (x .:? "Attributes" .!= mempty) <*> (x .:? "Id"))

instance Hashable InstanceSummary where

instance NFData InstanceSummary where

-- | A complex type that contains information about a specified namespace.
--
--
--
-- /See:/ 'namespace' smart constructor.
data Namespace = Namespace'
  { _nARN              :: !(Maybe Text)
  , _nCreatorRequestId :: !(Maybe Text)
  , _nCreateDate       :: !(Maybe POSIX)
  , _nServiceCount     :: !(Maybe Int)
  , _nName             :: !(Maybe Text)
  , _nId               :: !(Maybe Text)
  , _nType             :: !(Maybe NamespaceType)
  , _nDescription      :: !(Maybe Text)
  , _nProperties       :: !(Maybe NamespaceProperties)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Namespace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nARN' - The Amazon Resource Name (ARN) that Route 53 assigns to the namespace when you create it.
--
-- * 'nCreatorRequestId' - A unique string that identifies the request and that allows failed requests to be retried without the risk of executing an operation twice.
--
-- * 'nCreateDate' - The date that the namespace was created, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'nServiceCount' - The number of services that are associated with the namespace.
--
-- * 'nName' - The name of the namespace, such as @example.com@ .
--
-- * 'nId' - The ID of a namespace.
--
-- * 'nType' - The type of the namespace. Valid values are @DNS_PUBLIC@ and @DNS_PRIVATE@ .
--
-- * 'nDescription' - The description that you specify for the namespace when you create it.
--
-- * 'nProperties' - A complex type that contains information that's specific to the type of the namespace.
namespace
    :: Namespace
namespace =
  Namespace'
    { _nARN = Nothing
    , _nCreatorRequestId = Nothing
    , _nCreateDate = Nothing
    , _nServiceCount = Nothing
    , _nName = Nothing
    , _nId = Nothing
    , _nType = Nothing
    , _nDescription = Nothing
    , _nProperties = Nothing
    }


-- | The Amazon Resource Name (ARN) that Route 53 assigns to the namespace when you create it.
nARN :: Lens' Namespace (Maybe Text)
nARN = lens _nARN (\ s a -> s{_nARN = a})

-- | A unique string that identifies the request and that allows failed requests to be retried without the risk of executing an operation twice.
nCreatorRequestId :: Lens' Namespace (Maybe Text)
nCreatorRequestId = lens _nCreatorRequestId (\ s a -> s{_nCreatorRequestId = a})

-- | The date that the namespace was created, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
nCreateDate :: Lens' Namespace (Maybe UTCTime)
nCreateDate = lens _nCreateDate (\ s a -> s{_nCreateDate = a}) . mapping _Time

-- | The number of services that are associated with the namespace.
nServiceCount :: Lens' Namespace (Maybe Int)
nServiceCount = lens _nServiceCount (\ s a -> s{_nServiceCount = a})

-- | The name of the namespace, such as @example.com@ .
nName :: Lens' Namespace (Maybe Text)
nName = lens _nName (\ s a -> s{_nName = a})

-- | The ID of a namespace.
nId :: Lens' Namespace (Maybe Text)
nId = lens _nId (\ s a -> s{_nId = a})

-- | The type of the namespace. Valid values are @DNS_PUBLIC@ and @DNS_PRIVATE@ .
nType :: Lens' Namespace (Maybe NamespaceType)
nType = lens _nType (\ s a -> s{_nType = a})

-- | The description that you specify for the namespace when you create it.
nDescription :: Lens' Namespace (Maybe Text)
nDescription = lens _nDescription (\ s a -> s{_nDescription = a})

-- | A complex type that contains information that's specific to the type of the namespace.
nProperties :: Lens' Namespace (Maybe NamespaceProperties)
nProperties = lens _nProperties (\ s a -> s{_nProperties = a})

instance FromJSON Namespace where
        parseJSON
          = withObject "Namespace"
              (\ x ->
                 Namespace' <$>
                   (x .:? "Arn") <*> (x .:? "CreatorRequestId") <*>
                     (x .:? "CreateDate")
                     <*> (x .:? "ServiceCount")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "Type")
                     <*> (x .:? "Description")
                     <*> (x .:? "Properties"))

instance Hashable Namespace where

instance NFData Namespace where

-- | A complex type that identifies the namespaces that you want to list. You can choose to list public or private namespaces.
--
--
--
-- /See:/ 'namespaceFilter' smart constructor.
data NamespaceFilter = NamespaceFilter'
  { _nfCondition :: !(Maybe FilterCondition)
  , _nfName      :: !NamespaceFilterName
  , _nfValues    :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NamespaceFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nfCondition' - The operator that you want to use to determine whether @ListNamespaces@ returns a namespace. Valid values for @condition@ include:     * @EQ@ : When you specify @EQ@ for the condition, you can choose to list only public namespaces or private namespaces, but not both. @EQ@ is the default condition and can be omitted.     * @IN@ : When you specify @IN@ for the condition, you can choose to list public namespaces, private namespaces, or both.      * @BETWEEN@ : Not applicable
--
-- * 'nfName' - Specify @TYPE@ .
--
-- * 'nfValues' - If you specify @EQ@ for @Condition@ , specify either @DNS_PUBLIC@ or @DNS_PRIVATE@ . If you specify @IN@ for @Condition@ , you can specify @DNS_PUBLIC@ , @DNS_PRIVATE@ , or both.
namespaceFilter
    :: NamespaceFilterName -- ^ 'nfName'
    -> NamespaceFilter
namespaceFilter pName_ =
  NamespaceFilter'
    {_nfCondition = Nothing, _nfName = pName_, _nfValues = mempty}


-- | The operator that you want to use to determine whether @ListNamespaces@ returns a namespace. Valid values for @condition@ include:     * @EQ@ : When you specify @EQ@ for the condition, you can choose to list only public namespaces or private namespaces, but not both. @EQ@ is the default condition and can be omitted.     * @IN@ : When you specify @IN@ for the condition, you can choose to list public namespaces, private namespaces, or both.      * @BETWEEN@ : Not applicable
nfCondition :: Lens' NamespaceFilter (Maybe FilterCondition)
nfCondition = lens _nfCondition (\ s a -> s{_nfCondition = a})

-- | Specify @TYPE@ .
nfName :: Lens' NamespaceFilter NamespaceFilterName
nfName = lens _nfName (\ s a -> s{_nfName = a})

-- | If you specify @EQ@ for @Condition@ , specify either @DNS_PUBLIC@ or @DNS_PRIVATE@ . If you specify @IN@ for @Condition@ , you can specify @DNS_PUBLIC@ , @DNS_PRIVATE@ , or both.
nfValues :: Lens' NamespaceFilter [Text]
nfValues = lens _nfValues (\ s a -> s{_nfValues = a}) . _Coerce

instance Hashable NamespaceFilter where

instance NFData NamespaceFilter where

instance ToJSON NamespaceFilter where
        toJSON NamespaceFilter'{..}
          = object
              (catMaybes
                 [("Condition" .=) <$> _nfCondition,
                  Just ("Name" .= _nfName),
                  Just ("Values" .= _nfValues)])

-- | A complex type that contains information that is specific to the namespace type.
--
--
--
-- /See:/ 'namespaceProperties' smart constructor.
newtype NamespaceProperties = NamespaceProperties'
  { _npDNSProperties :: Maybe DNSProperties
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NamespaceProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'npDNSProperties' - A complex type that contains the ID for the hosted zone that Route 53 creates when you create a namespace.
namespaceProperties
    :: NamespaceProperties
namespaceProperties = NamespaceProperties' {_npDNSProperties = Nothing}


-- | A complex type that contains the ID for the hosted zone that Route 53 creates when you create a namespace.
npDNSProperties :: Lens' NamespaceProperties (Maybe DNSProperties)
npDNSProperties = lens _npDNSProperties (\ s a -> s{_npDNSProperties = a})

instance FromJSON NamespaceProperties where
        parseJSON
          = withObject "NamespaceProperties"
              (\ x ->
                 NamespaceProperties' <$> (x .:? "DnsProperties"))

instance Hashable NamespaceProperties where

instance NFData NamespaceProperties where

-- | A complex type that contains information about a namespace.
--
--
--
-- /See:/ 'namespaceSummary' smart constructor.
data NamespaceSummary = NamespaceSummary'
  { _nsARN  :: !(Maybe Text)
  , _nsName :: !(Maybe Text)
  , _nsId   :: !(Maybe Text)
  , _nsType :: !(Maybe NamespaceType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NamespaceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nsARN' - The Amazon Resource Name (ARN) that Route 53 assigns to the namespace when you create it.
--
-- * 'nsName' - The name of the namespace. When you create a namespace, Route 53 automatically creates a hosted zone that has the same name as the namespace.
--
-- * 'nsId' - The ID of the namespace.
--
-- * 'nsType' - The type of the namespace, either public or private.
namespaceSummary
    :: NamespaceSummary
namespaceSummary =
  NamespaceSummary'
    {_nsARN = Nothing, _nsName = Nothing, _nsId = Nothing, _nsType = Nothing}


-- | The Amazon Resource Name (ARN) that Route 53 assigns to the namespace when you create it.
nsARN :: Lens' NamespaceSummary (Maybe Text)
nsARN = lens _nsARN (\ s a -> s{_nsARN = a})

-- | The name of the namespace. When you create a namespace, Route 53 automatically creates a hosted zone that has the same name as the namespace.
nsName :: Lens' NamespaceSummary (Maybe Text)
nsName = lens _nsName (\ s a -> s{_nsName = a})

-- | The ID of the namespace.
nsId :: Lens' NamespaceSummary (Maybe Text)
nsId = lens _nsId (\ s a -> s{_nsId = a})

-- | The type of the namespace, either public or private.
nsType :: Lens' NamespaceSummary (Maybe NamespaceType)
nsType = lens _nsType (\ s a -> s{_nsType = a})

instance FromJSON NamespaceSummary where
        parseJSON
          = withObject "NamespaceSummary"
              (\ x ->
                 NamespaceSummary' <$>
                   (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Id") <*>
                     (x .:? "Type"))

instance Hashable NamespaceSummary where

instance NFData NamespaceSummary where

-- | A complex type that contains information about a specified operation.
--
--
--
-- /See:/ 'operation' smart constructor.
data Operation = Operation'
  { _oStatus       :: !(Maybe OperationStatus)
  , _oUpdateDate   :: !(Maybe POSIX)
  , _oCreateDate   :: !(Maybe POSIX)
  , _oTargets      :: !(Maybe (Map OperationTargetType Text))
  , _oErrorCode    :: !(Maybe Text)
  , _oId           :: !(Maybe Text)
  , _oType         :: !(Maybe OperationType)
  , _oErrorMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Operation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oStatus' - The status of the operation. Values include the following:     * __SUBMITTED__ : This is the initial state immediately after you submit a request.     * __PENDING__ : Route 53 is performing the operation.     * __SUCCESS__ : The operation succeeded.     * __FAIL__ : The operation failed. For the failure reason, see @ErrorMessage@ .
--
-- * 'oUpdateDate' - The date and time that the value of @Status@ changed to the current value, in Unix date/time format and Coordinated Universal Time (UTC). The value of @UpdateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'oCreateDate' - The date and time that the request was submitted, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'oTargets' - The name of the target entity that is associated with the operation:     * __NAMESPACE__ : The namespace ID is returned in the @ResourceId@ property.     * __SERVICE__ : The service ID is returned in the @ResourceId@ property.     * __INSTANCE__ : The instance ID is returned in the @ResourceId@ property.
--
-- * 'oErrorCode' - The code associated with @ErrorMessage@ . Values for @ErrorCode@ include the following:     * @ACCESS_DENIED@      * @CANNOT_CREATE_HOSTED_ZONE@      * @EXPIRED_TOKEN@      * @HOSTED_ZONE_NOT_FOUND@      * @INTERNAL_FAILURE@      * @INVALID_CHANGE_BATCH@      * @THROTTLED_REQUEST@
--
-- * 'oId' - The ID of the operation that you want to get information about.
--
-- * 'oType' - The name of the operation that is associated with the specified ID.
--
-- * 'oErrorMessage' - If the value of @Status@ is @FAIL@ , the reason that the operation failed.
operation
    :: Operation
operation =
  Operation'
    { _oStatus = Nothing
    , _oUpdateDate = Nothing
    , _oCreateDate = Nothing
    , _oTargets = Nothing
    , _oErrorCode = Nothing
    , _oId = Nothing
    , _oType = Nothing
    , _oErrorMessage = Nothing
    }


-- | The status of the operation. Values include the following:     * __SUBMITTED__ : This is the initial state immediately after you submit a request.     * __PENDING__ : Route 53 is performing the operation.     * __SUCCESS__ : The operation succeeded.     * __FAIL__ : The operation failed. For the failure reason, see @ErrorMessage@ .
oStatus :: Lens' Operation (Maybe OperationStatus)
oStatus = lens _oStatus (\ s a -> s{_oStatus = a})

-- | The date and time that the value of @Status@ changed to the current value, in Unix date/time format and Coordinated Universal Time (UTC). The value of @UpdateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
oUpdateDate :: Lens' Operation (Maybe UTCTime)
oUpdateDate = lens _oUpdateDate (\ s a -> s{_oUpdateDate = a}) . mapping _Time

-- | The date and time that the request was submitted, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
oCreateDate :: Lens' Operation (Maybe UTCTime)
oCreateDate = lens _oCreateDate (\ s a -> s{_oCreateDate = a}) . mapping _Time

-- | The name of the target entity that is associated with the operation:     * __NAMESPACE__ : The namespace ID is returned in the @ResourceId@ property.     * __SERVICE__ : The service ID is returned in the @ResourceId@ property.     * __INSTANCE__ : The instance ID is returned in the @ResourceId@ property.
oTargets :: Lens' Operation (HashMap OperationTargetType Text)
oTargets = lens _oTargets (\ s a -> s{_oTargets = a}) . _Default . _Map

-- | The code associated with @ErrorMessage@ . Values for @ErrorCode@ include the following:     * @ACCESS_DENIED@      * @CANNOT_CREATE_HOSTED_ZONE@      * @EXPIRED_TOKEN@      * @HOSTED_ZONE_NOT_FOUND@      * @INTERNAL_FAILURE@      * @INVALID_CHANGE_BATCH@      * @THROTTLED_REQUEST@
oErrorCode :: Lens' Operation (Maybe Text)
oErrorCode = lens _oErrorCode (\ s a -> s{_oErrorCode = a})

-- | The ID of the operation that you want to get information about.
oId :: Lens' Operation (Maybe Text)
oId = lens _oId (\ s a -> s{_oId = a})

-- | The name of the operation that is associated with the specified ID.
oType :: Lens' Operation (Maybe OperationType)
oType = lens _oType (\ s a -> s{_oType = a})

-- | If the value of @Status@ is @FAIL@ , the reason that the operation failed.
oErrorMessage :: Lens' Operation (Maybe Text)
oErrorMessage = lens _oErrorMessage (\ s a -> s{_oErrorMessage = a})

instance FromJSON Operation where
        parseJSON
          = withObject "Operation"
              (\ x ->
                 Operation' <$>
                   (x .:? "Status") <*> (x .:? "UpdateDate") <*>
                     (x .:? "CreateDate")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "ErrorCode")
                     <*> (x .:? "Id")
                     <*> (x .:? "Type")
                     <*> (x .:? "ErrorMessage"))

instance Hashable Operation where

instance NFData Operation where

-- | A complex type that lets you select the operations that you want to list.
--
--
--
-- /See:/ 'operationFilter' smart constructor.
data OperationFilter = OperationFilter'
  { _ofCondition :: !(Maybe FilterCondition)
  , _ofName      :: !OperationFilterName
  , _ofValues    :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OperationFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ofCondition' - The operator that you want to use to determine whether an operation matches the specified value. Valid values for condition include:     * @EQ@ : When you specify @EQ@ for the condition, you can specify only one value. @EQ@ is supported for @NAMESPACE_ID@ , @SERVICE_ID@ , @STATUS@ , and @TYPE@ . @EQ@ is the default condition and can be omitted.     * @IN@ : When you specify @IN@ for the condition, you can specify a list of one or more values. @IN@ is supported for @STATUS@ and @TYPE@ . An operation must match one of the specified values to be returned in the response.     * @BETWEEN@ : Specify a start date and an end date in Unix date/time format and Coordinated Universal Time (UTC). The start date must be the first value. @BETWEEN@ is supported for @UPDATE_DATE@ .
--
-- * 'ofName' - Specify the operations that you want to get:     * __NAMESPACE_ID__ : Gets operations related to specified namespaces.     * __SERVICE_ID__ : Gets operations related to specified services.     * __STATUS__ : Gets operations based on the status of the operations: @SUBMITTED@ , @PENDING@ , @SUCCEED@ , or @FAIL@ .     * __TYPE__ : Gets specified types of operation.     * __UPDATE_DATE__ : Gets operations that changed status during a specified date/time range.
--
-- * 'ofValues' - Specify values that are applicable to the value that you specify for @Name@ :      * __NAMESPACE_ID__ : Specify one namespace ID.     * __SERVICE_ID__ : Specify one service ID.     * __STATUS__ : Specify one or more statuses: @SUBMITTED@ , @PENDING@ , @SUCCEED@ , or @FAIL@ .     * __TYPE__ : Specify one or more of the following types: @CREATE_NAMESPACE@ , @DELETE_NAMESPACE@ , @UPDATE_SERVICE@ , @REGISTER_INSTANCE@ , or @DEREGISTER_INSTANCE@ .     * __UPDATE_DATE__ : Specify a start date and an end date in Unix date/time format and Coordinated Universal Time (UTC). The start date must be the first value.
operationFilter
    :: OperationFilterName -- ^ 'ofName'
    -> OperationFilter
operationFilter pName_ =
  OperationFilter'
    {_ofCondition = Nothing, _ofName = pName_, _ofValues = mempty}


-- | The operator that you want to use to determine whether an operation matches the specified value. Valid values for condition include:     * @EQ@ : When you specify @EQ@ for the condition, you can specify only one value. @EQ@ is supported for @NAMESPACE_ID@ , @SERVICE_ID@ , @STATUS@ , and @TYPE@ . @EQ@ is the default condition and can be omitted.     * @IN@ : When you specify @IN@ for the condition, you can specify a list of one or more values. @IN@ is supported for @STATUS@ and @TYPE@ . An operation must match one of the specified values to be returned in the response.     * @BETWEEN@ : Specify a start date and an end date in Unix date/time format and Coordinated Universal Time (UTC). The start date must be the first value. @BETWEEN@ is supported for @UPDATE_DATE@ .
ofCondition :: Lens' OperationFilter (Maybe FilterCondition)
ofCondition = lens _ofCondition (\ s a -> s{_ofCondition = a})

-- | Specify the operations that you want to get:     * __NAMESPACE_ID__ : Gets operations related to specified namespaces.     * __SERVICE_ID__ : Gets operations related to specified services.     * __STATUS__ : Gets operations based on the status of the operations: @SUBMITTED@ , @PENDING@ , @SUCCEED@ , or @FAIL@ .     * __TYPE__ : Gets specified types of operation.     * __UPDATE_DATE__ : Gets operations that changed status during a specified date/time range.
ofName :: Lens' OperationFilter OperationFilterName
ofName = lens _ofName (\ s a -> s{_ofName = a})

-- | Specify values that are applicable to the value that you specify for @Name@ :      * __NAMESPACE_ID__ : Specify one namespace ID.     * __SERVICE_ID__ : Specify one service ID.     * __STATUS__ : Specify one or more statuses: @SUBMITTED@ , @PENDING@ , @SUCCEED@ , or @FAIL@ .     * __TYPE__ : Specify one or more of the following types: @CREATE_NAMESPACE@ , @DELETE_NAMESPACE@ , @UPDATE_SERVICE@ , @REGISTER_INSTANCE@ , or @DEREGISTER_INSTANCE@ .     * __UPDATE_DATE__ : Specify a start date and an end date in Unix date/time format and Coordinated Universal Time (UTC). The start date must be the first value.
ofValues :: Lens' OperationFilter [Text]
ofValues = lens _ofValues (\ s a -> s{_ofValues = a}) . _Coerce

instance Hashable OperationFilter where

instance NFData OperationFilter where

instance ToJSON OperationFilter where
        toJSON OperationFilter'{..}
          = object
              (catMaybes
                 [("Condition" .=) <$> _ofCondition,
                  Just ("Name" .= _ofName),
                  Just ("Values" .= _ofValues)])

-- | A complex type that contains information about an operation that matches the criteria that you specified in a 'ListOperations' request.
--
--
--
-- /See:/ 'operationSummary' smart constructor.
data OperationSummary = OperationSummary'
  { _osStatus :: !(Maybe OperationStatus)
  , _osId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OperationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osStatus' - The status of the operation. Values include the following:     * __SUBMITTED__ : This is the initial state immediately after you submit a request.     * __PENDING__ : Route 53 is performing the operation.     * __SUCCESS__ : The operation succeeded.     * __FAIL__ : The operation failed. For the failure reason, see @ErrorMessage@ .
--
-- * 'osId' - The ID for an operation.
operationSummary
    :: OperationSummary
operationSummary = OperationSummary' {_osStatus = Nothing, _osId = Nothing}


-- | The status of the operation. Values include the following:     * __SUBMITTED__ : This is the initial state immediately after you submit a request.     * __PENDING__ : Route 53 is performing the operation.     * __SUCCESS__ : The operation succeeded.     * __FAIL__ : The operation failed. For the failure reason, see @ErrorMessage@ .
osStatus :: Lens' OperationSummary (Maybe OperationStatus)
osStatus = lens _osStatus (\ s a -> s{_osStatus = a})

-- | The ID for an operation.
osId :: Lens' OperationSummary (Maybe Text)
osId = lens _osId (\ s a -> s{_osId = a})

instance FromJSON OperationSummary where
        parseJSON
          = withObject "OperationSummary"
              (\ x ->
                 OperationSummary' <$>
                   (x .:? "Status") <*> (x .:? "Id"))

instance Hashable OperationSummary where

instance NFData OperationSummary where

-- | A complex type that contains changes to an existing service.
--
--
--
-- /See:/ 'serviceChange' smart constructor.
data ServiceChange = ServiceChange'
  { _scHealthCheckConfig :: !(Maybe HealthCheckConfig)
  , _scDescription       :: !(Maybe Text)
  , _scDNSConfig         :: !DNSConfigChange
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scHealthCheckConfig' - Undocumented member.
--
-- * 'scDescription' - A description for the service.
--
-- * 'scDNSConfig' - A complex type that contains information about the records that you want Route 53 to create when you register an instance.
serviceChange
    :: DNSConfigChange -- ^ 'scDNSConfig'
    -> ServiceChange
serviceChange pDNSConfig_ =
  ServiceChange'
    { _scHealthCheckConfig = Nothing
    , _scDescription = Nothing
    , _scDNSConfig = pDNSConfig_
    }


-- | Undocumented member.
scHealthCheckConfig :: Lens' ServiceChange (Maybe HealthCheckConfig)
scHealthCheckConfig = lens _scHealthCheckConfig (\ s a -> s{_scHealthCheckConfig = a})

-- | A description for the service.
scDescription :: Lens' ServiceChange (Maybe Text)
scDescription = lens _scDescription (\ s a -> s{_scDescription = a})

-- | A complex type that contains information about the records that you want Route 53 to create when you register an instance.
scDNSConfig :: Lens' ServiceChange DNSConfigChange
scDNSConfig = lens _scDNSConfig (\ s a -> s{_scDNSConfig = a})

instance Hashable ServiceChange where

instance NFData ServiceChange where

instance ToJSON ServiceChange where
        toJSON ServiceChange'{..}
          = object
              (catMaybes
                 [("HealthCheckConfig" .=) <$> _scHealthCheckConfig,
                  ("Description" .=) <$> _scDescription,
                  Just ("DnsConfig" .= _scDNSConfig)])

-- | A complex type that lets you specify the namespaces that you want to list services for.
--
--
--
-- /See:/ 'serviceFilter' smart constructor.
data ServiceFilter = ServiceFilter'
  { _sfCondition :: !(Maybe FilterCondition)
  , _sfName      :: !ServiceFilterName
  , _sfValues    :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfCondition' - The operator that you want to use to determine whether a service is returned by @ListServices@ . Valid values for @Condition@ include the following:     * @EQ@ : When you specify @EQ@ , specify one namespace ID for @Values@ . @EQ@ is the default condition and can be omitted.     * @IN@ : When you specify @IN@ , specify a list of the IDs for the namespaces that you want @ListServices@ to return a list of services for.     * @BETWEEN@ : Not applicable.
--
-- * 'sfName' - Specify @NAMESPACE_ID@ .
--
-- * 'sfValues' - The values that are applicable to the value that you specify for @Condition@ to filter the list of services.
serviceFilter
    :: ServiceFilterName -- ^ 'sfName'
    -> ServiceFilter
serviceFilter pName_ =
  ServiceFilter' {_sfCondition = Nothing, _sfName = pName_, _sfValues = mempty}


-- | The operator that you want to use to determine whether a service is returned by @ListServices@ . Valid values for @Condition@ include the following:     * @EQ@ : When you specify @EQ@ , specify one namespace ID for @Values@ . @EQ@ is the default condition and can be omitted.     * @IN@ : When you specify @IN@ , specify a list of the IDs for the namespaces that you want @ListServices@ to return a list of services for.     * @BETWEEN@ : Not applicable.
sfCondition :: Lens' ServiceFilter (Maybe FilterCondition)
sfCondition = lens _sfCondition (\ s a -> s{_sfCondition = a})

-- | Specify @NAMESPACE_ID@ .
sfName :: Lens' ServiceFilter ServiceFilterName
sfName = lens _sfName (\ s a -> s{_sfName = a})

-- | The values that are applicable to the value that you specify for @Condition@ to filter the list of services.
sfValues :: Lens' ServiceFilter [Text]
sfValues = lens _sfValues (\ s a -> s{_sfValues = a}) . _Coerce

instance Hashable ServiceFilter where

instance NFData ServiceFilter where

instance ToJSON ServiceFilter where
        toJSON ServiceFilter'{..}
          = object
              (catMaybes
                 [("Condition" .=) <$> _sfCondition,
                  Just ("Name" .= _sfName),
                  Just ("Values" .= _sfValues)])

-- | A complex type that contains information about the specified service.
--
--
--
-- /See:/ 'serviceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { _siInstanceCount           :: !(Maybe Int)
  , _siARN                     :: !(Maybe Text)
  , _siHealthCheckConfig       :: !(Maybe HealthCheckConfig)
  , _siCreatorRequestId        :: !(Maybe Text)
  , _siCreateDate              :: !(Maybe POSIX)
  , _siHealthCheckCustomConfig :: !(Maybe HealthCheckCustomConfig)
  , _siName                    :: !(Maybe Text)
  , _siId                      :: !(Maybe Text)
  , _siDNSConfig               :: !(Maybe DNSConfig)
  , _siDescription             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siInstanceCount' - The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count.
--
-- * 'siARN' - The Amazon Resource Name (ARN) that Route 53 assigns to the service when you create it.
--
-- * 'siHealthCheckConfig' - /Public DNS namespaces only./ A complex type that contains settings for an optional health check. If you specify settings for a health check, Route 53 associates the health check with all the records that you specify in @DnsConfig@ . For information about the charges for health checks, see <http://aws.amazon.com/route53/pricing Route 53 Pricing> .
--
-- * 'siCreatorRequestId' - A unique string that identifies the request and that allows failed requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- * 'siCreateDate' - The date and time that the service was created, in Unix format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'siHealthCheckCustomConfig' - Undocumented member.
--
-- * 'siName' - The name of the service.
--
-- * 'siId' - The ID that Route 53 assigned to the service when you created it.
--
-- * 'siDNSConfig' - A complex type that contains information about the records that you want Route 53 to create when you register an instance.
--
-- * 'siDescription' - The description of the service.
serviceInfo
    :: ServiceInfo
serviceInfo =
  ServiceInfo'
    { _siInstanceCount = Nothing
    , _siARN = Nothing
    , _siHealthCheckConfig = Nothing
    , _siCreatorRequestId = Nothing
    , _siCreateDate = Nothing
    , _siHealthCheckCustomConfig = Nothing
    , _siName = Nothing
    , _siId = Nothing
    , _siDNSConfig = Nothing
    , _siDescription = Nothing
    }


-- | The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count.
siInstanceCount :: Lens' ServiceInfo (Maybe Int)
siInstanceCount = lens _siInstanceCount (\ s a -> s{_siInstanceCount = a})

-- | The Amazon Resource Name (ARN) that Route 53 assigns to the service when you create it.
siARN :: Lens' ServiceInfo (Maybe Text)
siARN = lens _siARN (\ s a -> s{_siARN = a})

-- | /Public DNS namespaces only./ A complex type that contains settings for an optional health check. If you specify settings for a health check, Route 53 associates the health check with all the records that you specify in @DnsConfig@ . For information about the charges for health checks, see <http://aws.amazon.com/route53/pricing Route 53 Pricing> .
siHealthCheckConfig :: Lens' ServiceInfo (Maybe HealthCheckConfig)
siHealthCheckConfig = lens _siHealthCheckConfig (\ s a -> s{_siHealthCheckConfig = a})

-- | A unique string that identifies the request and that allows failed requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
siCreatorRequestId :: Lens' ServiceInfo (Maybe Text)
siCreatorRequestId = lens _siCreatorRequestId (\ s a -> s{_siCreatorRequestId = a})

-- | The date and time that the service was created, in Unix format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
siCreateDate :: Lens' ServiceInfo (Maybe UTCTime)
siCreateDate = lens _siCreateDate (\ s a -> s{_siCreateDate = a}) . mapping _Time

-- | Undocumented member.
siHealthCheckCustomConfig :: Lens' ServiceInfo (Maybe HealthCheckCustomConfig)
siHealthCheckCustomConfig = lens _siHealthCheckCustomConfig (\ s a -> s{_siHealthCheckCustomConfig = a})

-- | The name of the service.
siName :: Lens' ServiceInfo (Maybe Text)
siName = lens _siName (\ s a -> s{_siName = a})

-- | The ID that Route 53 assigned to the service when you created it.
siId :: Lens' ServiceInfo (Maybe Text)
siId = lens _siId (\ s a -> s{_siId = a})

-- | A complex type that contains information about the records that you want Route 53 to create when you register an instance.
siDNSConfig :: Lens' ServiceInfo (Maybe DNSConfig)
siDNSConfig = lens _siDNSConfig (\ s a -> s{_siDNSConfig = a})

-- | The description of the service.
siDescription :: Lens' ServiceInfo (Maybe Text)
siDescription = lens _siDescription (\ s a -> s{_siDescription = a})

instance FromJSON ServiceInfo where
        parseJSON
          = withObject "ServiceInfo"
              (\ x ->
                 ServiceInfo' <$>
                   (x .:? "InstanceCount") <*> (x .:? "Arn") <*>
                     (x .:? "HealthCheckConfig")
                     <*> (x .:? "CreatorRequestId")
                     <*> (x .:? "CreateDate")
                     <*> (x .:? "HealthCheckCustomConfig")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "DnsConfig")
                     <*> (x .:? "Description"))

instance Hashable ServiceInfo where

instance NFData ServiceInfo where

-- | A complex type that contains information about a specified service.
--
--
--
-- /See:/ 'serviceSummary' smart constructor.
data ServiceSummary = ServiceSummary'
  { _ssInstanceCount :: !(Maybe Int)
  , _ssARN           :: !(Maybe Text)
  , _ssName          :: !(Maybe Text)
  , _ssId            :: !(Maybe Text)
  , _ssDescription   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssInstanceCount' - The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count.
--
-- * 'ssARN' - The Amazon Resource Name (ARN) that Route 53 assigns to the service when you create it.
--
-- * 'ssName' - The name of the service.
--
-- * 'ssId' - The ID that Route 53 assigned to the service when you created it.
--
-- * 'ssDescription' - The description that you specify when you create the service.
serviceSummary
    :: ServiceSummary
serviceSummary =
  ServiceSummary'
    { _ssInstanceCount = Nothing
    , _ssARN = Nothing
    , _ssName = Nothing
    , _ssId = Nothing
    , _ssDescription = Nothing
    }


-- | The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count.
ssInstanceCount :: Lens' ServiceSummary (Maybe Int)
ssInstanceCount = lens _ssInstanceCount (\ s a -> s{_ssInstanceCount = a})

-- | The Amazon Resource Name (ARN) that Route 53 assigns to the service when you create it.
ssARN :: Lens' ServiceSummary (Maybe Text)
ssARN = lens _ssARN (\ s a -> s{_ssARN = a})

-- | The name of the service.
ssName :: Lens' ServiceSummary (Maybe Text)
ssName = lens _ssName (\ s a -> s{_ssName = a})

-- | The ID that Route 53 assigned to the service when you created it.
ssId :: Lens' ServiceSummary (Maybe Text)
ssId = lens _ssId (\ s a -> s{_ssId = a})

-- | The description that you specify when you create the service.
ssDescription :: Lens' ServiceSummary (Maybe Text)
ssDescription = lens _ssDescription (\ s a -> s{_ssDescription = a})

instance FromJSON ServiceSummary where
        parseJSON
          = withObject "ServiceSummary"
              (\ x ->
                 ServiceSummary' <$>
                   (x .:? "InstanceCount") <*> (x .:? "Arn") <*>
                     (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "Description"))

instance Hashable ServiceSummary where

instance NFData ServiceSummary where
