{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.HealthCheckConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.HealthCheckConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.HealthCheckType

-- | /Public DNS and HTTP namespaces only./ A complex type that contains settings for an optional health check. If you specify settings for a health check, AWS Cloud Map associates the health check with the records that you specify in @DnsConfig@ .
--
--
-- /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
--
-- Health checks are basic Route 53 health checks that monitor an AWS endpoint. For information about pricing for health checks, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
--
-- Note the following about configuring health checks.
--
-- __@A@ and @AAAA@ records__
--
-- If @DnsConfig@ includes configurations for both @A@ and @AAAA@ records, AWS Cloud Map creates a health check that uses the IPv4 address to check the health of the resource. If the endpoint that is specified by the IPv4 address is unhealthy, Route 53 considers both the @A@ and @AAAA@ records to be unhealthy.
--
-- __@CNAME@ records__
--
-- You can't specify settings for @HealthCheckConfig@ when the @DNSConfig@ includes @CNAME@ for the value of @Type@ . If you do, the @CreateService@ request will fail with an @InvalidInput@ error.
--
-- __Request interval__
--
-- A Route 53 health checker in each health-checking region sends a health check request to an endpoint every 30 seconds. On average, your endpoint receives a health check request about every two seconds. However, health checkers don't coordinate with one another, so you'll sometimes see several requests per second followed by a few seconds with no health checks at all.
--
-- __Health checking regions__
--
-- Health checkers perform checks from all Route 53 health-checking regions. For a list of the current regions, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-Regions Regions> .
--
-- __Alias records__
--
-- When you register an instance, if you include the @AWS_ALIAS_DNS_NAME@ attribute, AWS Cloud Map creates a Route 53 alias record. Note the following:
--
--     * Route 53 automatically sets @EvaluateTargetHealth@ to true for alias records. When @EvaluateTargetHealth@ is true, the alias record inherits the health of the referenced AWS resource. such as an ELB load balancer. For more information, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html#Route53-Type-AliasTarget-EvaluateTargetHealth EvaluateTargetHealth> .
--
--     * If you include @HealthCheckConfig@ and then use the service to register an instance that creates an alias record, Route 53 doesn't create the health check.
--
--
--
-- __Charges for health checks__
--
-- Health checks are basic Route 53 health checks that monitor an AWS endpoint. For information about pricing for health checks, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
--
--
-- /See:/ 'healthCheckConfig' smart constructor.
data HealthCheckConfig = HealthCheckConfig'
  { _hccFailureThreshold ::
      !(Maybe Nat),
    _hccResourcePath :: !(Maybe Text),
    _hccType :: !HealthCheckType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HealthCheckConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hccFailureThreshold' - The number of consecutive health checks that an endpoint must pass or fail for Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy> in the /Route 53 Developer Guide/ .
--
-- * 'hccResourcePath' - The path that you want Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, such as the file @/docs/route53-health-check.html@ . Route 53 automatically adds the DNS name for the service. If you don't specify a value for @ResourcePath@ , the default value is @/@ . If you specify @TCP@ for @Type@ , you must /not/ specify a value for @ResourcePath@ .
--
-- * 'hccType' - The type of health check that you want to create, which indicates how Route 53 determines whether an endpoint is healthy. /Important:/ You can't change the value of @Type@ after you create a health check. You can create the following types of health checks:     * __HTTP__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTP request and waits for an HTTP status code of 200 or greater and less than 400.     * __HTTPS__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTPS request and waits for an HTTP status code of 200 or greater and less than 400. /Important:/ If you specify HTTPS for the value of @Type@ , the endpoint must support TLS v1.0 or later.     * __TCP__ : Route 53 tries to establish a TCP connection. If you specify @TCP@ for @Type@ , don't specify a value for @ResourcePath@ . For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy> in the /Route 53 Developer Guide/ .
healthCheckConfig ::
  -- | 'hccType'
  HealthCheckType ->
  HealthCheckConfig
healthCheckConfig pType_ =
  HealthCheckConfig'
    { _hccFailureThreshold = Nothing,
      _hccResourcePath = Nothing,
      _hccType = pType_
    }

-- | The number of consecutive health checks that an endpoint must pass or fail for Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy> in the /Route 53 Developer Guide/ .
hccFailureThreshold :: Lens' HealthCheckConfig (Maybe Natural)
hccFailureThreshold = lens _hccFailureThreshold (\s a -> s {_hccFailureThreshold = a}) . mapping _Nat

-- | The path that you want Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, such as the file @/docs/route53-health-check.html@ . Route 53 automatically adds the DNS name for the service. If you don't specify a value for @ResourcePath@ , the default value is @/@ . If you specify @TCP@ for @Type@ , you must /not/ specify a value for @ResourcePath@ .
hccResourcePath :: Lens' HealthCheckConfig (Maybe Text)
hccResourcePath = lens _hccResourcePath (\s a -> s {_hccResourcePath = a})

-- | The type of health check that you want to create, which indicates how Route 53 determines whether an endpoint is healthy. /Important:/ You can't change the value of @Type@ after you create a health check. You can create the following types of health checks:     * __HTTP__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTP request and waits for an HTTP status code of 200 or greater and less than 400.     * __HTTPS__ : Route 53 tries to establish a TCP connection. If successful, Route 53 submits an HTTPS request and waits for an HTTP status code of 200 or greater and less than 400. /Important:/ If you specify HTTPS for the value of @Type@ , the endpoint must support TLS v1.0 or later.     * __TCP__ : Route 53 tries to establish a TCP connection. If you specify @TCP@ for @Type@ , don't specify a value for @ResourcePath@ . For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Route 53 Determines Whether an Endpoint Is Healthy> in the /Route 53 Developer Guide/ .
hccType :: Lens' HealthCheckConfig HealthCheckType
hccType = lens _hccType (\s a -> s {_hccType = a})

instance FromJSON HealthCheckConfig where
  parseJSON =
    withObject
      "HealthCheckConfig"
      ( \x ->
          HealthCheckConfig'
            <$> (x .:? "FailureThreshold")
            <*> (x .:? "ResourcePath")
            <*> (x .: "Type")
      )

instance Hashable HealthCheckConfig

instance NFData HealthCheckConfig

instance ToJSON HealthCheckConfig where
  toJSON HealthCheckConfig' {..} =
    object
      ( catMaybes
          [ ("FailureThreshold" .=) <$> _hccFailureThreshold,
            ("ResourcePath" .=) <$> _hccResourcePath,
            Just ("Type" .= _hccType)
          ]
      )
