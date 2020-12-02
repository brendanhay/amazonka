{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.DNSConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.DNSConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.DNSRecord
import Network.AWS.Route53AutoNaming.Types.RoutingPolicy

-- | A complex type that contains information about the Amazon Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
--
--
--
-- /See:/ 'dnsConfig' smart constructor.
data DNSConfig = DNSConfig'
  { _dcRoutingPolicy ::
      !(Maybe RoutingPolicy),
    _dcNamespaceId :: !(Maybe Text),
    _dcDNSRecords :: ![DNSRecord]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DNSConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcRoutingPolicy' - The routing policy that you want to apply to all Route 53 DNS records that AWS Cloud Map creates when you register an instance and specify this service. You can specify the following values: __MULTIVALUE__  If you define a health check for the service and the health check is healthy, Route 53 returns the applicable value for up to eight instances. For example, suppose the service includes configurations for one @A@ record and a health check, and you use the service to register 10 instances. Route 53 responds to DNS queries with IP addresses for up to eight healthy instances. If fewer than eight instances are healthy, Route 53 responds to every DNS query with the IP addresses for all of the healthy instances. If you don't define a health check for the service, Route 53 assumes that all instances are healthy and returns the values for up to eight instances. For more information about the multivalue routing policy, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-multivalue Multivalue Answer Routing> in the /Route 53 Developer Guide/ . __WEIGHTED__  Route 53 returns the applicable value from one randomly selected instance from among the instances that you registered using the same service. Currently, all records have the same weight, so you can't route more or less traffic to any instances. For example, suppose the service includes configurations for one @A@ record and a health check, and you use the service to register 10 instances. Route 53 responds to DNS queries with the IP address for one randomly selected instance from among the healthy instances. If no instances are healthy, Route 53 responds to DNS queries as if all of the instances were healthy. If you don't define a health check for the service, Route 53 assumes that all instances are healthy and returns the applicable value for one randomly selected instance. For more information about the weighted routing policy, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-weighted Weighted Routing> in the /Route 53 Developer Guide/ .
--
-- * 'dcNamespaceId' - The ID of the namespace to use for DNS configuration.
--
-- * 'dcDNSRecords' - An array that contains one @DnsRecord@ object for each Route 53 DNS record that you want AWS Cloud Map to create when you register an instance.
dnsConfig ::
  DNSConfig
dnsConfig =
  DNSConfig'
    { _dcRoutingPolicy = Nothing,
      _dcNamespaceId = Nothing,
      _dcDNSRecords = mempty
    }

-- | The routing policy that you want to apply to all Route 53 DNS records that AWS Cloud Map creates when you register an instance and specify this service. You can specify the following values: __MULTIVALUE__  If you define a health check for the service and the health check is healthy, Route 53 returns the applicable value for up to eight instances. For example, suppose the service includes configurations for one @A@ record and a health check, and you use the service to register 10 instances. Route 53 responds to DNS queries with IP addresses for up to eight healthy instances. If fewer than eight instances are healthy, Route 53 responds to every DNS query with the IP addresses for all of the healthy instances. If you don't define a health check for the service, Route 53 assumes that all instances are healthy and returns the values for up to eight instances. For more information about the multivalue routing policy, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-multivalue Multivalue Answer Routing> in the /Route 53 Developer Guide/ . __WEIGHTED__  Route 53 returns the applicable value from one randomly selected instance from among the instances that you registered using the same service. Currently, all records have the same weight, so you can't route more or less traffic to any instances. For example, suppose the service includes configurations for one @A@ record and a health check, and you use the service to register 10 instances. Route 53 responds to DNS queries with the IP address for one randomly selected instance from among the healthy instances. If no instances are healthy, Route 53 responds to DNS queries as if all of the instances were healthy. If you don't define a health check for the service, Route 53 assumes that all instances are healthy and returns the applicable value for one randomly selected instance. For more information about the weighted routing policy, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html#routing-policy-weighted Weighted Routing> in the /Route 53 Developer Guide/ .
dcRoutingPolicy :: Lens' DNSConfig (Maybe RoutingPolicy)
dcRoutingPolicy = lens _dcRoutingPolicy (\s a -> s {_dcRoutingPolicy = a})

-- | The ID of the namespace to use for DNS configuration.
dcNamespaceId :: Lens' DNSConfig (Maybe Text)
dcNamespaceId = lens _dcNamespaceId (\s a -> s {_dcNamespaceId = a})

-- | An array that contains one @DnsRecord@ object for each Route 53 DNS record that you want AWS Cloud Map to create when you register an instance.
dcDNSRecords :: Lens' DNSConfig [DNSRecord]
dcDNSRecords = lens _dcDNSRecords (\s a -> s {_dcDNSRecords = a}) . _Coerce

instance FromJSON DNSConfig where
  parseJSON =
    withObject
      "DNSConfig"
      ( \x ->
          DNSConfig'
            <$> (x .:? "RoutingPolicy")
            <*> (x .:? "NamespaceId")
            <*> (x .:? "DnsRecords" .!= mempty)
      )

instance Hashable DNSConfig

instance NFData DNSConfig

instance ToJSON DNSConfig where
  toJSON DNSConfig' {..} =
    object
      ( catMaybes
          [ ("RoutingPolicy" .=) <$> _dcRoutingPolicy,
            ("NamespaceId" .=) <$> _dcNamespaceId,
            Just ("DnsRecords" .= _dcDNSRecords)
          ]
      )
