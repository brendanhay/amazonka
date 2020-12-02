{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.InstanceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.InstanceSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains information about the instances that you registered by using a specified service.
--
--
--
-- /See:/ 'instanceSummary' smart constructor.
data InstanceSummary = InstanceSummary'
  { _isAttributes ::
      !(Maybe (Map Text (Text))),
    _isId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isAttributes' - A string map that contains the following information:     * The attributes that are associate with the instance.      * For each attribute, the applicable value. Supported attribute keys include the following:     * @AWS_ALIAS_DNS_NAME@ : For an alias record that routes traffic to an Elastic Load Balancing load balancer, the DNS name that is associated with the load balancer.      * @AWS_EC2_INSTANCE_ID@ : (HTTP namespaces only) The Amazon EC2 instance ID for the instance. When the @AWS_EC2_INSTANCE_ID@ attribute is specified, then the @AWS_INSTANCE_IPV4@ attribute contains the primary private IPv4 address.     * @AWS_INSTANCE_CNAME@ : For a @CNAME@ record, the domain name that Route 53 returns in response to DNS queries, for example, @example.com@ .     * @AWS_INSTANCE_IPV4@ : For an @A@ record, the IPv4 address that Route 53 returns in response to DNS queries, for example, @192.0.2.44@ .     * @AWS_INSTANCE_IPV6@ : For an @AAAA@ record, the IPv6 address that Route 53 returns in response to DNS queries, for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ .     * @AWS_INSTANCE_PORT@ : For an @SRV@ record, the value that Route 53 returns for the port. In addition, if the service includes @HealthCheckConfig@ , the port on the endpoint that Route 53 sends requests to.
--
-- * 'isId' - The ID for an instance that you created by using a specified service.
instanceSummary ::
  InstanceSummary
instanceSummary =
  InstanceSummary' {_isAttributes = Nothing, _isId = Nothing}

-- | A string map that contains the following information:     * The attributes that are associate with the instance.      * For each attribute, the applicable value. Supported attribute keys include the following:     * @AWS_ALIAS_DNS_NAME@ : For an alias record that routes traffic to an Elastic Load Balancing load balancer, the DNS name that is associated with the load balancer.      * @AWS_EC2_INSTANCE_ID@ : (HTTP namespaces only) The Amazon EC2 instance ID for the instance. When the @AWS_EC2_INSTANCE_ID@ attribute is specified, then the @AWS_INSTANCE_IPV4@ attribute contains the primary private IPv4 address.     * @AWS_INSTANCE_CNAME@ : For a @CNAME@ record, the domain name that Route 53 returns in response to DNS queries, for example, @example.com@ .     * @AWS_INSTANCE_IPV4@ : For an @A@ record, the IPv4 address that Route 53 returns in response to DNS queries, for example, @192.0.2.44@ .     * @AWS_INSTANCE_IPV6@ : For an @AAAA@ record, the IPv6 address that Route 53 returns in response to DNS queries, for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ .     * @AWS_INSTANCE_PORT@ : For an @SRV@ record, the value that Route 53 returns for the port. In addition, if the service includes @HealthCheckConfig@ , the port on the endpoint that Route 53 sends requests to.
isAttributes :: Lens' InstanceSummary (HashMap Text (Text))
isAttributes = lens _isAttributes (\s a -> s {_isAttributes = a}) . _Default . _Map

-- | The ID for an instance that you created by using a specified service.
isId :: Lens' InstanceSummary (Maybe Text)
isId = lens _isId (\s a -> s {_isId = a})

instance FromJSON InstanceSummary where
  parseJSON =
    withObject
      "InstanceSummary"
      ( \x ->
          InstanceSummary'
            <$> (x .:? "Attributes" .!= mempty) <*> (x .:? "Id")
      )

instance Hashable InstanceSummary

instance NFData InstanceSummary
