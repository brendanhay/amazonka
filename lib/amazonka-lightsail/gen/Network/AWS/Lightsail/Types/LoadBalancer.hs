{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancer where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.InstanceHealthSummary
import Network.AWS.Lightsail.Types.LoadBalancerAttributeName
import Network.AWS.Lightsail.Types.LoadBalancerProtocol
import Network.AWS.Lightsail.Types.LoadBalancerState
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateSummary
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import Network.AWS.Prelude

-- | Describes the Lightsail load balancer.
--
--
--
-- /See:/ 'loadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { _lbHealthCheckPath ::
      !(Maybe Text),
    _lbState :: !(Maybe LoadBalancerState),
    _lbResourceType :: !(Maybe ResourceType),
    _lbArn :: !(Maybe Text),
    _lbCreatedAt :: !(Maybe POSIX),
    _lbLocation :: !(Maybe ResourceLocation),
    _lbInstancePort :: !(Maybe Int),
    _lbConfigurationOptions ::
      !(Maybe (Map LoadBalancerAttributeName (Text))),
    _lbProtocol :: !(Maybe LoadBalancerProtocol),
    _lbTlsCertificateSummaries ::
      !(Maybe [LoadBalancerTLSCertificateSummary]),
    _lbName :: !(Maybe Text),
    _lbSupportCode :: !(Maybe Text),
    _lbPublicPorts :: !(Maybe [Int]),
    _lbDnsName :: !(Maybe Text),
    _lbInstanceHealthSummary :: !(Maybe [InstanceHealthSummary]),
    _lbTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbHealthCheckPath' - The path you specified to perform your health checks. If no path is specified, the load balancer tries to make a request to the default (root) page.
--
-- * 'lbState' - The status of your load balancer. Valid values are below.
--
-- * 'lbResourceType' - The resource type (e.g., @LoadBalancer@ .
--
-- * 'lbArn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- * 'lbCreatedAt' - The date when your load balancer was created.
--
-- * 'lbLocation' - The AWS Region where your load balancer was created (e.g., @us-east-2a@ ). Lightsail automatically creates your load balancer across Availability Zones.
--
-- * 'lbInstancePort' - The port where the load balancer will direct traffic to your Lightsail instances. For HTTP traffic, it's port 80. For HTTPS traffic, it's port 443.
--
-- * 'lbConfigurationOptions' - A string to string map of the configuration options for your load balancer. Valid values are listed below.
--
-- * 'lbProtocol' - The protocol you have enabled for your load balancer. Valid values are below. You can't just have @HTTP_HTTPS@ , but you can have just @HTTP@ .
--
-- * 'lbTlsCertificateSummaries' - An array of LoadBalancerTlsCertificateSummary objects that provide additional information about the SSL/TLS certificates. For example, if @true@ , the certificate is attached to the load balancer.
--
-- * 'lbName' - The name of the load balancer (e.g., @my-load-balancer@ ).
--
-- * 'lbSupportCode' - The support code. Include this code in your email to support when you have questions about your Lightsail load balancer. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'lbPublicPorts' - An array of public port settings for your load balancer. For HTTP, use port 80. For HTTPS, use port 443.
--
-- * 'lbDnsName' - The DNS name of your Lightsail load balancer.
--
-- * 'lbInstanceHealthSummary' - An array of InstanceHealthSummary objects describing the health of the load balancer.
--
-- * 'lbTags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
loadBalancer ::
  LoadBalancer
loadBalancer =
  LoadBalancer'
    { _lbHealthCheckPath = Nothing,
      _lbState = Nothing,
      _lbResourceType = Nothing,
      _lbArn = Nothing,
      _lbCreatedAt = Nothing,
      _lbLocation = Nothing,
      _lbInstancePort = Nothing,
      _lbConfigurationOptions = Nothing,
      _lbProtocol = Nothing,
      _lbTlsCertificateSummaries = Nothing,
      _lbName = Nothing,
      _lbSupportCode = Nothing,
      _lbPublicPorts = Nothing,
      _lbDnsName = Nothing,
      _lbInstanceHealthSummary = Nothing,
      _lbTags = Nothing
    }

-- | The path you specified to perform your health checks. If no path is specified, the load balancer tries to make a request to the default (root) page.
lbHealthCheckPath :: Lens' LoadBalancer (Maybe Text)
lbHealthCheckPath = lens _lbHealthCheckPath (\s a -> s {_lbHealthCheckPath = a})

-- | The status of your load balancer. Valid values are below.
lbState :: Lens' LoadBalancer (Maybe LoadBalancerState)
lbState = lens _lbState (\s a -> s {_lbState = a})

-- | The resource type (e.g., @LoadBalancer@ .
lbResourceType :: Lens' LoadBalancer (Maybe ResourceType)
lbResourceType = lens _lbResourceType (\s a -> s {_lbResourceType = a})

-- | The Amazon Resource Name (ARN) of the load balancer.
lbArn :: Lens' LoadBalancer (Maybe Text)
lbArn = lens _lbArn (\s a -> s {_lbArn = a})

-- | The date when your load balancer was created.
lbCreatedAt :: Lens' LoadBalancer (Maybe UTCTime)
lbCreatedAt = lens _lbCreatedAt (\s a -> s {_lbCreatedAt = a}) . mapping _Time

-- | The AWS Region where your load balancer was created (e.g., @us-east-2a@ ). Lightsail automatically creates your load balancer across Availability Zones.
lbLocation :: Lens' LoadBalancer (Maybe ResourceLocation)
lbLocation = lens _lbLocation (\s a -> s {_lbLocation = a})

-- | The port where the load balancer will direct traffic to your Lightsail instances. For HTTP traffic, it's port 80. For HTTPS traffic, it's port 443.
lbInstancePort :: Lens' LoadBalancer (Maybe Int)
lbInstancePort = lens _lbInstancePort (\s a -> s {_lbInstancePort = a})

-- | A string to string map of the configuration options for your load balancer. Valid values are listed below.
lbConfigurationOptions :: Lens' LoadBalancer (HashMap LoadBalancerAttributeName (Text))
lbConfigurationOptions = lens _lbConfigurationOptions (\s a -> s {_lbConfigurationOptions = a}) . _Default . _Map

-- | The protocol you have enabled for your load balancer. Valid values are below. You can't just have @HTTP_HTTPS@ , but you can have just @HTTP@ .
lbProtocol :: Lens' LoadBalancer (Maybe LoadBalancerProtocol)
lbProtocol = lens _lbProtocol (\s a -> s {_lbProtocol = a})

-- | An array of LoadBalancerTlsCertificateSummary objects that provide additional information about the SSL/TLS certificates. For example, if @true@ , the certificate is attached to the load balancer.
lbTlsCertificateSummaries :: Lens' LoadBalancer [LoadBalancerTLSCertificateSummary]
lbTlsCertificateSummaries = lens _lbTlsCertificateSummaries (\s a -> s {_lbTlsCertificateSummaries = a}) . _Default . _Coerce

-- | The name of the load balancer (e.g., @my-load-balancer@ ).
lbName :: Lens' LoadBalancer (Maybe Text)
lbName = lens _lbName (\s a -> s {_lbName = a})

-- | The support code. Include this code in your email to support when you have questions about your Lightsail load balancer. This code enables our support team to look up your Lightsail information more easily.
lbSupportCode :: Lens' LoadBalancer (Maybe Text)
lbSupportCode = lens _lbSupportCode (\s a -> s {_lbSupportCode = a})

-- | An array of public port settings for your load balancer. For HTTP, use port 80. For HTTPS, use port 443.
lbPublicPorts :: Lens' LoadBalancer [Int]
lbPublicPorts = lens _lbPublicPorts (\s a -> s {_lbPublicPorts = a}) . _Default . _Coerce

-- | The DNS name of your Lightsail load balancer.
lbDnsName :: Lens' LoadBalancer (Maybe Text)
lbDnsName = lens _lbDnsName (\s a -> s {_lbDnsName = a})

-- | An array of InstanceHealthSummary objects describing the health of the load balancer.
lbInstanceHealthSummary :: Lens' LoadBalancer [InstanceHealthSummary]
lbInstanceHealthSummary = lens _lbInstanceHealthSummary (\s a -> s {_lbInstanceHealthSummary = a}) . _Default . _Coerce

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
lbTags :: Lens' LoadBalancer [Tag]
lbTags = lens _lbTags (\s a -> s {_lbTags = a}) . _Default . _Coerce

instance FromJSON LoadBalancer where
  parseJSON =
    withObject
      "LoadBalancer"
      ( \x ->
          LoadBalancer'
            <$> (x .:? "healthCheckPath")
            <*> (x .:? "state")
            <*> (x .:? "resourceType")
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "location")
            <*> (x .:? "instancePort")
            <*> (x .:? "configurationOptions" .!= mempty)
            <*> (x .:? "protocol")
            <*> (x .:? "tlsCertificateSummaries" .!= mempty)
            <*> (x .:? "name")
            <*> (x .:? "supportCode")
            <*> (x .:? "publicPorts" .!= mempty)
            <*> (x .:? "dnsName")
            <*> (x .:? "instanceHealthSummary" .!= mempty)
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable LoadBalancer

instance NFData LoadBalancer
