{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LightsailDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LightsailDistribution where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.CacheBehavior
import Network.AWS.Lightsail.Types.CacheBehaviorPerPath
import Network.AWS.Lightsail.Types.CacheSettings
import Network.AWS.Lightsail.Types.Origin
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import Network.AWS.Prelude

-- | Describes an Amazon Lightsail content delivery network (CDN) distribution.
--
--
--
-- /See:/ 'lightsailDistribution' smart constructor.
data LightsailDistribution = LightsailDistribution'
  { _ldStatus ::
      !(Maybe Text),
    _ldOrigin :: !(Maybe Origin),
    _ldCertificateName :: !(Maybe Text),
    _ldResourceType :: !(Maybe ResourceType),
    _ldArn :: !(Maybe Text),
    _ldCreatedAt :: !(Maybe POSIX),
    _ldLocation :: !(Maybe ResourceLocation),
    _ldCacheBehaviorSettings ::
      !(Maybe CacheSettings),
    _ldAlternativeDomainNames :: !(Maybe [Text]),
    _ldBundleId :: !(Maybe Text),
    _ldAbleToUpdateBundle :: !(Maybe Bool),
    _ldOriginPublicDNS :: !(Maybe Text),
    _ldDomainName :: !(Maybe Text),
    _ldName :: !(Maybe Text),
    _ldIsEnabled :: !(Maybe Bool),
    _ldSupportCode :: !(Maybe Text),
    _ldDefaultCacheBehavior ::
      !(Maybe CacheBehavior),
    _ldCacheBehaviors ::
      !(Maybe [CacheBehaviorPerPath]),
    _ldTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LightsailDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldStatus' - The status of the distribution.
--
-- * 'ldOrigin' - An object that describes the origin resource of the distribution, such as a Lightsail instance or load balancer. The distribution pulls, caches, and serves content from the origin.
--
-- * 'ldCertificateName' - The name of the SSL/TLS certificate attached to the distribution, if any.
--
-- * 'ldResourceType' - The Lightsail resource type (e.g., @Distribution@ ).
--
-- * 'ldArn' - The Amazon Resource Name (ARN) of the distribution.
--
-- * 'ldCreatedAt' - The timestamp when the distribution was created.
--
-- * 'ldLocation' - An object that describes the location of the distribution, such as the AWS Region and Availability Zone.
--
-- * 'ldCacheBehaviorSettings' - An object that describes the cache behavior settings of the distribution.
--
-- * 'ldAlternativeDomainNames' - The alternate domain names of the distribution.
--
-- * 'ldBundleId' - The ID of the bundle currently applied to the distribution.
--
-- * 'ldAbleToUpdateBundle' - Indicates whether the bundle that is currently applied to your distribution, specified using the @distributionName@ parameter, can be changed to another bundle. Use the @UpdateDistributionBundle@ action to change your distribution's bundle.
--
-- * 'ldOriginPublicDNS' - The public DNS of the origin.
--
-- * 'ldDomainName' - The domain name of the distribution.
--
-- * 'ldName' - The name of the distribution.
--
-- * 'ldIsEnabled' - Indicates whether the distribution is enabled.
--
-- * 'ldSupportCode' - The support code. Include this code in your email to support when you have questions about your Lightsail distribution. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'ldDefaultCacheBehavior' - An object that describes the default cache behavior of the distribution.
--
-- * 'ldCacheBehaviors' - An array of objects that describe the per-path cache behavior of the distribution.
--
-- * 'ldTags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
lightsailDistribution ::
  LightsailDistribution
lightsailDistribution =
  LightsailDistribution'
    { _ldStatus = Nothing,
      _ldOrigin = Nothing,
      _ldCertificateName = Nothing,
      _ldResourceType = Nothing,
      _ldArn = Nothing,
      _ldCreatedAt = Nothing,
      _ldLocation = Nothing,
      _ldCacheBehaviorSettings = Nothing,
      _ldAlternativeDomainNames = Nothing,
      _ldBundleId = Nothing,
      _ldAbleToUpdateBundle = Nothing,
      _ldOriginPublicDNS = Nothing,
      _ldDomainName = Nothing,
      _ldName = Nothing,
      _ldIsEnabled = Nothing,
      _ldSupportCode = Nothing,
      _ldDefaultCacheBehavior = Nothing,
      _ldCacheBehaviors = Nothing,
      _ldTags = Nothing
    }

-- | The status of the distribution.
ldStatus :: Lens' LightsailDistribution (Maybe Text)
ldStatus = lens _ldStatus (\s a -> s {_ldStatus = a})

-- | An object that describes the origin resource of the distribution, such as a Lightsail instance or load balancer. The distribution pulls, caches, and serves content from the origin.
ldOrigin :: Lens' LightsailDistribution (Maybe Origin)
ldOrigin = lens _ldOrigin (\s a -> s {_ldOrigin = a})

-- | The name of the SSL/TLS certificate attached to the distribution, if any.
ldCertificateName :: Lens' LightsailDistribution (Maybe Text)
ldCertificateName = lens _ldCertificateName (\s a -> s {_ldCertificateName = a})

-- | The Lightsail resource type (e.g., @Distribution@ ).
ldResourceType :: Lens' LightsailDistribution (Maybe ResourceType)
ldResourceType = lens _ldResourceType (\s a -> s {_ldResourceType = a})

-- | The Amazon Resource Name (ARN) of the distribution.
ldArn :: Lens' LightsailDistribution (Maybe Text)
ldArn = lens _ldArn (\s a -> s {_ldArn = a})

-- | The timestamp when the distribution was created.
ldCreatedAt :: Lens' LightsailDistribution (Maybe UTCTime)
ldCreatedAt = lens _ldCreatedAt (\s a -> s {_ldCreatedAt = a}) . mapping _Time

-- | An object that describes the location of the distribution, such as the AWS Region and Availability Zone.
ldLocation :: Lens' LightsailDistribution (Maybe ResourceLocation)
ldLocation = lens _ldLocation (\s a -> s {_ldLocation = a})

-- | An object that describes the cache behavior settings of the distribution.
ldCacheBehaviorSettings :: Lens' LightsailDistribution (Maybe CacheSettings)
ldCacheBehaviorSettings = lens _ldCacheBehaviorSettings (\s a -> s {_ldCacheBehaviorSettings = a})

-- | The alternate domain names of the distribution.
ldAlternativeDomainNames :: Lens' LightsailDistribution [Text]
ldAlternativeDomainNames = lens _ldAlternativeDomainNames (\s a -> s {_ldAlternativeDomainNames = a}) . _Default . _Coerce

-- | The ID of the bundle currently applied to the distribution.
ldBundleId :: Lens' LightsailDistribution (Maybe Text)
ldBundleId = lens _ldBundleId (\s a -> s {_ldBundleId = a})

-- | Indicates whether the bundle that is currently applied to your distribution, specified using the @distributionName@ parameter, can be changed to another bundle. Use the @UpdateDistributionBundle@ action to change your distribution's bundle.
ldAbleToUpdateBundle :: Lens' LightsailDistribution (Maybe Bool)
ldAbleToUpdateBundle = lens _ldAbleToUpdateBundle (\s a -> s {_ldAbleToUpdateBundle = a})

-- | The public DNS of the origin.
ldOriginPublicDNS :: Lens' LightsailDistribution (Maybe Text)
ldOriginPublicDNS = lens _ldOriginPublicDNS (\s a -> s {_ldOriginPublicDNS = a})

-- | The domain name of the distribution.
ldDomainName :: Lens' LightsailDistribution (Maybe Text)
ldDomainName = lens _ldDomainName (\s a -> s {_ldDomainName = a})

-- | The name of the distribution.
ldName :: Lens' LightsailDistribution (Maybe Text)
ldName = lens _ldName (\s a -> s {_ldName = a})

-- | Indicates whether the distribution is enabled.
ldIsEnabled :: Lens' LightsailDistribution (Maybe Bool)
ldIsEnabled = lens _ldIsEnabled (\s a -> s {_ldIsEnabled = a})

-- | The support code. Include this code in your email to support when you have questions about your Lightsail distribution. This code enables our support team to look up your Lightsail information more easily.
ldSupportCode :: Lens' LightsailDistribution (Maybe Text)
ldSupportCode = lens _ldSupportCode (\s a -> s {_ldSupportCode = a})

-- | An object that describes the default cache behavior of the distribution.
ldDefaultCacheBehavior :: Lens' LightsailDistribution (Maybe CacheBehavior)
ldDefaultCacheBehavior = lens _ldDefaultCacheBehavior (\s a -> s {_ldDefaultCacheBehavior = a})

-- | An array of objects that describe the per-path cache behavior of the distribution.
ldCacheBehaviors :: Lens' LightsailDistribution [CacheBehaviorPerPath]
ldCacheBehaviors = lens _ldCacheBehaviors (\s a -> s {_ldCacheBehaviors = a}) . _Default . _Coerce

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
ldTags :: Lens' LightsailDistribution [Tag]
ldTags = lens _ldTags (\s a -> s {_ldTags = a}) . _Default . _Coerce

instance FromJSON LightsailDistribution where
  parseJSON =
    withObject
      "LightsailDistribution"
      ( \x ->
          LightsailDistribution'
            <$> (x .:? "status")
            <*> (x .:? "origin")
            <*> (x .:? "certificateName")
            <*> (x .:? "resourceType")
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "location")
            <*> (x .:? "cacheBehaviorSettings")
            <*> (x .:? "alternativeDomainNames" .!= mempty)
            <*> (x .:? "bundleId")
            <*> (x .:? "ableToUpdateBundle")
            <*> (x .:? "originPublicDNS")
            <*> (x .:? "domainName")
            <*> (x .:? "name")
            <*> (x .:? "isEnabled")
            <*> (x .:? "supportCode")
            <*> (x .:? "defaultCacheBehavior")
            <*> (x .:? "cacheBehaviors" .!= mempty)
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable LightsailDistribution

instance NFData LightsailDistribution
