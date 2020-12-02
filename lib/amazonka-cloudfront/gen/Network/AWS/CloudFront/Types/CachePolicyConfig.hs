{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyConfig where

import Network.AWS.CloudFront.Types.ParametersInCacheKeyAndForwardedToOrigin
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A cache policy configuration.
--
--
-- This configuration determines the following:
--
--     * The values that CloudFront includes in the cache key. These values can include HTTP headers, cookies, and URL query strings. CloudFront uses the cache key to find an object in its cache that it can return to the viewer.
--
--     * The default, minimum, and maximum time to live (TTL) values that you want objects to stay in the CloudFront cache.
--
--
--
-- The headers, cookies, and query strings that are included in the cache key are automatically included in requests that CloudFront sends to the origin. CloudFront sends a request when it can’t find a valid object in its cache that matches the request’s cache key. If you want to send values to the origin but /not/ include them in the cache key, use @OriginRequestPolicy@ .
--
--
-- /See:/ 'cachePolicyConfig' smart constructor.
data CachePolicyConfig = CachePolicyConfig'
  { _cpcMaxTTL ::
      !(Maybe Integer),
    _cpcParametersInCacheKeyAndForwardedToOrigin ::
      !(Maybe ParametersInCacheKeyAndForwardedToOrigin),
    _cpcDefaultTTL :: !(Maybe Integer),
    _cpcComment :: !(Maybe Text),
    _cpcName :: !Text,
    _cpcMinTTL :: !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CachePolicyConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpcMaxTTL' - The maximum amount of time, in seconds, that objects stay in the CloudFront cache before CloudFront sends another request to the origin to see if the object has been updated. CloudFront uses this value only when the origin sends @Cache-Control@ or @Expires@ headers with the object. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ . The default value for this field is 31536000 seconds (one year). If the value of @MinTTL@ or @DefaultTTL@ is more than 31536000 seconds, then the default value for this field is the same as the value of @DefaultTTL@ .
--
-- * 'cpcParametersInCacheKeyAndForwardedToOrigin' - The HTTP headers, cookies, and URL query strings to include in the cache key. The values included in the cache key are automatically included in requests that CloudFront sends to the origin.
--
-- * 'cpcDefaultTTL' - The default amount of time, in seconds, that you want objects to stay in the CloudFront cache before CloudFront sends another request to the origin to see if the object has been updated. CloudFront uses this value as the object’s time to live (TTL) only when the origin does /not/ send @Cache-Control@ or @Expires@ headers with the object. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ . The default value for this field is 86400 seconds (one day). If the value of @MinTTL@ is more than 86400 seconds, then the default value for this field is the same as the value of @MinTTL@ .
--
-- * 'cpcComment' - A comment to describe the cache policy.
--
-- * 'cpcName' - A unique name to identify the cache policy.
--
-- * 'cpcMinTTL' - The minimum amount of time, in seconds, that you want objects to stay in the CloudFront cache before CloudFront sends another request to the origin to see if the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
cachePolicyConfig ::
  -- | 'cpcName'
  Text ->
  -- | 'cpcMinTTL'
  Integer ->
  CachePolicyConfig
cachePolicyConfig pName_ pMinTTL_ =
  CachePolicyConfig'
    { _cpcMaxTTL = Nothing,
      _cpcParametersInCacheKeyAndForwardedToOrigin = Nothing,
      _cpcDefaultTTL = Nothing,
      _cpcComment = Nothing,
      _cpcName = pName_,
      _cpcMinTTL = pMinTTL_
    }

-- | The maximum amount of time, in seconds, that objects stay in the CloudFront cache before CloudFront sends another request to the origin to see if the object has been updated. CloudFront uses this value only when the origin sends @Cache-Control@ or @Expires@ headers with the object. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ . The default value for this field is 31536000 seconds (one year). If the value of @MinTTL@ or @DefaultTTL@ is more than 31536000 seconds, then the default value for this field is the same as the value of @DefaultTTL@ .
cpcMaxTTL :: Lens' CachePolicyConfig (Maybe Integer)
cpcMaxTTL = lens _cpcMaxTTL (\s a -> s {_cpcMaxTTL = a})

-- | The HTTP headers, cookies, and URL query strings to include in the cache key. The values included in the cache key are automatically included in requests that CloudFront sends to the origin.
cpcParametersInCacheKeyAndForwardedToOrigin :: Lens' CachePolicyConfig (Maybe ParametersInCacheKeyAndForwardedToOrigin)
cpcParametersInCacheKeyAndForwardedToOrigin = lens _cpcParametersInCacheKeyAndForwardedToOrigin (\s a -> s {_cpcParametersInCacheKeyAndForwardedToOrigin = a})

-- | The default amount of time, in seconds, that you want objects to stay in the CloudFront cache before CloudFront sends another request to the origin to see if the object has been updated. CloudFront uses this value as the object’s time to live (TTL) only when the origin does /not/ send @Cache-Control@ or @Expires@ headers with the object. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ . The default value for this field is 86400 seconds (one day). If the value of @MinTTL@ is more than 86400 seconds, then the default value for this field is the same as the value of @MinTTL@ .
cpcDefaultTTL :: Lens' CachePolicyConfig (Maybe Integer)
cpcDefaultTTL = lens _cpcDefaultTTL (\s a -> s {_cpcDefaultTTL = a})

-- | A comment to describe the cache policy.
cpcComment :: Lens' CachePolicyConfig (Maybe Text)
cpcComment = lens _cpcComment (\s a -> s {_cpcComment = a})

-- | A unique name to identify the cache policy.
cpcName :: Lens' CachePolicyConfig Text
cpcName = lens _cpcName (\s a -> s {_cpcName = a})

-- | The minimum amount of time, in seconds, that you want objects to stay in the CloudFront cache before CloudFront sends another request to the origin to see if the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
cpcMinTTL :: Lens' CachePolicyConfig Integer
cpcMinTTL = lens _cpcMinTTL (\s a -> s {_cpcMinTTL = a})

instance FromXML CachePolicyConfig where
  parseXML x =
    CachePolicyConfig'
      <$> (x .@? "MaxTTL")
      <*> (x .@? "ParametersInCacheKeyAndForwardedToOrigin")
      <*> (x .@? "DefaultTTL")
      <*> (x .@? "Comment")
      <*> (x .@ "Name")
      <*> (x .@ "MinTTL")

instance Hashable CachePolicyConfig

instance NFData CachePolicyConfig

instance ToXML CachePolicyConfig where
  toXML CachePolicyConfig' {..} =
    mconcat
      [ "MaxTTL" @= _cpcMaxTTL,
        "ParametersInCacheKeyAndForwardedToOrigin"
          @= _cpcParametersInCacheKeyAndForwardedToOrigin,
        "DefaultTTL" @= _cpcDefaultTTL,
        "Comment" @= _cpcComment,
        "Name" @= _cpcName,
        "MinTTL" @= _cpcMinTTL
      ]
