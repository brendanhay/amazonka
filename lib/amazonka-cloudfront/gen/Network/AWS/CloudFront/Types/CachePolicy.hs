{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicy where

import Network.AWS.CloudFront.Types.CachePolicyConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A cache policy.
--
--
-- When it’s attached to a cache behavior, the cache policy determines the following:
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
-- /See:/ 'cachePolicy' smart constructor.
data CachePolicy = CachePolicy'
  { _cpId :: !Text,
    _cpLastModifiedTime :: !ISO8601,
    _cpCachePolicyConfig :: !CachePolicyConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CachePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpId' - The unique identifier for the cache policy.
--
-- * 'cpLastModifiedTime' - The date and time when the cache policy was last modified.
--
-- * 'cpCachePolicyConfig' - The cache policy configuration.
cachePolicy ::
  -- | 'cpId'
  Text ->
  -- | 'cpLastModifiedTime'
  UTCTime ->
  -- | 'cpCachePolicyConfig'
  CachePolicyConfig ->
  CachePolicy
cachePolicy pId_ pLastModifiedTime_ pCachePolicyConfig_ =
  CachePolicy'
    { _cpId = pId_,
      _cpLastModifiedTime = _Time # pLastModifiedTime_,
      _cpCachePolicyConfig = pCachePolicyConfig_
    }

-- | The unique identifier for the cache policy.
cpId :: Lens' CachePolicy Text
cpId = lens _cpId (\s a -> s {_cpId = a})

-- | The date and time when the cache policy was last modified.
cpLastModifiedTime :: Lens' CachePolicy UTCTime
cpLastModifiedTime = lens _cpLastModifiedTime (\s a -> s {_cpLastModifiedTime = a}) . _Time

-- | The cache policy configuration.
cpCachePolicyConfig :: Lens' CachePolicy CachePolicyConfig
cpCachePolicyConfig = lens _cpCachePolicyConfig (\s a -> s {_cpCachePolicyConfig = a})

instance FromXML CachePolicy where
  parseXML x =
    CachePolicy'
      <$> (x .@ "Id")
      <*> (x .@ "LastModifiedTime")
      <*> (x .@ "CachePolicyConfig")

instance Hashable CachePolicy

instance NFData CachePolicy
