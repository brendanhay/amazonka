{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateCachePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cache policy.
--
--
-- After you create a cache policy, you can attach it to one or more cache behaviors. When it’s attached to a cache behavior, the cache policy determines the following:
--
--     * The values that CloudFront includes in the /cache key/ . These values can include HTTP headers, cookies, and URL query strings. CloudFront uses the cache key to find an object in its cache that it can return to the viewer.
--
--     * The default, minimum, and maximum time to live (TTL) values that you want objects to stay in the CloudFront cache.
--
--
--
-- The headers, cookies, and query strings that are included in the cache key are automatically included in requests that CloudFront sends to the origin. CloudFront sends a request when it can’t find an object in its cache that matches the request’s cache key. If you want to send values to the origin but /not/ include them in the cache key, use @OriginRequestPolicy@ .
--
-- For more information about cache policies, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html Controlling the cache key> in the /Amazon CloudFront Developer Guide/ .
module Network.AWS.CloudFront.CreateCachePolicy
  ( -- * Creating a Request
    createCachePolicy,
    CreateCachePolicy,

    -- * Request Lenses
    ccpCachePolicyConfig,

    -- * Destructuring the Response
    createCachePolicyResponse,
    CreateCachePolicyResponse,

    -- * Response Lenses
    ccprsCachePolicy,
    ccprsETag,
    ccprsLocation,
    ccprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCachePolicy' smart constructor.
newtype CreateCachePolicy = CreateCachePolicy'
  { _ccpCachePolicyConfig ::
      CachePolicyConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCachePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccpCachePolicyConfig' - A cache policy configuration.
createCachePolicy ::
  -- | 'ccpCachePolicyConfig'
  CachePolicyConfig ->
  CreateCachePolicy
createCachePolicy pCachePolicyConfig_ =
  CreateCachePolicy' {_ccpCachePolicyConfig = pCachePolicyConfig_}

-- | A cache policy configuration.
ccpCachePolicyConfig :: Lens' CreateCachePolicy CachePolicyConfig
ccpCachePolicyConfig = lens _ccpCachePolicyConfig (\s a -> s {_ccpCachePolicyConfig = a})

instance AWSRequest CreateCachePolicy where
  type Rs CreateCachePolicy = CreateCachePolicyResponse
  request = postXML cloudFront
  response =
    receiveXML
      ( \s h x ->
          CreateCachePolicyResponse'
            <$> (parseXML x)
            <*> (h .#? "ETag")
            <*> (h .#? "Location")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateCachePolicy

instance NFData CreateCachePolicy

instance ToElement CreateCachePolicy where
  toElement =
    mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CachePolicyConfig"
      . _ccpCachePolicyConfig

instance ToHeaders CreateCachePolicy where
  toHeaders = const mempty

instance ToPath CreateCachePolicy where
  toPath = const "/2020-05-31/cache-policy"

instance ToQuery CreateCachePolicy where
  toQuery = const mempty

-- | /See:/ 'createCachePolicyResponse' smart constructor.
data CreateCachePolicyResponse = CreateCachePolicyResponse'
  { _ccprsCachePolicy ::
      !(Maybe CachePolicy),
    _ccprsETag :: !(Maybe Text),
    _ccprsLocation :: !(Maybe Text),
    _ccprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCachePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccprsCachePolicy' - A cache policy.
--
-- * 'ccprsETag' - The current version of the cache policy.
--
-- * 'ccprsLocation' - The fully qualified URI of the cache policy just created.
--
-- * 'ccprsResponseStatus' - -- | The response status code.
createCachePolicyResponse ::
  -- | 'ccprsResponseStatus'
  Int ->
  CreateCachePolicyResponse
createCachePolicyResponse pResponseStatus_ =
  CreateCachePolicyResponse'
    { _ccprsCachePolicy = Nothing,
      _ccprsETag = Nothing,
      _ccprsLocation = Nothing,
      _ccprsResponseStatus = pResponseStatus_
    }

-- | A cache policy.
ccprsCachePolicy :: Lens' CreateCachePolicyResponse (Maybe CachePolicy)
ccprsCachePolicy = lens _ccprsCachePolicy (\s a -> s {_ccprsCachePolicy = a})

-- | The current version of the cache policy.
ccprsETag :: Lens' CreateCachePolicyResponse (Maybe Text)
ccprsETag = lens _ccprsETag (\s a -> s {_ccprsETag = a})

-- | The fully qualified URI of the cache policy just created.
ccprsLocation :: Lens' CreateCachePolicyResponse (Maybe Text)
ccprsLocation = lens _ccprsLocation (\s a -> s {_ccprsLocation = a})

-- | -- | The response status code.
ccprsResponseStatus :: Lens' CreateCachePolicyResponse Int
ccprsResponseStatus = lens _ccprsResponseStatus (\s a -> s {_ccprsResponseStatus = a})

instance NFData CreateCachePolicyResponse
