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
-- Module      : Network.AWS.CloudFront.GetCachePolicyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a cache policy configuration.
--
--
-- To get a cache policy configuration, you must provide the policy’s identifier. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
module Network.AWS.CloudFront.GetCachePolicyConfig
  ( -- * Creating a Request
    getCachePolicyConfig,
    GetCachePolicyConfig,

    -- * Request Lenses
    gcpcId,

    -- * Destructuring the Response
    getCachePolicyConfigResponse,
    GetCachePolicyConfigResponse,

    -- * Response Lenses
    gcpcrsETag,
    gcpcrsCachePolicyConfig,
    gcpcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCachePolicyConfig' smart constructor.
newtype GetCachePolicyConfig = GetCachePolicyConfig'
  { _gcpcId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCachePolicyConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcpcId' - The unique identifier for the cache policy. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
getCachePolicyConfig ::
  -- | 'gcpcId'
  Text ->
  GetCachePolicyConfig
getCachePolicyConfig pId_ = GetCachePolicyConfig' {_gcpcId = pId_}

-- | The unique identifier for the cache policy. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
gcpcId :: Lens' GetCachePolicyConfig Text
gcpcId = lens _gcpcId (\s a -> s {_gcpcId = a})

instance AWSRequest GetCachePolicyConfig where
  type Rs GetCachePolicyConfig = GetCachePolicyConfigResponse
  request = get cloudFront
  response =
    receiveXML
      ( \s h x ->
          GetCachePolicyConfigResponse'
            <$> (h .#? "ETag") <*> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetCachePolicyConfig

instance NFData GetCachePolicyConfig

instance ToHeaders GetCachePolicyConfig where
  toHeaders = const mempty

instance ToPath GetCachePolicyConfig where
  toPath GetCachePolicyConfig' {..} =
    mconcat ["/2020-05-31/cache-policy/", toBS _gcpcId, "/config"]

instance ToQuery GetCachePolicyConfig where
  toQuery = const mempty

-- | /See:/ 'getCachePolicyConfigResponse' smart constructor.
data GetCachePolicyConfigResponse = GetCachePolicyConfigResponse'
  { _gcpcrsETag ::
      !(Maybe Text),
    _gcpcrsCachePolicyConfig ::
      !(Maybe CachePolicyConfig),
    _gcpcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCachePolicyConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcpcrsETag' - The current version of the cache policy.
--
-- * 'gcpcrsCachePolicyConfig' - The cache policy configuration.
--
-- * 'gcpcrsResponseStatus' - -- | The response status code.
getCachePolicyConfigResponse ::
  -- | 'gcpcrsResponseStatus'
  Int ->
  GetCachePolicyConfigResponse
getCachePolicyConfigResponse pResponseStatus_ =
  GetCachePolicyConfigResponse'
    { _gcpcrsETag = Nothing,
      _gcpcrsCachePolicyConfig = Nothing,
      _gcpcrsResponseStatus = pResponseStatus_
    }

-- | The current version of the cache policy.
gcpcrsETag :: Lens' GetCachePolicyConfigResponse (Maybe Text)
gcpcrsETag = lens _gcpcrsETag (\s a -> s {_gcpcrsETag = a})

-- | The cache policy configuration.
gcpcrsCachePolicyConfig :: Lens' GetCachePolicyConfigResponse (Maybe CachePolicyConfig)
gcpcrsCachePolicyConfig = lens _gcpcrsCachePolicyConfig (\s a -> s {_gcpcrsCachePolicyConfig = a})

-- | -- | The response status code.
gcpcrsResponseStatus :: Lens' GetCachePolicyConfigResponse Int
gcpcrsResponseStatus = lens _gcpcrsResponseStatus (\s a -> s {_gcpcrsResponseStatus = a})

instance NFData GetCachePolicyConfigResponse
