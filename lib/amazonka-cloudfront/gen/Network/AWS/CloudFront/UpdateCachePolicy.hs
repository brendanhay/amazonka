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
-- Module      : Network.AWS.CloudFront.UpdateCachePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a cache policy configuration.
--
--
-- When you update a cache policy configuration, all the fields are updated with the values provided in the request. You cannot update some fields independent of others. To update a cache policy configuration:
--
--     * Use @GetCachePolicyConfig@ to get the current configuration.
--
--     * Locally modify the fields in the cache policy configuration that you want to update.
--
--     * Call @UpdateCachePolicy@ by providing the entire cache policy configuration, including the fields that you modified and those that you didn’t.
module Network.AWS.CloudFront.UpdateCachePolicy
  ( -- * Creating a Request
    updateCachePolicy,
    UpdateCachePolicy,

    -- * Request Lenses
    ucpIfMatch,
    ucpCachePolicyConfig,
    ucpId,

    -- * Destructuring the Response
    updateCachePolicyResponse,
    UpdateCachePolicyResponse,

    -- * Response Lenses
    ucprsCachePolicy,
    ucprsETag,
    ucprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateCachePolicy' smart constructor.
data UpdateCachePolicy = UpdateCachePolicy'
  { _ucpIfMatch ::
      !(Maybe Text),
    _ucpCachePolicyConfig :: !CachePolicyConfig,
    _ucpId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateCachePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucpIfMatch' - The version of the cache policy that you are updating. The version is returned in the cache policy’s @ETag@ field in the response to @GetCachePolicyConfig@ .
--
-- * 'ucpCachePolicyConfig' - A cache policy configuration.
--
-- * 'ucpId' - The unique identifier for the cache policy that you are updating. The identifier is returned in a cache behavior’s @CachePolicyId@ field in the response to @GetDistributionConfig@ .
updateCachePolicy ::
  -- | 'ucpCachePolicyConfig'
  CachePolicyConfig ->
  -- | 'ucpId'
  Text ->
  UpdateCachePolicy
updateCachePolicy pCachePolicyConfig_ pId_ =
  UpdateCachePolicy'
    { _ucpIfMatch = Nothing,
      _ucpCachePolicyConfig = pCachePolicyConfig_,
      _ucpId = pId_
    }

-- | The version of the cache policy that you are updating. The version is returned in the cache policy’s @ETag@ field in the response to @GetCachePolicyConfig@ .
ucpIfMatch :: Lens' UpdateCachePolicy (Maybe Text)
ucpIfMatch = lens _ucpIfMatch (\s a -> s {_ucpIfMatch = a})

-- | A cache policy configuration.
ucpCachePolicyConfig :: Lens' UpdateCachePolicy CachePolicyConfig
ucpCachePolicyConfig = lens _ucpCachePolicyConfig (\s a -> s {_ucpCachePolicyConfig = a})

-- | The unique identifier for the cache policy that you are updating. The identifier is returned in a cache behavior’s @CachePolicyId@ field in the response to @GetDistributionConfig@ .
ucpId :: Lens' UpdateCachePolicy Text
ucpId = lens _ucpId (\s a -> s {_ucpId = a})

instance AWSRequest UpdateCachePolicy where
  type Rs UpdateCachePolicy = UpdateCachePolicyResponse
  request = putXML cloudFront
  response =
    receiveXML
      ( \s h x ->
          UpdateCachePolicyResponse'
            <$> (parseXML x) <*> (h .#? "ETag") <*> (pure (fromEnum s))
      )

instance Hashable UpdateCachePolicy

instance NFData UpdateCachePolicy

instance ToElement UpdateCachePolicy where
  toElement =
    mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CachePolicyConfig"
      . _ucpCachePolicyConfig

instance ToHeaders UpdateCachePolicy where
  toHeaders UpdateCachePolicy' {..} =
    mconcat ["If-Match" =# _ucpIfMatch]

instance ToPath UpdateCachePolicy where
  toPath UpdateCachePolicy' {..} =
    mconcat ["/2020-05-31/cache-policy/", toBS _ucpId]

instance ToQuery UpdateCachePolicy where
  toQuery = const mempty

-- | /See:/ 'updateCachePolicyResponse' smart constructor.
data UpdateCachePolicyResponse = UpdateCachePolicyResponse'
  { _ucprsCachePolicy ::
      !(Maybe CachePolicy),
    _ucprsETag :: !(Maybe Text),
    _ucprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateCachePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucprsCachePolicy' - A cache policy.
--
-- * 'ucprsETag' - The current version of the cache policy.
--
-- * 'ucprsResponseStatus' - -- | The response status code.
updateCachePolicyResponse ::
  -- | 'ucprsResponseStatus'
  Int ->
  UpdateCachePolicyResponse
updateCachePolicyResponse pResponseStatus_ =
  UpdateCachePolicyResponse'
    { _ucprsCachePolicy = Nothing,
      _ucprsETag = Nothing,
      _ucprsResponseStatus = pResponseStatus_
    }

-- | A cache policy.
ucprsCachePolicy :: Lens' UpdateCachePolicyResponse (Maybe CachePolicy)
ucprsCachePolicy = lens _ucprsCachePolicy (\s a -> s {_ucprsCachePolicy = a})

-- | The current version of the cache policy.
ucprsETag :: Lens' UpdateCachePolicyResponse (Maybe Text)
ucprsETag = lens _ucprsETag (\s a -> s {_ucprsETag = a})

-- | -- | The response status code.
ucprsResponseStatus :: Lens' UpdateCachePolicyResponse Int
ucprsResponseStatus = lens _ucprsResponseStatus (\s a -> s {_ucprsResponseStatus = a})

instance NFData UpdateCachePolicyResponse
