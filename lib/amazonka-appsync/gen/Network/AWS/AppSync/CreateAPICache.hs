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
-- Module      : Network.AWS.AppSync.CreateAPICache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cache for the GraphQL API.
module Network.AWS.AppSync.CreateAPICache
  ( -- * Creating a Request
    createAPICache,
    CreateAPICache,

    -- * Request Lenses
    cacAtRestEncryptionEnabled,
    cacTransitEncryptionEnabled,
    cacApiId,
    cacTtl,
    cacApiCachingBehavior,
    cacType,

    -- * Destructuring the Response
    createAPICacheResponse,
    CreateAPICacheResponse,

    -- * Response Lenses
    cacrsApiCache,
    cacrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @CreateApiCache@ operation.
--
--
--
-- /See:/ 'createAPICache' smart constructor.
data CreateAPICache = CreateAPICache'
  { _cacAtRestEncryptionEnabled ::
      !(Maybe Bool),
    _cacTransitEncryptionEnabled :: !(Maybe Bool),
    _cacApiId :: !Text,
    _cacTtl :: !Integer,
    _cacApiCachingBehavior :: !APICachingBehavior,
    _cacType :: !APICacheType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAPICache' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cacAtRestEncryptionEnabled' - At rest encryption flag for cache. This setting cannot be updated after creation.
--
-- * 'cacTransitEncryptionEnabled' - Transit encryption flag when connecting to cache. This setting cannot be updated after creation.
--
-- * 'cacApiId' - The GraphQL API Id.
--
-- * 'cacTtl' - TTL in seconds for cache entries. Valid values are between 1 and 3600 seconds.
--
-- * 'cacApiCachingBehavior' - Caching behavior.     * __FULL_REQUEST_CACHING__ : All requests are fully cached.     * __PER_RESOLVER_CACHING__ : Individual resolvers that you specify are cached.
--
-- * 'cacType' - The cache instance type. Valid values are      * @SMALL@      * @MEDIUM@      * @LARGE@      * @XLARGE@      * @LARGE_2X@      * @LARGE_4X@      * @LARGE_8X@ (not available in all regions)     * @LARGE_12X@  Historically, instance types were identified by an EC2-style value. As of July 2020, this is deprecated, and the generic identifiers above should be used. The following legacy instance types are available, but their use is discouraged:     * __T2_SMALL__ : A t2.small instance type.     * __T2_MEDIUM__ : A t2.medium instance type.     * __R4_LARGE__ : A r4.large instance type.     * __R4_XLARGE__ : A r4.xlarge instance type.     * __R4_2XLARGE__ : A r4.2xlarge instance type.     * __R4_4XLARGE__ : A r4.4xlarge instance type.     * __R4_8XLARGE__ : A r4.8xlarge instance type.
createAPICache ::
  -- | 'cacApiId'
  Text ->
  -- | 'cacTtl'
  Integer ->
  -- | 'cacApiCachingBehavior'
  APICachingBehavior ->
  -- | 'cacType'
  APICacheType ->
  CreateAPICache
createAPICache pApiId_ pTtl_ pApiCachingBehavior_ pType_ =
  CreateAPICache'
    { _cacAtRestEncryptionEnabled = Nothing,
      _cacTransitEncryptionEnabled = Nothing,
      _cacApiId = pApiId_,
      _cacTtl = pTtl_,
      _cacApiCachingBehavior = pApiCachingBehavior_,
      _cacType = pType_
    }

-- | At rest encryption flag for cache. This setting cannot be updated after creation.
cacAtRestEncryptionEnabled :: Lens' CreateAPICache (Maybe Bool)
cacAtRestEncryptionEnabled = lens _cacAtRestEncryptionEnabled (\s a -> s {_cacAtRestEncryptionEnabled = a})

-- | Transit encryption flag when connecting to cache. This setting cannot be updated after creation.
cacTransitEncryptionEnabled :: Lens' CreateAPICache (Maybe Bool)
cacTransitEncryptionEnabled = lens _cacTransitEncryptionEnabled (\s a -> s {_cacTransitEncryptionEnabled = a})

-- | The GraphQL API Id.
cacApiId :: Lens' CreateAPICache Text
cacApiId = lens _cacApiId (\s a -> s {_cacApiId = a})

-- | TTL in seconds for cache entries. Valid values are between 1 and 3600 seconds.
cacTtl :: Lens' CreateAPICache Integer
cacTtl = lens _cacTtl (\s a -> s {_cacTtl = a})

-- | Caching behavior.     * __FULL_REQUEST_CACHING__ : All requests are fully cached.     * __PER_RESOLVER_CACHING__ : Individual resolvers that you specify are cached.
cacApiCachingBehavior :: Lens' CreateAPICache APICachingBehavior
cacApiCachingBehavior = lens _cacApiCachingBehavior (\s a -> s {_cacApiCachingBehavior = a})

-- | The cache instance type. Valid values are      * @SMALL@      * @MEDIUM@      * @LARGE@      * @XLARGE@      * @LARGE_2X@      * @LARGE_4X@      * @LARGE_8X@ (not available in all regions)     * @LARGE_12X@  Historically, instance types were identified by an EC2-style value. As of July 2020, this is deprecated, and the generic identifiers above should be used. The following legacy instance types are available, but their use is discouraged:     * __T2_SMALL__ : A t2.small instance type.     * __T2_MEDIUM__ : A t2.medium instance type.     * __R4_LARGE__ : A r4.large instance type.     * __R4_XLARGE__ : A r4.xlarge instance type.     * __R4_2XLARGE__ : A r4.2xlarge instance type.     * __R4_4XLARGE__ : A r4.4xlarge instance type.     * __R4_8XLARGE__ : A r4.8xlarge instance type.
cacType :: Lens' CreateAPICache APICacheType
cacType = lens _cacType (\s a -> s {_cacType = a})

instance AWSRequest CreateAPICache where
  type Rs CreateAPICache = CreateAPICacheResponse
  request = postJSON appSync
  response =
    receiveJSON
      ( \s h x ->
          CreateAPICacheResponse'
            <$> (x .?> "apiCache") <*> (pure (fromEnum s))
      )

instance Hashable CreateAPICache

instance NFData CreateAPICache

instance ToHeaders CreateAPICache where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateAPICache where
  toJSON CreateAPICache' {..} =
    object
      ( catMaybes
          [ ("atRestEncryptionEnabled" .=) <$> _cacAtRestEncryptionEnabled,
            ("transitEncryptionEnabled" .=) <$> _cacTransitEncryptionEnabled,
            Just ("ttl" .= _cacTtl),
            Just ("apiCachingBehavior" .= _cacApiCachingBehavior),
            Just ("type" .= _cacType)
          ]
      )

instance ToPath CreateAPICache where
  toPath CreateAPICache' {..} =
    mconcat ["/v1/apis/", toBS _cacApiId, "/ApiCaches"]

instance ToQuery CreateAPICache where
  toQuery = const mempty

-- | Represents the output of a @CreateApiCache@ operation.
--
--
--
-- /See:/ 'createAPICacheResponse' smart constructor.
data CreateAPICacheResponse = CreateAPICacheResponse'
  { _cacrsApiCache ::
      !(Maybe APICache),
    _cacrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAPICacheResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cacrsApiCache' - The @ApiCache@ object.
--
-- * 'cacrsResponseStatus' - -- | The response status code.
createAPICacheResponse ::
  -- | 'cacrsResponseStatus'
  Int ->
  CreateAPICacheResponse
createAPICacheResponse pResponseStatus_ =
  CreateAPICacheResponse'
    { _cacrsApiCache = Nothing,
      _cacrsResponseStatus = pResponseStatus_
    }

-- | The @ApiCache@ object.
cacrsApiCache :: Lens' CreateAPICacheResponse (Maybe APICache)
cacrsApiCache = lens _cacrsApiCache (\s a -> s {_cacrsApiCache = a})

-- | -- | The response status code.
cacrsResponseStatus :: Lens' CreateAPICacheResponse Int
cacrsResponseStatus = lens _cacrsResponseStatus (\s a -> s {_cacrsResponseStatus = a})

instance NFData CreateAPICacheResponse
