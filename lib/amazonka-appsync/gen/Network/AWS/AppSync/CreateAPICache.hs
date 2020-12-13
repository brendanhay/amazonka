{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    CreateAPICache (..),
    mkCreateAPICache,

    -- ** Request lenses
    cacTtl,
    cacAtRestEncryptionEnabled,
    cacApiId,
    cacTransitEncryptionEnabled,
    cacApiCachingBehavior,
    cacType,

    -- * Destructuring the response
    CreateAPICacheResponse (..),
    mkCreateAPICacheResponse,

    -- ** Response lenses
    cacrsApiCache,
    cacrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @CreateApiCache@ operation.
--
-- /See:/ 'mkCreateAPICache' smart constructor.
data CreateAPICache = CreateAPICache'
  { -- | TTL in seconds for cache entries.
    --
    -- Valid values are between 1 and 3600 seconds.
    ttl :: Lude.Integer,
    -- | At rest encryption flag for cache. This setting cannot be updated after creation.
    atRestEncryptionEnabled :: Lude.Maybe Lude.Bool,
    -- | The GraphQL API Id.
    apiId :: Lude.Text,
    -- | Transit encryption flag when connecting to cache. This setting cannot be updated after creation.
    transitEncryptionEnabled :: Lude.Maybe Lude.Bool,
    -- | Caching behavior.
    --
    --
    --     * __FULL_REQUEST_CACHING__ : All requests are fully cached.
    --
    --
    --     * __PER_RESOLVER_CACHING__ : Individual resolvers that you specify are cached.
    apiCachingBehavior :: APICachingBehavior,
    -- | The cache instance type. Valid values are
    --
    --
    --     * @SMALL@
    --
    --
    --     * @MEDIUM@
    --
    --
    --     * @LARGE@
    --
    --
    --     * @XLARGE@
    --
    --
    --     * @LARGE_2X@
    --
    --
    --     * @LARGE_4X@
    --
    --
    --     * @LARGE_8X@ (not available in all regions)
    --
    --
    --     * @LARGE_12X@
    --
    --
    -- Historically, instance types were identified by an EC2-style value. As of July 2020, this is deprecated, and the generic identifiers above should be used.
    -- The following legacy instance types are available, but their use is discouraged:
    --
    --     * __T2_SMALL__ : A t2.small instance type.
    --
    --
    --     * __T2_MEDIUM__ : A t2.medium instance type.
    --
    --
    --     * __R4_LARGE__ : A r4.large instance type.
    --
    --
    --     * __R4_XLARGE__ : A r4.xlarge instance type.
    --
    --
    --     * __R4_2XLARGE__ : A r4.2xlarge instance type.
    --
    --
    --     * __R4_4XLARGE__ : A r4.4xlarge instance type.
    --
    --
    --     * __R4_8XLARGE__ : A r4.8xlarge instance type.
    type' :: APICacheType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAPICache' with the minimum fields required to make a request.
--
-- * 'ttl' - TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
-- * 'atRestEncryptionEnabled' - At rest encryption flag for cache. This setting cannot be updated after creation.
-- * 'apiId' - The GraphQL API Id.
-- * 'transitEncryptionEnabled' - Transit encryption flag when connecting to cache. This setting cannot be updated after creation.
-- * 'apiCachingBehavior' - Caching behavior.
--
--
--     * __FULL_REQUEST_CACHING__ : All requests are fully cached.
--
--
--     * __PER_RESOLVER_CACHING__ : Individual resolvers that you specify are cached.
--
--
-- * 'type'' - The cache instance type. Valid values are
--
--
--     * @SMALL@
--
--
--     * @MEDIUM@
--
--
--     * @LARGE@
--
--
--     * @XLARGE@
--
--
--     * @LARGE_2X@
--
--
--     * @LARGE_4X@
--
--
--     * @LARGE_8X@ (not available in all regions)
--
--
--     * @LARGE_12X@
--
--
-- Historically, instance types were identified by an EC2-style value. As of July 2020, this is deprecated, and the generic identifiers above should be used.
-- The following legacy instance types are available, but their use is discouraged:
--
--     * __T2_SMALL__ : A t2.small instance type.
--
--
--     * __T2_MEDIUM__ : A t2.medium instance type.
--
--
--     * __R4_LARGE__ : A r4.large instance type.
--
--
--     * __R4_XLARGE__ : A r4.xlarge instance type.
--
--
--     * __R4_2XLARGE__ : A r4.2xlarge instance type.
--
--
--     * __R4_4XLARGE__ : A r4.4xlarge instance type.
--
--
--     * __R4_8XLARGE__ : A r4.8xlarge instance type.
mkCreateAPICache ::
  -- | 'ttl'
  Lude.Integer ->
  -- | 'apiId'
  Lude.Text ->
  -- | 'apiCachingBehavior'
  APICachingBehavior ->
  -- | 'type''
  APICacheType ->
  CreateAPICache
mkCreateAPICache pTtl_ pApiId_ pApiCachingBehavior_ pType_ =
  CreateAPICache'
    { ttl = pTtl_,
      atRestEncryptionEnabled = Lude.Nothing,
      apiId = pApiId_,
      transitEncryptionEnabled = Lude.Nothing,
      apiCachingBehavior = pApiCachingBehavior_,
      type' = pType_
    }

-- | TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
--
-- /Note:/ Consider using 'ttl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacTtl :: Lens.Lens' CreateAPICache Lude.Integer
cacTtl = Lens.lens (ttl :: CreateAPICache -> Lude.Integer) (\s a -> s {ttl = a} :: CreateAPICache)
{-# DEPRECATED cacTtl "Use generic-lens or generic-optics with 'ttl' instead." #-}

-- | At rest encryption flag for cache. This setting cannot be updated after creation.
--
-- /Note:/ Consider using 'atRestEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacAtRestEncryptionEnabled :: Lens.Lens' CreateAPICache (Lude.Maybe Lude.Bool)
cacAtRestEncryptionEnabled = Lens.lens (atRestEncryptionEnabled :: CreateAPICache -> Lude.Maybe Lude.Bool) (\s a -> s {atRestEncryptionEnabled = a} :: CreateAPICache)
{-# DEPRECATED cacAtRestEncryptionEnabled "Use generic-lens or generic-optics with 'atRestEncryptionEnabled' instead." #-}

-- | The GraphQL API Id.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacApiId :: Lens.Lens' CreateAPICache Lude.Text
cacApiId = Lens.lens (apiId :: CreateAPICache -> Lude.Text) (\s a -> s {apiId = a} :: CreateAPICache)
{-# DEPRECATED cacApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | Transit encryption flag when connecting to cache. This setting cannot be updated after creation.
--
-- /Note:/ Consider using 'transitEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacTransitEncryptionEnabled :: Lens.Lens' CreateAPICache (Lude.Maybe Lude.Bool)
cacTransitEncryptionEnabled = Lens.lens (transitEncryptionEnabled :: CreateAPICache -> Lude.Maybe Lude.Bool) (\s a -> s {transitEncryptionEnabled = a} :: CreateAPICache)
{-# DEPRECATED cacTransitEncryptionEnabled "Use generic-lens or generic-optics with 'transitEncryptionEnabled' instead." #-}

-- | Caching behavior.
--
--
--     * __FULL_REQUEST_CACHING__ : All requests are fully cached.
--
--
--     * __PER_RESOLVER_CACHING__ : Individual resolvers that you specify are cached.
--
--
--
-- /Note:/ Consider using 'apiCachingBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacApiCachingBehavior :: Lens.Lens' CreateAPICache APICachingBehavior
cacApiCachingBehavior = Lens.lens (apiCachingBehavior :: CreateAPICache -> APICachingBehavior) (\s a -> s {apiCachingBehavior = a} :: CreateAPICache)
{-# DEPRECATED cacApiCachingBehavior "Use generic-lens or generic-optics with 'apiCachingBehavior' instead." #-}

-- | The cache instance type. Valid values are
--
--
--     * @SMALL@
--
--
--     * @MEDIUM@
--
--
--     * @LARGE@
--
--
--     * @XLARGE@
--
--
--     * @LARGE_2X@
--
--
--     * @LARGE_4X@
--
--
--     * @LARGE_8X@ (not available in all regions)
--
--
--     * @LARGE_12X@
--
--
-- Historically, instance types were identified by an EC2-style value. As of July 2020, this is deprecated, and the generic identifiers above should be used.
-- The following legacy instance types are available, but their use is discouraged:
--
--     * __T2_SMALL__ : A t2.small instance type.
--
--
--     * __T2_MEDIUM__ : A t2.medium instance type.
--
--
--     * __R4_LARGE__ : A r4.large instance type.
--
--
--     * __R4_XLARGE__ : A r4.xlarge instance type.
--
--
--     * __R4_2XLARGE__ : A r4.2xlarge instance type.
--
--
--     * __R4_4XLARGE__ : A r4.4xlarge instance type.
--
--
--     * __R4_8XLARGE__ : A r4.8xlarge instance type.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacType :: Lens.Lens' CreateAPICache APICacheType
cacType = Lens.lens (type' :: CreateAPICache -> APICacheType) (\s a -> s {type' = a} :: CreateAPICache)
{-# DEPRECATED cacType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest CreateAPICache where
  type Rs CreateAPICache = CreateAPICacheResponse
  request = Req.postJSON appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAPICacheResponse'
            Lude.<$> (x Lude..?> "apiCache") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAPICache where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAPICache where
  toJSON CreateAPICache' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ttl" Lude..= ttl),
            ("atRestEncryptionEnabled" Lude..=)
              Lude.<$> atRestEncryptionEnabled,
            ("transitEncryptionEnabled" Lude..=)
              Lude.<$> transitEncryptionEnabled,
            Lude.Just ("apiCachingBehavior" Lude..= apiCachingBehavior),
            Lude.Just ("type" Lude..= type')
          ]
      )

instance Lude.ToPath CreateAPICache where
  toPath CreateAPICache' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId, "/ApiCaches"]

instance Lude.ToQuery CreateAPICache where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @CreateApiCache@ operation.
--
-- /See:/ 'mkCreateAPICacheResponse' smart constructor.
data CreateAPICacheResponse = CreateAPICacheResponse'
  { -- | The @ApiCache@ object.
    apiCache :: Lude.Maybe APICache,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAPICacheResponse' with the minimum fields required to make a request.
--
-- * 'apiCache' - The @ApiCache@ object.
-- * 'responseStatus' - The response status code.
mkCreateAPICacheResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAPICacheResponse
mkCreateAPICacheResponse pResponseStatus_ =
  CreateAPICacheResponse'
    { apiCache = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ApiCache@ object.
--
-- /Note:/ Consider using 'apiCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacrsApiCache :: Lens.Lens' CreateAPICacheResponse (Lude.Maybe APICache)
cacrsApiCache = Lens.lens (apiCache :: CreateAPICacheResponse -> Lude.Maybe APICache) (\s a -> s {apiCache = a} :: CreateAPICacheResponse)
{-# DEPRECATED cacrsApiCache "Use generic-lens or generic-optics with 'apiCache' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacrsResponseStatus :: Lens.Lens' CreateAPICacheResponse Lude.Int
cacrsResponseStatus = Lens.lens (responseStatus :: CreateAPICacheResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAPICacheResponse)
{-# DEPRECATED cacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
