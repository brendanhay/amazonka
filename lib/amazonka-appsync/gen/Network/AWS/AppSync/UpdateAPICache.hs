{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.UpdateAPICache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the cache for the GraphQL API.
module Network.AWS.AppSync.UpdateAPICache
  ( -- * Creating a request
    UpdateAPICache (..),
    mkUpdateAPICache,

    -- ** Request lenses
    uacApiId,
    uacTtl,
    uacApiCachingBehavior,
    uacType,

    -- * Destructuring the response
    UpdateAPICacheResponse (..),
    mkUpdateAPICacheResponse,

    -- ** Response lenses
    uacrsApiCache,
    uacrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @UpdateApiCache@ operation.
--
-- /See:/ 'mkUpdateAPICache' smart constructor.
data UpdateAPICache = UpdateAPICache'
  { apiId :: Lude.Text,
    ttl :: Lude.Integer,
    apiCachingBehavior :: APICachingBehavior,
    type' :: APICacheType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAPICache' with the minimum fields required to make a request.
--
-- * 'apiCachingBehavior' - Caching behavior.
--
--
--     * __FULL_REQUEST_CACHING__ : All requests are fully cached.
--
--
--     * __PER_RESOLVER_CACHING__ : Individual resolvers that you specify are cached.
--
--
-- * 'apiId' - The GraphQL API Id.
-- * 'ttl' - TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
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
mkUpdateAPICache ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'ttl'
  Lude.Integer ->
  -- | 'apiCachingBehavior'
  APICachingBehavior ->
  -- | 'type''
  APICacheType ->
  UpdateAPICache
mkUpdateAPICache pApiId_ pTtl_ pApiCachingBehavior_ pType_ =
  UpdateAPICache'
    { apiId = pApiId_,
      ttl = pTtl_,
      apiCachingBehavior = pApiCachingBehavior_,
      type' = pType_
    }

-- | The GraphQL API Id.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacApiId :: Lens.Lens' UpdateAPICache Lude.Text
uacApiId = Lens.lens (apiId :: UpdateAPICache -> Lude.Text) (\s a -> s {apiId = a} :: UpdateAPICache)
{-# DEPRECATED uacApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
--
-- /Note:/ Consider using 'ttl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacTtl :: Lens.Lens' UpdateAPICache Lude.Integer
uacTtl = Lens.lens (ttl :: UpdateAPICache -> Lude.Integer) (\s a -> s {ttl = a} :: UpdateAPICache)
{-# DEPRECATED uacTtl "Use generic-lens or generic-optics with 'ttl' instead." #-}

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
uacApiCachingBehavior :: Lens.Lens' UpdateAPICache APICachingBehavior
uacApiCachingBehavior = Lens.lens (apiCachingBehavior :: UpdateAPICache -> APICachingBehavior) (\s a -> s {apiCachingBehavior = a} :: UpdateAPICache)
{-# DEPRECATED uacApiCachingBehavior "Use generic-lens or generic-optics with 'apiCachingBehavior' instead." #-}

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
uacType :: Lens.Lens' UpdateAPICache APICacheType
uacType = Lens.lens (type' :: UpdateAPICache -> APICacheType) (\s a -> s {type' = a} :: UpdateAPICache)
{-# DEPRECATED uacType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest UpdateAPICache where
  type Rs UpdateAPICache = UpdateAPICacheResponse
  request = Req.postJSON appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAPICacheResponse'
            Lude.<$> (x Lude..?> "apiCache") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateAPICache where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAPICache where
  toJSON UpdateAPICache' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ttl" Lude..= ttl),
            Lude.Just ("apiCachingBehavior" Lude..= apiCachingBehavior),
            Lude.Just ("type" Lude..= type')
          ]
      )

instance Lude.ToPath UpdateAPICache where
  toPath UpdateAPICache' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId, "/ApiCaches/update"]

instance Lude.ToQuery UpdateAPICache where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @UpdateApiCache@ operation.
--
-- /See:/ 'mkUpdateAPICacheResponse' smart constructor.
data UpdateAPICacheResponse = UpdateAPICacheResponse'
  { apiCache ::
      Lude.Maybe APICache,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAPICacheResponse' with the minimum fields required to make a request.
--
-- * 'apiCache' - The @ApiCache@ object.
-- * 'responseStatus' - The response status code.
mkUpdateAPICacheResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAPICacheResponse
mkUpdateAPICacheResponse pResponseStatus_ =
  UpdateAPICacheResponse'
    { apiCache = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ApiCache@ object.
--
-- /Note:/ Consider using 'apiCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacrsApiCache :: Lens.Lens' UpdateAPICacheResponse (Lude.Maybe APICache)
uacrsApiCache = Lens.lens (apiCache :: UpdateAPICacheResponse -> Lude.Maybe APICache) (\s a -> s {apiCache = a} :: UpdateAPICacheResponse)
{-# DEPRECATED uacrsApiCache "Use generic-lens or generic-optics with 'apiCache' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacrsResponseStatus :: Lens.Lens' UpdateAPICacheResponse Lude.Int
uacrsResponseStatus = Lens.lens (responseStatus :: UpdateAPICacheResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAPICacheResponse)
{-# DEPRECATED uacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
