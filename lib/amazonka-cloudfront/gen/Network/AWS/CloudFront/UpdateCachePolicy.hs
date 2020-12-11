{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- When you update a cache policy configuration, all the fields are updated with the values provided in the request. You cannot update some fields independent of others. To update a cache policy configuration:
--
--     * Use @GetCachePolicyConfig@ to get the current configuration.
--
--
--     * Locally modify the fields in the cache policy configuration that you want to update.
--
--
--     * Call @UpdateCachePolicy@ by providing the entire cache policy configuration, including the fields that you modified and those that you didn’t.
module Network.AWS.CloudFront.UpdateCachePolicy
  ( -- * Creating a request
    UpdateCachePolicy (..),
    mkUpdateCachePolicy,

    -- ** Request lenses
    ucpIfMatch,
    ucpCachePolicyConfig,
    ucpId,

    -- * Destructuring the response
    UpdateCachePolicyResponse (..),
    mkUpdateCachePolicyResponse,

    -- ** Response lenses
    ucprsCachePolicy,
    ucprsETag,
    ucprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateCachePolicy' smart constructor.
data UpdateCachePolicy = UpdateCachePolicy'
  { ifMatch ::
      Lude.Maybe Lude.Text,
    cachePolicyConfig :: CachePolicyConfig,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCachePolicy' with the minimum fields required to make a request.
--
-- * 'cachePolicyConfig' - A cache policy configuration.
-- * 'id' - The unique identifier for the cache policy that you are updating. The identifier is returned in a cache behavior’s @CachePolicyId@ field in the response to @GetDistributionConfig@ .
-- * 'ifMatch' - The version of the cache policy that you are updating. The version is returned in the cache policy’s @ETag@ field in the response to @GetCachePolicyConfig@ .
mkUpdateCachePolicy ::
  -- | 'cachePolicyConfig'
  CachePolicyConfig ->
  -- | 'id'
  Lude.Text ->
  UpdateCachePolicy
mkUpdateCachePolicy pCachePolicyConfig_ pId_ =
  UpdateCachePolicy'
    { ifMatch = Lude.Nothing,
      cachePolicyConfig = pCachePolicyConfig_,
      id = pId_
    }

-- | The version of the cache policy that you are updating. The version is returned in the cache policy’s @ETag@ field in the response to @GetCachePolicyConfig@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpIfMatch :: Lens.Lens' UpdateCachePolicy (Lude.Maybe Lude.Text)
ucpIfMatch = Lens.lens (ifMatch :: UpdateCachePolicy -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: UpdateCachePolicy)
{-# DEPRECATED ucpIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | A cache policy configuration.
--
-- /Note:/ Consider using 'cachePolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpCachePolicyConfig :: Lens.Lens' UpdateCachePolicy CachePolicyConfig
ucpCachePolicyConfig = Lens.lens (cachePolicyConfig :: UpdateCachePolicy -> CachePolicyConfig) (\s a -> s {cachePolicyConfig = a} :: UpdateCachePolicy)
{-# DEPRECATED ucpCachePolicyConfig "Use generic-lens or generic-optics with 'cachePolicyConfig' instead." #-}

-- | The unique identifier for the cache policy that you are updating. The identifier is returned in a cache behavior’s @CachePolicyId@ field in the response to @GetDistributionConfig@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpId :: Lens.Lens' UpdateCachePolicy Lude.Text
ucpId = Lens.lens (id :: UpdateCachePolicy -> Lude.Text) (\s a -> s {id = a} :: UpdateCachePolicy)
{-# DEPRECATED ucpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest UpdateCachePolicy where
  type Rs UpdateCachePolicy = UpdateCachePolicyResponse
  request = Req.putXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          UpdateCachePolicyResponse'
            Lude.<$> (Lude.parseXML x)
            Lude.<*> (h Lude..#? "ETag")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement UpdateCachePolicy where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CachePolicyConfig"
      Lude.. cachePolicyConfig

instance Lude.ToHeaders UpdateCachePolicy where
  toHeaders UpdateCachePolicy' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath UpdateCachePolicy where
  toPath UpdateCachePolicy' {..} =
    Lude.mconcat ["/2020-05-31/cache-policy/", Lude.toBS id]

instance Lude.ToQuery UpdateCachePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateCachePolicyResponse' smart constructor.
data UpdateCachePolicyResponse = UpdateCachePolicyResponse'
  { cachePolicy ::
      Lude.Maybe CachePolicy,
    eTag :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateCachePolicyResponse' with the minimum fields required to make a request.
--
-- * 'cachePolicy' - A cache policy.
-- * 'eTag' - The current version of the cache policy.
-- * 'responseStatus' - The response status code.
mkUpdateCachePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateCachePolicyResponse
mkUpdateCachePolicyResponse pResponseStatus_ =
  UpdateCachePolicyResponse'
    { cachePolicy = Lude.Nothing,
      eTag = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A cache policy.
--
-- /Note:/ Consider using 'cachePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprsCachePolicy :: Lens.Lens' UpdateCachePolicyResponse (Lude.Maybe CachePolicy)
ucprsCachePolicy = Lens.lens (cachePolicy :: UpdateCachePolicyResponse -> Lude.Maybe CachePolicy) (\s a -> s {cachePolicy = a} :: UpdateCachePolicyResponse)
{-# DEPRECATED ucprsCachePolicy "Use generic-lens or generic-optics with 'cachePolicy' instead." #-}

-- | The current version of the cache policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprsETag :: Lens.Lens' UpdateCachePolicyResponse (Lude.Maybe Lude.Text)
ucprsETag = Lens.lens (eTag :: UpdateCachePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: UpdateCachePolicyResponse)
{-# DEPRECATED ucprsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprsResponseStatus :: Lens.Lens' UpdateCachePolicyResponse Lude.Int
ucprsResponseStatus = Lens.lens (responseStatus :: UpdateCachePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateCachePolicyResponse)
{-# DEPRECATED ucprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
