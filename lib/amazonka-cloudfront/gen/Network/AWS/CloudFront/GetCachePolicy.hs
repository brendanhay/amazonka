{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetCachePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a cache policy, including the following metadata:
--
--
--     * The policy’s identifier.
--
--
--     * The date and time when the policy was last modified.
--
--
-- To get a cache policy, you must provide the policy’s identifier. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
module Network.AWS.CloudFront.GetCachePolicy
  ( -- * Creating a request
    GetCachePolicy (..),
    mkGetCachePolicy,

    -- ** Request lenses
    gcpId,

    -- * Destructuring the response
    GetCachePolicyResponse (..),
    mkGetCachePolicyResponse,

    -- ** Response lenses
    gcprsCachePolicy,
    gcprsETag,
    gcprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCachePolicy' smart constructor.
newtype GetCachePolicy = GetCachePolicy' {id :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCachePolicy' with the minimum fields required to make a request.
--
-- * 'id' - The unique identifier for the cache policy. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
mkGetCachePolicy ::
  -- | 'id'
  Lude.Text ->
  GetCachePolicy
mkGetCachePolicy pId_ = GetCachePolicy' {id = pId_}

-- | The unique identifier for the cache policy. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpId :: Lens.Lens' GetCachePolicy Lude.Text
gcpId = Lens.lens (id :: GetCachePolicy -> Lude.Text) (\s a -> s {id = a} :: GetCachePolicy)
{-# DEPRECATED gcpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetCachePolicy where
  type Rs GetCachePolicy = GetCachePolicyResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetCachePolicyResponse'
            Lude.<$> (Lude.parseXML x)
            Lude.<*> (h Lude..#? "ETag")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCachePolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetCachePolicy where
  toPath GetCachePolicy' {..} =
    Lude.mconcat ["/2020-05-31/cache-policy/", Lude.toBS id]

instance Lude.ToQuery GetCachePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCachePolicyResponse' smart constructor.
data GetCachePolicyResponse = GetCachePolicyResponse'
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

-- | Creates a value of 'GetCachePolicyResponse' with the minimum fields required to make a request.
--
-- * 'cachePolicy' - The cache policy.
-- * 'eTag' - The current version of the cache policy.
-- * 'responseStatus' - The response status code.
mkGetCachePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCachePolicyResponse
mkGetCachePolicyResponse pResponseStatus_ =
  GetCachePolicyResponse'
    { cachePolicy = Lude.Nothing,
      eTag = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The cache policy.
--
-- /Note:/ Consider using 'cachePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcprsCachePolicy :: Lens.Lens' GetCachePolicyResponse (Lude.Maybe CachePolicy)
gcprsCachePolicy = Lens.lens (cachePolicy :: GetCachePolicyResponse -> Lude.Maybe CachePolicy) (\s a -> s {cachePolicy = a} :: GetCachePolicyResponse)
{-# DEPRECATED gcprsCachePolicy "Use generic-lens or generic-optics with 'cachePolicy' instead." #-}

-- | The current version of the cache policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcprsETag :: Lens.Lens' GetCachePolicyResponse (Lude.Maybe Lude.Text)
gcprsETag = Lens.lens (eTag :: GetCachePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetCachePolicyResponse)
{-# DEPRECATED gcprsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcprsResponseStatus :: Lens.Lens' GetCachePolicyResponse Lude.Int
gcprsResponseStatus = Lens.lens (responseStatus :: GetCachePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCachePolicyResponse)
{-# DEPRECATED gcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
