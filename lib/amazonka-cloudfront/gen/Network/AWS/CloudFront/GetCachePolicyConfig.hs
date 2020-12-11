{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- To get a cache policy configuration, you must provide the policy’s identifier. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
module Network.AWS.CloudFront.GetCachePolicyConfig
  ( -- * Creating a request
    GetCachePolicyConfig (..),
    mkGetCachePolicyConfig,

    -- ** Request lenses
    gcpcId,

    -- * Destructuring the response
    GetCachePolicyConfigResponse (..),
    mkGetCachePolicyConfigResponse,

    -- ** Response lenses
    gcpcrsETag,
    gcpcrsCachePolicyConfig,
    gcpcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCachePolicyConfig' smart constructor.
newtype GetCachePolicyConfig = GetCachePolicyConfig'
  { id ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCachePolicyConfig' with the minimum fields required to make a request.
--
-- * 'id' - The unique identifier for the cache policy. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
mkGetCachePolicyConfig ::
  -- | 'id'
  Lude.Text ->
  GetCachePolicyConfig
mkGetCachePolicyConfig pId_ = GetCachePolicyConfig' {id = pId_}

-- | The unique identifier for the cache policy. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcId :: Lens.Lens' GetCachePolicyConfig Lude.Text
gcpcId = Lens.lens (id :: GetCachePolicyConfig -> Lude.Text) (\s a -> s {id = a} :: GetCachePolicyConfig)
{-# DEPRECATED gcpcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetCachePolicyConfig where
  type Rs GetCachePolicyConfig = GetCachePolicyConfigResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetCachePolicyConfigResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCachePolicyConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetCachePolicyConfig where
  toPath GetCachePolicyConfig' {..} =
    Lude.mconcat
      ["/2020-05-31/cache-policy/", Lude.toBS id, "/config"]

instance Lude.ToQuery GetCachePolicyConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCachePolicyConfigResponse' smart constructor.
data GetCachePolicyConfigResponse = GetCachePolicyConfigResponse'
  { eTag ::
      Lude.Maybe Lude.Text,
    cachePolicyConfig ::
      Lude.Maybe CachePolicyConfig,
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

-- | Creates a value of 'GetCachePolicyConfigResponse' with the minimum fields required to make a request.
--
-- * 'cachePolicyConfig' - The cache policy configuration.
-- * 'eTag' - The current version of the cache policy.
-- * 'responseStatus' - The response status code.
mkGetCachePolicyConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCachePolicyConfigResponse
mkGetCachePolicyConfigResponse pResponseStatus_ =
  GetCachePolicyConfigResponse'
    { eTag = Lude.Nothing,
      cachePolicyConfig = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the cache policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcrsETag :: Lens.Lens' GetCachePolicyConfigResponse (Lude.Maybe Lude.Text)
gcpcrsETag = Lens.lens (eTag :: GetCachePolicyConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetCachePolicyConfigResponse)
{-# DEPRECATED gcpcrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The cache policy configuration.
--
-- /Note:/ Consider using 'cachePolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcrsCachePolicyConfig :: Lens.Lens' GetCachePolicyConfigResponse (Lude.Maybe CachePolicyConfig)
gcpcrsCachePolicyConfig = Lens.lens (cachePolicyConfig :: GetCachePolicyConfigResponse -> Lude.Maybe CachePolicyConfig) (\s a -> s {cachePolicyConfig = a} :: GetCachePolicyConfigResponse)
{-# DEPRECATED gcpcrsCachePolicyConfig "Use generic-lens or generic-optics with 'cachePolicyConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcrsResponseStatus :: Lens.Lens' GetCachePolicyConfigResponse Lude.Int
gcpcrsResponseStatus = Lens.lens (responseStatus :: GetCachePolicyConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCachePolicyConfigResponse)
{-# DEPRECATED gcpcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
