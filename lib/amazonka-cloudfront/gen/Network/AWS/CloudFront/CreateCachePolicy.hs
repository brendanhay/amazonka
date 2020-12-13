{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- After you create a cache policy, you can attach it to one or more cache behaviors. When it’s attached to a cache behavior, the cache policy determines the following:
--
--     * The values that CloudFront includes in the /cache key/ . These values can include HTTP headers, cookies, and URL query strings. CloudFront uses the cache key to find an object in its cache that it can return to the viewer.
--
--
--     * The default, minimum, and maximum time to live (TTL) values that you want objects to stay in the CloudFront cache.
--
--
-- The headers, cookies, and query strings that are included in the cache key are automatically included in requests that CloudFront sends to the origin. CloudFront sends a request when it can’t find an object in its cache that matches the request’s cache key. If you want to send values to the origin but /not/ include them in the cache key, use @OriginRequestPolicy@ .
-- For more information about cache policies, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html Controlling the cache key> in the /Amazon CloudFront Developer Guide/ .
module Network.AWS.CloudFront.CreateCachePolicy
  ( -- * Creating a request
    CreateCachePolicy (..),
    mkCreateCachePolicy,

    -- ** Request lenses
    ccpCachePolicyConfig,

    -- * Destructuring the response
    CreateCachePolicyResponse (..),
    mkCreateCachePolicyResponse,

    -- ** Response lenses
    ccprsCachePolicy,
    ccprsETag,
    ccprsLocation,
    ccprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCachePolicy' smart constructor.
newtype CreateCachePolicy = CreateCachePolicy'
  { -- | A cache policy configuration.
    cachePolicyConfig :: CachePolicyConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCachePolicy' with the minimum fields required to make a request.
--
-- * 'cachePolicyConfig' - A cache policy configuration.
mkCreateCachePolicy ::
  -- | 'cachePolicyConfig'
  CachePolicyConfig ->
  CreateCachePolicy
mkCreateCachePolicy pCachePolicyConfig_ =
  CreateCachePolicy' {cachePolicyConfig = pCachePolicyConfig_}

-- | A cache policy configuration.
--
-- /Note:/ Consider using 'cachePolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpCachePolicyConfig :: Lens.Lens' CreateCachePolicy CachePolicyConfig
ccpCachePolicyConfig = Lens.lens (cachePolicyConfig :: CreateCachePolicy -> CachePolicyConfig) (\s a -> s {cachePolicyConfig = a} :: CreateCachePolicy)
{-# DEPRECATED ccpCachePolicyConfig "Use generic-lens or generic-optics with 'cachePolicyConfig' instead." #-}

instance Lude.AWSRequest CreateCachePolicy where
  type Rs CreateCachePolicy = CreateCachePolicyResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          CreateCachePolicyResponse'
            Lude.<$> (Lude.parseXML x)
            Lude.<*> (h Lude..#? "ETag")
            Lude.<*> (h Lude..#? "Location")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateCachePolicy where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CachePolicyConfig"
      Lude.. cachePolicyConfig

instance Lude.ToHeaders CreateCachePolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateCachePolicy where
  toPath = Lude.const "/2020-05-31/cache-policy"

instance Lude.ToQuery CreateCachePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCachePolicyResponse' smart constructor.
data CreateCachePolicyResponse = CreateCachePolicyResponse'
  { -- | A cache policy.
    cachePolicy :: Lude.Maybe CachePolicy,
    -- | The current version of the cache policy.
    eTag :: Lude.Maybe Lude.Text,
    -- | The fully qualified URI of the cache policy just created.
    location :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCachePolicyResponse' with the minimum fields required to make a request.
--
-- * 'cachePolicy' - A cache policy.
-- * 'eTag' - The current version of the cache policy.
-- * 'location' - The fully qualified URI of the cache policy just created.
-- * 'responseStatus' - The response status code.
mkCreateCachePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCachePolicyResponse
mkCreateCachePolicyResponse pResponseStatus_ =
  CreateCachePolicyResponse'
    { cachePolicy = Lude.Nothing,
      eTag = Lude.Nothing,
      location = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A cache policy.
--
-- /Note:/ Consider using 'cachePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprsCachePolicy :: Lens.Lens' CreateCachePolicyResponse (Lude.Maybe CachePolicy)
ccprsCachePolicy = Lens.lens (cachePolicy :: CreateCachePolicyResponse -> Lude.Maybe CachePolicy) (\s a -> s {cachePolicy = a} :: CreateCachePolicyResponse)
{-# DEPRECATED ccprsCachePolicy "Use generic-lens or generic-optics with 'cachePolicy' instead." #-}

-- | The current version of the cache policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprsETag :: Lens.Lens' CreateCachePolicyResponse (Lude.Maybe Lude.Text)
ccprsETag = Lens.lens (eTag :: CreateCachePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: CreateCachePolicyResponse)
{-# DEPRECATED ccprsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The fully qualified URI of the cache policy just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprsLocation :: Lens.Lens' CreateCachePolicyResponse (Lude.Maybe Lude.Text)
ccprsLocation = Lens.lens (location :: CreateCachePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: CreateCachePolicyResponse)
{-# DEPRECATED ccprsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprsResponseStatus :: Lens.Lens' CreateCachePolicyResponse Lude.Int
ccprsResponseStatus = Lens.lens (responseStatus :: CreateCachePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCachePolicyResponse)
{-# DEPRECATED ccprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
