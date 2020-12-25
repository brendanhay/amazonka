{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicy
  ( CachePolicy (..),

    -- * Smart constructor
    mkCachePolicy,

    -- * Lenses
    cpId,
    cpLastModifiedTime,
    cpCachePolicyConfig,
  )
where

import qualified Network.AWS.CloudFront.Types.CachePolicyConfig as Types
import qualified Network.AWS.CloudFront.Types.Id as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A cache policy.
--
-- When it’s attached to a cache behavior, the cache policy determines the following:
--
--     * The values that CloudFront includes in the cache key. These values can include HTTP headers, cookies, and URL query strings. CloudFront uses the cache key to find an object in its cache that it can return to the viewer.
--
--
--     * The default, minimum, and maximum time to live (TTL) values that you want objects to stay in the CloudFront cache.
--
--
-- The headers, cookies, and query strings that are included in the cache key are automatically included in requests that CloudFront sends to the origin. CloudFront sends a request when it can’t find a valid object in its cache that matches the request’s cache key. If you want to send values to the origin but /not/ include them in the cache key, use @OriginRequestPolicy@ .
--
-- /See:/ 'mkCachePolicy' smart constructor.
data CachePolicy = CachePolicy'
  { -- | The unique identifier for the cache policy.
    id :: Types.Id,
    -- | The date and time when the cache policy was last modified.
    lastModifiedTime :: Core.UTCTime,
    -- | The cache policy configuration.
    cachePolicyConfig :: Types.CachePolicyConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CachePolicy' value with any optional fields omitted.
mkCachePolicy ::
  -- | 'id'
  Types.Id ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  -- | 'cachePolicyConfig'
  Types.CachePolicyConfig ->
  CachePolicy
mkCachePolicy id lastModifiedTime cachePolicyConfig =
  CachePolicy' {id, lastModifiedTime, cachePolicyConfig}

-- | The unique identifier for the cache policy.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpId :: Lens.Lens' CachePolicy Types.Id
cpId = Lens.field @"id"
{-# DEPRECATED cpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time when the cache policy was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpLastModifiedTime :: Lens.Lens' CachePolicy Core.UTCTime
cpLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED cpLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The cache policy configuration.
--
-- /Note:/ Consider using 'cachePolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpCachePolicyConfig :: Lens.Lens' CachePolicy Types.CachePolicyConfig
cpCachePolicyConfig = Lens.field @"cachePolicyConfig"
{-# DEPRECATED cpCachePolicyConfig "Use generic-lens or generic-optics with 'cachePolicyConfig' instead." #-}

instance Core.FromXML CachePolicy where
  parseXML x =
    CachePolicy'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "LastModifiedTime")
      Core.<*> (x Core..@ "CachePolicyConfig")
