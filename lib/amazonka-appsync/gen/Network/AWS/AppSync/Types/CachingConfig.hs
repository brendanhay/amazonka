{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.CachingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.CachingConfig
  ( CachingConfig (..)
  -- * Smart constructor
  , mkCachingConfig
  -- * Lenses
  , ccCachingKeys
  , ccTtl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The caching configuration for a resolver that has caching enabled.
--
-- /See:/ 'mkCachingConfig' smart constructor.
data CachingConfig = CachingConfig'
  { cachingKeys :: Core.Maybe [Core.Text]
    -- ^ The caching keys for a resolver that has caching enabled.
--
-- Valid values are entries from the @> context.arguments@ , @> context.source@ , and @> context.identity@ maps.
  , ttl :: Core.Maybe Core.Integer
    -- ^ The TTL in seconds for a resolver that has caching enabled.
--
-- Valid values are between 1 and 3600 seconds.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CachingConfig' value with any optional fields omitted.
mkCachingConfig
    :: CachingConfig
mkCachingConfig
  = CachingConfig'{cachingKeys = Core.Nothing, ttl = Core.Nothing}

-- | The caching keys for a resolver that has caching enabled.
--
-- Valid values are entries from the @> context.arguments@ , @> context.source@ , and @> context.identity@ maps.
--
-- /Note:/ Consider using 'cachingKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCachingKeys :: Lens.Lens' CachingConfig (Core.Maybe [Core.Text])
ccCachingKeys = Lens.field @"cachingKeys"
{-# INLINEABLE ccCachingKeys #-}
{-# DEPRECATED cachingKeys "Use generic-lens or generic-optics with 'cachingKeys' instead"  #-}

-- | The TTL in seconds for a resolver that has caching enabled.
--
-- Valid values are between 1 and 3600 seconds.
--
-- /Note:/ Consider using 'ttl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTtl :: Lens.Lens' CachingConfig (Core.Maybe Core.Integer)
ccTtl = Lens.field @"ttl"
{-# INLINEABLE ccTtl #-}
{-# DEPRECATED ttl "Use generic-lens or generic-optics with 'ttl' instead"  #-}

instance Core.FromJSON CachingConfig where
        toJSON CachingConfig{..}
          = Core.object
              (Core.catMaybes
                 [("cachingKeys" Core..=) Core.<$> cachingKeys,
                  ("ttl" Core..=) Core.<$> ttl])

instance Core.FromJSON CachingConfig where
        parseJSON
          = Core.withObject "CachingConfig" Core.$
              \ x ->
                CachingConfig' Core.<$>
                  (x Core..:? "cachingKeys") Core.<*> x Core..:? "ttl"
