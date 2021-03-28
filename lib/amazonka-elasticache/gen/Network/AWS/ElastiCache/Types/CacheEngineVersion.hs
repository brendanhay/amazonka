{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheEngineVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.CacheEngineVersion
  ( CacheEngineVersion (..)
  -- * Smart constructor
  , mkCacheEngineVersion
  -- * Lenses
  , cevCacheEngineDescription
  , cevCacheEngineVersionDescription
  , cevCacheParameterGroupFamily
  , cevEngine
  , cevEngineVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides all of the details about a particular cache engine version.
--
-- /See:/ 'mkCacheEngineVersion' smart constructor.
data CacheEngineVersion = CacheEngineVersion'
  { cacheEngineDescription :: Core.Maybe Core.Text
    -- ^ The description of the cache engine.
  , cacheEngineVersionDescription :: Core.Maybe Core.Text
    -- ^ The description of the cache engine version.
  , cacheParameterGroupFamily :: Core.Maybe Core.Text
    -- ^ The name of the cache parameter group family associated with this cache engine.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ | 
  , engine :: Core.Maybe Core.Text
    -- ^ The name of the cache engine.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The version number of the cache engine.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CacheEngineVersion' value with any optional fields omitted.
mkCacheEngineVersion
    :: CacheEngineVersion
mkCacheEngineVersion
  = CacheEngineVersion'{cacheEngineDescription = Core.Nothing,
                        cacheEngineVersionDescription = Core.Nothing,
                        cacheParameterGroupFamily = Core.Nothing, engine = Core.Nothing,
                        engineVersion = Core.Nothing}

-- | The description of the cache engine.
--
-- /Note:/ Consider using 'cacheEngineDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cevCacheEngineDescription :: Lens.Lens' CacheEngineVersion (Core.Maybe Core.Text)
cevCacheEngineDescription = Lens.field @"cacheEngineDescription"
{-# INLINEABLE cevCacheEngineDescription #-}
{-# DEPRECATED cacheEngineDescription "Use generic-lens or generic-optics with 'cacheEngineDescription' instead"  #-}

-- | The description of the cache engine version.
--
-- /Note:/ Consider using 'cacheEngineVersionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cevCacheEngineVersionDescription :: Lens.Lens' CacheEngineVersion (Core.Maybe Core.Text)
cevCacheEngineVersionDescription = Lens.field @"cacheEngineVersionDescription"
{-# INLINEABLE cevCacheEngineVersionDescription #-}
{-# DEPRECATED cacheEngineVersionDescription "Use generic-lens or generic-optics with 'cacheEngineVersionDescription' instead"  #-}

-- | The name of the cache parameter group family associated with this cache engine.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ | 
--
-- /Note:/ Consider using 'cacheParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cevCacheParameterGroupFamily :: Lens.Lens' CacheEngineVersion (Core.Maybe Core.Text)
cevCacheParameterGroupFamily = Lens.field @"cacheParameterGroupFamily"
{-# INLINEABLE cevCacheParameterGroupFamily #-}
{-# DEPRECATED cacheParameterGroupFamily "Use generic-lens or generic-optics with 'cacheParameterGroupFamily' instead"  #-}

-- | The name of the cache engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cevEngine :: Lens.Lens' CacheEngineVersion (Core.Maybe Core.Text)
cevEngine = Lens.field @"engine"
{-# INLINEABLE cevEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The version number of the cache engine.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cevEngineVersion :: Lens.Lens' CacheEngineVersion (Core.Maybe Core.Text)
cevEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE cevEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

instance Core.FromXML CacheEngineVersion where
        parseXML x
          = CacheEngineVersion' Core.<$>
              (x Core..@? "CacheEngineDescription") Core.<*>
                x Core..@? "CacheEngineVersionDescription"
                Core.<*> x Core..@? "CacheParameterGroupFamily"
                Core.<*> x Core..@? "Engine"
                Core.<*> x Core..@? "EngineVersion"
