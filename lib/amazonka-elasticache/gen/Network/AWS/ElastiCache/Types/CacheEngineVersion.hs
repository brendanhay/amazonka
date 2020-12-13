{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheEngineVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheEngineVersion
  ( CacheEngineVersion (..),

    -- * Smart constructor
    mkCacheEngineVersion,

    -- * Lenses
    cevEngineVersion,
    cevCacheParameterGroupFamily,
    cevCacheEngineDescription,
    cevEngine,
    cevCacheEngineVersionDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides all of the details about a particular cache engine version.
--
-- /See:/ 'mkCacheEngineVersion' smart constructor.
data CacheEngineVersion = CacheEngineVersion'
  { -- | The version number of the cache engine.
    engineVersion :: Lude.Maybe Lude.Text,
    -- | The name of the cache parameter group family associated with this cache engine.
    --
    -- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ |
    cacheParameterGroupFamily :: Lude.Maybe Lude.Text,
    -- | The description of the cache engine.
    cacheEngineDescription :: Lude.Maybe Lude.Text,
    -- | The name of the cache engine.
    engine :: Lude.Maybe Lude.Text,
    -- | The description of the cache engine version.
    cacheEngineVersionDescription :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheEngineVersion' with the minimum fields required to make a request.
--
-- * 'engineVersion' - The version number of the cache engine.
-- * 'cacheParameterGroupFamily' - The name of the cache parameter group family associated with this cache engine.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ |
-- * 'cacheEngineDescription' - The description of the cache engine.
-- * 'engine' - The name of the cache engine.
-- * 'cacheEngineVersionDescription' - The description of the cache engine version.
mkCacheEngineVersion ::
  CacheEngineVersion
mkCacheEngineVersion =
  CacheEngineVersion'
    { engineVersion = Lude.Nothing,
      cacheParameterGroupFamily = Lude.Nothing,
      cacheEngineDescription = Lude.Nothing,
      engine = Lude.Nothing,
      cacheEngineVersionDescription = Lude.Nothing
    }

-- | The version number of the cache engine.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cevEngineVersion :: Lens.Lens' CacheEngineVersion (Lude.Maybe Lude.Text)
cevEngineVersion = Lens.lens (engineVersion :: CacheEngineVersion -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: CacheEngineVersion)
{-# DEPRECATED cevEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The name of the cache parameter group family associated with this cache engine.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ |
--
-- /Note:/ Consider using 'cacheParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cevCacheParameterGroupFamily :: Lens.Lens' CacheEngineVersion (Lude.Maybe Lude.Text)
cevCacheParameterGroupFamily = Lens.lens (cacheParameterGroupFamily :: CacheEngineVersion -> Lude.Maybe Lude.Text) (\s a -> s {cacheParameterGroupFamily = a} :: CacheEngineVersion)
{-# DEPRECATED cevCacheParameterGroupFamily "Use generic-lens or generic-optics with 'cacheParameterGroupFamily' instead." #-}

-- | The description of the cache engine.
--
-- /Note:/ Consider using 'cacheEngineDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cevCacheEngineDescription :: Lens.Lens' CacheEngineVersion (Lude.Maybe Lude.Text)
cevCacheEngineDescription = Lens.lens (cacheEngineDescription :: CacheEngineVersion -> Lude.Maybe Lude.Text) (\s a -> s {cacheEngineDescription = a} :: CacheEngineVersion)
{-# DEPRECATED cevCacheEngineDescription "Use generic-lens or generic-optics with 'cacheEngineDescription' instead." #-}

-- | The name of the cache engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cevEngine :: Lens.Lens' CacheEngineVersion (Lude.Maybe Lude.Text)
cevEngine = Lens.lens (engine :: CacheEngineVersion -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: CacheEngineVersion)
{-# DEPRECATED cevEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The description of the cache engine version.
--
-- /Note:/ Consider using 'cacheEngineVersionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cevCacheEngineVersionDescription :: Lens.Lens' CacheEngineVersion (Lude.Maybe Lude.Text)
cevCacheEngineVersionDescription = Lens.lens (cacheEngineVersionDescription :: CacheEngineVersion -> Lude.Maybe Lude.Text) (\s a -> s {cacheEngineVersionDescription = a} :: CacheEngineVersion)
{-# DEPRECATED cevCacheEngineVersionDescription "Use generic-lens or generic-optics with 'cacheEngineVersionDescription' instead." #-}

instance Lude.FromXML CacheEngineVersion where
  parseXML x =
    CacheEngineVersion'
      Lude.<$> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "CacheParameterGroupFamily")
      Lude.<*> (x Lude..@? "CacheEngineDescription")
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "CacheEngineVersionDescription")
