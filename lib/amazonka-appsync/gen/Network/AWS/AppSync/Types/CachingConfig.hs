-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.CachingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.CachingConfig
  ( CachingConfig (..),

    -- * Smart constructor
    mkCachingConfig,

    -- * Lenses
    ccTtl,
    ccCachingKeys,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The caching configuration for a resolver that has caching enabled.
--
-- /See:/ 'mkCachingConfig' smart constructor.
data CachingConfig = CachingConfig'
  { ttl :: Lude.Maybe Lude.Integer,
    cachingKeys :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CachingConfig' with the minimum fields required to make a request.
--
-- * 'cachingKeys' - The caching keys for a resolver that has caching enabled.
--
-- Valid values are entries from the @> context.arguments@ , @> context.source@ , and @> context.identity@ maps.
-- * 'ttl' - The TTL in seconds for a resolver that has caching enabled.
--
-- Valid values are between 1 and 3600 seconds.
mkCachingConfig ::
  CachingConfig
mkCachingConfig =
  CachingConfig' {ttl = Lude.Nothing, cachingKeys = Lude.Nothing}

-- | The TTL in seconds for a resolver that has caching enabled.
--
-- Valid values are between 1 and 3600 seconds.
--
-- /Note:/ Consider using 'ttl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTtl :: Lens.Lens' CachingConfig (Lude.Maybe Lude.Integer)
ccTtl = Lens.lens (ttl :: CachingConfig -> Lude.Maybe Lude.Integer) (\s a -> s {ttl = a} :: CachingConfig)
{-# DEPRECATED ccTtl "Use generic-lens or generic-optics with 'ttl' instead." #-}

-- | The caching keys for a resolver that has caching enabled.
--
-- Valid values are entries from the @> context.arguments@ , @> context.source@ , and @> context.identity@ maps.
--
-- /Note:/ Consider using 'cachingKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCachingKeys :: Lens.Lens' CachingConfig (Lude.Maybe [Lude.Text])
ccCachingKeys = Lens.lens (cachingKeys :: CachingConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {cachingKeys = a} :: CachingConfig)
{-# DEPRECATED ccCachingKeys "Use generic-lens or generic-optics with 'cachingKeys' instead." #-}

instance Lude.FromJSON CachingConfig where
  parseJSON =
    Lude.withObject
      "CachingConfig"
      ( \x ->
          CachingConfig'
            Lude.<$> (x Lude..:? "ttl")
            Lude.<*> (x Lude..:? "cachingKeys" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON CachingConfig where
  toJSON CachingConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ttl" Lude..=) Lude.<$> ttl,
            ("cachingKeys" Lude..=) Lude.<$> cachingKeys
          ]
      )
