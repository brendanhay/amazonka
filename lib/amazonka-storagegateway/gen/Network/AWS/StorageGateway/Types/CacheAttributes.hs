{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.CacheAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.CacheAttributes
  ( CacheAttributes (..),

    -- * Smart constructor
    mkCacheAttributes,

    -- * Lenses
    caCacheStaleTimeoutInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Lists refresh cache information.
--
-- /See:/ 'mkCacheAttributes' smart constructor.
newtype CacheAttributes = CacheAttributes'
  { -- | Refreshes a file share's cache by using Time To Live (TTL). TTL is the length of time since the last refresh after which access to the directory would cause the file gateway to first refresh that directory's contents from the Amazon S3 bucket. The TTL duration is in seconds.
    --
    -- Valid Values: 300 to 2,592,000 seconds (5 minutes to 30 days)
    cacheStaleTimeoutInSeconds :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CacheAttributes' value with any optional fields omitted.
mkCacheAttributes ::
  CacheAttributes
mkCacheAttributes =
  CacheAttributes' {cacheStaleTimeoutInSeconds = Core.Nothing}

-- | Refreshes a file share's cache by using Time To Live (TTL). TTL is the length of time since the last refresh after which access to the directory would cause the file gateway to first refresh that directory's contents from the Amazon S3 bucket. The TTL duration is in seconds.
--
-- Valid Values: 300 to 2,592,000 seconds (5 minutes to 30 days)
--
-- /Note:/ Consider using 'cacheStaleTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCacheStaleTimeoutInSeconds :: Lens.Lens' CacheAttributes (Core.Maybe Core.Int)
caCacheStaleTimeoutInSeconds = Lens.field @"cacheStaleTimeoutInSeconds"
{-# DEPRECATED caCacheStaleTimeoutInSeconds "Use generic-lens or generic-optics with 'cacheStaleTimeoutInSeconds' instead." #-}

instance Core.FromJSON CacheAttributes where
  toJSON CacheAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ ("CacheStaleTimeoutInSeconds" Core..=)
              Core.<$> cacheStaleTimeoutInSeconds
          ]
      )

instance Core.FromJSON CacheAttributes where
  parseJSON =
    Core.withObject "CacheAttributes" Core.$
      \x ->
        CacheAttributes' Core.<$> (x Core..:? "CacheStaleTimeoutInSeconds")
