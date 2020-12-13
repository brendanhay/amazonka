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
import qualified Network.AWS.Prelude as Lude

-- | Lists refresh cache information.
--
-- /See:/ 'mkCacheAttributes' smart constructor.
newtype CacheAttributes = CacheAttributes'
  { -- | Refreshes a file share's cache by using Time To Live (TTL). TTL is the length of time since the last refresh after which access to the directory would cause the file gateway to first refresh that directory's contents from the Amazon S3 bucket. The TTL duration is in seconds.
    --
    -- Valid Values: 300 to 2,592,000 seconds (5 minutes to 30 days)
    cacheStaleTimeoutInSeconds :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheAttributes' with the minimum fields required to make a request.
--
-- * 'cacheStaleTimeoutInSeconds' - Refreshes a file share's cache by using Time To Live (TTL). TTL is the length of time since the last refresh after which access to the directory would cause the file gateway to first refresh that directory's contents from the Amazon S3 bucket. The TTL duration is in seconds.
--
-- Valid Values: 300 to 2,592,000 seconds (5 minutes to 30 days)
mkCacheAttributes ::
  CacheAttributes
mkCacheAttributes =
  CacheAttributes' {cacheStaleTimeoutInSeconds = Lude.Nothing}

-- | Refreshes a file share's cache by using Time To Live (TTL). TTL is the length of time since the last refresh after which access to the directory would cause the file gateway to first refresh that directory's contents from the Amazon S3 bucket. The TTL duration is in seconds.
--
-- Valid Values: 300 to 2,592,000 seconds (5 minutes to 30 days)
--
-- /Note:/ Consider using 'cacheStaleTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCacheStaleTimeoutInSeconds :: Lens.Lens' CacheAttributes (Lude.Maybe Lude.Int)
caCacheStaleTimeoutInSeconds = Lens.lens (cacheStaleTimeoutInSeconds :: CacheAttributes -> Lude.Maybe Lude.Int) (\s a -> s {cacheStaleTimeoutInSeconds = a} :: CacheAttributes)
{-# DEPRECATED caCacheStaleTimeoutInSeconds "Use generic-lens or generic-optics with 'cacheStaleTimeoutInSeconds' instead." #-}

instance Lude.FromJSON CacheAttributes where
  parseJSON =
    Lude.withObject
      "CacheAttributes"
      ( \x ->
          CacheAttributes'
            Lude.<$> (x Lude..:? "CacheStaleTimeoutInSeconds")
      )

instance Lude.ToJSON CacheAttributes where
  toJSON CacheAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CacheStaleTimeoutInSeconds" Lude..=)
              Lude.<$> cacheStaleTimeoutInSeconds
          ]
      )
