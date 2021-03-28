{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsMediaStoreSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsMediaStoreSettings
  ( HlsMediaStoreSettings (..)
  -- * Smart constructor
  , mkHlsMediaStoreSettings
  -- * Lenses
  , hmssConnectionRetryInterval
  , hmssFilecacheDuration
  , hmssMediaStoreStorageClass
  , hmssNumRetries
  , hmssRestartDelay
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.HlsMediaStoreStorageClass as Types
import qualified Network.AWS.Prelude as Core

-- | Hls Media Store Settings
--
-- /See:/ 'mkHlsMediaStoreSettings' smart constructor.
data HlsMediaStoreSettings = HlsMediaStoreSettings'
  { connectionRetryInterval :: Core.Maybe Core.Natural
    -- ^ Number of seconds to wait before retrying connection to the CDN if the connection is lost.
  , filecacheDuration :: Core.Maybe Core.Natural
    -- ^ Size in seconds of file cache for streaming outputs.
  , mediaStoreStorageClass :: Core.Maybe Types.HlsMediaStoreStorageClass
    -- ^ When set to temporal, output files are stored in non-persistent memory for faster reading and writing.
  , numRetries :: Core.Maybe Core.Natural
    -- ^ Number of retry attempts that will be made before the Live Event is put into an error state.
  , restartDelay :: Core.Maybe Core.Natural
    -- ^ If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsMediaStoreSettings' value with any optional fields omitted.
mkHlsMediaStoreSettings
    :: HlsMediaStoreSettings
mkHlsMediaStoreSettings
  = HlsMediaStoreSettings'{connectionRetryInterval = Core.Nothing,
                           filecacheDuration = Core.Nothing,
                           mediaStoreStorageClass = Core.Nothing, numRetries = Core.Nothing,
                           restartDelay = Core.Nothing}

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- /Note:/ Consider using 'connectionRetryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmssConnectionRetryInterval :: Lens.Lens' HlsMediaStoreSettings (Core.Maybe Core.Natural)
hmssConnectionRetryInterval = Lens.field @"connectionRetryInterval"
{-# INLINEABLE hmssConnectionRetryInterval #-}
{-# DEPRECATED connectionRetryInterval "Use generic-lens or generic-optics with 'connectionRetryInterval' instead"  #-}

-- | Size in seconds of file cache for streaming outputs.
--
-- /Note:/ Consider using 'filecacheDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmssFilecacheDuration :: Lens.Lens' HlsMediaStoreSettings (Core.Maybe Core.Natural)
hmssFilecacheDuration = Lens.field @"filecacheDuration"
{-# INLINEABLE hmssFilecacheDuration #-}
{-# DEPRECATED filecacheDuration "Use generic-lens or generic-optics with 'filecacheDuration' instead"  #-}

-- | When set to temporal, output files are stored in non-persistent memory for faster reading and writing.
--
-- /Note:/ Consider using 'mediaStoreStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmssMediaStoreStorageClass :: Lens.Lens' HlsMediaStoreSettings (Core.Maybe Types.HlsMediaStoreStorageClass)
hmssMediaStoreStorageClass = Lens.field @"mediaStoreStorageClass"
{-# INLINEABLE hmssMediaStoreStorageClass #-}
{-# DEPRECATED mediaStoreStorageClass "Use generic-lens or generic-optics with 'mediaStoreStorageClass' instead"  #-}

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- /Note:/ Consider using 'numRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmssNumRetries :: Lens.Lens' HlsMediaStoreSettings (Core.Maybe Core.Natural)
hmssNumRetries = Lens.field @"numRetries"
{-# INLINEABLE hmssNumRetries #-}
{-# DEPRECATED numRetries "Use generic-lens or generic-optics with 'numRetries' instead"  #-}

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
--
-- /Note:/ Consider using 'restartDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmssRestartDelay :: Lens.Lens' HlsMediaStoreSettings (Core.Maybe Core.Natural)
hmssRestartDelay = Lens.field @"restartDelay"
{-# INLINEABLE hmssRestartDelay #-}
{-# DEPRECATED restartDelay "Use generic-lens or generic-optics with 'restartDelay' instead"  #-}

instance Core.FromJSON HlsMediaStoreSettings where
        toJSON HlsMediaStoreSettings{..}
          = Core.object
              (Core.catMaybes
                 [("connectionRetryInterval" Core..=) Core.<$>
                    connectionRetryInterval,
                  ("filecacheDuration" Core..=) Core.<$> filecacheDuration,
                  ("mediaStoreStorageClass" Core..=) Core.<$> mediaStoreStorageClass,
                  ("numRetries" Core..=) Core.<$> numRetries,
                  ("restartDelay" Core..=) Core.<$> restartDelay])

instance Core.FromJSON HlsMediaStoreSettings where
        parseJSON
          = Core.withObject "HlsMediaStoreSettings" Core.$
              \ x ->
                HlsMediaStoreSettings' Core.<$>
                  (x Core..:? "connectionRetryInterval") Core.<*>
                    x Core..:? "filecacheDuration"
                    Core.<*> x Core..:? "mediaStoreStorageClass"
                    Core.<*> x Core..:? "numRetries"
                    Core.<*> x Core..:? "restartDelay"
