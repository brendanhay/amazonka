{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsBasicPutSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsBasicPutSettings
  ( HlsBasicPutSettings (..)
  -- * Smart constructor
  , mkHlsBasicPutSettings
  -- * Lenses
  , hbpsConnectionRetryInterval
  , hbpsFilecacheDuration
  , hbpsNumRetries
  , hbpsRestartDelay
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Hls Basic Put Settings
--
-- /See:/ 'mkHlsBasicPutSettings' smart constructor.
data HlsBasicPutSettings = HlsBasicPutSettings'
  { connectionRetryInterval :: Core.Maybe Core.Natural
    -- ^ Number of seconds to wait before retrying connection to the CDN if the connection is lost.
  , filecacheDuration :: Core.Maybe Core.Natural
    -- ^ Size in seconds of file cache for streaming outputs.
  , numRetries :: Core.Maybe Core.Natural
    -- ^ Number of retry attempts that will be made before the Live Event is put into an error state.
  , restartDelay :: Core.Maybe Core.Natural
    -- ^ If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsBasicPutSettings' value with any optional fields omitted.
mkHlsBasicPutSettings
    :: HlsBasicPutSettings
mkHlsBasicPutSettings
  = HlsBasicPutSettings'{connectionRetryInterval = Core.Nothing,
                         filecacheDuration = Core.Nothing, numRetries = Core.Nothing,
                         restartDelay = Core.Nothing}

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- /Note:/ Consider using 'connectionRetryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hbpsConnectionRetryInterval :: Lens.Lens' HlsBasicPutSettings (Core.Maybe Core.Natural)
hbpsConnectionRetryInterval = Lens.field @"connectionRetryInterval"
{-# INLINEABLE hbpsConnectionRetryInterval #-}
{-# DEPRECATED connectionRetryInterval "Use generic-lens or generic-optics with 'connectionRetryInterval' instead"  #-}

-- | Size in seconds of file cache for streaming outputs.
--
-- /Note:/ Consider using 'filecacheDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hbpsFilecacheDuration :: Lens.Lens' HlsBasicPutSettings (Core.Maybe Core.Natural)
hbpsFilecacheDuration = Lens.field @"filecacheDuration"
{-# INLINEABLE hbpsFilecacheDuration #-}
{-# DEPRECATED filecacheDuration "Use generic-lens or generic-optics with 'filecacheDuration' instead"  #-}

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- /Note:/ Consider using 'numRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hbpsNumRetries :: Lens.Lens' HlsBasicPutSettings (Core.Maybe Core.Natural)
hbpsNumRetries = Lens.field @"numRetries"
{-# INLINEABLE hbpsNumRetries #-}
{-# DEPRECATED numRetries "Use generic-lens or generic-optics with 'numRetries' instead"  #-}

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
--
-- /Note:/ Consider using 'restartDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hbpsRestartDelay :: Lens.Lens' HlsBasicPutSettings (Core.Maybe Core.Natural)
hbpsRestartDelay = Lens.field @"restartDelay"
{-# INLINEABLE hbpsRestartDelay #-}
{-# DEPRECATED restartDelay "Use generic-lens or generic-optics with 'restartDelay' instead"  #-}

instance Core.FromJSON HlsBasicPutSettings where
        toJSON HlsBasicPutSettings{..}
          = Core.object
              (Core.catMaybes
                 [("connectionRetryInterval" Core..=) Core.<$>
                    connectionRetryInterval,
                  ("filecacheDuration" Core..=) Core.<$> filecacheDuration,
                  ("numRetries" Core..=) Core.<$> numRetries,
                  ("restartDelay" Core..=) Core.<$> restartDelay])

instance Core.FromJSON HlsBasicPutSettings where
        parseJSON
          = Core.withObject "HlsBasicPutSettings" Core.$
              \ x ->
                HlsBasicPutSettings' Core.<$>
                  (x Core..:? "connectionRetryInterval") Core.<*>
                    x Core..:? "filecacheDuration"
                    Core.<*> x Core..:? "numRetries"
                    Core.<*> x Core..:? "restartDelay"
