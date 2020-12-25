{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsWebdavSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsWebdavSettings
  ( HlsWebdavSettings (..),

    -- * Smart constructor
    mkHlsWebdavSettings,

    -- * Lenses
    hwsConnectionRetryInterval,
    hwsFilecacheDuration,
    hwsHttpTransferMode,
    hwsNumRetries,
    hwsRestartDelay,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.HlsWebdavHttpTransferMode as Types
import qualified Network.AWS.Prelude as Core

-- | Hls Webdav Settings
--
-- /See:/ 'mkHlsWebdavSettings' smart constructor.
data HlsWebdavSettings = HlsWebdavSettings'
  { -- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
    connectionRetryInterval :: Core.Maybe Core.Natural,
    -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Core.Maybe Core.Natural,
    -- | Specify whether or not to use chunked transfer encoding to WebDAV.
    httpTransferMode :: Core.Maybe Types.HlsWebdavHttpTransferMode,
    -- | Number of retry attempts that will be made before the Live Event is put into an error state.
    numRetries :: Core.Maybe Core.Natural,
    -- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
    restartDelay :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsWebdavSettings' value with any optional fields omitted.
mkHlsWebdavSettings ::
  HlsWebdavSettings
mkHlsWebdavSettings =
  HlsWebdavSettings'
    { connectionRetryInterval = Core.Nothing,
      filecacheDuration = Core.Nothing,
      httpTransferMode = Core.Nothing,
      numRetries = Core.Nothing,
      restartDelay = Core.Nothing
    }

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- /Note:/ Consider using 'connectionRetryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hwsConnectionRetryInterval :: Lens.Lens' HlsWebdavSettings (Core.Maybe Core.Natural)
hwsConnectionRetryInterval = Lens.field @"connectionRetryInterval"
{-# DEPRECATED hwsConnectionRetryInterval "Use generic-lens or generic-optics with 'connectionRetryInterval' instead." #-}

-- | Size in seconds of file cache for streaming outputs.
--
-- /Note:/ Consider using 'filecacheDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hwsFilecacheDuration :: Lens.Lens' HlsWebdavSettings (Core.Maybe Core.Natural)
hwsFilecacheDuration = Lens.field @"filecacheDuration"
{-# DEPRECATED hwsFilecacheDuration "Use generic-lens or generic-optics with 'filecacheDuration' instead." #-}

-- | Specify whether or not to use chunked transfer encoding to WebDAV.
--
-- /Note:/ Consider using 'httpTransferMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hwsHttpTransferMode :: Lens.Lens' HlsWebdavSettings (Core.Maybe Types.HlsWebdavHttpTransferMode)
hwsHttpTransferMode = Lens.field @"httpTransferMode"
{-# DEPRECATED hwsHttpTransferMode "Use generic-lens or generic-optics with 'httpTransferMode' instead." #-}

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- /Note:/ Consider using 'numRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hwsNumRetries :: Lens.Lens' HlsWebdavSettings (Core.Maybe Core.Natural)
hwsNumRetries = Lens.field @"numRetries"
{-# DEPRECATED hwsNumRetries "Use generic-lens or generic-optics with 'numRetries' instead." #-}

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
--
-- /Note:/ Consider using 'restartDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hwsRestartDelay :: Lens.Lens' HlsWebdavSettings (Core.Maybe Core.Natural)
hwsRestartDelay = Lens.field @"restartDelay"
{-# DEPRECATED hwsRestartDelay "Use generic-lens or generic-optics with 'restartDelay' instead." #-}

instance Core.FromJSON HlsWebdavSettings where
  toJSON HlsWebdavSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("connectionRetryInterval" Core..=)
              Core.<$> connectionRetryInterval,
            ("filecacheDuration" Core..=) Core.<$> filecacheDuration,
            ("httpTransferMode" Core..=) Core.<$> httpTransferMode,
            ("numRetries" Core..=) Core.<$> numRetries,
            ("restartDelay" Core..=) Core.<$> restartDelay
          ]
      )

instance Core.FromJSON HlsWebdavSettings where
  parseJSON =
    Core.withObject "HlsWebdavSettings" Core.$
      \x ->
        HlsWebdavSettings'
          Core.<$> (x Core..:? "connectionRetryInterval")
          Core.<*> (x Core..:? "filecacheDuration")
          Core.<*> (x Core..:? "httpTransferMode")
          Core.<*> (x Core..:? "numRetries")
          Core.<*> (x Core..:? "restartDelay")
