{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsAkamaiSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsAkamaiSettings
  ( HlsAkamaiSettings (..),

    -- * Smart constructor
    mkHlsAkamaiSettings,

    -- * Lenses
    hasConnectionRetryInterval,
    hasFilecacheDuration,
    hasHttpTransferMode,
    hasNumRetries,
    hasRestartDelay,
    hasSalt,
    hasToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.HlsAkamaiHttpTransferMode as Types
import qualified Network.AWS.Prelude as Core

-- | Hls Akamai Settings
--
-- /See:/ 'mkHlsAkamaiSettings' smart constructor.
data HlsAkamaiSettings = HlsAkamaiSettings'
  { -- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
    connectionRetryInterval :: Core.Maybe Core.Natural,
    -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Core.Maybe Core.Natural,
    -- | Specify whether or not to use chunked transfer encoding to Akamai. User should contact Akamai to enable this feature.
    httpTransferMode :: Core.Maybe Types.HlsAkamaiHttpTransferMode,
    -- | Number of retry attempts that will be made before the Live Event is put into an error state.
    numRetries :: Core.Maybe Core.Natural,
    -- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
    restartDelay :: Core.Maybe Core.Natural,
    -- | Salt for authenticated Akamai.
    salt :: Core.Maybe Core.Text,
    -- | Token parameter for authenticated akamai. If not specified, _gda_ is used.
    token :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsAkamaiSettings' value with any optional fields omitted.
mkHlsAkamaiSettings ::
  HlsAkamaiSettings
mkHlsAkamaiSettings =
  HlsAkamaiSettings'
    { connectionRetryInterval = Core.Nothing,
      filecacheDuration = Core.Nothing,
      httpTransferMode = Core.Nothing,
      numRetries = Core.Nothing,
      restartDelay = Core.Nothing,
      salt = Core.Nothing,
      token = Core.Nothing
    }

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- /Note:/ Consider using 'connectionRetryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hasConnectionRetryInterval :: Lens.Lens' HlsAkamaiSettings (Core.Maybe Core.Natural)
hasConnectionRetryInterval = Lens.field @"connectionRetryInterval"
{-# DEPRECATED hasConnectionRetryInterval "Use generic-lens or generic-optics with 'connectionRetryInterval' instead." #-}

-- | Size in seconds of file cache for streaming outputs.
--
-- /Note:/ Consider using 'filecacheDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hasFilecacheDuration :: Lens.Lens' HlsAkamaiSettings (Core.Maybe Core.Natural)
hasFilecacheDuration = Lens.field @"filecacheDuration"
{-# DEPRECATED hasFilecacheDuration "Use generic-lens or generic-optics with 'filecacheDuration' instead." #-}

-- | Specify whether or not to use chunked transfer encoding to Akamai. User should contact Akamai to enable this feature.
--
-- /Note:/ Consider using 'httpTransferMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hasHttpTransferMode :: Lens.Lens' HlsAkamaiSettings (Core.Maybe Types.HlsAkamaiHttpTransferMode)
hasHttpTransferMode = Lens.field @"httpTransferMode"
{-# DEPRECATED hasHttpTransferMode "Use generic-lens or generic-optics with 'httpTransferMode' instead." #-}

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- /Note:/ Consider using 'numRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hasNumRetries :: Lens.Lens' HlsAkamaiSettings (Core.Maybe Core.Natural)
hasNumRetries = Lens.field @"numRetries"
{-# DEPRECATED hasNumRetries "Use generic-lens or generic-optics with 'numRetries' instead." #-}

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
--
-- /Note:/ Consider using 'restartDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hasRestartDelay :: Lens.Lens' HlsAkamaiSettings (Core.Maybe Core.Natural)
hasRestartDelay = Lens.field @"restartDelay"
{-# DEPRECATED hasRestartDelay "Use generic-lens or generic-optics with 'restartDelay' instead." #-}

-- | Salt for authenticated Akamai.
--
-- /Note:/ Consider using 'salt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hasSalt :: Lens.Lens' HlsAkamaiSettings (Core.Maybe Core.Text)
hasSalt = Lens.field @"salt"
{-# DEPRECATED hasSalt "Use generic-lens or generic-optics with 'salt' instead." #-}

-- | Token parameter for authenticated akamai. If not specified, _gda_ is used.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hasToken :: Lens.Lens' HlsAkamaiSettings (Core.Maybe Core.Text)
hasToken = Lens.field @"token"
{-# DEPRECATED hasToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Core.FromJSON HlsAkamaiSettings where
  toJSON HlsAkamaiSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("connectionRetryInterval" Core..=)
              Core.<$> connectionRetryInterval,
            ("filecacheDuration" Core..=) Core.<$> filecacheDuration,
            ("httpTransferMode" Core..=) Core.<$> httpTransferMode,
            ("numRetries" Core..=) Core.<$> numRetries,
            ("restartDelay" Core..=) Core.<$> restartDelay,
            ("salt" Core..=) Core.<$> salt,
            ("token" Core..=) Core.<$> token
          ]
      )

instance Core.FromJSON HlsAkamaiSettings where
  parseJSON =
    Core.withObject "HlsAkamaiSettings" Core.$
      \x ->
        HlsAkamaiSettings'
          Core.<$> (x Core..:? "connectionRetryInterval")
          Core.<*> (x Core..:? "filecacheDuration")
          Core.<*> (x Core..:? "httpTransferMode")
          Core.<*> (x Core..:? "numRetries")
          Core.<*> (x Core..:? "restartDelay")
          Core.<*> (x Core..:? "salt")
          Core.<*> (x Core..:? "token")
