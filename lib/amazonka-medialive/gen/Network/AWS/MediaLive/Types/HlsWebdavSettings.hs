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
    hwsHTTPTransferMode,
    hwsNumRetries,
    hwsConnectionRetryInterval,
    hwsFilecacheDuration,
    hwsRestartDelay,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.HlsWebdavHTTPTransferMode
import qualified Network.AWS.Prelude as Lude

-- | Hls Webdav Settings
--
-- /See:/ 'mkHlsWebdavSettings' smart constructor.
data HlsWebdavSettings = HlsWebdavSettings'
  { -- | Specify whether or not to use chunked transfer encoding to WebDAV.
    hTTPTransferMode :: Lude.Maybe HlsWebdavHTTPTransferMode,
    -- | Number of retry attempts that will be made before the Live Event is put into an error state.
    numRetries :: Lude.Maybe Lude.Natural,
    -- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
    connectionRetryInterval :: Lude.Maybe Lude.Natural,
    -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Lude.Maybe Lude.Natural,
    -- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
    restartDelay :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsWebdavSettings' with the minimum fields required to make a request.
--
-- * 'hTTPTransferMode' - Specify whether or not to use chunked transfer encoding to WebDAV.
-- * 'numRetries' - Number of retry attempts that will be made before the Live Event is put into an error state.
-- * 'connectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the connection is lost.
-- * 'filecacheDuration' - Size in seconds of file cache for streaming outputs.
-- * 'restartDelay' - If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
mkHlsWebdavSettings ::
  HlsWebdavSettings
mkHlsWebdavSettings =
  HlsWebdavSettings'
    { hTTPTransferMode = Lude.Nothing,
      numRetries = Lude.Nothing,
      connectionRetryInterval = Lude.Nothing,
      filecacheDuration = Lude.Nothing,
      restartDelay = Lude.Nothing
    }

-- | Specify whether or not to use chunked transfer encoding to WebDAV.
--
-- /Note:/ Consider using 'hTTPTransferMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hwsHTTPTransferMode :: Lens.Lens' HlsWebdavSettings (Lude.Maybe HlsWebdavHTTPTransferMode)
hwsHTTPTransferMode = Lens.lens (hTTPTransferMode :: HlsWebdavSettings -> Lude.Maybe HlsWebdavHTTPTransferMode) (\s a -> s {hTTPTransferMode = a} :: HlsWebdavSettings)
{-# DEPRECATED hwsHTTPTransferMode "Use generic-lens or generic-optics with 'hTTPTransferMode' instead." #-}

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- /Note:/ Consider using 'numRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hwsNumRetries :: Lens.Lens' HlsWebdavSettings (Lude.Maybe Lude.Natural)
hwsNumRetries = Lens.lens (numRetries :: HlsWebdavSettings -> Lude.Maybe Lude.Natural) (\s a -> s {numRetries = a} :: HlsWebdavSettings)
{-# DEPRECATED hwsNumRetries "Use generic-lens or generic-optics with 'numRetries' instead." #-}

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- /Note:/ Consider using 'connectionRetryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hwsConnectionRetryInterval :: Lens.Lens' HlsWebdavSettings (Lude.Maybe Lude.Natural)
hwsConnectionRetryInterval = Lens.lens (connectionRetryInterval :: HlsWebdavSettings -> Lude.Maybe Lude.Natural) (\s a -> s {connectionRetryInterval = a} :: HlsWebdavSettings)
{-# DEPRECATED hwsConnectionRetryInterval "Use generic-lens or generic-optics with 'connectionRetryInterval' instead." #-}

-- | Size in seconds of file cache for streaming outputs.
--
-- /Note:/ Consider using 'filecacheDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hwsFilecacheDuration :: Lens.Lens' HlsWebdavSettings (Lude.Maybe Lude.Natural)
hwsFilecacheDuration = Lens.lens (filecacheDuration :: HlsWebdavSettings -> Lude.Maybe Lude.Natural) (\s a -> s {filecacheDuration = a} :: HlsWebdavSettings)
{-# DEPRECATED hwsFilecacheDuration "Use generic-lens or generic-optics with 'filecacheDuration' instead." #-}

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
--
-- /Note:/ Consider using 'restartDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hwsRestartDelay :: Lens.Lens' HlsWebdavSettings (Lude.Maybe Lude.Natural)
hwsRestartDelay = Lens.lens (restartDelay :: HlsWebdavSettings -> Lude.Maybe Lude.Natural) (\s a -> s {restartDelay = a} :: HlsWebdavSettings)
{-# DEPRECATED hwsRestartDelay "Use generic-lens or generic-optics with 'restartDelay' instead." #-}

instance Lude.FromJSON HlsWebdavSettings where
  parseJSON =
    Lude.withObject
      "HlsWebdavSettings"
      ( \x ->
          HlsWebdavSettings'
            Lude.<$> (x Lude..:? "httpTransferMode")
            Lude.<*> (x Lude..:? "numRetries")
            Lude.<*> (x Lude..:? "connectionRetryInterval")
            Lude.<*> (x Lude..:? "filecacheDuration")
            Lude.<*> (x Lude..:? "restartDelay")
      )

instance Lude.ToJSON HlsWebdavSettings where
  toJSON HlsWebdavSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("httpTransferMode" Lude..=) Lude.<$> hTTPTransferMode,
            ("numRetries" Lude..=) Lude.<$> numRetries,
            ("connectionRetryInterval" Lude..=)
              Lude.<$> connectionRetryInterval,
            ("filecacheDuration" Lude..=) Lude.<$> filecacheDuration,
            ("restartDelay" Lude..=) Lude.<$> restartDelay
          ]
      )
