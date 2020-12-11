-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsMediaStoreSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsMediaStoreSettings
  ( HlsMediaStoreSettings (..),

    -- * Smart constructor
    mkHlsMediaStoreSettings,

    -- * Lenses
    hmssNumRetries,
    hmssConnectionRetryInterval,
    hmssFilecacheDuration,
    hmssMediaStoreStorageClass,
    hmssRestartDelay,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.HlsMediaStoreStorageClass
import qualified Network.AWS.Prelude as Lude

-- | Hls Media Store Settings
--
-- /See:/ 'mkHlsMediaStoreSettings' smart constructor.
data HlsMediaStoreSettings = HlsMediaStoreSettings'
  { numRetries ::
      Lude.Maybe Lude.Natural,
    connectionRetryInterval ::
      Lude.Maybe Lude.Natural,
    filecacheDuration :: Lude.Maybe Lude.Natural,
    mediaStoreStorageClass ::
      Lude.Maybe HlsMediaStoreStorageClass,
    restartDelay :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsMediaStoreSettings' with the minimum fields required to make a request.
--
-- * 'connectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the connection is lost.
-- * 'filecacheDuration' - Size in seconds of file cache for streaming outputs.
-- * 'mediaStoreStorageClass' - When set to temporal, output files are stored in non-persistent memory for faster reading and writing.
-- * 'numRetries' - Number of retry attempts that will be made before the Live Event is put into an error state.
-- * 'restartDelay' - If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
mkHlsMediaStoreSettings ::
  HlsMediaStoreSettings
mkHlsMediaStoreSettings =
  HlsMediaStoreSettings'
    { numRetries = Lude.Nothing,
      connectionRetryInterval = Lude.Nothing,
      filecacheDuration = Lude.Nothing,
      mediaStoreStorageClass = Lude.Nothing,
      restartDelay = Lude.Nothing
    }

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- /Note:/ Consider using 'numRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmssNumRetries :: Lens.Lens' HlsMediaStoreSettings (Lude.Maybe Lude.Natural)
hmssNumRetries = Lens.lens (numRetries :: HlsMediaStoreSettings -> Lude.Maybe Lude.Natural) (\s a -> s {numRetries = a} :: HlsMediaStoreSettings)
{-# DEPRECATED hmssNumRetries "Use generic-lens or generic-optics with 'numRetries' instead." #-}

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- /Note:/ Consider using 'connectionRetryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmssConnectionRetryInterval :: Lens.Lens' HlsMediaStoreSettings (Lude.Maybe Lude.Natural)
hmssConnectionRetryInterval = Lens.lens (connectionRetryInterval :: HlsMediaStoreSettings -> Lude.Maybe Lude.Natural) (\s a -> s {connectionRetryInterval = a} :: HlsMediaStoreSettings)
{-# DEPRECATED hmssConnectionRetryInterval "Use generic-lens or generic-optics with 'connectionRetryInterval' instead." #-}

-- | Size in seconds of file cache for streaming outputs.
--
-- /Note:/ Consider using 'filecacheDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmssFilecacheDuration :: Lens.Lens' HlsMediaStoreSettings (Lude.Maybe Lude.Natural)
hmssFilecacheDuration = Lens.lens (filecacheDuration :: HlsMediaStoreSettings -> Lude.Maybe Lude.Natural) (\s a -> s {filecacheDuration = a} :: HlsMediaStoreSettings)
{-# DEPRECATED hmssFilecacheDuration "Use generic-lens or generic-optics with 'filecacheDuration' instead." #-}

-- | When set to temporal, output files are stored in non-persistent memory for faster reading and writing.
--
-- /Note:/ Consider using 'mediaStoreStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmssMediaStoreStorageClass :: Lens.Lens' HlsMediaStoreSettings (Lude.Maybe HlsMediaStoreStorageClass)
hmssMediaStoreStorageClass = Lens.lens (mediaStoreStorageClass :: HlsMediaStoreSettings -> Lude.Maybe HlsMediaStoreStorageClass) (\s a -> s {mediaStoreStorageClass = a} :: HlsMediaStoreSettings)
{-# DEPRECATED hmssMediaStoreStorageClass "Use generic-lens or generic-optics with 'mediaStoreStorageClass' instead." #-}

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
--
-- /Note:/ Consider using 'restartDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmssRestartDelay :: Lens.Lens' HlsMediaStoreSettings (Lude.Maybe Lude.Natural)
hmssRestartDelay = Lens.lens (restartDelay :: HlsMediaStoreSettings -> Lude.Maybe Lude.Natural) (\s a -> s {restartDelay = a} :: HlsMediaStoreSettings)
{-# DEPRECATED hmssRestartDelay "Use generic-lens or generic-optics with 'restartDelay' instead." #-}

instance Lude.FromJSON HlsMediaStoreSettings where
  parseJSON =
    Lude.withObject
      "HlsMediaStoreSettings"
      ( \x ->
          HlsMediaStoreSettings'
            Lude.<$> (x Lude..:? "numRetries")
            Lude.<*> (x Lude..:? "connectionRetryInterval")
            Lude.<*> (x Lude..:? "filecacheDuration")
            Lude.<*> (x Lude..:? "mediaStoreStorageClass")
            Lude.<*> (x Lude..:? "restartDelay")
      )

instance Lude.ToJSON HlsMediaStoreSettings where
  toJSON HlsMediaStoreSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("numRetries" Lude..=) Lude.<$> numRetries,
            ("connectionRetryInterval" Lude..=)
              Lude.<$> connectionRetryInterval,
            ("filecacheDuration" Lude..=) Lude.<$> filecacheDuration,
            ("mediaStoreStorageClass" Lude..=) Lude.<$> mediaStoreStorageClass,
            ("restartDelay" Lude..=) Lude.<$> restartDelay
          ]
      )
