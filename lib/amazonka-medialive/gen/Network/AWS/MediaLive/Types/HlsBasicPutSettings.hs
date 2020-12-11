-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsBasicPutSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsBasicPutSettings
  ( HlsBasicPutSettings (..),

    -- * Smart constructor
    mkHlsBasicPutSettings,

    -- * Lenses
    hbpsNumRetries,
    hbpsConnectionRetryInterval,
    hbpsFilecacheDuration,
    hbpsRestartDelay,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Hls Basic Put Settings
--
-- /See:/ 'mkHlsBasicPutSettings' smart constructor.
data HlsBasicPutSettings = HlsBasicPutSettings'
  { numRetries ::
      Lude.Maybe Lude.Natural,
    connectionRetryInterval :: Lude.Maybe Lude.Natural,
    filecacheDuration :: Lude.Maybe Lude.Natural,
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

-- | Creates a value of 'HlsBasicPutSettings' with the minimum fields required to make a request.
--
-- * 'connectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the connection is lost.
-- * 'filecacheDuration' - Size in seconds of file cache for streaming outputs.
-- * 'numRetries' - Number of retry attempts that will be made before the Live Event is put into an error state.
-- * 'restartDelay' - If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
mkHlsBasicPutSettings ::
  HlsBasicPutSettings
mkHlsBasicPutSettings =
  HlsBasicPutSettings'
    { numRetries = Lude.Nothing,
      connectionRetryInterval = Lude.Nothing,
      filecacheDuration = Lude.Nothing,
      restartDelay = Lude.Nothing
    }

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- /Note:/ Consider using 'numRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hbpsNumRetries :: Lens.Lens' HlsBasicPutSettings (Lude.Maybe Lude.Natural)
hbpsNumRetries = Lens.lens (numRetries :: HlsBasicPutSettings -> Lude.Maybe Lude.Natural) (\s a -> s {numRetries = a} :: HlsBasicPutSettings)
{-# DEPRECATED hbpsNumRetries "Use generic-lens or generic-optics with 'numRetries' instead." #-}

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- /Note:/ Consider using 'connectionRetryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hbpsConnectionRetryInterval :: Lens.Lens' HlsBasicPutSettings (Lude.Maybe Lude.Natural)
hbpsConnectionRetryInterval = Lens.lens (connectionRetryInterval :: HlsBasicPutSettings -> Lude.Maybe Lude.Natural) (\s a -> s {connectionRetryInterval = a} :: HlsBasicPutSettings)
{-# DEPRECATED hbpsConnectionRetryInterval "Use generic-lens or generic-optics with 'connectionRetryInterval' instead." #-}

-- | Size in seconds of file cache for streaming outputs.
--
-- /Note:/ Consider using 'filecacheDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hbpsFilecacheDuration :: Lens.Lens' HlsBasicPutSettings (Lude.Maybe Lude.Natural)
hbpsFilecacheDuration = Lens.lens (filecacheDuration :: HlsBasicPutSettings -> Lude.Maybe Lude.Natural) (\s a -> s {filecacheDuration = a} :: HlsBasicPutSettings)
{-# DEPRECATED hbpsFilecacheDuration "Use generic-lens or generic-optics with 'filecacheDuration' instead." #-}

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
--
-- /Note:/ Consider using 'restartDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hbpsRestartDelay :: Lens.Lens' HlsBasicPutSettings (Lude.Maybe Lude.Natural)
hbpsRestartDelay = Lens.lens (restartDelay :: HlsBasicPutSettings -> Lude.Maybe Lude.Natural) (\s a -> s {restartDelay = a} :: HlsBasicPutSettings)
{-# DEPRECATED hbpsRestartDelay "Use generic-lens or generic-optics with 'restartDelay' instead." #-}

instance Lude.FromJSON HlsBasicPutSettings where
  parseJSON =
    Lude.withObject
      "HlsBasicPutSettings"
      ( \x ->
          HlsBasicPutSettings'
            Lude.<$> (x Lude..:? "numRetries")
            Lude.<*> (x Lude..:? "connectionRetryInterval")
            Lude.<*> (x Lude..:? "filecacheDuration")
            Lude.<*> (x Lude..:? "restartDelay")
      )

instance Lude.ToJSON HlsBasicPutSettings where
  toJSON HlsBasicPutSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("numRetries" Lude..=) Lude.<$> numRetries,
            ("connectionRetryInterval" Lude..=)
              Lude.<$> connectionRetryInterval,
            ("filecacheDuration" Lude..=) Lude.<$> filecacheDuration,
            ("restartDelay" Lude..=) Lude.<$> restartDelay
          ]
      )
