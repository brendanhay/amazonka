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
    hasHTTPTransferMode,
    hasNumRetries,
    hasToken,
    hasConnectionRetryInterval,
    hasFilecacheDuration,
    hasRestartDelay,
    hasSalt,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.HlsAkamaiHTTPTransferMode
import qualified Network.AWS.Prelude as Lude

-- | Hls Akamai Settings
--
-- /See:/ 'mkHlsAkamaiSettings' smart constructor.
data HlsAkamaiSettings = HlsAkamaiSettings'
  { hTTPTransferMode ::
      Lude.Maybe HlsAkamaiHTTPTransferMode,
    numRetries :: Lude.Maybe Lude.Natural,
    token :: Lude.Maybe Lude.Text,
    connectionRetryInterval :: Lude.Maybe Lude.Natural,
    filecacheDuration :: Lude.Maybe Lude.Natural,
    restartDelay :: Lude.Maybe Lude.Natural,
    salt :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsAkamaiSettings' with the minimum fields required to make a request.
--
-- * 'connectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the connection is lost.
-- * 'filecacheDuration' - Size in seconds of file cache for streaming outputs.
-- * 'hTTPTransferMode' - Specify whether or not to use chunked transfer encoding to Akamai. User should contact Akamai to enable this feature.
-- * 'numRetries' - Number of retry attempts that will be made before the Live Event is put into an error state.
-- * 'restartDelay' - If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
-- * 'salt' - Salt for authenticated Akamai.
-- * 'token' - Token parameter for authenticated akamai. If not specified, _gda_ is used.
mkHlsAkamaiSettings ::
  HlsAkamaiSettings
mkHlsAkamaiSettings =
  HlsAkamaiSettings'
    { hTTPTransferMode = Lude.Nothing,
      numRetries = Lude.Nothing,
      token = Lude.Nothing,
      connectionRetryInterval = Lude.Nothing,
      filecacheDuration = Lude.Nothing,
      restartDelay = Lude.Nothing,
      salt = Lude.Nothing
    }

-- | Specify whether or not to use chunked transfer encoding to Akamai. User should contact Akamai to enable this feature.
--
-- /Note:/ Consider using 'hTTPTransferMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hasHTTPTransferMode :: Lens.Lens' HlsAkamaiSettings (Lude.Maybe HlsAkamaiHTTPTransferMode)
hasHTTPTransferMode = Lens.lens (hTTPTransferMode :: HlsAkamaiSettings -> Lude.Maybe HlsAkamaiHTTPTransferMode) (\s a -> s {hTTPTransferMode = a} :: HlsAkamaiSettings)
{-# DEPRECATED hasHTTPTransferMode "Use generic-lens or generic-optics with 'hTTPTransferMode' instead." #-}

-- | Number of retry attempts that will be made before the Live Event is put into an error state.
--
-- /Note:/ Consider using 'numRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hasNumRetries :: Lens.Lens' HlsAkamaiSettings (Lude.Maybe Lude.Natural)
hasNumRetries = Lens.lens (numRetries :: HlsAkamaiSettings -> Lude.Maybe Lude.Natural) (\s a -> s {numRetries = a} :: HlsAkamaiSettings)
{-# DEPRECATED hasNumRetries "Use generic-lens or generic-optics with 'numRetries' instead." #-}

-- | Token parameter for authenticated akamai. If not specified, _gda_ is used.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hasToken :: Lens.Lens' HlsAkamaiSettings (Lude.Maybe Lude.Text)
hasToken = Lens.lens (token :: HlsAkamaiSettings -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: HlsAkamaiSettings)
{-# DEPRECATED hasToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | Number of seconds to wait before retrying connection to the CDN if the connection is lost.
--
-- /Note:/ Consider using 'connectionRetryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hasConnectionRetryInterval :: Lens.Lens' HlsAkamaiSettings (Lude.Maybe Lude.Natural)
hasConnectionRetryInterval = Lens.lens (connectionRetryInterval :: HlsAkamaiSettings -> Lude.Maybe Lude.Natural) (\s a -> s {connectionRetryInterval = a} :: HlsAkamaiSettings)
{-# DEPRECATED hasConnectionRetryInterval "Use generic-lens or generic-optics with 'connectionRetryInterval' instead." #-}

-- | Size in seconds of file cache for streaming outputs.
--
-- /Note:/ Consider using 'filecacheDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hasFilecacheDuration :: Lens.Lens' HlsAkamaiSettings (Lude.Maybe Lude.Natural)
hasFilecacheDuration = Lens.lens (filecacheDuration :: HlsAkamaiSettings -> Lude.Maybe Lude.Natural) (\s a -> s {filecacheDuration = a} :: HlsAkamaiSettings)
{-# DEPRECATED hasFilecacheDuration "Use generic-lens or generic-optics with 'filecacheDuration' instead." #-}

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
--
-- /Note:/ Consider using 'restartDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hasRestartDelay :: Lens.Lens' HlsAkamaiSettings (Lude.Maybe Lude.Natural)
hasRestartDelay = Lens.lens (restartDelay :: HlsAkamaiSettings -> Lude.Maybe Lude.Natural) (\s a -> s {restartDelay = a} :: HlsAkamaiSettings)
{-# DEPRECATED hasRestartDelay "Use generic-lens or generic-optics with 'restartDelay' instead." #-}

-- | Salt for authenticated Akamai.
--
-- /Note:/ Consider using 'salt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hasSalt :: Lens.Lens' HlsAkamaiSettings (Lude.Maybe Lude.Text)
hasSalt = Lens.lens (salt :: HlsAkamaiSettings -> Lude.Maybe Lude.Text) (\s a -> s {salt = a} :: HlsAkamaiSettings)
{-# DEPRECATED hasSalt "Use generic-lens or generic-optics with 'salt' instead." #-}

instance Lude.FromJSON HlsAkamaiSettings where
  parseJSON =
    Lude.withObject
      "HlsAkamaiSettings"
      ( \x ->
          HlsAkamaiSettings'
            Lude.<$> (x Lude..:? "httpTransferMode")
            Lude.<*> (x Lude..:? "numRetries")
            Lude.<*> (x Lude..:? "token")
            Lude.<*> (x Lude..:? "connectionRetryInterval")
            Lude.<*> (x Lude..:? "filecacheDuration")
            Lude.<*> (x Lude..:? "restartDelay")
            Lude.<*> (x Lude..:? "salt")
      )

instance Lude.ToJSON HlsAkamaiSettings where
  toJSON HlsAkamaiSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("httpTransferMode" Lude..=) Lude.<$> hTTPTransferMode,
            ("numRetries" Lude..=) Lude.<$> numRetries,
            ("token" Lude..=) Lude.<$> token,
            ("connectionRetryInterval" Lude..=)
              Lude.<$> connectionRetryInterval,
            ("filecacheDuration" Lude..=) Lude.<$> filecacheDuration,
            ("restartDelay" Lude..=) Lude.<$> restartDelay,
            ("salt" Lude..=) Lude.<$> salt
          ]
      )
