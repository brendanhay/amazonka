{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsAkamaiSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsAkamaiSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.HlsAkamaiHttpTransferMode
import qualified Network.AWS.Prelude as Prelude

-- | Hls Akamai Settings
--
-- /See:/ 'newHlsAkamaiSettings' smart constructor.
data HlsAkamaiSettings = HlsAkamaiSettings'
  { -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Prelude.Maybe Prelude.Natural,
    -- | Specify whether or not to use chunked transfer encoding to Akamai. User
    -- should contact Akamai to enable this feature.
    httpTransferMode :: Prelude.Maybe HlsAkamaiHttpTransferMode,
    -- | Number of retry attempts that will be made before the Live Event is put
    -- into an error state.
    numRetries :: Prelude.Maybe Prelude.Natural,
    -- | Number of seconds to wait before retrying connection to the CDN if the
    -- connection is lost.
    connectionRetryInterval :: Prelude.Maybe Prelude.Natural,
    -- | Token parameter for authenticated akamai. If not specified, _gda_ is
    -- used.
    token :: Prelude.Maybe Prelude.Text,
    -- | If a streaming output fails, number of seconds to wait until a restart
    -- is initiated. A value of 0 means never restart.
    restartDelay :: Prelude.Maybe Prelude.Natural,
    -- | Salt for authenticated Akamai.
    salt :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsAkamaiSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filecacheDuration', 'hlsAkamaiSettings_filecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- 'httpTransferMode', 'hlsAkamaiSettings_httpTransferMode' - Specify whether or not to use chunked transfer encoding to Akamai. User
-- should contact Akamai to enable this feature.
--
-- 'numRetries', 'hlsAkamaiSettings_numRetries' - Number of retry attempts that will be made before the Live Event is put
-- into an error state.
--
-- 'connectionRetryInterval', 'hlsAkamaiSettings_connectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
--
-- 'token', 'hlsAkamaiSettings_token' - Token parameter for authenticated akamai. If not specified, _gda_ is
-- used.
--
-- 'restartDelay', 'hlsAkamaiSettings_restartDelay' - If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
--
-- 'salt', 'hlsAkamaiSettings_salt' - Salt for authenticated Akamai.
newHlsAkamaiSettings ::
  HlsAkamaiSettings
newHlsAkamaiSettings =
  HlsAkamaiSettings'
    { filecacheDuration =
        Prelude.Nothing,
      httpTransferMode = Prelude.Nothing,
      numRetries = Prelude.Nothing,
      connectionRetryInterval = Prelude.Nothing,
      token = Prelude.Nothing,
      restartDelay = Prelude.Nothing,
      salt = Prelude.Nothing
    }

-- | Size in seconds of file cache for streaming outputs.
hlsAkamaiSettings_filecacheDuration :: Lens.Lens' HlsAkamaiSettings (Prelude.Maybe Prelude.Natural)
hlsAkamaiSettings_filecacheDuration = Lens.lens (\HlsAkamaiSettings' {filecacheDuration} -> filecacheDuration) (\s@HlsAkamaiSettings' {} a -> s {filecacheDuration = a} :: HlsAkamaiSettings)

-- | Specify whether or not to use chunked transfer encoding to Akamai. User
-- should contact Akamai to enable this feature.
hlsAkamaiSettings_httpTransferMode :: Lens.Lens' HlsAkamaiSettings (Prelude.Maybe HlsAkamaiHttpTransferMode)
hlsAkamaiSettings_httpTransferMode = Lens.lens (\HlsAkamaiSettings' {httpTransferMode} -> httpTransferMode) (\s@HlsAkamaiSettings' {} a -> s {httpTransferMode = a} :: HlsAkamaiSettings)

-- | Number of retry attempts that will be made before the Live Event is put
-- into an error state.
hlsAkamaiSettings_numRetries :: Lens.Lens' HlsAkamaiSettings (Prelude.Maybe Prelude.Natural)
hlsAkamaiSettings_numRetries = Lens.lens (\HlsAkamaiSettings' {numRetries} -> numRetries) (\s@HlsAkamaiSettings' {} a -> s {numRetries = a} :: HlsAkamaiSettings)

-- | Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
hlsAkamaiSettings_connectionRetryInterval :: Lens.Lens' HlsAkamaiSettings (Prelude.Maybe Prelude.Natural)
hlsAkamaiSettings_connectionRetryInterval = Lens.lens (\HlsAkamaiSettings' {connectionRetryInterval} -> connectionRetryInterval) (\s@HlsAkamaiSettings' {} a -> s {connectionRetryInterval = a} :: HlsAkamaiSettings)

-- | Token parameter for authenticated akamai. If not specified, _gda_ is
-- used.
hlsAkamaiSettings_token :: Lens.Lens' HlsAkamaiSettings (Prelude.Maybe Prelude.Text)
hlsAkamaiSettings_token = Lens.lens (\HlsAkamaiSettings' {token} -> token) (\s@HlsAkamaiSettings' {} a -> s {token = a} :: HlsAkamaiSettings)

-- | If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
hlsAkamaiSettings_restartDelay :: Lens.Lens' HlsAkamaiSettings (Prelude.Maybe Prelude.Natural)
hlsAkamaiSettings_restartDelay = Lens.lens (\HlsAkamaiSettings' {restartDelay} -> restartDelay) (\s@HlsAkamaiSettings' {} a -> s {restartDelay = a} :: HlsAkamaiSettings)

-- | Salt for authenticated Akamai.
hlsAkamaiSettings_salt :: Lens.Lens' HlsAkamaiSettings (Prelude.Maybe Prelude.Text)
hlsAkamaiSettings_salt = Lens.lens (\HlsAkamaiSettings' {salt} -> salt) (\s@HlsAkamaiSettings' {} a -> s {salt = a} :: HlsAkamaiSettings)

instance Core.FromJSON HlsAkamaiSettings where
  parseJSON =
    Core.withObject
      "HlsAkamaiSettings"
      ( \x ->
          HlsAkamaiSettings'
            Prelude.<$> (x Core..:? "filecacheDuration")
            Prelude.<*> (x Core..:? "httpTransferMode")
            Prelude.<*> (x Core..:? "numRetries")
            Prelude.<*> (x Core..:? "connectionRetryInterval")
            Prelude.<*> (x Core..:? "token")
            Prelude.<*> (x Core..:? "restartDelay")
            Prelude.<*> (x Core..:? "salt")
      )

instance Prelude.Hashable HlsAkamaiSettings

instance Prelude.NFData HlsAkamaiSettings

instance Core.ToJSON HlsAkamaiSettings where
  toJSON HlsAkamaiSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filecacheDuration" Core..=)
              Prelude.<$> filecacheDuration,
            ("httpTransferMode" Core..=)
              Prelude.<$> httpTransferMode,
            ("numRetries" Core..=) Prelude.<$> numRetries,
            ("connectionRetryInterval" Core..=)
              Prelude.<$> connectionRetryInterval,
            ("token" Core..=) Prelude.<$> token,
            ("restartDelay" Core..=) Prelude.<$> restartDelay,
            ("salt" Core..=) Prelude.<$> salt
          ]
      )
