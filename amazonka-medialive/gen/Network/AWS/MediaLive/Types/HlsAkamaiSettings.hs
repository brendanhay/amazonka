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

-- | Hls Akamai Settings
--
-- /See:/ 'newHlsAkamaiSettings' smart constructor.
data HlsAkamaiSettings = HlsAkamaiSettings'
  { -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Core.Maybe Core.Natural,
    -- | Number of retry attempts that will be made before the Live Event is put
    -- into an error state.
    numRetries :: Core.Maybe Core.Natural,
    -- | Specify whether or not to use chunked transfer encoding to Akamai. User
    -- should contact Akamai to enable this feature.
    httpTransferMode :: Core.Maybe HlsAkamaiHttpTransferMode,
    -- | Number of seconds to wait before retrying connection to the CDN if the
    -- connection is lost.
    connectionRetryInterval :: Core.Maybe Core.Natural,
    -- | Token parameter for authenticated akamai. If not specified, _gda_ is
    -- used.
    token :: Core.Maybe Core.Text,
    -- | If a streaming output fails, number of seconds to wait until a restart
    -- is initiated. A value of 0 means never restart.
    restartDelay :: Core.Maybe Core.Natural,
    -- | Salt for authenticated Akamai.
    salt :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'numRetries', 'hlsAkamaiSettings_numRetries' - Number of retry attempts that will be made before the Live Event is put
-- into an error state.
--
-- 'httpTransferMode', 'hlsAkamaiSettings_httpTransferMode' - Specify whether or not to use chunked transfer encoding to Akamai. User
-- should contact Akamai to enable this feature.
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
        Core.Nothing,
      numRetries = Core.Nothing,
      httpTransferMode = Core.Nothing,
      connectionRetryInterval = Core.Nothing,
      token = Core.Nothing,
      restartDelay = Core.Nothing,
      salt = Core.Nothing
    }

-- | Size in seconds of file cache for streaming outputs.
hlsAkamaiSettings_filecacheDuration :: Lens.Lens' HlsAkamaiSettings (Core.Maybe Core.Natural)
hlsAkamaiSettings_filecacheDuration = Lens.lens (\HlsAkamaiSettings' {filecacheDuration} -> filecacheDuration) (\s@HlsAkamaiSettings' {} a -> s {filecacheDuration = a} :: HlsAkamaiSettings)

-- | Number of retry attempts that will be made before the Live Event is put
-- into an error state.
hlsAkamaiSettings_numRetries :: Lens.Lens' HlsAkamaiSettings (Core.Maybe Core.Natural)
hlsAkamaiSettings_numRetries = Lens.lens (\HlsAkamaiSettings' {numRetries} -> numRetries) (\s@HlsAkamaiSettings' {} a -> s {numRetries = a} :: HlsAkamaiSettings)

-- | Specify whether or not to use chunked transfer encoding to Akamai. User
-- should contact Akamai to enable this feature.
hlsAkamaiSettings_httpTransferMode :: Lens.Lens' HlsAkamaiSettings (Core.Maybe HlsAkamaiHttpTransferMode)
hlsAkamaiSettings_httpTransferMode = Lens.lens (\HlsAkamaiSettings' {httpTransferMode} -> httpTransferMode) (\s@HlsAkamaiSettings' {} a -> s {httpTransferMode = a} :: HlsAkamaiSettings)

-- | Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
hlsAkamaiSettings_connectionRetryInterval :: Lens.Lens' HlsAkamaiSettings (Core.Maybe Core.Natural)
hlsAkamaiSettings_connectionRetryInterval = Lens.lens (\HlsAkamaiSettings' {connectionRetryInterval} -> connectionRetryInterval) (\s@HlsAkamaiSettings' {} a -> s {connectionRetryInterval = a} :: HlsAkamaiSettings)

-- | Token parameter for authenticated akamai. If not specified, _gda_ is
-- used.
hlsAkamaiSettings_token :: Lens.Lens' HlsAkamaiSettings (Core.Maybe Core.Text)
hlsAkamaiSettings_token = Lens.lens (\HlsAkamaiSettings' {token} -> token) (\s@HlsAkamaiSettings' {} a -> s {token = a} :: HlsAkamaiSettings)

-- | If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
hlsAkamaiSettings_restartDelay :: Lens.Lens' HlsAkamaiSettings (Core.Maybe Core.Natural)
hlsAkamaiSettings_restartDelay = Lens.lens (\HlsAkamaiSettings' {restartDelay} -> restartDelay) (\s@HlsAkamaiSettings' {} a -> s {restartDelay = a} :: HlsAkamaiSettings)

-- | Salt for authenticated Akamai.
hlsAkamaiSettings_salt :: Lens.Lens' HlsAkamaiSettings (Core.Maybe Core.Text)
hlsAkamaiSettings_salt = Lens.lens (\HlsAkamaiSettings' {salt} -> salt) (\s@HlsAkamaiSettings' {} a -> s {salt = a} :: HlsAkamaiSettings)

instance Core.FromJSON HlsAkamaiSettings where
  parseJSON =
    Core.withObject
      "HlsAkamaiSettings"
      ( \x ->
          HlsAkamaiSettings'
            Core.<$> (x Core..:? "filecacheDuration")
            Core.<*> (x Core..:? "numRetries")
            Core.<*> (x Core..:? "httpTransferMode")
            Core.<*> (x Core..:? "connectionRetryInterval")
            Core.<*> (x Core..:? "token")
            Core.<*> (x Core..:? "restartDelay")
            Core.<*> (x Core..:? "salt")
      )

instance Core.Hashable HlsAkamaiSettings

instance Core.NFData HlsAkamaiSettings

instance Core.ToJSON HlsAkamaiSettings where
  toJSON HlsAkamaiSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("filecacheDuration" Core..=)
              Core.<$> filecacheDuration,
            ("numRetries" Core..=) Core.<$> numRetries,
            ("httpTransferMode" Core..=)
              Core.<$> httpTransferMode,
            ("connectionRetryInterval" Core..=)
              Core.<$> connectionRetryInterval,
            ("token" Core..=) Core.<$> token,
            ("restartDelay" Core..=) Core.<$> restartDelay,
            ("salt" Core..=) Core.<$> salt
          ]
      )
