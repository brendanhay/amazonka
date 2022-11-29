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
-- Module      : Amazonka.MediaLive.Types.HlsAkamaiSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsAkamaiSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.HlsAkamaiHttpTransferMode
import qualified Amazonka.Prelude as Prelude

-- | Hls Akamai Settings
--
-- /See:/ 'newHlsAkamaiSettings' smart constructor.
data HlsAkamaiSettings = HlsAkamaiSettings'
  { -- | Salt for authenticated Akamai.
    salt :: Prelude.Maybe Prelude.Text,
    -- | Number of retry attempts that will be made before the Live Event is put
    -- into an error state.
    numRetries :: Prelude.Maybe Prelude.Natural,
    -- | Number of seconds to wait before retrying connection to the CDN if the
    -- connection is lost.
    connectionRetryInterval :: Prelude.Maybe Prelude.Natural,
    -- | Specify whether or not to use chunked transfer encoding to Akamai. User
    -- should contact Akamai to enable this feature.
    httpTransferMode :: Prelude.Maybe HlsAkamaiHttpTransferMode,
    -- | If a streaming output fails, number of seconds to wait until a restart
    -- is initiated. A value of 0 means never restart.
    restartDelay :: Prelude.Maybe Prelude.Natural,
    -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Prelude.Maybe Prelude.Natural,
    -- | Token parameter for authenticated akamai. If not specified, _gda_ is
    -- used.
    token :: Prelude.Maybe Prelude.Text
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
-- 'salt', 'hlsAkamaiSettings_salt' - Salt for authenticated Akamai.
--
-- 'numRetries', 'hlsAkamaiSettings_numRetries' - Number of retry attempts that will be made before the Live Event is put
-- into an error state.
--
-- 'connectionRetryInterval', 'hlsAkamaiSettings_connectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
--
-- 'httpTransferMode', 'hlsAkamaiSettings_httpTransferMode' - Specify whether or not to use chunked transfer encoding to Akamai. User
-- should contact Akamai to enable this feature.
--
-- 'restartDelay', 'hlsAkamaiSettings_restartDelay' - If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
--
-- 'filecacheDuration', 'hlsAkamaiSettings_filecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- 'token', 'hlsAkamaiSettings_token' - Token parameter for authenticated akamai. If not specified, _gda_ is
-- used.
newHlsAkamaiSettings ::
  HlsAkamaiSettings
newHlsAkamaiSettings =
  HlsAkamaiSettings'
    { salt = Prelude.Nothing,
      numRetries = Prelude.Nothing,
      connectionRetryInterval = Prelude.Nothing,
      httpTransferMode = Prelude.Nothing,
      restartDelay = Prelude.Nothing,
      filecacheDuration = Prelude.Nothing,
      token = Prelude.Nothing
    }

-- | Salt for authenticated Akamai.
hlsAkamaiSettings_salt :: Lens.Lens' HlsAkamaiSettings (Prelude.Maybe Prelude.Text)
hlsAkamaiSettings_salt = Lens.lens (\HlsAkamaiSettings' {salt} -> salt) (\s@HlsAkamaiSettings' {} a -> s {salt = a} :: HlsAkamaiSettings)

-- | Number of retry attempts that will be made before the Live Event is put
-- into an error state.
hlsAkamaiSettings_numRetries :: Lens.Lens' HlsAkamaiSettings (Prelude.Maybe Prelude.Natural)
hlsAkamaiSettings_numRetries = Lens.lens (\HlsAkamaiSettings' {numRetries} -> numRetries) (\s@HlsAkamaiSettings' {} a -> s {numRetries = a} :: HlsAkamaiSettings)

-- | Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
hlsAkamaiSettings_connectionRetryInterval :: Lens.Lens' HlsAkamaiSettings (Prelude.Maybe Prelude.Natural)
hlsAkamaiSettings_connectionRetryInterval = Lens.lens (\HlsAkamaiSettings' {connectionRetryInterval} -> connectionRetryInterval) (\s@HlsAkamaiSettings' {} a -> s {connectionRetryInterval = a} :: HlsAkamaiSettings)

-- | Specify whether or not to use chunked transfer encoding to Akamai. User
-- should contact Akamai to enable this feature.
hlsAkamaiSettings_httpTransferMode :: Lens.Lens' HlsAkamaiSettings (Prelude.Maybe HlsAkamaiHttpTransferMode)
hlsAkamaiSettings_httpTransferMode = Lens.lens (\HlsAkamaiSettings' {httpTransferMode} -> httpTransferMode) (\s@HlsAkamaiSettings' {} a -> s {httpTransferMode = a} :: HlsAkamaiSettings)

-- | If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
hlsAkamaiSettings_restartDelay :: Lens.Lens' HlsAkamaiSettings (Prelude.Maybe Prelude.Natural)
hlsAkamaiSettings_restartDelay = Lens.lens (\HlsAkamaiSettings' {restartDelay} -> restartDelay) (\s@HlsAkamaiSettings' {} a -> s {restartDelay = a} :: HlsAkamaiSettings)

-- | Size in seconds of file cache for streaming outputs.
hlsAkamaiSettings_filecacheDuration :: Lens.Lens' HlsAkamaiSettings (Prelude.Maybe Prelude.Natural)
hlsAkamaiSettings_filecacheDuration = Lens.lens (\HlsAkamaiSettings' {filecacheDuration} -> filecacheDuration) (\s@HlsAkamaiSettings' {} a -> s {filecacheDuration = a} :: HlsAkamaiSettings)

-- | Token parameter for authenticated akamai. If not specified, _gda_ is
-- used.
hlsAkamaiSettings_token :: Lens.Lens' HlsAkamaiSettings (Prelude.Maybe Prelude.Text)
hlsAkamaiSettings_token = Lens.lens (\HlsAkamaiSettings' {token} -> token) (\s@HlsAkamaiSettings' {} a -> s {token = a} :: HlsAkamaiSettings)

instance Core.FromJSON HlsAkamaiSettings where
  parseJSON =
    Core.withObject
      "HlsAkamaiSettings"
      ( \x ->
          HlsAkamaiSettings'
            Prelude.<$> (x Core..:? "salt")
            Prelude.<*> (x Core..:? "numRetries")
            Prelude.<*> (x Core..:? "connectionRetryInterval")
            Prelude.<*> (x Core..:? "httpTransferMode")
            Prelude.<*> (x Core..:? "restartDelay")
            Prelude.<*> (x Core..:? "filecacheDuration")
            Prelude.<*> (x Core..:? "token")
      )

instance Prelude.Hashable HlsAkamaiSettings where
  hashWithSalt _salt HlsAkamaiSettings' {..} =
    _salt `Prelude.hashWithSalt` salt
      `Prelude.hashWithSalt` numRetries
      `Prelude.hashWithSalt` connectionRetryInterval
      `Prelude.hashWithSalt` httpTransferMode
      `Prelude.hashWithSalt` restartDelay
      `Prelude.hashWithSalt` filecacheDuration
      `Prelude.hashWithSalt` token

instance Prelude.NFData HlsAkamaiSettings where
  rnf HlsAkamaiSettings' {..} =
    Prelude.rnf salt
      `Prelude.seq` Prelude.rnf numRetries
      `Prelude.seq` Prelude.rnf connectionRetryInterval
      `Prelude.seq` Prelude.rnf httpTransferMode
      `Prelude.seq` Prelude.rnf restartDelay
      `Prelude.seq` Prelude.rnf filecacheDuration
      `Prelude.seq` Prelude.rnf token

instance Core.ToJSON HlsAkamaiSettings where
  toJSON HlsAkamaiSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("salt" Core..=) Prelude.<$> salt,
            ("numRetries" Core..=) Prelude.<$> numRetries,
            ("connectionRetryInterval" Core..=)
              Prelude.<$> connectionRetryInterval,
            ("httpTransferMode" Core..=)
              Prelude.<$> httpTransferMode,
            ("restartDelay" Core..=) Prelude.<$> restartDelay,
            ("filecacheDuration" Core..=)
              Prelude.<$> filecacheDuration,
            ("token" Core..=) Prelude.<$> token
          ]
      )
