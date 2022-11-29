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
-- Module      : Amazonka.MediaLive.Types.HlsWebdavSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsWebdavSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.HlsWebdavHttpTransferMode
import qualified Amazonka.Prelude as Prelude

-- | Hls Webdav Settings
--
-- /See:/ 'newHlsWebdavSettings' smart constructor.
data HlsWebdavSettings = HlsWebdavSettings'
  { -- | Number of retry attempts that will be made before the Live Event is put
    -- into an error state.
    numRetries :: Prelude.Maybe Prelude.Natural,
    -- | Number of seconds to wait before retrying connection to the CDN if the
    -- connection is lost.
    connectionRetryInterval :: Prelude.Maybe Prelude.Natural,
    -- | Specify whether or not to use chunked transfer encoding to WebDAV.
    httpTransferMode :: Prelude.Maybe HlsWebdavHttpTransferMode,
    -- | If a streaming output fails, number of seconds to wait until a restart
    -- is initiated. A value of 0 means never restart.
    restartDelay :: Prelude.Maybe Prelude.Natural,
    -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsWebdavSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numRetries', 'hlsWebdavSettings_numRetries' - Number of retry attempts that will be made before the Live Event is put
-- into an error state.
--
-- 'connectionRetryInterval', 'hlsWebdavSettings_connectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
--
-- 'httpTransferMode', 'hlsWebdavSettings_httpTransferMode' - Specify whether or not to use chunked transfer encoding to WebDAV.
--
-- 'restartDelay', 'hlsWebdavSettings_restartDelay' - If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
--
-- 'filecacheDuration', 'hlsWebdavSettings_filecacheDuration' - Size in seconds of file cache for streaming outputs.
newHlsWebdavSettings ::
  HlsWebdavSettings
newHlsWebdavSettings =
  HlsWebdavSettings'
    { numRetries = Prelude.Nothing,
      connectionRetryInterval = Prelude.Nothing,
      httpTransferMode = Prelude.Nothing,
      restartDelay = Prelude.Nothing,
      filecacheDuration = Prelude.Nothing
    }

-- | Number of retry attempts that will be made before the Live Event is put
-- into an error state.
hlsWebdavSettings_numRetries :: Lens.Lens' HlsWebdavSettings (Prelude.Maybe Prelude.Natural)
hlsWebdavSettings_numRetries = Lens.lens (\HlsWebdavSettings' {numRetries} -> numRetries) (\s@HlsWebdavSettings' {} a -> s {numRetries = a} :: HlsWebdavSettings)

-- | Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
hlsWebdavSettings_connectionRetryInterval :: Lens.Lens' HlsWebdavSettings (Prelude.Maybe Prelude.Natural)
hlsWebdavSettings_connectionRetryInterval = Lens.lens (\HlsWebdavSettings' {connectionRetryInterval} -> connectionRetryInterval) (\s@HlsWebdavSettings' {} a -> s {connectionRetryInterval = a} :: HlsWebdavSettings)

-- | Specify whether or not to use chunked transfer encoding to WebDAV.
hlsWebdavSettings_httpTransferMode :: Lens.Lens' HlsWebdavSettings (Prelude.Maybe HlsWebdavHttpTransferMode)
hlsWebdavSettings_httpTransferMode = Lens.lens (\HlsWebdavSettings' {httpTransferMode} -> httpTransferMode) (\s@HlsWebdavSettings' {} a -> s {httpTransferMode = a} :: HlsWebdavSettings)

-- | If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
hlsWebdavSettings_restartDelay :: Lens.Lens' HlsWebdavSettings (Prelude.Maybe Prelude.Natural)
hlsWebdavSettings_restartDelay = Lens.lens (\HlsWebdavSettings' {restartDelay} -> restartDelay) (\s@HlsWebdavSettings' {} a -> s {restartDelay = a} :: HlsWebdavSettings)

-- | Size in seconds of file cache for streaming outputs.
hlsWebdavSettings_filecacheDuration :: Lens.Lens' HlsWebdavSettings (Prelude.Maybe Prelude.Natural)
hlsWebdavSettings_filecacheDuration = Lens.lens (\HlsWebdavSettings' {filecacheDuration} -> filecacheDuration) (\s@HlsWebdavSettings' {} a -> s {filecacheDuration = a} :: HlsWebdavSettings)

instance Core.FromJSON HlsWebdavSettings where
  parseJSON =
    Core.withObject
      "HlsWebdavSettings"
      ( \x ->
          HlsWebdavSettings'
            Prelude.<$> (x Core..:? "numRetries")
            Prelude.<*> (x Core..:? "connectionRetryInterval")
            Prelude.<*> (x Core..:? "httpTransferMode")
            Prelude.<*> (x Core..:? "restartDelay")
            Prelude.<*> (x Core..:? "filecacheDuration")
      )

instance Prelude.Hashable HlsWebdavSettings where
  hashWithSalt _salt HlsWebdavSettings' {..} =
    _salt `Prelude.hashWithSalt` numRetries
      `Prelude.hashWithSalt` connectionRetryInterval
      `Prelude.hashWithSalt` httpTransferMode
      `Prelude.hashWithSalt` restartDelay
      `Prelude.hashWithSalt` filecacheDuration

instance Prelude.NFData HlsWebdavSettings where
  rnf HlsWebdavSettings' {..} =
    Prelude.rnf numRetries
      `Prelude.seq` Prelude.rnf connectionRetryInterval
      `Prelude.seq` Prelude.rnf httpTransferMode
      `Prelude.seq` Prelude.rnf restartDelay
      `Prelude.seq` Prelude.rnf filecacheDuration

instance Core.ToJSON HlsWebdavSettings where
  toJSON HlsWebdavSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("numRetries" Core..=) Prelude.<$> numRetries,
            ("connectionRetryInterval" Core..=)
              Prelude.<$> connectionRetryInterval,
            ("httpTransferMode" Core..=)
              Prelude.<$> httpTransferMode,
            ("restartDelay" Core..=) Prelude.<$> restartDelay,
            ("filecacheDuration" Core..=)
              Prelude.<$> filecacheDuration
          ]
      )
