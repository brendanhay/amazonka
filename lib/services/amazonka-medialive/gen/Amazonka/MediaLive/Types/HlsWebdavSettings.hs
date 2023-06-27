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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsWebdavSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.HlsWebdavHttpTransferMode
import qualified Amazonka.Prelude as Prelude

-- | Hls Webdav Settings
--
-- /See:/ 'newHlsWebdavSettings' smart constructor.
data HlsWebdavSettings = HlsWebdavSettings'
  { -- | Number of seconds to wait before retrying connection to the CDN if the
    -- connection is lost.
    connectionRetryInterval :: Prelude.Maybe Prelude.Natural,
    -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Prelude.Maybe Prelude.Natural,
    -- | Specify whether or not to use chunked transfer encoding to WebDAV.
    httpTransferMode :: Prelude.Maybe HlsWebdavHttpTransferMode,
    -- | Number of retry attempts that will be made before the Live Event is put
    -- into an error state. Applies only if the CDN destination URI begins with
    -- \"s3\" or \"mediastore\". For other URIs, the value is always 3.
    numRetries :: Prelude.Maybe Prelude.Natural,
    -- | If a streaming output fails, number of seconds to wait until a restart
    -- is initiated. A value of 0 means never restart.
    restartDelay :: Prelude.Maybe Prelude.Natural
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
-- 'connectionRetryInterval', 'hlsWebdavSettings_connectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
--
-- 'filecacheDuration', 'hlsWebdavSettings_filecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- 'httpTransferMode', 'hlsWebdavSettings_httpTransferMode' - Specify whether or not to use chunked transfer encoding to WebDAV.
--
-- 'numRetries', 'hlsWebdavSettings_numRetries' - Number of retry attempts that will be made before the Live Event is put
-- into an error state. Applies only if the CDN destination URI begins with
-- \"s3\" or \"mediastore\". For other URIs, the value is always 3.
--
-- 'restartDelay', 'hlsWebdavSettings_restartDelay' - If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
newHlsWebdavSettings ::
  HlsWebdavSettings
newHlsWebdavSettings =
  HlsWebdavSettings'
    { connectionRetryInterval =
        Prelude.Nothing,
      filecacheDuration = Prelude.Nothing,
      httpTransferMode = Prelude.Nothing,
      numRetries = Prelude.Nothing,
      restartDelay = Prelude.Nothing
    }

-- | Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
hlsWebdavSettings_connectionRetryInterval :: Lens.Lens' HlsWebdavSettings (Prelude.Maybe Prelude.Natural)
hlsWebdavSettings_connectionRetryInterval = Lens.lens (\HlsWebdavSettings' {connectionRetryInterval} -> connectionRetryInterval) (\s@HlsWebdavSettings' {} a -> s {connectionRetryInterval = a} :: HlsWebdavSettings)

-- | Size in seconds of file cache for streaming outputs.
hlsWebdavSettings_filecacheDuration :: Lens.Lens' HlsWebdavSettings (Prelude.Maybe Prelude.Natural)
hlsWebdavSettings_filecacheDuration = Lens.lens (\HlsWebdavSettings' {filecacheDuration} -> filecacheDuration) (\s@HlsWebdavSettings' {} a -> s {filecacheDuration = a} :: HlsWebdavSettings)

-- | Specify whether or not to use chunked transfer encoding to WebDAV.
hlsWebdavSettings_httpTransferMode :: Lens.Lens' HlsWebdavSettings (Prelude.Maybe HlsWebdavHttpTransferMode)
hlsWebdavSettings_httpTransferMode = Lens.lens (\HlsWebdavSettings' {httpTransferMode} -> httpTransferMode) (\s@HlsWebdavSettings' {} a -> s {httpTransferMode = a} :: HlsWebdavSettings)

-- | Number of retry attempts that will be made before the Live Event is put
-- into an error state. Applies only if the CDN destination URI begins with
-- \"s3\" or \"mediastore\". For other URIs, the value is always 3.
hlsWebdavSettings_numRetries :: Lens.Lens' HlsWebdavSettings (Prelude.Maybe Prelude.Natural)
hlsWebdavSettings_numRetries = Lens.lens (\HlsWebdavSettings' {numRetries} -> numRetries) (\s@HlsWebdavSettings' {} a -> s {numRetries = a} :: HlsWebdavSettings)

-- | If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
hlsWebdavSettings_restartDelay :: Lens.Lens' HlsWebdavSettings (Prelude.Maybe Prelude.Natural)
hlsWebdavSettings_restartDelay = Lens.lens (\HlsWebdavSettings' {restartDelay} -> restartDelay) (\s@HlsWebdavSettings' {} a -> s {restartDelay = a} :: HlsWebdavSettings)

instance Data.FromJSON HlsWebdavSettings where
  parseJSON =
    Data.withObject
      "HlsWebdavSettings"
      ( \x ->
          HlsWebdavSettings'
            Prelude.<$> (x Data..:? "connectionRetryInterval")
            Prelude.<*> (x Data..:? "filecacheDuration")
            Prelude.<*> (x Data..:? "httpTransferMode")
            Prelude.<*> (x Data..:? "numRetries")
            Prelude.<*> (x Data..:? "restartDelay")
      )

instance Prelude.Hashable HlsWebdavSettings where
  hashWithSalt _salt HlsWebdavSettings' {..} =
    _salt
      `Prelude.hashWithSalt` connectionRetryInterval
      `Prelude.hashWithSalt` filecacheDuration
      `Prelude.hashWithSalt` httpTransferMode
      `Prelude.hashWithSalt` numRetries
      `Prelude.hashWithSalt` restartDelay

instance Prelude.NFData HlsWebdavSettings where
  rnf HlsWebdavSettings' {..} =
    Prelude.rnf connectionRetryInterval
      `Prelude.seq` Prelude.rnf filecacheDuration
      `Prelude.seq` Prelude.rnf httpTransferMode
      `Prelude.seq` Prelude.rnf numRetries
      `Prelude.seq` Prelude.rnf restartDelay

instance Data.ToJSON HlsWebdavSettings where
  toJSON HlsWebdavSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("connectionRetryInterval" Data..=)
              Prelude.<$> connectionRetryInterval,
            ("filecacheDuration" Data..=)
              Prelude.<$> filecacheDuration,
            ("httpTransferMode" Data..=)
              Prelude.<$> httpTransferMode,
            ("numRetries" Data..=) Prelude.<$> numRetries,
            ("restartDelay" Data..=) Prelude.<$> restartDelay
          ]
      )
