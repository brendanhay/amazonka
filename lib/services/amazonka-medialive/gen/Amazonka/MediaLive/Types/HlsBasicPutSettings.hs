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
-- Module      : Amazonka.MediaLive.Types.HlsBasicPutSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsBasicPutSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Hls Basic Put Settings
--
-- /See:/ 'newHlsBasicPutSettings' smart constructor.
data HlsBasicPutSettings = HlsBasicPutSettings'
  { -- | Number of seconds to wait before retrying connection to the CDN if the
    -- connection is lost.
    connectionRetryInterval :: Prelude.Maybe Prelude.Natural,
    -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Prelude.Maybe Prelude.Natural,
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
-- Create a value of 'HlsBasicPutSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionRetryInterval', 'hlsBasicPutSettings_connectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
--
-- 'filecacheDuration', 'hlsBasicPutSettings_filecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- 'numRetries', 'hlsBasicPutSettings_numRetries' - Number of retry attempts that will be made before the Live Event is put
-- into an error state. Applies only if the CDN destination URI begins with
-- \"s3\" or \"mediastore\". For other URIs, the value is always 3.
--
-- 'restartDelay', 'hlsBasicPutSettings_restartDelay' - If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
newHlsBasicPutSettings ::
  HlsBasicPutSettings
newHlsBasicPutSettings =
  HlsBasicPutSettings'
    { connectionRetryInterval =
        Prelude.Nothing,
      filecacheDuration = Prelude.Nothing,
      numRetries = Prelude.Nothing,
      restartDelay = Prelude.Nothing
    }

-- | Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
hlsBasicPutSettings_connectionRetryInterval :: Lens.Lens' HlsBasicPutSettings (Prelude.Maybe Prelude.Natural)
hlsBasicPutSettings_connectionRetryInterval = Lens.lens (\HlsBasicPutSettings' {connectionRetryInterval} -> connectionRetryInterval) (\s@HlsBasicPutSettings' {} a -> s {connectionRetryInterval = a} :: HlsBasicPutSettings)

-- | Size in seconds of file cache for streaming outputs.
hlsBasicPutSettings_filecacheDuration :: Lens.Lens' HlsBasicPutSettings (Prelude.Maybe Prelude.Natural)
hlsBasicPutSettings_filecacheDuration = Lens.lens (\HlsBasicPutSettings' {filecacheDuration} -> filecacheDuration) (\s@HlsBasicPutSettings' {} a -> s {filecacheDuration = a} :: HlsBasicPutSettings)

-- | Number of retry attempts that will be made before the Live Event is put
-- into an error state. Applies only if the CDN destination URI begins with
-- \"s3\" or \"mediastore\". For other URIs, the value is always 3.
hlsBasicPutSettings_numRetries :: Lens.Lens' HlsBasicPutSettings (Prelude.Maybe Prelude.Natural)
hlsBasicPutSettings_numRetries = Lens.lens (\HlsBasicPutSettings' {numRetries} -> numRetries) (\s@HlsBasicPutSettings' {} a -> s {numRetries = a} :: HlsBasicPutSettings)

-- | If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
hlsBasicPutSettings_restartDelay :: Lens.Lens' HlsBasicPutSettings (Prelude.Maybe Prelude.Natural)
hlsBasicPutSettings_restartDelay = Lens.lens (\HlsBasicPutSettings' {restartDelay} -> restartDelay) (\s@HlsBasicPutSettings' {} a -> s {restartDelay = a} :: HlsBasicPutSettings)

instance Data.FromJSON HlsBasicPutSettings where
  parseJSON =
    Data.withObject
      "HlsBasicPutSettings"
      ( \x ->
          HlsBasicPutSettings'
            Prelude.<$> (x Data..:? "connectionRetryInterval")
            Prelude.<*> (x Data..:? "filecacheDuration")
            Prelude.<*> (x Data..:? "numRetries")
            Prelude.<*> (x Data..:? "restartDelay")
      )

instance Prelude.Hashable HlsBasicPutSettings where
  hashWithSalt _salt HlsBasicPutSettings' {..} =
    _salt
      `Prelude.hashWithSalt` connectionRetryInterval
      `Prelude.hashWithSalt` filecacheDuration
      `Prelude.hashWithSalt` numRetries
      `Prelude.hashWithSalt` restartDelay

instance Prelude.NFData HlsBasicPutSettings where
  rnf HlsBasicPutSettings' {..} =
    Prelude.rnf connectionRetryInterval
      `Prelude.seq` Prelude.rnf filecacheDuration
      `Prelude.seq` Prelude.rnf numRetries
      `Prelude.seq` Prelude.rnf restartDelay

instance Data.ToJSON HlsBasicPutSettings where
  toJSON HlsBasicPutSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("connectionRetryInterval" Data..=)
              Prelude.<$> connectionRetryInterval,
            ("filecacheDuration" Data..=)
              Prelude.<$> filecacheDuration,
            ("numRetries" Data..=) Prelude.<$> numRetries,
            ("restartDelay" Data..=) Prelude.<$> restartDelay
          ]
      )
