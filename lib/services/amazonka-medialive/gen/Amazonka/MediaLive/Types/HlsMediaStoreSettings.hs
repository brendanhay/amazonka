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
-- Module      : Amazonka.MediaLive.Types.HlsMediaStoreSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsMediaStoreSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.HlsMediaStoreStorageClass
import qualified Amazonka.Prelude as Prelude

-- | Hls Media Store Settings
--
-- /See:/ 'newHlsMediaStoreSettings' smart constructor.
data HlsMediaStoreSettings = HlsMediaStoreSettings'
  { -- | Number of retry attempts that will be made before the Live Event is put
    -- into an error state.
    numRetries :: Prelude.Maybe Prelude.Natural,
    -- | Number of seconds to wait before retrying connection to the CDN if the
    -- connection is lost.
    connectionRetryInterval :: Prelude.Maybe Prelude.Natural,
    -- | When set to temporal, output files are stored in non-persistent memory
    -- for faster reading and writing.
    mediaStoreStorageClass :: Prelude.Maybe HlsMediaStoreStorageClass,
    -- | If a streaming output fails, number of seconds to wait until a restart
    -- is initiated. A value of 0 means never restart.
    restartDelay :: Prelude.Maybe Prelude.Natural,
    -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsMediaStoreSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numRetries', 'hlsMediaStoreSettings_numRetries' - Number of retry attempts that will be made before the Live Event is put
-- into an error state.
--
-- 'connectionRetryInterval', 'hlsMediaStoreSettings_connectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
--
-- 'mediaStoreStorageClass', 'hlsMediaStoreSettings_mediaStoreStorageClass' - When set to temporal, output files are stored in non-persistent memory
-- for faster reading and writing.
--
-- 'restartDelay', 'hlsMediaStoreSettings_restartDelay' - If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
--
-- 'filecacheDuration', 'hlsMediaStoreSettings_filecacheDuration' - Size in seconds of file cache for streaming outputs.
newHlsMediaStoreSettings ::
  HlsMediaStoreSettings
newHlsMediaStoreSettings =
  HlsMediaStoreSettings'
    { numRetries =
        Prelude.Nothing,
      connectionRetryInterval = Prelude.Nothing,
      mediaStoreStorageClass = Prelude.Nothing,
      restartDelay = Prelude.Nothing,
      filecacheDuration = Prelude.Nothing
    }

-- | Number of retry attempts that will be made before the Live Event is put
-- into an error state.
hlsMediaStoreSettings_numRetries :: Lens.Lens' HlsMediaStoreSettings (Prelude.Maybe Prelude.Natural)
hlsMediaStoreSettings_numRetries = Lens.lens (\HlsMediaStoreSettings' {numRetries} -> numRetries) (\s@HlsMediaStoreSettings' {} a -> s {numRetries = a} :: HlsMediaStoreSettings)

-- | Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
hlsMediaStoreSettings_connectionRetryInterval :: Lens.Lens' HlsMediaStoreSettings (Prelude.Maybe Prelude.Natural)
hlsMediaStoreSettings_connectionRetryInterval = Lens.lens (\HlsMediaStoreSettings' {connectionRetryInterval} -> connectionRetryInterval) (\s@HlsMediaStoreSettings' {} a -> s {connectionRetryInterval = a} :: HlsMediaStoreSettings)

-- | When set to temporal, output files are stored in non-persistent memory
-- for faster reading and writing.
hlsMediaStoreSettings_mediaStoreStorageClass :: Lens.Lens' HlsMediaStoreSettings (Prelude.Maybe HlsMediaStoreStorageClass)
hlsMediaStoreSettings_mediaStoreStorageClass = Lens.lens (\HlsMediaStoreSettings' {mediaStoreStorageClass} -> mediaStoreStorageClass) (\s@HlsMediaStoreSettings' {} a -> s {mediaStoreStorageClass = a} :: HlsMediaStoreSettings)

-- | If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
hlsMediaStoreSettings_restartDelay :: Lens.Lens' HlsMediaStoreSettings (Prelude.Maybe Prelude.Natural)
hlsMediaStoreSettings_restartDelay = Lens.lens (\HlsMediaStoreSettings' {restartDelay} -> restartDelay) (\s@HlsMediaStoreSettings' {} a -> s {restartDelay = a} :: HlsMediaStoreSettings)

-- | Size in seconds of file cache for streaming outputs.
hlsMediaStoreSettings_filecacheDuration :: Lens.Lens' HlsMediaStoreSettings (Prelude.Maybe Prelude.Natural)
hlsMediaStoreSettings_filecacheDuration = Lens.lens (\HlsMediaStoreSettings' {filecacheDuration} -> filecacheDuration) (\s@HlsMediaStoreSettings' {} a -> s {filecacheDuration = a} :: HlsMediaStoreSettings)

instance Core.FromJSON HlsMediaStoreSettings where
  parseJSON =
    Core.withObject
      "HlsMediaStoreSettings"
      ( \x ->
          HlsMediaStoreSettings'
            Prelude.<$> (x Core..:? "numRetries")
            Prelude.<*> (x Core..:? "connectionRetryInterval")
            Prelude.<*> (x Core..:? "mediaStoreStorageClass")
            Prelude.<*> (x Core..:? "restartDelay")
            Prelude.<*> (x Core..:? "filecacheDuration")
      )

instance Prelude.Hashable HlsMediaStoreSettings where
  hashWithSalt _salt HlsMediaStoreSettings' {..} =
    _salt `Prelude.hashWithSalt` numRetries
      `Prelude.hashWithSalt` connectionRetryInterval
      `Prelude.hashWithSalt` mediaStoreStorageClass
      `Prelude.hashWithSalt` restartDelay
      `Prelude.hashWithSalt` filecacheDuration

instance Prelude.NFData HlsMediaStoreSettings where
  rnf HlsMediaStoreSettings' {..} =
    Prelude.rnf numRetries
      `Prelude.seq` Prelude.rnf connectionRetryInterval
      `Prelude.seq` Prelude.rnf mediaStoreStorageClass
      `Prelude.seq` Prelude.rnf restartDelay
      `Prelude.seq` Prelude.rnf filecacheDuration

instance Core.ToJSON HlsMediaStoreSettings where
  toJSON HlsMediaStoreSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("numRetries" Core..=) Prelude.<$> numRetries,
            ("connectionRetryInterval" Core..=)
              Prelude.<$> connectionRetryInterval,
            ("mediaStoreStorageClass" Core..=)
              Prelude.<$> mediaStoreStorageClass,
            ("restartDelay" Core..=) Prelude.<$> restartDelay,
            ("filecacheDuration" Core..=)
              Prelude.<$> filecacheDuration
          ]
      )
