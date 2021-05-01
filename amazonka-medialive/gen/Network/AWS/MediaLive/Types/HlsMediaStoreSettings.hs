{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.HlsMediaStoreSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsMediaStoreSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.HlsMediaStoreStorageClass
import qualified Network.AWS.Prelude as Prelude

-- | Hls Media Store Settings
--
-- /See:/ 'newHlsMediaStoreSettings' smart constructor.
data HlsMediaStoreSettings = HlsMediaStoreSettings'
  { -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Prelude.Maybe Prelude.Natural,
    -- | Number of retry attempts that will be made before the Live Event is put
    -- into an error state.
    numRetries :: Prelude.Maybe Prelude.Natural,
    -- | When set to temporal, output files are stored in non-persistent memory
    -- for faster reading and writing.
    mediaStoreStorageClass :: Prelude.Maybe HlsMediaStoreStorageClass,
    -- | Number of seconds to wait before retrying connection to the CDN if the
    -- connection is lost.
    connectionRetryInterval :: Prelude.Maybe Prelude.Natural,
    -- | If a streaming output fails, number of seconds to wait until a restart
    -- is initiated. A value of 0 means never restart.
    restartDelay :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HlsMediaStoreSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filecacheDuration', 'hlsMediaStoreSettings_filecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- 'numRetries', 'hlsMediaStoreSettings_numRetries' - Number of retry attempts that will be made before the Live Event is put
-- into an error state.
--
-- 'mediaStoreStorageClass', 'hlsMediaStoreSettings_mediaStoreStorageClass' - When set to temporal, output files are stored in non-persistent memory
-- for faster reading and writing.
--
-- 'connectionRetryInterval', 'hlsMediaStoreSettings_connectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
--
-- 'restartDelay', 'hlsMediaStoreSettings_restartDelay' - If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
newHlsMediaStoreSettings ::
  HlsMediaStoreSettings
newHlsMediaStoreSettings =
  HlsMediaStoreSettings'
    { filecacheDuration =
        Prelude.Nothing,
      numRetries = Prelude.Nothing,
      mediaStoreStorageClass = Prelude.Nothing,
      connectionRetryInterval = Prelude.Nothing,
      restartDelay = Prelude.Nothing
    }

-- | Size in seconds of file cache for streaming outputs.
hlsMediaStoreSettings_filecacheDuration :: Lens.Lens' HlsMediaStoreSettings (Prelude.Maybe Prelude.Natural)
hlsMediaStoreSettings_filecacheDuration = Lens.lens (\HlsMediaStoreSettings' {filecacheDuration} -> filecacheDuration) (\s@HlsMediaStoreSettings' {} a -> s {filecacheDuration = a} :: HlsMediaStoreSettings)

-- | Number of retry attempts that will be made before the Live Event is put
-- into an error state.
hlsMediaStoreSettings_numRetries :: Lens.Lens' HlsMediaStoreSettings (Prelude.Maybe Prelude.Natural)
hlsMediaStoreSettings_numRetries = Lens.lens (\HlsMediaStoreSettings' {numRetries} -> numRetries) (\s@HlsMediaStoreSettings' {} a -> s {numRetries = a} :: HlsMediaStoreSettings)

-- | When set to temporal, output files are stored in non-persistent memory
-- for faster reading and writing.
hlsMediaStoreSettings_mediaStoreStorageClass :: Lens.Lens' HlsMediaStoreSettings (Prelude.Maybe HlsMediaStoreStorageClass)
hlsMediaStoreSettings_mediaStoreStorageClass = Lens.lens (\HlsMediaStoreSettings' {mediaStoreStorageClass} -> mediaStoreStorageClass) (\s@HlsMediaStoreSettings' {} a -> s {mediaStoreStorageClass = a} :: HlsMediaStoreSettings)

-- | Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
hlsMediaStoreSettings_connectionRetryInterval :: Lens.Lens' HlsMediaStoreSettings (Prelude.Maybe Prelude.Natural)
hlsMediaStoreSettings_connectionRetryInterval = Lens.lens (\HlsMediaStoreSettings' {connectionRetryInterval} -> connectionRetryInterval) (\s@HlsMediaStoreSettings' {} a -> s {connectionRetryInterval = a} :: HlsMediaStoreSettings)

-- | If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
hlsMediaStoreSettings_restartDelay :: Lens.Lens' HlsMediaStoreSettings (Prelude.Maybe Prelude.Natural)
hlsMediaStoreSettings_restartDelay = Lens.lens (\HlsMediaStoreSettings' {restartDelay} -> restartDelay) (\s@HlsMediaStoreSettings' {} a -> s {restartDelay = a} :: HlsMediaStoreSettings)

instance Prelude.FromJSON HlsMediaStoreSettings where
  parseJSON =
    Prelude.withObject
      "HlsMediaStoreSettings"
      ( \x ->
          HlsMediaStoreSettings'
            Prelude.<$> (x Prelude..:? "filecacheDuration")
            Prelude.<*> (x Prelude..:? "numRetries")
            Prelude.<*> (x Prelude..:? "mediaStoreStorageClass")
            Prelude.<*> (x Prelude..:? "connectionRetryInterval")
            Prelude.<*> (x Prelude..:? "restartDelay")
      )

instance Prelude.Hashable HlsMediaStoreSettings

instance Prelude.NFData HlsMediaStoreSettings

instance Prelude.ToJSON HlsMediaStoreSettings where
  toJSON HlsMediaStoreSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("filecacheDuration" Prelude..=)
              Prelude.<$> filecacheDuration,
            ("numRetries" Prelude..=) Prelude.<$> numRetries,
            ("mediaStoreStorageClass" Prelude..=)
              Prelude.<$> mediaStoreStorageClass,
            ("connectionRetryInterval" Prelude..=)
              Prelude.<$> connectionRetryInterval,
            ("restartDelay" Prelude..=)
              Prelude.<$> restartDelay
          ]
      )
