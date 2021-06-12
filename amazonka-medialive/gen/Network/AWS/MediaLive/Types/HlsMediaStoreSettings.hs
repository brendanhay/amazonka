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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.HlsMediaStoreStorageClass

-- | Hls Media Store Settings
--
-- /See:/ 'newHlsMediaStoreSettings' smart constructor.
data HlsMediaStoreSettings = HlsMediaStoreSettings'
  { -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Core.Maybe Core.Natural,
    -- | Number of retry attempts that will be made before the Live Event is put
    -- into an error state.
    numRetries :: Core.Maybe Core.Natural,
    -- | When set to temporal, output files are stored in non-persistent memory
    -- for faster reading and writing.
    mediaStoreStorageClass :: Core.Maybe HlsMediaStoreStorageClass,
    -- | Number of seconds to wait before retrying connection to the CDN if the
    -- connection is lost.
    connectionRetryInterval :: Core.Maybe Core.Natural,
    -- | If a streaming output fails, number of seconds to wait until a restart
    -- is initiated. A value of 0 means never restart.
    restartDelay :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      numRetries = Core.Nothing,
      mediaStoreStorageClass = Core.Nothing,
      connectionRetryInterval = Core.Nothing,
      restartDelay = Core.Nothing
    }

-- | Size in seconds of file cache for streaming outputs.
hlsMediaStoreSettings_filecacheDuration :: Lens.Lens' HlsMediaStoreSettings (Core.Maybe Core.Natural)
hlsMediaStoreSettings_filecacheDuration = Lens.lens (\HlsMediaStoreSettings' {filecacheDuration} -> filecacheDuration) (\s@HlsMediaStoreSettings' {} a -> s {filecacheDuration = a} :: HlsMediaStoreSettings)

-- | Number of retry attempts that will be made before the Live Event is put
-- into an error state.
hlsMediaStoreSettings_numRetries :: Lens.Lens' HlsMediaStoreSettings (Core.Maybe Core.Natural)
hlsMediaStoreSettings_numRetries = Lens.lens (\HlsMediaStoreSettings' {numRetries} -> numRetries) (\s@HlsMediaStoreSettings' {} a -> s {numRetries = a} :: HlsMediaStoreSettings)

-- | When set to temporal, output files are stored in non-persistent memory
-- for faster reading and writing.
hlsMediaStoreSettings_mediaStoreStorageClass :: Lens.Lens' HlsMediaStoreSettings (Core.Maybe HlsMediaStoreStorageClass)
hlsMediaStoreSettings_mediaStoreStorageClass = Lens.lens (\HlsMediaStoreSettings' {mediaStoreStorageClass} -> mediaStoreStorageClass) (\s@HlsMediaStoreSettings' {} a -> s {mediaStoreStorageClass = a} :: HlsMediaStoreSettings)

-- | Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
hlsMediaStoreSettings_connectionRetryInterval :: Lens.Lens' HlsMediaStoreSettings (Core.Maybe Core.Natural)
hlsMediaStoreSettings_connectionRetryInterval = Lens.lens (\HlsMediaStoreSettings' {connectionRetryInterval} -> connectionRetryInterval) (\s@HlsMediaStoreSettings' {} a -> s {connectionRetryInterval = a} :: HlsMediaStoreSettings)

-- | If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
hlsMediaStoreSettings_restartDelay :: Lens.Lens' HlsMediaStoreSettings (Core.Maybe Core.Natural)
hlsMediaStoreSettings_restartDelay = Lens.lens (\HlsMediaStoreSettings' {restartDelay} -> restartDelay) (\s@HlsMediaStoreSettings' {} a -> s {restartDelay = a} :: HlsMediaStoreSettings)

instance Core.FromJSON HlsMediaStoreSettings where
  parseJSON =
    Core.withObject
      "HlsMediaStoreSettings"
      ( \x ->
          HlsMediaStoreSettings'
            Core.<$> (x Core..:? "filecacheDuration")
            Core.<*> (x Core..:? "numRetries")
            Core.<*> (x Core..:? "mediaStoreStorageClass")
            Core.<*> (x Core..:? "connectionRetryInterval")
            Core.<*> (x Core..:? "restartDelay")
      )

instance Core.Hashable HlsMediaStoreSettings

instance Core.NFData HlsMediaStoreSettings

instance Core.ToJSON HlsMediaStoreSettings where
  toJSON HlsMediaStoreSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("filecacheDuration" Core..=)
              Core.<$> filecacheDuration,
            ("numRetries" Core..=) Core.<$> numRetries,
            ("mediaStoreStorageClass" Core..=)
              Core.<$> mediaStoreStorageClass,
            ("connectionRetryInterval" Core..=)
              Core.<$> connectionRetryInterval,
            ("restartDelay" Core..=) Core.<$> restartDelay
          ]
      )
