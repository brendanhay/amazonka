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
-- Module      : Network.AWS.MediaLive.Types.HlsBasicPutSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsBasicPutSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Hls Basic Put Settings
--
-- /See:/ 'newHlsBasicPutSettings' smart constructor.
data HlsBasicPutSettings = HlsBasicPutSettings'
  { -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Core.Maybe Core.Natural,
    -- | Number of retry attempts that will be made before the Live Event is put
    -- into an error state.
    numRetries :: Core.Maybe Core.Natural,
    -- | Number of seconds to wait before retrying connection to the CDN if the
    -- connection is lost.
    connectionRetryInterval :: Core.Maybe Core.Natural,
    -- | If a streaming output fails, number of seconds to wait until a restart
    -- is initiated. A value of 0 means never restart.
    restartDelay :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HlsBasicPutSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filecacheDuration', 'hlsBasicPutSettings_filecacheDuration' - Size in seconds of file cache for streaming outputs.
--
-- 'numRetries', 'hlsBasicPutSettings_numRetries' - Number of retry attempts that will be made before the Live Event is put
-- into an error state.
--
-- 'connectionRetryInterval', 'hlsBasicPutSettings_connectionRetryInterval' - Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
--
-- 'restartDelay', 'hlsBasicPutSettings_restartDelay' - If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
newHlsBasicPutSettings ::
  HlsBasicPutSettings
newHlsBasicPutSettings =
  HlsBasicPutSettings'
    { filecacheDuration =
        Core.Nothing,
      numRetries = Core.Nothing,
      connectionRetryInterval = Core.Nothing,
      restartDelay = Core.Nothing
    }

-- | Size in seconds of file cache for streaming outputs.
hlsBasicPutSettings_filecacheDuration :: Lens.Lens' HlsBasicPutSettings (Core.Maybe Core.Natural)
hlsBasicPutSettings_filecacheDuration = Lens.lens (\HlsBasicPutSettings' {filecacheDuration} -> filecacheDuration) (\s@HlsBasicPutSettings' {} a -> s {filecacheDuration = a} :: HlsBasicPutSettings)

-- | Number of retry attempts that will be made before the Live Event is put
-- into an error state.
hlsBasicPutSettings_numRetries :: Lens.Lens' HlsBasicPutSettings (Core.Maybe Core.Natural)
hlsBasicPutSettings_numRetries = Lens.lens (\HlsBasicPutSettings' {numRetries} -> numRetries) (\s@HlsBasicPutSettings' {} a -> s {numRetries = a} :: HlsBasicPutSettings)

-- | Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
hlsBasicPutSettings_connectionRetryInterval :: Lens.Lens' HlsBasicPutSettings (Core.Maybe Core.Natural)
hlsBasicPutSettings_connectionRetryInterval = Lens.lens (\HlsBasicPutSettings' {connectionRetryInterval} -> connectionRetryInterval) (\s@HlsBasicPutSettings' {} a -> s {connectionRetryInterval = a} :: HlsBasicPutSettings)

-- | If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
hlsBasicPutSettings_restartDelay :: Lens.Lens' HlsBasicPutSettings (Core.Maybe Core.Natural)
hlsBasicPutSettings_restartDelay = Lens.lens (\HlsBasicPutSettings' {restartDelay} -> restartDelay) (\s@HlsBasicPutSettings' {} a -> s {restartDelay = a} :: HlsBasicPutSettings)

instance Core.FromJSON HlsBasicPutSettings where
  parseJSON =
    Core.withObject
      "HlsBasicPutSettings"
      ( \x ->
          HlsBasicPutSettings'
            Core.<$> (x Core..:? "filecacheDuration")
            Core.<*> (x Core..:? "numRetries")
            Core.<*> (x Core..:? "connectionRetryInterval")
            Core.<*> (x Core..:? "restartDelay")
      )

instance Core.Hashable HlsBasicPutSettings

instance Core.NFData HlsBasicPutSettings

instance Core.ToJSON HlsBasicPutSettings where
  toJSON HlsBasicPutSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("filecacheDuration" Core..=)
              Core.<$> filecacheDuration,
            ("numRetries" Core..=) Core.<$> numRetries,
            ("connectionRetryInterval" Core..=)
              Core.<$> connectionRetryInterval,
            ("restartDelay" Core..=) Core.<$> restartDelay
          ]
      )
