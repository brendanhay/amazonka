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
-- Module      : Network.AWS.MediaLive.Types.HlsBasicPutSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsBasicPutSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Hls Basic Put Settings
--
-- /See:/ 'newHlsBasicPutSettings' smart constructor.
data HlsBasicPutSettings = HlsBasicPutSettings'
  { -- | Size in seconds of file cache for streaming outputs.
    filecacheDuration :: Prelude.Maybe Prelude.Natural,
    -- | Number of retry attempts that will be made before the Live Event is put
    -- into an error state.
    numRetries :: Prelude.Maybe Prelude.Natural,
    -- | Number of seconds to wait before retrying connection to the CDN if the
    -- connection is lost.
    connectionRetryInterval :: Prelude.Maybe Prelude.Natural,
    -- | If a streaming output fails, number of seconds to wait until a restart
    -- is initiated. A value of 0 means never restart.
    restartDelay :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      numRetries = Prelude.Nothing,
      connectionRetryInterval = Prelude.Nothing,
      restartDelay = Prelude.Nothing
    }

-- | Size in seconds of file cache for streaming outputs.
hlsBasicPutSettings_filecacheDuration :: Lens.Lens' HlsBasicPutSettings (Prelude.Maybe Prelude.Natural)
hlsBasicPutSettings_filecacheDuration = Lens.lens (\HlsBasicPutSettings' {filecacheDuration} -> filecacheDuration) (\s@HlsBasicPutSettings' {} a -> s {filecacheDuration = a} :: HlsBasicPutSettings)

-- | Number of retry attempts that will be made before the Live Event is put
-- into an error state.
hlsBasicPutSettings_numRetries :: Lens.Lens' HlsBasicPutSettings (Prelude.Maybe Prelude.Natural)
hlsBasicPutSettings_numRetries = Lens.lens (\HlsBasicPutSettings' {numRetries} -> numRetries) (\s@HlsBasicPutSettings' {} a -> s {numRetries = a} :: HlsBasicPutSettings)

-- | Number of seconds to wait before retrying connection to the CDN if the
-- connection is lost.
hlsBasicPutSettings_connectionRetryInterval :: Lens.Lens' HlsBasicPutSettings (Prelude.Maybe Prelude.Natural)
hlsBasicPutSettings_connectionRetryInterval = Lens.lens (\HlsBasicPutSettings' {connectionRetryInterval} -> connectionRetryInterval) (\s@HlsBasicPutSettings' {} a -> s {connectionRetryInterval = a} :: HlsBasicPutSettings)

-- | If a streaming output fails, number of seconds to wait until a restart
-- is initiated. A value of 0 means never restart.
hlsBasicPutSettings_restartDelay :: Lens.Lens' HlsBasicPutSettings (Prelude.Maybe Prelude.Natural)
hlsBasicPutSettings_restartDelay = Lens.lens (\HlsBasicPutSettings' {restartDelay} -> restartDelay) (\s@HlsBasicPutSettings' {} a -> s {restartDelay = a} :: HlsBasicPutSettings)

instance Prelude.FromJSON HlsBasicPutSettings where
  parseJSON =
    Prelude.withObject
      "HlsBasicPutSettings"
      ( \x ->
          HlsBasicPutSettings'
            Prelude.<$> (x Prelude..:? "filecacheDuration")
            Prelude.<*> (x Prelude..:? "numRetries")
            Prelude.<*> (x Prelude..:? "connectionRetryInterval")
            Prelude.<*> (x Prelude..:? "restartDelay")
      )

instance Prelude.Hashable HlsBasicPutSettings

instance Prelude.NFData HlsBasicPutSettings

instance Prelude.ToJSON HlsBasicPutSettings where
  toJSON HlsBasicPutSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("filecacheDuration" Prelude..=)
              Prelude.<$> filecacheDuration,
            ("numRetries" Prelude..=) Prelude.<$> numRetries,
            ("connectionRetryInterval" Prelude..=)
              Prelude.<$> connectionRetryInterval,
            ("restartDelay" Prelude..=)
              Prelude.<$> restartDelay
          ]
      )
