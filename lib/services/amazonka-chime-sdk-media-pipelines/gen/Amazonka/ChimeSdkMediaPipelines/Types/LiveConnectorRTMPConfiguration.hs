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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorRTMPConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorRTMPConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.AudioChannelsOption
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The media pipeline\'s RTMP configuration object.
--
-- /See:/ 'newLiveConnectorRTMPConfiguration' smart constructor.
data LiveConnectorRTMPConfiguration = LiveConnectorRTMPConfiguration'
  { -- | The audio channels set for the RTMP configuration
    audioChannels :: Prelude.Maybe AudioChannelsOption,
    -- | The audio sample rate set for the RTMP configuration. Default: 48000.
    audioSampleRate :: Prelude.Maybe Prelude.Text,
    -- | The URL of the RTMP configuration.
    url :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LiveConnectorRTMPConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioChannels', 'liveConnectorRTMPConfiguration_audioChannels' - The audio channels set for the RTMP configuration
--
-- 'audioSampleRate', 'liveConnectorRTMPConfiguration_audioSampleRate' - The audio sample rate set for the RTMP configuration. Default: 48000.
--
-- 'url', 'liveConnectorRTMPConfiguration_url' - The URL of the RTMP configuration.
newLiveConnectorRTMPConfiguration ::
  -- | 'url'
  Prelude.Text ->
  LiveConnectorRTMPConfiguration
newLiveConnectorRTMPConfiguration pUrl_ =
  LiveConnectorRTMPConfiguration'
    { audioChannels =
        Prelude.Nothing,
      audioSampleRate = Prelude.Nothing,
      url = Core._Sensitive Lens.# pUrl_
    }

-- | The audio channels set for the RTMP configuration
liveConnectorRTMPConfiguration_audioChannels :: Lens.Lens' LiveConnectorRTMPConfiguration (Prelude.Maybe AudioChannelsOption)
liveConnectorRTMPConfiguration_audioChannels = Lens.lens (\LiveConnectorRTMPConfiguration' {audioChannels} -> audioChannels) (\s@LiveConnectorRTMPConfiguration' {} a -> s {audioChannels = a} :: LiveConnectorRTMPConfiguration)

-- | The audio sample rate set for the RTMP configuration. Default: 48000.
liveConnectorRTMPConfiguration_audioSampleRate :: Lens.Lens' LiveConnectorRTMPConfiguration (Prelude.Maybe Prelude.Text)
liveConnectorRTMPConfiguration_audioSampleRate = Lens.lens (\LiveConnectorRTMPConfiguration' {audioSampleRate} -> audioSampleRate) (\s@LiveConnectorRTMPConfiguration' {} a -> s {audioSampleRate = a} :: LiveConnectorRTMPConfiguration)

-- | The URL of the RTMP configuration.
liveConnectorRTMPConfiguration_url :: Lens.Lens' LiveConnectorRTMPConfiguration Prelude.Text
liveConnectorRTMPConfiguration_url = Lens.lens (\LiveConnectorRTMPConfiguration' {url} -> url) (\s@LiveConnectorRTMPConfiguration' {} a -> s {url = a} :: LiveConnectorRTMPConfiguration) Prelude.. Core._Sensitive

instance Core.FromJSON LiveConnectorRTMPConfiguration where
  parseJSON =
    Core.withObject
      "LiveConnectorRTMPConfiguration"
      ( \x ->
          LiveConnectorRTMPConfiguration'
            Prelude.<$> (x Core..:? "AudioChannels")
            Prelude.<*> (x Core..:? "AudioSampleRate")
            Prelude.<*> (x Core..: "Url")
      )

instance
  Prelude.Hashable
    LiveConnectorRTMPConfiguration
  where
  hashWithSalt
    _salt
    LiveConnectorRTMPConfiguration' {..} =
      _salt `Prelude.hashWithSalt` audioChannels
        `Prelude.hashWithSalt` audioSampleRate
        `Prelude.hashWithSalt` url

instance
  Prelude.NFData
    LiveConnectorRTMPConfiguration
  where
  rnf LiveConnectorRTMPConfiguration' {..} =
    Prelude.rnf audioChannels
      `Prelude.seq` Prelude.rnf audioSampleRate
      `Prelude.seq` Prelude.rnf url

instance Core.ToJSON LiveConnectorRTMPConfiguration where
  toJSON LiveConnectorRTMPConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AudioChannels" Core..=) Prelude.<$> audioChannels,
            ("AudioSampleRate" Core..=)
              Prelude.<$> audioSampleRate,
            Prelude.Just ("Url" Core..= url)
          ]
      )
