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
-- Module      : Amazonka.MediaConvert.Types.PresetSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.PresetSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AudioDescription
import Amazonka.MediaConvert.Types.CaptionDescriptionPreset
import Amazonka.MediaConvert.Types.ContainerSettings
import Amazonka.MediaConvert.Types.VideoDescription
import qualified Amazonka.Prelude as Prelude

-- | Settings for preset
--
-- /See:/ 'newPresetSettings' smart constructor.
data PresetSettings = PresetSettings'
  { -- | Container specific settings.
    containerSettings :: Prelude.Maybe ContainerSettings,
    -- | This object holds groups of settings related to captions for one output.
    -- For each output that has captions, include one instance of
    -- CaptionDescriptions.
    captionDescriptions :: Prelude.Maybe [CaptionDescriptionPreset],
    -- | (AudioDescriptions) contains groups of audio encoding settings organized
    -- by audio codec. Include one instance of (AudioDescriptions) per output.
    -- (AudioDescriptions) can contain multiple groups of encoding settings.
    audioDescriptions :: Prelude.Maybe [AudioDescription],
    -- | VideoDescription contains a group of video encoding settings. The
    -- specific video settings depend on the video codec that you choose for
    -- the property codec. Include one instance of VideoDescription per output.
    videoDescription :: Prelude.Maybe VideoDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PresetSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerSettings', 'presetSettings_containerSettings' - Container specific settings.
--
-- 'captionDescriptions', 'presetSettings_captionDescriptions' - This object holds groups of settings related to captions for one output.
-- For each output that has captions, include one instance of
-- CaptionDescriptions.
--
-- 'audioDescriptions', 'presetSettings_audioDescriptions' - (AudioDescriptions) contains groups of audio encoding settings organized
-- by audio codec. Include one instance of (AudioDescriptions) per output.
-- (AudioDescriptions) can contain multiple groups of encoding settings.
--
-- 'videoDescription', 'presetSettings_videoDescription' - VideoDescription contains a group of video encoding settings. The
-- specific video settings depend on the video codec that you choose for
-- the property codec. Include one instance of VideoDescription per output.
newPresetSettings ::
  PresetSettings
newPresetSettings =
  PresetSettings'
    { containerSettings =
        Prelude.Nothing,
      captionDescriptions = Prelude.Nothing,
      audioDescriptions = Prelude.Nothing,
      videoDescription = Prelude.Nothing
    }

-- | Container specific settings.
presetSettings_containerSettings :: Lens.Lens' PresetSettings (Prelude.Maybe ContainerSettings)
presetSettings_containerSettings = Lens.lens (\PresetSettings' {containerSettings} -> containerSettings) (\s@PresetSettings' {} a -> s {containerSettings = a} :: PresetSettings)

-- | This object holds groups of settings related to captions for one output.
-- For each output that has captions, include one instance of
-- CaptionDescriptions.
presetSettings_captionDescriptions :: Lens.Lens' PresetSettings (Prelude.Maybe [CaptionDescriptionPreset])
presetSettings_captionDescriptions = Lens.lens (\PresetSettings' {captionDescriptions} -> captionDescriptions) (\s@PresetSettings' {} a -> s {captionDescriptions = a} :: PresetSettings) Prelude.. Lens.mapping Lens.coerced

-- | (AudioDescriptions) contains groups of audio encoding settings organized
-- by audio codec. Include one instance of (AudioDescriptions) per output.
-- (AudioDescriptions) can contain multiple groups of encoding settings.
presetSettings_audioDescriptions :: Lens.Lens' PresetSettings (Prelude.Maybe [AudioDescription])
presetSettings_audioDescriptions = Lens.lens (\PresetSettings' {audioDescriptions} -> audioDescriptions) (\s@PresetSettings' {} a -> s {audioDescriptions = a} :: PresetSettings) Prelude.. Lens.mapping Lens.coerced

-- | VideoDescription contains a group of video encoding settings. The
-- specific video settings depend on the video codec that you choose for
-- the property codec. Include one instance of VideoDescription per output.
presetSettings_videoDescription :: Lens.Lens' PresetSettings (Prelude.Maybe VideoDescription)
presetSettings_videoDescription = Lens.lens (\PresetSettings' {videoDescription} -> videoDescription) (\s@PresetSettings' {} a -> s {videoDescription = a} :: PresetSettings)

instance Data.FromJSON PresetSettings where
  parseJSON =
    Data.withObject
      "PresetSettings"
      ( \x ->
          PresetSettings'
            Prelude.<$> (x Data..:? "containerSettings")
            Prelude.<*> ( x Data..:? "captionDescriptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "audioDescriptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "videoDescription")
      )

instance Prelude.Hashable PresetSettings where
  hashWithSalt _salt PresetSettings' {..} =
    _salt `Prelude.hashWithSalt` containerSettings
      `Prelude.hashWithSalt` captionDescriptions
      `Prelude.hashWithSalt` audioDescriptions
      `Prelude.hashWithSalt` videoDescription

instance Prelude.NFData PresetSettings where
  rnf PresetSettings' {..} =
    Prelude.rnf containerSettings
      `Prelude.seq` Prelude.rnf captionDescriptions
      `Prelude.seq` Prelude.rnf audioDescriptions
      `Prelude.seq` Prelude.rnf videoDescription

instance Data.ToJSON PresetSettings where
  toJSON PresetSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("containerSettings" Data..=)
              Prelude.<$> containerSettings,
            ("captionDescriptions" Data..=)
              Prelude.<$> captionDescriptions,
            ("audioDescriptions" Data..=)
              Prelude.<$> audioDescriptions,
            ("videoDescription" Data..=)
              Prelude.<$> videoDescription
          ]
      )
