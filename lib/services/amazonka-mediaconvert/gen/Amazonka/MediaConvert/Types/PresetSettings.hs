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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.PresetSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.AudioDescription
import Amazonka.MediaConvert.Types.CaptionDescriptionPreset
import Amazonka.MediaConvert.Types.ContainerSettings
import Amazonka.MediaConvert.Types.VideoDescription
import qualified Amazonka.Prelude as Prelude

-- | Settings for preset
--
-- /See:/ 'newPresetSettings' smart constructor.
data PresetSettings = PresetSettings'
  { -- | This object holds groups of settings related to captions for one output.
    -- For each output that has captions, include one instance of
    -- CaptionDescriptions.
    captionDescriptions :: Prelude.Maybe [CaptionDescriptionPreset],
    -- | VideoDescription contains a group of video encoding settings. The
    -- specific video settings depend on the video codec that you choose for
    -- the property codec. Include one instance of VideoDescription per output.
    videoDescription :: Prelude.Maybe VideoDescription,
    -- | Container specific settings.
    containerSettings :: Prelude.Maybe ContainerSettings,
    -- | (AudioDescriptions) contains groups of audio encoding settings organized
    -- by audio codec. Include one instance of (AudioDescriptions) per output.
    -- (AudioDescriptions) can contain multiple groups of encoding settings.
    audioDescriptions :: Prelude.Maybe [AudioDescription]
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
-- 'captionDescriptions', 'presetSettings_captionDescriptions' - This object holds groups of settings related to captions for one output.
-- For each output that has captions, include one instance of
-- CaptionDescriptions.
--
-- 'videoDescription', 'presetSettings_videoDescription' - VideoDescription contains a group of video encoding settings. The
-- specific video settings depend on the video codec that you choose for
-- the property codec. Include one instance of VideoDescription per output.
--
-- 'containerSettings', 'presetSettings_containerSettings' - Container specific settings.
--
-- 'audioDescriptions', 'presetSettings_audioDescriptions' - (AudioDescriptions) contains groups of audio encoding settings organized
-- by audio codec. Include one instance of (AudioDescriptions) per output.
-- (AudioDescriptions) can contain multiple groups of encoding settings.
newPresetSettings ::
  PresetSettings
newPresetSettings =
  PresetSettings'
    { captionDescriptions =
        Prelude.Nothing,
      videoDescription = Prelude.Nothing,
      containerSettings = Prelude.Nothing,
      audioDescriptions = Prelude.Nothing
    }

-- | This object holds groups of settings related to captions for one output.
-- For each output that has captions, include one instance of
-- CaptionDescriptions.
presetSettings_captionDescriptions :: Lens.Lens' PresetSettings (Prelude.Maybe [CaptionDescriptionPreset])
presetSettings_captionDescriptions = Lens.lens (\PresetSettings' {captionDescriptions} -> captionDescriptions) (\s@PresetSettings' {} a -> s {captionDescriptions = a} :: PresetSettings) Prelude.. Lens.mapping Lens.coerced

-- | VideoDescription contains a group of video encoding settings. The
-- specific video settings depend on the video codec that you choose for
-- the property codec. Include one instance of VideoDescription per output.
presetSettings_videoDescription :: Lens.Lens' PresetSettings (Prelude.Maybe VideoDescription)
presetSettings_videoDescription = Lens.lens (\PresetSettings' {videoDescription} -> videoDescription) (\s@PresetSettings' {} a -> s {videoDescription = a} :: PresetSettings)

-- | Container specific settings.
presetSettings_containerSettings :: Lens.Lens' PresetSettings (Prelude.Maybe ContainerSettings)
presetSettings_containerSettings = Lens.lens (\PresetSettings' {containerSettings} -> containerSettings) (\s@PresetSettings' {} a -> s {containerSettings = a} :: PresetSettings)

-- | (AudioDescriptions) contains groups of audio encoding settings organized
-- by audio codec. Include one instance of (AudioDescriptions) per output.
-- (AudioDescriptions) can contain multiple groups of encoding settings.
presetSettings_audioDescriptions :: Lens.Lens' PresetSettings (Prelude.Maybe [AudioDescription])
presetSettings_audioDescriptions = Lens.lens (\PresetSettings' {audioDescriptions} -> audioDescriptions) (\s@PresetSettings' {} a -> s {audioDescriptions = a} :: PresetSettings) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON PresetSettings where
  parseJSON =
    Core.withObject
      "PresetSettings"
      ( \x ->
          PresetSettings'
            Prelude.<$> ( x Core..:? "captionDescriptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "videoDescription")
            Prelude.<*> (x Core..:? "containerSettings")
            Prelude.<*> ( x Core..:? "audioDescriptions"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PresetSettings where
  hashWithSalt _salt PresetSettings' {..} =
    _salt `Prelude.hashWithSalt` captionDescriptions
      `Prelude.hashWithSalt` videoDescription
      `Prelude.hashWithSalt` containerSettings
      `Prelude.hashWithSalt` audioDescriptions

instance Prelude.NFData PresetSettings where
  rnf PresetSettings' {..} =
    Prelude.rnf captionDescriptions
      `Prelude.seq` Prelude.rnf videoDescription
      `Prelude.seq` Prelude.rnf containerSettings
      `Prelude.seq` Prelude.rnf audioDescriptions

instance Core.ToJSON PresetSettings where
  toJSON PresetSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("captionDescriptions" Core..=)
              Prelude.<$> captionDescriptions,
            ("videoDescription" Core..=)
              Prelude.<$> videoDescription,
            ("containerSettings" Core..=)
              Prelude.<$> containerSettings,
            ("audioDescriptions" Core..=)
              Prelude.<$> audioDescriptions
          ]
      )
