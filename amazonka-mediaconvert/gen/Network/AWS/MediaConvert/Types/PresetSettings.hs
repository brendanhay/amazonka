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
-- Module      : Network.AWS.MediaConvert.Types.PresetSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.PresetSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AudioDescription
import Network.AWS.MediaConvert.Types.CaptionDescriptionPreset
import Network.AWS.MediaConvert.Types.ContainerSettings
import Network.AWS.MediaConvert.Types.VideoDescription
import qualified Network.AWS.Prelude as Prelude

-- | Settings for preset
--
-- /See:/ 'newPresetSettings' smart constructor.
data PresetSettings = PresetSettings'
  { -- | (AudioDescriptions) contains groups of audio encoding settings organized
    -- by audio codec. Include one instance of (AudioDescriptions) per output.
    -- (AudioDescriptions) can contain multiple groups of encoding settings.
    audioDescriptions :: Prelude.Maybe [AudioDescription],
    -- | Container specific settings.
    containerSettings :: Prelude.Maybe ContainerSettings,
    -- | (VideoDescription) contains a group of video encoding settings. The
    -- specific video settings depend on the video codec that you choose when
    -- you specify a value for Video codec (codec). Include one instance of
    -- (VideoDescription) per output.
    videoDescription :: Prelude.Maybe VideoDescription,
    -- | Caption settings for this preset. There can be multiple caption settings
    -- in a single output.
    captionDescriptions :: Prelude.Maybe [CaptionDescriptionPreset]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PresetSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioDescriptions', 'presetSettings_audioDescriptions' - (AudioDescriptions) contains groups of audio encoding settings organized
-- by audio codec. Include one instance of (AudioDescriptions) per output.
-- (AudioDescriptions) can contain multiple groups of encoding settings.
--
-- 'containerSettings', 'presetSettings_containerSettings' - Container specific settings.
--
-- 'videoDescription', 'presetSettings_videoDescription' - (VideoDescription) contains a group of video encoding settings. The
-- specific video settings depend on the video codec that you choose when
-- you specify a value for Video codec (codec). Include one instance of
-- (VideoDescription) per output.
--
-- 'captionDescriptions', 'presetSettings_captionDescriptions' - Caption settings for this preset. There can be multiple caption settings
-- in a single output.
newPresetSettings ::
  PresetSettings
newPresetSettings =
  PresetSettings'
    { audioDescriptions =
        Prelude.Nothing,
      containerSettings = Prelude.Nothing,
      videoDescription = Prelude.Nothing,
      captionDescriptions = Prelude.Nothing
    }

-- | (AudioDescriptions) contains groups of audio encoding settings organized
-- by audio codec. Include one instance of (AudioDescriptions) per output.
-- (AudioDescriptions) can contain multiple groups of encoding settings.
presetSettings_audioDescriptions :: Lens.Lens' PresetSettings (Prelude.Maybe [AudioDescription])
presetSettings_audioDescriptions = Lens.lens (\PresetSettings' {audioDescriptions} -> audioDescriptions) (\s@PresetSettings' {} a -> s {audioDescriptions = a} :: PresetSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | Container specific settings.
presetSettings_containerSettings :: Lens.Lens' PresetSettings (Prelude.Maybe ContainerSettings)
presetSettings_containerSettings = Lens.lens (\PresetSettings' {containerSettings} -> containerSettings) (\s@PresetSettings' {} a -> s {containerSettings = a} :: PresetSettings)

-- | (VideoDescription) contains a group of video encoding settings. The
-- specific video settings depend on the video codec that you choose when
-- you specify a value for Video codec (codec). Include one instance of
-- (VideoDescription) per output.
presetSettings_videoDescription :: Lens.Lens' PresetSettings (Prelude.Maybe VideoDescription)
presetSettings_videoDescription = Lens.lens (\PresetSettings' {videoDescription} -> videoDescription) (\s@PresetSettings' {} a -> s {videoDescription = a} :: PresetSettings)

-- | Caption settings for this preset. There can be multiple caption settings
-- in a single output.
presetSettings_captionDescriptions :: Lens.Lens' PresetSettings (Prelude.Maybe [CaptionDescriptionPreset])
presetSettings_captionDescriptions = Lens.lens (\PresetSettings' {captionDescriptions} -> captionDescriptions) (\s@PresetSettings' {} a -> s {captionDescriptions = a} :: PresetSettings) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON PresetSettings where
  parseJSON =
    Prelude.withObject
      "PresetSettings"
      ( \x ->
          PresetSettings'
            Prelude.<$> ( x Prelude..:? "audioDescriptions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "containerSettings")
            Prelude.<*> (x Prelude..:? "videoDescription")
            Prelude.<*> ( x Prelude..:? "captionDescriptions"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PresetSettings

instance Prelude.NFData PresetSettings

instance Prelude.ToJSON PresetSettings where
  toJSON PresetSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("audioDescriptions" Prelude..=)
              Prelude.<$> audioDescriptions,
            ("containerSettings" Prelude..=)
              Prelude.<$> containerSettings,
            ("videoDescription" Prelude..=)
              Prelude.<$> videoDescription,
            ("captionDescriptions" Prelude..=)
              Prelude.<$> captionDescriptions
          ]
      )
