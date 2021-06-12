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
-- Module      : Network.AWS.MediaLive.Types.InputSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioSelector
import Network.AWS.MediaLive.Types.CaptionSelector
import Network.AWS.MediaLive.Types.InputDeblockFilter
import Network.AWS.MediaLive.Types.InputDenoiseFilter
import Network.AWS.MediaLive.Types.InputFilter
import Network.AWS.MediaLive.Types.InputSourceEndBehavior
import Network.AWS.MediaLive.Types.NetworkInputSettings
import Network.AWS.MediaLive.Types.Smpte2038DataPreference
import Network.AWS.MediaLive.Types.VideoSelector

-- | Live Event input parameters. There can be multiple inputs in a single
-- Live Event.
--
-- /See:/ 'newInputSettings' smart constructor.
data InputSettings = InputSettings'
  { -- | Enable or disable the denoise filter when filtering.
    denoiseFilter :: Core.Maybe InputDenoiseFilter,
    -- | Used to select the audio stream to decode for inputs that have multiple
    -- available.
    audioSelectors :: Core.Maybe [AudioSelector],
    -- | Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
    filterStrength :: Core.Maybe Core.Natural,
    -- | Specifies whether to extract applicable ancillary data from a SMPTE-2038
    -- source in this input. Applicable data types are captions, timecode, AFD,
    -- and SCTE-104 messages. - PREFER: Extract from SMPTE-2038 if present in
    -- this input, otherwise extract from another source (if any). - IGNORE:
    -- Never extract any ancillary data from SMPTE-2038.
    smpte2038DataPreference :: Core.Maybe Smpte2038DataPreference,
    -- | Informs which video elementary stream to decode for input types that
    -- have multiple available.
    videoSelector :: Core.Maybe VideoSelector,
    -- | Loop input if it is a file. This allows a file input to be streamed
    -- indefinitely.
    sourceEndBehavior :: Core.Maybe InputSourceEndBehavior,
    -- | Turns on the filter for this input. MPEG-2 inputs have the deblocking
    -- filter enabled by default. 1) auto - filtering will be applied depending
    -- on input type\/quality 2) disabled - no filtering will be applied to the
    -- input 3) forced - filtering will be applied regardless of input type
    inputFilter :: Core.Maybe InputFilter,
    -- | Enable or disable the deblock filter when filtering.
    deblockFilter :: Core.Maybe InputDeblockFilter,
    -- | Used to select the caption input to use for inputs that have multiple
    -- available.
    captionSelectors :: Core.Maybe [CaptionSelector],
    -- | Input settings.
    networkInputSettings :: Core.Maybe NetworkInputSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'denoiseFilter', 'inputSettings_denoiseFilter' - Enable or disable the denoise filter when filtering.
--
-- 'audioSelectors', 'inputSettings_audioSelectors' - Used to select the audio stream to decode for inputs that have multiple
-- available.
--
-- 'filterStrength', 'inputSettings_filterStrength' - Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
--
-- 'smpte2038DataPreference', 'inputSettings_smpte2038DataPreference' - Specifies whether to extract applicable ancillary data from a SMPTE-2038
-- source in this input. Applicable data types are captions, timecode, AFD,
-- and SCTE-104 messages. - PREFER: Extract from SMPTE-2038 if present in
-- this input, otherwise extract from another source (if any). - IGNORE:
-- Never extract any ancillary data from SMPTE-2038.
--
-- 'videoSelector', 'inputSettings_videoSelector' - Informs which video elementary stream to decode for input types that
-- have multiple available.
--
-- 'sourceEndBehavior', 'inputSettings_sourceEndBehavior' - Loop input if it is a file. This allows a file input to be streamed
-- indefinitely.
--
-- 'inputFilter', 'inputSettings_inputFilter' - Turns on the filter for this input. MPEG-2 inputs have the deblocking
-- filter enabled by default. 1) auto - filtering will be applied depending
-- on input type\/quality 2) disabled - no filtering will be applied to the
-- input 3) forced - filtering will be applied regardless of input type
--
-- 'deblockFilter', 'inputSettings_deblockFilter' - Enable or disable the deblock filter when filtering.
--
-- 'captionSelectors', 'inputSettings_captionSelectors' - Used to select the caption input to use for inputs that have multiple
-- available.
--
-- 'networkInputSettings', 'inputSettings_networkInputSettings' - Input settings.
newInputSettings ::
  InputSettings
newInputSettings =
  InputSettings'
    { denoiseFilter = Core.Nothing,
      audioSelectors = Core.Nothing,
      filterStrength = Core.Nothing,
      smpte2038DataPreference = Core.Nothing,
      videoSelector = Core.Nothing,
      sourceEndBehavior = Core.Nothing,
      inputFilter = Core.Nothing,
      deblockFilter = Core.Nothing,
      captionSelectors = Core.Nothing,
      networkInputSettings = Core.Nothing
    }

-- | Enable or disable the denoise filter when filtering.
inputSettings_denoiseFilter :: Lens.Lens' InputSettings (Core.Maybe InputDenoiseFilter)
inputSettings_denoiseFilter = Lens.lens (\InputSettings' {denoiseFilter} -> denoiseFilter) (\s@InputSettings' {} a -> s {denoiseFilter = a} :: InputSettings)

-- | Used to select the audio stream to decode for inputs that have multiple
-- available.
inputSettings_audioSelectors :: Lens.Lens' InputSettings (Core.Maybe [AudioSelector])
inputSettings_audioSelectors = Lens.lens (\InputSettings' {audioSelectors} -> audioSelectors) (\s@InputSettings' {} a -> s {audioSelectors = a} :: InputSettings) Core.. Lens.mapping Lens._Coerce

-- | Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
inputSettings_filterStrength :: Lens.Lens' InputSettings (Core.Maybe Core.Natural)
inputSettings_filterStrength = Lens.lens (\InputSettings' {filterStrength} -> filterStrength) (\s@InputSettings' {} a -> s {filterStrength = a} :: InputSettings)

-- | Specifies whether to extract applicable ancillary data from a SMPTE-2038
-- source in this input. Applicable data types are captions, timecode, AFD,
-- and SCTE-104 messages. - PREFER: Extract from SMPTE-2038 if present in
-- this input, otherwise extract from another source (if any). - IGNORE:
-- Never extract any ancillary data from SMPTE-2038.
inputSettings_smpte2038DataPreference :: Lens.Lens' InputSettings (Core.Maybe Smpte2038DataPreference)
inputSettings_smpte2038DataPreference = Lens.lens (\InputSettings' {smpte2038DataPreference} -> smpte2038DataPreference) (\s@InputSettings' {} a -> s {smpte2038DataPreference = a} :: InputSettings)

-- | Informs which video elementary stream to decode for input types that
-- have multiple available.
inputSettings_videoSelector :: Lens.Lens' InputSettings (Core.Maybe VideoSelector)
inputSettings_videoSelector = Lens.lens (\InputSettings' {videoSelector} -> videoSelector) (\s@InputSettings' {} a -> s {videoSelector = a} :: InputSettings)

-- | Loop input if it is a file. This allows a file input to be streamed
-- indefinitely.
inputSettings_sourceEndBehavior :: Lens.Lens' InputSettings (Core.Maybe InputSourceEndBehavior)
inputSettings_sourceEndBehavior = Lens.lens (\InputSettings' {sourceEndBehavior} -> sourceEndBehavior) (\s@InputSettings' {} a -> s {sourceEndBehavior = a} :: InputSettings)

-- | Turns on the filter for this input. MPEG-2 inputs have the deblocking
-- filter enabled by default. 1) auto - filtering will be applied depending
-- on input type\/quality 2) disabled - no filtering will be applied to the
-- input 3) forced - filtering will be applied regardless of input type
inputSettings_inputFilter :: Lens.Lens' InputSettings (Core.Maybe InputFilter)
inputSettings_inputFilter = Lens.lens (\InputSettings' {inputFilter} -> inputFilter) (\s@InputSettings' {} a -> s {inputFilter = a} :: InputSettings)

-- | Enable or disable the deblock filter when filtering.
inputSettings_deblockFilter :: Lens.Lens' InputSettings (Core.Maybe InputDeblockFilter)
inputSettings_deblockFilter = Lens.lens (\InputSettings' {deblockFilter} -> deblockFilter) (\s@InputSettings' {} a -> s {deblockFilter = a} :: InputSettings)

-- | Used to select the caption input to use for inputs that have multiple
-- available.
inputSettings_captionSelectors :: Lens.Lens' InputSettings (Core.Maybe [CaptionSelector])
inputSettings_captionSelectors = Lens.lens (\InputSettings' {captionSelectors} -> captionSelectors) (\s@InputSettings' {} a -> s {captionSelectors = a} :: InputSettings) Core.. Lens.mapping Lens._Coerce

-- | Input settings.
inputSettings_networkInputSettings :: Lens.Lens' InputSettings (Core.Maybe NetworkInputSettings)
inputSettings_networkInputSettings = Lens.lens (\InputSettings' {networkInputSettings} -> networkInputSettings) (\s@InputSettings' {} a -> s {networkInputSettings = a} :: InputSettings)

instance Core.FromJSON InputSettings where
  parseJSON =
    Core.withObject
      "InputSettings"
      ( \x ->
          InputSettings'
            Core.<$> (x Core..:? "denoiseFilter")
            Core.<*> (x Core..:? "audioSelectors" Core..!= Core.mempty)
            Core.<*> (x Core..:? "filterStrength")
            Core.<*> (x Core..:? "smpte2038DataPreference")
            Core.<*> (x Core..:? "videoSelector")
            Core.<*> (x Core..:? "sourceEndBehavior")
            Core.<*> (x Core..:? "inputFilter")
            Core.<*> (x Core..:? "deblockFilter")
            Core.<*> (x Core..:? "captionSelectors" Core..!= Core.mempty)
            Core.<*> (x Core..:? "networkInputSettings")
      )

instance Core.Hashable InputSettings

instance Core.NFData InputSettings

instance Core.ToJSON InputSettings where
  toJSON InputSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("denoiseFilter" Core..=) Core.<$> denoiseFilter,
            ("audioSelectors" Core..=) Core.<$> audioSelectors,
            ("filterStrength" Core..=) Core.<$> filterStrength,
            ("smpte2038DataPreference" Core..=)
              Core.<$> smpte2038DataPreference,
            ("videoSelector" Core..=) Core.<$> videoSelector,
            ("sourceEndBehavior" Core..=)
              Core.<$> sourceEndBehavior,
            ("inputFilter" Core..=) Core.<$> inputFilter,
            ("deblockFilter" Core..=) Core.<$> deblockFilter,
            ("captionSelectors" Core..=)
              Core.<$> captionSelectors,
            ("networkInputSettings" Core..=)
              Core.<$> networkInputSettings
          ]
      )
