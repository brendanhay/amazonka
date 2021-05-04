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
-- Module      : Network.AWS.MediaLive.Types.InputSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSettings where

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
import qualified Network.AWS.Prelude as Prelude

-- | Live Event input parameters. There can be multiple inputs in a single
-- Live Event.
--
-- /See:/ 'newInputSettings' smart constructor.
data InputSettings = InputSettings'
  { -- | Enable or disable the denoise filter when filtering.
    denoiseFilter :: Prelude.Maybe InputDenoiseFilter,
    -- | Used to select the audio stream to decode for inputs that have multiple
    -- available.
    audioSelectors :: Prelude.Maybe [AudioSelector],
    -- | Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
    filterStrength :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether to extract applicable ancillary data from a SMPTE-2038
    -- source in this input. Applicable data types are captions, timecode, AFD,
    -- and SCTE-104 messages. - PREFER: Extract from SMPTE-2038 if present in
    -- this input, otherwise extract from another source (if any). - IGNORE:
    -- Never extract any ancillary data from SMPTE-2038.
    smpte2038DataPreference :: Prelude.Maybe Smpte2038DataPreference,
    -- | Informs which video elementary stream to decode for input types that
    -- have multiple available.
    videoSelector :: Prelude.Maybe VideoSelector,
    -- | Loop input if it is a file. This allows a file input to be streamed
    -- indefinitely.
    sourceEndBehavior :: Prelude.Maybe InputSourceEndBehavior,
    -- | Turns on the filter for this input. MPEG-2 inputs have the deblocking
    -- filter enabled by default. 1) auto - filtering will be applied depending
    -- on input type\/quality 2) disabled - no filtering will be applied to the
    -- input 3) forced - filtering will be applied regardless of input type
    inputFilter :: Prelude.Maybe InputFilter,
    -- | Enable or disable the deblock filter when filtering.
    deblockFilter :: Prelude.Maybe InputDeblockFilter,
    -- | Used to select the caption input to use for inputs that have multiple
    -- available.
    captionSelectors :: Prelude.Maybe [CaptionSelector],
    -- | Input settings.
    networkInputSettings :: Prelude.Maybe NetworkInputSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { denoiseFilter = Prelude.Nothing,
      audioSelectors = Prelude.Nothing,
      filterStrength = Prelude.Nothing,
      smpte2038DataPreference = Prelude.Nothing,
      videoSelector = Prelude.Nothing,
      sourceEndBehavior = Prelude.Nothing,
      inputFilter = Prelude.Nothing,
      deblockFilter = Prelude.Nothing,
      captionSelectors = Prelude.Nothing,
      networkInputSettings = Prelude.Nothing
    }

-- | Enable or disable the denoise filter when filtering.
inputSettings_denoiseFilter :: Lens.Lens' InputSettings (Prelude.Maybe InputDenoiseFilter)
inputSettings_denoiseFilter = Lens.lens (\InputSettings' {denoiseFilter} -> denoiseFilter) (\s@InputSettings' {} a -> s {denoiseFilter = a} :: InputSettings)

-- | Used to select the audio stream to decode for inputs that have multiple
-- available.
inputSettings_audioSelectors :: Lens.Lens' InputSettings (Prelude.Maybe [AudioSelector])
inputSettings_audioSelectors = Lens.lens (\InputSettings' {audioSelectors} -> audioSelectors) (\s@InputSettings' {} a -> s {audioSelectors = a} :: InputSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
inputSettings_filterStrength :: Lens.Lens' InputSettings (Prelude.Maybe Prelude.Natural)
inputSettings_filterStrength = Lens.lens (\InputSettings' {filterStrength} -> filterStrength) (\s@InputSettings' {} a -> s {filterStrength = a} :: InputSettings)

-- | Specifies whether to extract applicable ancillary data from a SMPTE-2038
-- source in this input. Applicable data types are captions, timecode, AFD,
-- and SCTE-104 messages. - PREFER: Extract from SMPTE-2038 if present in
-- this input, otherwise extract from another source (if any). - IGNORE:
-- Never extract any ancillary data from SMPTE-2038.
inputSettings_smpte2038DataPreference :: Lens.Lens' InputSettings (Prelude.Maybe Smpte2038DataPreference)
inputSettings_smpte2038DataPreference = Lens.lens (\InputSettings' {smpte2038DataPreference} -> smpte2038DataPreference) (\s@InputSettings' {} a -> s {smpte2038DataPreference = a} :: InputSettings)

-- | Informs which video elementary stream to decode for input types that
-- have multiple available.
inputSettings_videoSelector :: Lens.Lens' InputSettings (Prelude.Maybe VideoSelector)
inputSettings_videoSelector = Lens.lens (\InputSettings' {videoSelector} -> videoSelector) (\s@InputSettings' {} a -> s {videoSelector = a} :: InputSettings)

-- | Loop input if it is a file. This allows a file input to be streamed
-- indefinitely.
inputSettings_sourceEndBehavior :: Lens.Lens' InputSettings (Prelude.Maybe InputSourceEndBehavior)
inputSettings_sourceEndBehavior = Lens.lens (\InputSettings' {sourceEndBehavior} -> sourceEndBehavior) (\s@InputSettings' {} a -> s {sourceEndBehavior = a} :: InputSettings)

-- | Turns on the filter for this input. MPEG-2 inputs have the deblocking
-- filter enabled by default. 1) auto - filtering will be applied depending
-- on input type\/quality 2) disabled - no filtering will be applied to the
-- input 3) forced - filtering will be applied regardless of input type
inputSettings_inputFilter :: Lens.Lens' InputSettings (Prelude.Maybe InputFilter)
inputSettings_inputFilter = Lens.lens (\InputSettings' {inputFilter} -> inputFilter) (\s@InputSettings' {} a -> s {inputFilter = a} :: InputSettings)

-- | Enable or disable the deblock filter when filtering.
inputSettings_deblockFilter :: Lens.Lens' InputSettings (Prelude.Maybe InputDeblockFilter)
inputSettings_deblockFilter = Lens.lens (\InputSettings' {deblockFilter} -> deblockFilter) (\s@InputSettings' {} a -> s {deblockFilter = a} :: InputSettings)

-- | Used to select the caption input to use for inputs that have multiple
-- available.
inputSettings_captionSelectors :: Lens.Lens' InputSettings (Prelude.Maybe [CaptionSelector])
inputSettings_captionSelectors = Lens.lens (\InputSettings' {captionSelectors} -> captionSelectors) (\s@InputSettings' {} a -> s {captionSelectors = a} :: InputSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | Input settings.
inputSettings_networkInputSettings :: Lens.Lens' InputSettings (Prelude.Maybe NetworkInputSettings)
inputSettings_networkInputSettings = Lens.lens (\InputSettings' {networkInputSettings} -> networkInputSettings) (\s@InputSettings' {} a -> s {networkInputSettings = a} :: InputSettings)

instance Prelude.FromJSON InputSettings where
  parseJSON =
    Prelude.withObject
      "InputSettings"
      ( \x ->
          InputSettings'
            Prelude.<$> (x Prelude..:? "denoiseFilter")
            Prelude.<*> ( x Prelude..:? "audioSelectors"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "filterStrength")
            Prelude.<*> (x Prelude..:? "smpte2038DataPreference")
            Prelude.<*> (x Prelude..:? "videoSelector")
            Prelude.<*> (x Prelude..:? "sourceEndBehavior")
            Prelude.<*> (x Prelude..:? "inputFilter")
            Prelude.<*> (x Prelude..:? "deblockFilter")
            Prelude.<*> ( x Prelude..:? "captionSelectors"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "networkInputSettings")
      )

instance Prelude.Hashable InputSettings

instance Prelude.NFData InputSettings

instance Prelude.ToJSON InputSettings where
  toJSON InputSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("denoiseFilter" Prelude..=)
              Prelude.<$> denoiseFilter,
            ("audioSelectors" Prelude..=)
              Prelude.<$> audioSelectors,
            ("filterStrength" Prelude..=)
              Prelude.<$> filterStrength,
            ("smpte2038DataPreference" Prelude..=)
              Prelude.<$> smpte2038DataPreference,
            ("videoSelector" Prelude..=)
              Prelude.<$> videoSelector,
            ("sourceEndBehavior" Prelude..=)
              Prelude.<$> sourceEndBehavior,
            ("inputFilter" Prelude..=) Prelude.<$> inputFilter,
            ("deblockFilter" Prelude..=)
              Prelude.<$> deblockFilter,
            ("captionSelectors" Prelude..=)
              Prelude.<$> captionSelectors,
            ("networkInputSettings" Prelude..=)
              Prelude.<$> networkInputSettings
          ]
      )
