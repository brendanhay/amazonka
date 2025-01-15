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
-- Module      : Amazonka.MediaLive.Types.InputSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.AudioSelector
import Amazonka.MediaLive.Types.CaptionSelector
import Amazonka.MediaLive.Types.InputDeblockFilter
import Amazonka.MediaLive.Types.InputDenoiseFilter
import Amazonka.MediaLive.Types.InputFilter
import Amazonka.MediaLive.Types.InputSourceEndBehavior
import Amazonka.MediaLive.Types.NetworkInputSettings
import Amazonka.MediaLive.Types.Smpte2038DataPreference
import Amazonka.MediaLive.Types.VideoSelector
import qualified Amazonka.Prelude as Prelude

-- | Live Event input parameters. There can be multiple inputs in a single
-- Live Event.
--
-- /See:/ 'newInputSettings' smart constructor.
data InputSettings = InputSettings'
  { -- | Used to select the audio stream to decode for inputs that have multiple
    -- available.
    audioSelectors :: Prelude.Maybe [AudioSelector],
    -- | Used to select the caption input to use for inputs that have multiple
    -- available.
    captionSelectors :: Prelude.Maybe [CaptionSelector],
    -- | Enable or disable the deblock filter when filtering.
    deblockFilter :: Prelude.Maybe InputDeblockFilter,
    -- | Enable or disable the denoise filter when filtering.
    denoiseFilter :: Prelude.Maybe InputDenoiseFilter,
    -- | Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
    filterStrength :: Prelude.Maybe Prelude.Natural,
    -- | Turns on the filter for this input. MPEG-2 inputs have the deblocking
    -- filter enabled by default. 1) auto - filtering will be applied depending
    -- on input type\/quality 2) disabled - no filtering will be applied to the
    -- input 3) forced - filtering will be applied regardless of input type
    inputFilter :: Prelude.Maybe InputFilter,
    -- | Input settings.
    networkInputSettings :: Prelude.Maybe NetworkInputSettings,
    -- | PID from which to read SCTE-35 messages. If left undefined, EML will
    -- select the first SCTE-35 PID found in the input.
    scte35Pid :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether to extract applicable ancillary data from a SMPTE-2038
    -- source in this input. Applicable data types are captions, timecode, AFD,
    -- and SCTE-104 messages. - PREFER: Extract from SMPTE-2038 if present in
    -- this input, otherwise extract from another source (if any). - IGNORE:
    -- Never extract any ancillary data from SMPTE-2038.
    smpte2038DataPreference :: Prelude.Maybe Smpte2038DataPreference,
    -- | Loop input if it is a file. This allows a file input to be streamed
    -- indefinitely.
    sourceEndBehavior :: Prelude.Maybe InputSourceEndBehavior,
    -- | Informs which video elementary stream to decode for input types that
    -- have multiple available.
    videoSelector :: Prelude.Maybe VideoSelector
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioSelectors', 'inputSettings_audioSelectors' - Used to select the audio stream to decode for inputs that have multiple
-- available.
--
-- 'captionSelectors', 'inputSettings_captionSelectors' - Used to select the caption input to use for inputs that have multiple
-- available.
--
-- 'deblockFilter', 'inputSettings_deblockFilter' - Enable or disable the deblock filter when filtering.
--
-- 'denoiseFilter', 'inputSettings_denoiseFilter' - Enable or disable the denoise filter when filtering.
--
-- 'filterStrength', 'inputSettings_filterStrength' - Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
--
-- 'inputFilter', 'inputSettings_inputFilter' - Turns on the filter for this input. MPEG-2 inputs have the deblocking
-- filter enabled by default. 1) auto - filtering will be applied depending
-- on input type\/quality 2) disabled - no filtering will be applied to the
-- input 3) forced - filtering will be applied regardless of input type
--
-- 'networkInputSettings', 'inputSettings_networkInputSettings' - Input settings.
--
-- 'scte35Pid', 'inputSettings_scte35Pid' - PID from which to read SCTE-35 messages. If left undefined, EML will
-- select the first SCTE-35 PID found in the input.
--
-- 'smpte2038DataPreference', 'inputSettings_smpte2038DataPreference' - Specifies whether to extract applicable ancillary data from a SMPTE-2038
-- source in this input. Applicable data types are captions, timecode, AFD,
-- and SCTE-104 messages. - PREFER: Extract from SMPTE-2038 if present in
-- this input, otherwise extract from another source (if any). - IGNORE:
-- Never extract any ancillary data from SMPTE-2038.
--
-- 'sourceEndBehavior', 'inputSettings_sourceEndBehavior' - Loop input if it is a file. This allows a file input to be streamed
-- indefinitely.
--
-- 'videoSelector', 'inputSettings_videoSelector' - Informs which video elementary stream to decode for input types that
-- have multiple available.
newInputSettings ::
  InputSettings
newInputSettings =
  InputSettings'
    { audioSelectors = Prelude.Nothing,
      captionSelectors = Prelude.Nothing,
      deblockFilter = Prelude.Nothing,
      denoiseFilter = Prelude.Nothing,
      filterStrength = Prelude.Nothing,
      inputFilter = Prelude.Nothing,
      networkInputSettings = Prelude.Nothing,
      scte35Pid = Prelude.Nothing,
      smpte2038DataPreference = Prelude.Nothing,
      sourceEndBehavior = Prelude.Nothing,
      videoSelector = Prelude.Nothing
    }

-- | Used to select the audio stream to decode for inputs that have multiple
-- available.
inputSettings_audioSelectors :: Lens.Lens' InputSettings (Prelude.Maybe [AudioSelector])
inputSettings_audioSelectors = Lens.lens (\InputSettings' {audioSelectors} -> audioSelectors) (\s@InputSettings' {} a -> s {audioSelectors = a} :: InputSettings) Prelude.. Lens.mapping Lens.coerced

-- | Used to select the caption input to use for inputs that have multiple
-- available.
inputSettings_captionSelectors :: Lens.Lens' InputSettings (Prelude.Maybe [CaptionSelector])
inputSettings_captionSelectors = Lens.lens (\InputSettings' {captionSelectors} -> captionSelectors) (\s@InputSettings' {} a -> s {captionSelectors = a} :: InputSettings) Prelude.. Lens.mapping Lens.coerced

-- | Enable or disable the deblock filter when filtering.
inputSettings_deblockFilter :: Lens.Lens' InputSettings (Prelude.Maybe InputDeblockFilter)
inputSettings_deblockFilter = Lens.lens (\InputSettings' {deblockFilter} -> deblockFilter) (\s@InputSettings' {} a -> s {deblockFilter = a} :: InputSettings)

-- | Enable or disable the denoise filter when filtering.
inputSettings_denoiseFilter :: Lens.Lens' InputSettings (Prelude.Maybe InputDenoiseFilter)
inputSettings_denoiseFilter = Lens.lens (\InputSettings' {denoiseFilter} -> denoiseFilter) (\s@InputSettings' {} a -> s {denoiseFilter = a} :: InputSettings)

-- | Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
inputSettings_filterStrength :: Lens.Lens' InputSettings (Prelude.Maybe Prelude.Natural)
inputSettings_filterStrength = Lens.lens (\InputSettings' {filterStrength} -> filterStrength) (\s@InputSettings' {} a -> s {filterStrength = a} :: InputSettings)

-- | Turns on the filter for this input. MPEG-2 inputs have the deblocking
-- filter enabled by default. 1) auto - filtering will be applied depending
-- on input type\/quality 2) disabled - no filtering will be applied to the
-- input 3) forced - filtering will be applied regardless of input type
inputSettings_inputFilter :: Lens.Lens' InputSettings (Prelude.Maybe InputFilter)
inputSettings_inputFilter = Lens.lens (\InputSettings' {inputFilter} -> inputFilter) (\s@InputSettings' {} a -> s {inputFilter = a} :: InputSettings)

-- | Input settings.
inputSettings_networkInputSettings :: Lens.Lens' InputSettings (Prelude.Maybe NetworkInputSettings)
inputSettings_networkInputSettings = Lens.lens (\InputSettings' {networkInputSettings} -> networkInputSettings) (\s@InputSettings' {} a -> s {networkInputSettings = a} :: InputSettings)

-- | PID from which to read SCTE-35 messages. If left undefined, EML will
-- select the first SCTE-35 PID found in the input.
inputSettings_scte35Pid :: Lens.Lens' InputSettings (Prelude.Maybe Prelude.Natural)
inputSettings_scte35Pid = Lens.lens (\InputSettings' {scte35Pid} -> scte35Pid) (\s@InputSettings' {} a -> s {scte35Pid = a} :: InputSettings)

-- | Specifies whether to extract applicable ancillary data from a SMPTE-2038
-- source in this input. Applicable data types are captions, timecode, AFD,
-- and SCTE-104 messages. - PREFER: Extract from SMPTE-2038 if present in
-- this input, otherwise extract from another source (if any). - IGNORE:
-- Never extract any ancillary data from SMPTE-2038.
inputSettings_smpte2038DataPreference :: Lens.Lens' InputSettings (Prelude.Maybe Smpte2038DataPreference)
inputSettings_smpte2038DataPreference = Lens.lens (\InputSettings' {smpte2038DataPreference} -> smpte2038DataPreference) (\s@InputSettings' {} a -> s {smpte2038DataPreference = a} :: InputSettings)

-- | Loop input if it is a file. This allows a file input to be streamed
-- indefinitely.
inputSettings_sourceEndBehavior :: Lens.Lens' InputSettings (Prelude.Maybe InputSourceEndBehavior)
inputSettings_sourceEndBehavior = Lens.lens (\InputSettings' {sourceEndBehavior} -> sourceEndBehavior) (\s@InputSettings' {} a -> s {sourceEndBehavior = a} :: InputSettings)

-- | Informs which video elementary stream to decode for input types that
-- have multiple available.
inputSettings_videoSelector :: Lens.Lens' InputSettings (Prelude.Maybe VideoSelector)
inputSettings_videoSelector = Lens.lens (\InputSettings' {videoSelector} -> videoSelector) (\s@InputSettings' {} a -> s {videoSelector = a} :: InputSettings)

instance Data.FromJSON InputSettings where
  parseJSON =
    Data.withObject
      "InputSettings"
      ( \x ->
          InputSettings'
            Prelude.<$> (x Data..:? "audioSelectors" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "captionSelectors"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "deblockFilter")
            Prelude.<*> (x Data..:? "denoiseFilter")
            Prelude.<*> (x Data..:? "filterStrength")
            Prelude.<*> (x Data..:? "inputFilter")
            Prelude.<*> (x Data..:? "networkInputSettings")
            Prelude.<*> (x Data..:? "scte35Pid")
            Prelude.<*> (x Data..:? "smpte2038DataPreference")
            Prelude.<*> (x Data..:? "sourceEndBehavior")
            Prelude.<*> (x Data..:? "videoSelector")
      )

instance Prelude.Hashable InputSettings where
  hashWithSalt _salt InputSettings' {..} =
    _salt
      `Prelude.hashWithSalt` audioSelectors
      `Prelude.hashWithSalt` captionSelectors
      `Prelude.hashWithSalt` deblockFilter
      `Prelude.hashWithSalt` denoiseFilter
      `Prelude.hashWithSalt` filterStrength
      `Prelude.hashWithSalt` inputFilter
      `Prelude.hashWithSalt` networkInputSettings
      `Prelude.hashWithSalt` scte35Pid
      `Prelude.hashWithSalt` smpte2038DataPreference
      `Prelude.hashWithSalt` sourceEndBehavior
      `Prelude.hashWithSalt` videoSelector

instance Prelude.NFData InputSettings where
  rnf InputSettings' {..} =
    Prelude.rnf audioSelectors `Prelude.seq`
      Prelude.rnf captionSelectors `Prelude.seq`
        Prelude.rnf deblockFilter `Prelude.seq`
          Prelude.rnf denoiseFilter `Prelude.seq`
            Prelude.rnf filterStrength `Prelude.seq`
              Prelude.rnf inputFilter `Prelude.seq`
                Prelude.rnf networkInputSettings `Prelude.seq`
                  Prelude.rnf scte35Pid `Prelude.seq`
                    Prelude.rnf smpte2038DataPreference `Prelude.seq`
                      Prelude.rnf sourceEndBehavior `Prelude.seq`
                        Prelude.rnf videoSelector

instance Data.ToJSON InputSettings where
  toJSON InputSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audioSelectors" Data..=)
              Prelude.<$> audioSelectors,
            ("captionSelectors" Data..=)
              Prelude.<$> captionSelectors,
            ("deblockFilter" Data..=) Prelude.<$> deblockFilter,
            ("denoiseFilter" Data..=) Prelude.<$> denoiseFilter,
            ("filterStrength" Data..=)
              Prelude.<$> filterStrength,
            ("inputFilter" Data..=) Prelude.<$> inputFilter,
            ("networkInputSettings" Data..=)
              Prelude.<$> networkInputSettings,
            ("scte35Pid" Data..=) Prelude.<$> scte35Pid,
            ("smpte2038DataPreference" Data..=)
              Prelude.<$> smpte2038DataPreference,
            ("sourceEndBehavior" Data..=)
              Prelude.<$> sourceEndBehavior,
            ("videoSelector" Data..=) Prelude.<$> videoSelector
          ]
      )
