{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSettings
  ( InputSettings (..),

    -- * Smart constructor
    mkInputSettings,

    -- * Lenses
    isVideoSelector,
    isSmpte2038DataPreference,
    isNetworkInputSettings,
    isAudioSelectors,
    isDeblockFilter,
    isDenoiseFilter,
    isFilterStrength,
    isCaptionSelectors,
    isInputFilter,
    isSourceEndBehavior,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | Live Event input parameters. There can be multiple inputs in a single Live Event.
--
-- /See:/ 'mkInputSettings' smart constructor.
data InputSettings = InputSettings'
  { -- | Informs which video elementary stream to decode for input types that have multiple available.
    videoSelector :: Lude.Maybe VideoSelector,
    -- | Specifies whether to extract applicable ancillary data from a SMPTE-2038 source in this input. Applicable data types are captions, timecode, AFD, and SCTE-104 messages.
    --
    -- - PREFER: Extract from SMPTE-2038 if present in this input, otherwise extract from another source (if any).
    -- - IGNORE: Never extract any ancillary data from SMPTE-2038.
    smpte2038DataPreference :: Lude.Maybe Smpte2038DataPreference,
    -- | Input settings.
    networkInputSettings :: Lude.Maybe NetworkInputSettings,
    -- | Used to select the audio stream to decode for inputs that have multiple available.
    audioSelectors :: Lude.Maybe [AudioSelector],
    -- | Enable or disable the deblock filter when filtering.
    deblockFilter :: Lude.Maybe InputDeblockFilter,
    -- | Enable or disable the denoise filter when filtering.
    denoiseFilter :: Lude.Maybe InputDenoiseFilter,
    -- | Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
    filterStrength :: Lude.Maybe Lude.Natural,
    -- | Used to select the caption input to use for inputs that have multiple available.
    captionSelectors :: Lude.Maybe [CaptionSelector],
    -- | Turns on the filter for this input. MPEG-2 inputs have the deblocking filter enabled by default.
    --
    -- 1) auto - filtering will be applied depending on input type/quality
    -- 2) disabled - no filtering will be applied to the input
    -- 3) forced - filtering will be applied regardless of input type
    inputFilter :: Lude.Maybe InputFilter,
    -- | Loop input if it is a file. This allows a file input to be streamed indefinitely.
    sourceEndBehavior :: Lude.Maybe InputSourceEndBehavior
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputSettings' with the minimum fields required to make a request.
--
-- * 'videoSelector' - Informs which video elementary stream to decode for input types that have multiple available.
-- * 'smpte2038DataPreference' - Specifies whether to extract applicable ancillary data from a SMPTE-2038 source in this input. Applicable data types are captions, timecode, AFD, and SCTE-104 messages.
--
-- - PREFER: Extract from SMPTE-2038 if present in this input, otherwise extract from another source (if any).
-- - IGNORE: Never extract any ancillary data from SMPTE-2038.
-- * 'networkInputSettings' - Input settings.
-- * 'audioSelectors' - Used to select the audio stream to decode for inputs that have multiple available.
-- * 'deblockFilter' - Enable or disable the deblock filter when filtering.
-- * 'denoiseFilter' - Enable or disable the denoise filter when filtering.
-- * 'filterStrength' - Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
-- * 'captionSelectors' - Used to select the caption input to use for inputs that have multiple available.
-- * 'inputFilter' - Turns on the filter for this input. MPEG-2 inputs have the deblocking filter enabled by default.
--
-- 1) auto - filtering will be applied depending on input type/quality
-- 2) disabled - no filtering will be applied to the input
-- 3) forced - filtering will be applied regardless of input type
-- * 'sourceEndBehavior' - Loop input if it is a file. This allows a file input to be streamed indefinitely.
mkInputSettings ::
  InputSettings
mkInputSettings =
  InputSettings'
    { videoSelector = Lude.Nothing,
      smpte2038DataPreference = Lude.Nothing,
      networkInputSettings = Lude.Nothing,
      audioSelectors = Lude.Nothing,
      deblockFilter = Lude.Nothing,
      denoiseFilter = Lude.Nothing,
      filterStrength = Lude.Nothing,
      captionSelectors = Lude.Nothing,
      inputFilter = Lude.Nothing,
      sourceEndBehavior = Lude.Nothing
    }

-- | Informs which video elementary stream to decode for input types that have multiple available.
--
-- /Note:/ Consider using 'videoSelector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isVideoSelector :: Lens.Lens' InputSettings (Lude.Maybe VideoSelector)
isVideoSelector = Lens.lens (videoSelector :: InputSettings -> Lude.Maybe VideoSelector) (\s a -> s {videoSelector = a} :: InputSettings)
{-# DEPRECATED isVideoSelector "Use generic-lens or generic-optics with 'videoSelector' instead." #-}

-- | Specifies whether to extract applicable ancillary data from a SMPTE-2038 source in this input. Applicable data types are captions, timecode, AFD, and SCTE-104 messages.
--
-- - PREFER: Extract from SMPTE-2038 if present in this input, otherwise extract from another source (if any).
-- - IGNORE: Never extract any ancillary data from SMPTE-2038.
--
-- /Note:/ Consider using 'smpte2038DataPreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSmpte2038DataPreference :: Lens.Lens' InputSettings (Lude.Maybe Smpte2038DataPreference)
isSmpte2038DataPreference = Lens.lens (smpte2038DataPreference :: InputSettings -> Lude.Maybe Smpte2038DataPreference) (\s a -> s {smpte2038DataPreference = a} :: InputSettings)
{-# DEPRECATED isSmpte2038DataPreference "Use generic-lens or generic-optics with 'smpte2038DataPreference' instead." #-}

-- | Input settings.
--
-- /Note:/ Consider using 'networkInputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isNetworkInputSettings :: Lens.Lens' InputSettings (Lude.Maybe NetworkInputSettings)
isNetworkInputSettings = Lens.lens (networkInputSettings :: InputSettings -> Lude.Maybe NetworkInputSettings) (\s a -> s {networkInputSettings = a} :: InputSettings)
{-# DEPRECATED isNetworkInputSettings "Use generic-lens or generic-optics with 'networkInputSettings' instead." #-}

-- | Used to select the audio stream to decode for inputs that have multiple available.
--
-- /Note:/ Consider using 'audioSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isAudioSelectors :: Lens.Lens' InputSettings (Lude.Maybe [AudioSelector])
isAudioSelectors = Lens.lens (audioSelectors :: InputSettings -> Lude.Maybe [AudioSelector]) (\s a -> s {audioSelectors = a} :: InputSettings)
{-# DEPRECATED isAudioSelectors "Use generic-lens or generic-optics with 'audioSelectors' instead." #-}

-- | Enable or disable the deblock filter when filtering.
--
-- /Note:/ Consider using 'deblockFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDeblockFilter :: Lens.Lens' InputSettings (Lude.Maybe InputDeblockFilter)
isDeblockFilter = Lens.lens (deblockFilter :: InputSettings -> Lude.Maybe InputDeblockFilter) (\s a -> s {deblockFilter = a} :: InputSettings)
{-# DEPRECATED isDeblockFilter "Use generic-lens or generic-optics with 'deblockFilter' instead." #-}

-- | Enable or disable the denoise filter when filtering.
--
-- /Note:/ Consider using 'denoiseFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDenoiseFilter :: Lens.Lens' InputSettings (Lude.Maybe InputDenoiseFilter)
isDenoiseFilter = Lens.lens (denoiseFilter :: InputSettings -> Lude.Maybe InputDenoiseFilter) (\s a -> s {denoiseFilter = a} :: InputSettings)
{-# DEPRECATED isDenoiseFilter "Use generic-lens or generic-optics with 'denoiseFilter' instead." #-}

-- | Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
--
-- /Note:/ Consider using 'filterStrength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isFilterStrength :: Lens.Lens' InputSettings (Lude.Maybe Lude.Natural)
isFilterStrength = Lens.lens (filterStrength :: InputSettings -> Lude.Maybe Lude.Natural) (\s a -> s {filterStrength = a} :: InputSettings)
{-# DEPRECATED isFilterStrength "Use generic-lens or generic-optics with 'filterStrength' instead." #-}

-- | Used to select the caption input to use for inputs that have multiple available.
--
-- /Note:/ Consider using 'captionSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCaptionSelectors :: Lens.Lens' InputSettings (Lude.Maybe [CaptionSelector])
isCaptionSelectors = Lens.lens (captionSelectors :: InputSettings -> Lude.Maybe [CaptionSelector]) (\s a -> s {captionSelectors = a} :: InputSettings)
{-# DEPRECATED isCaptionSelectors "Use generic-lens or generic-optics with 'captionSelectors' instead." #-}

-- | Turns on the filter for this input. MPEG-2 inputs have the deblocking filter enabled by default.
--
-- 1) auto - filtering will be applied depending on input type/quality
-- 2) disabled - no filtering will be applied to the input
-- 3) forced - filtering will be applied regardless of input type
--
-- /Note:/ Consider using 'inputFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isInputFilter :: Lens.Lens' InputSettings (Lude.Maybe InputFilter)
isInputFilter = Lens.lens (inputFilter :: InputSettings -> Lude.Maybe InputFilter) (\s a -> s {inputFilter = a} :: InputSettings)
{-# DEPRECATED isInputFilter "Use generic-lens or generic-optics with 'inputFilter' instead." #-}

-- | Loop input if it is a file. This allows a file input to be streamed indefinitely.
--
-- /Note:/ Consider using 'sourceEndBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSourceEndBehavior :: Lens.Lens' InputSettings (Lude.Maybe InputSourceEndBehavior)
isSourceEndBehavior = Lens.lens (sourceEndBehavior :: InputSettings -> Lude.Maybe InputSourceEndBehavior) (\s a -> s {sourceEndBehavior = a} :: InputSettings)
{-# DEPRECATED isSourceEndBehavior "Use generic-lens or generic-optics with 'sourceEndBehavior' instead." #-}

instance Lude.FromJSON InputSettings where
  parseJSON =
    Lude.withObject
      "InputSettings"
      ( \x ->
          InputSettings'
            Lude.<$> (x Lude..:? "videoSelector")
            Lude.<*> (x Lude..:? "smpte2038DataPreference")
            Lude.<*> (x Lude..:? "networkInputSettings")
            Lude.<*> (x Lude..:? "audioSelectors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "deblockFilter")
            Lude.<*> (x Lude..:? "denoiseFilter")
            Lude.<*> (x Lude..:? "filterStrength")
            Lude.<*> (x Lude..:? "captionSelectors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "inputFilter")
            Lude.<*> (x Lude..:? "sourceEndBehavior")
      )

instance Lude.ToJSON InputSettings where
  toJSON InputSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("videoSelector" Lude..=) Lude.<$> videoSelector,
            ("smpte2038DataPreference" Lude..=)
              Lude.<$> smpte2038DataPreference,
            ("networkInputSettings" Lude..=) Lude.<$> networkInputSettings,
            ("audioSelectors" Lude..=) Lude.<$> audioSelectors,
            ("deblockFilter" Lude..=) Lude.<$> deblockFilter,
            ("denoiseFilter" Lude..=) Lude.<$> denoiseFilter,
            ("filterStrength" Lude..=) Lude.<$> filterStrength,
            ("captionSelectors" Lude..=) Lude.<$> captionSelectors,
            ("inputFilter" Lude..=) Lude.<$> inputFilter,
            ("sourceEndBehavior" Lude..=) Lude.<$> sourceEndBehavior
          ]
      )
