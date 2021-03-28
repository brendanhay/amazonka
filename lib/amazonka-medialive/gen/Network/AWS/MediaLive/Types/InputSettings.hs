{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputSettings
  ( InputSettings (..)
  -- * Smart constructor
  , mkInputSettings
  -- * Lenses
  , isAudioSelectors
  , isCaptionSelectors
  , isDeblockFilter
  , isDenoiseFilter
  , isFilterStrength
  , isInputFilter
  , isNetworkInputSettings
  , isSmpte2038DataPreference
  , isSourceEndBehavior
  , isVideoSelector
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AudioSelector as Types
import qualified Network.AWS.MediaLive.Types.CaptionSelector as Types
import qualified Network.AWS.MediaLive.Types.InputDeblockFilter as Types
import qualified Network.AWS.MediaLive.Types.InputDenoiseFilter as Types
import qualified Network.AWS.MediaLive.Types.InputFilter as Types
import qualified Network.AWS.MediaLive.Types.InputSourceEndBehavior as Types
import qualified Network.AWS.MediaLive.Types.NetworkInputSettings as Types
import qualified Network.AWS.MediaLive.Types.Smpte2038DataPreference as Types
import qualified Network.AWS.MediaLive.Types.VideoSelector as Types
import qualified Network.AWS.Prelude as Core

-- | Live Event input parameters. There can be multiple inputs in a single Live Event.
--
-- /See:/ 'mkInputSettings' smart constructor.
data InputSettings = InputSettings'
  { audioSelectors :: Core.Maybe [Types.AudioSelector]
    -- ^ Used to select the audio stream to decode for inputs that have multiple available.
  , captionSelectors :: Core.Maybe [Types.CaptionSelector]
    -- ^ Used to select the caption input to use for inputs that have multiple available.
  , deblockFilter :: Core.Maybe Types.InputDeblockFilter
    -- ^ Enable or disable the deblock filter when filtering.
  , denoiseFilter :: Core.Maybe Types.InputDenoiseFilter
    -- ^ Enable or disable the denoise filter when filtering.
  , filterStrength :: Core.Maybe Core.Natural
    -- ^ Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
  , inputFilter :: Core.Maybe Types.InputFilter
    -- ^ Turns on the filter for this input. MPEG-2 inputs have the deblocking filter enabled by default.
--
-- 1) auto - filtering will be applied depending on input type/quality
-- 2) disabled - no filtering will be applied to the input
-- 3) forced - filtering will be applied regardless of input type
  , networkInputSettings :: Core.Maybe Types.NetworkInputSettings
    -- ^ Input settings.
  , smpte2038DataPreference :: Core.Maybe Types.Smpte2038DataPreference
    -- ^ Specifies whether to extract applicable ancillary data from a SMPTE-2038 source in this input. Applicable data types are captions, timecode, AFD, and SCTE-104 messages.
--
-- - PREFER: Extract from SMPTE-2038 if present in this input, otherwise extract from another source (if any).
-- - IGNORE: Never extract any ancillary data from SMPTE-2038.
  , sourceEndBehavior :: Core.Maybe Types.InputSourceEndBehavior
    -- ^ Loop input if it is a file. This allows a file input to be streamed indefinitely.
  , videoSelector :: Core.Maybe Types.VideoSelector
    -- ^ Informs which video elementary stream to decode for input types that have multiple available.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputSettings' value with any optional fields omitted.
mkInputSettings
    :: InputSettings
mkInputSettings
  = InputSettings'{audioSelectors = Core.Nothing,
                   captionSelectors = Core.Nothing, deblockFilter = Core.Nothing,
                   denoiseFilter = Core.Nothing, filterStrength = Core.Nothing,
                   inputFilter = Core.Nothing, networkInputSettings = Core.Nothing,
                   smpte2038DataPreference = Core.Nothing,
                   sourceEndBehavior = Core.Nothing, videoSelector = Core.Nothing}

-- | Used to select the audio stream to decode for inputs that have multiple available.
--
-- /Note:/ Consider using 'audioSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isAudioSelectors :: Lens.Lens' InputSettings (Core.Maybe [Types.AudioSelector])
isAudioSelectors = Lens.field @"audioSelectors"
{-# INLINEABLE isAudioSelectors #-}
{-# DEPRECATED audioSelectors "Use generic-lens or generic-optics with 'audioSelectors' instead"  #-}

-- | Used to select the caption input to use for inputs that have multiple available.
--
-- /Note:/ Consider using 'captionSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCaptionSelectors :: Lens.Lens' InputSettings (Core.Maybe [Types.CaptionSelector])
isCaptionSelectors = Lens.field @"captionSelectors"
{-# INLINEABLE isCaptionSelectors #-}
{-# DEPRECATED captionSelectors "Use generic-lens or generic-optics with 'captionSelectors' instead"  #-}

-- | Enable or disable the deblock filter when filtering.
--
-- /Note:/ Consider using 'deblockFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDeblockFilter :: Lens.Lens' InputSettings (Core.Maybe Types.InputDeblockFilter)
isDeblockFilter = Lens.field @"deblockFilter"
{-# INLINEABLE isDeblockFilter #-}
{-# DEPRECATED deblockFilter "Use generic-lens or generic-optics with 'deblockFilter' instead"  #-}

-- | Enable or disable the denoise filter when filtering.
--
-- /Note:/ Consider using 'denoiseFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDenoiseFilter :: Lens.Lens' InputSettings (Core.Maybe Types.InputDenoiseFilter)
isDenoiseFilter = Lens.field @"denoiseFilter"
{-# INLINEABLE isDenoiseFilter #-}
{-# DEPRECATED denoiseFilter "Use generic-lens or generic-optics with 'denoiseFilter' instead"  #-}

-- | Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
--
-- /Note:/ Consider using 'filterStrength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isFilterStrength :: Lens.Lens' InputSettings (Core.Maybe Core.Natural)
isFilterStrength = Lens.field @"filterStrength"
{-# INLINEABLE isFilterStrength #-}
{-# DEPRECATED filterStrength "Use generic-lens or generic-optics with 'filterStrength' instead"  #-}

-- | Turns on the filter for this input. MPEG-2 inputs have the deblocking filter enabled by default.
--
-- 1) auto - filtering will be applied depending on input type/quality
-- 2) disabled - no filtering will be applied to the input
-- 3) forced - filtering will be applied regardless of input type
--
-- /Note:/ Consider using 'inputFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isInputFilter :: Lens.Lens' InputSettings (Core.Maybe Types.InputFilter)
isInputFilter = Lens.field @"inputFilter"
{-# INLINEABLE isInputFilter #-}
{-# DEPRECATED inputFilter "Use generic-lens or generic-optics with 'inputFilter' instead"  #-}

-- | Input settings.
--
-- /Note:/ Consider using 'networkInputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isNetworkInputSettings :: Lens.Lens' InputSettings (Core.Maybe Types.NetworkInputSettings)
isNetworkInputSettings = Lens.field @"networkInputSettings"
{-# INLINEABLE isNetworkInputSettings #-}
{-# DEPRECATED networkInputSettings "Use generic-lens or generic-optics with 'networkInputSettings' instead"  #-}

-- | Specifies whether to extract applicable ancillary data from a SMPTE-2038 source in this input. Applicable data types are captions, timecode, AFD, and SCTE-104 messages.
--
-- - PREFER: Extract from SMPTE-2038 if present in this input, otherwise extract from another source (if any).
-- - IGNORE: Never extract any ancillary data from SMPTE-2038.
--
-- /Note:/ Consider using 'smpte2038DataPreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSmpte2038DataPreference :: Lens.Lens' InputSettings (Core.Maybe Types.Smpte2038DataPreference)
isSmpte2038DataPreference = Lens.field @"smpte2038DataPreference"
{-# INLINEABLE isSmpte2038DataPreference #-}
{-# DEPRECATED smpte2038DataPreference "Use generic-lens or generic-optics with 'smpte2038DataPreference' instead"  #-}

-- | Loop input if it is a file. This allows a file input to be streamed indefinitely.
--
-- /Note:/ Consider using 'sourceEndBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSourceEndBehavior :: Lens.Lens' InputSettings (Core.Maybe Types.InputSourceEndBehavior)
isSourceEndBehavior = Lens.field @"sourceEndBehavior"
{-# INLINEABLE isSourceEndBehavior #-}
{-# DEPRECATED sourceEndBehavior "Use generic-lens or generic-optics with 'sourceEndBehavior' instead"  #-}

-- | Informs which video elementary stream to decode for input types that have multiple available.
--
-- /Note:/ Consider using 'videoSelector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isVideoSelector :: Lens.Lens' InputSettings (Core.Maybe Types.VideoSelector)
isVideoSelector = Lens.field @"videoSelector"
{-# INLINEABLE isVideoSelector #-}
{-# DEPRECATED videoSelector "Use generic-lens or generic-optics with 'videoSelector' instead"  #-}

instance Core.FromJSON InputSettings where
        toJSON InputSettings{..}
          = Core.object
              (Core.catMaybes
                 [("audioSelectors" Core..=) Core.<$> audioSelectors,
                  ("captionSelectors" Core..=) Core.<$> captionSelectors,
                  ("deblockFilter" Core..=) Core.<$> deblockFilter,
                  ("denoiseFilter" Core..=) Core.<$> denoiseFilter,
                  ("filterStrength" Core..=) Core.<$> filterStrength,
                  ("inputFilter" Core..=) Core.<$> inputFilter,
                  ("networkInputSettings" Core..=) Core.<$> networkInputSettings,
                  ("smpte2038DataPreference" Core..=) Core.<$>
                    smpte2038DataPreference,
                  ("sourceEndBehavior" Core..=) Core.<$> sourceEndBehavior,
                  ("videoSelector" Core..=) Core.<$> videoSelector])

instance Core.FromJSON InputSettings where
        parseJSON
          = Core.withObject "InputSettings" Core.$
              \ x ->
                InputSettings' Core.<$>
                  (x Core..:? "audioSelectors") Core.<*>
                    x Core..:? "captionSelectors"
                    Core.<*> x Core..:? "deblockFilter"
                    Core.<*> x Core..:? "denoiseFilter"
                    Core.<*> x Core..:? "filterStrength"
                    Core.<*> x Core..:? "inputFilter"
                    Core.<*> x Core..:? "networkInputSettings"
                    Core.<*> x Core..:? "smpte2038DataPreference"
                    Core.<*> x Core..:? "sourceEndBehavior"
                    Core.<*> x Core..:? "videoSelector"
