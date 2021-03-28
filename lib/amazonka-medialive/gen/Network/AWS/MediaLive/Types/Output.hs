{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Output
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Output
  ( Output (..)
  -- * Smart constructor
  , mkOutput
  -- * Lenses
  , oOutputSettings
  , oAudioDescriptionNames
  , oCaptionDescriptionNames
  , oOutputName
  , oVideoDescriptionName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.OutputSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Output settings. There can be multiple outputs within a group.
--
-- /See:/ 'mkOutput' smart constructor.
data Output = Output'
  { outputSettings :: Types.OutputSettings
    -- ^ Output type-specific settings.
  , audioDescriptionNames :: Core.Maybe [Core.Text]
    -- ^ The names of the AudioDescriptions used as audio sources for this output.
  , captionDescriptionNames :: Core.Maybe [Core.Text]
    -- ^ The names of the CaptionDescriptions used as caption sources for this output.
  , outputName :: Core.Maybe Core.Text
    -- ^ The name used to identify an output.
  , videoDescriptionName :: Core.Maybe Core.Text
    -- ^ The name of the VideoDescription used as the source for this output.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Output' value with any optional fields omitted.
mkOutput
    :: Types.OutputSettings -- ^ 'outputSettings'
    -> Output
mkOutput outputSettings
  = Output'{outputSettings, audioDescriptionNames = Core.Nothing,
            captionDescriptionNames = Core.Nothing, outputName = Core.Nothing,
            videoDescriptionName = Core.Nothing}

-- | Output type-specific settings.
--
-- /Note:/ Consider using 'outputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOutputSettings :: Lens.Lens' Output Types.OutputSettings
oOutputSettings = Lens.field @"outputSettings"
{-# INLINEABLE oOutputSettings #-}
{-# DEPRECATED outputSettings "Use generic-lens or generic-optics with 'outputSettings' instead"  #-}

-- | The names of the AudioDescriptions used as audio sources for this output.
--
-- /Note:/ Consider using 'audioDescriptionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oAudioDescriptionNames :: Lens.Lens' Output (Core.Maybe [Core.Text])
oAudioDescriptionNames = Lens.field @"audioDescriptionNames"
{-# INLINEABLE oAudioDescriptionNames #-}
{-# DEPRECATED audioDescriptionNames "Use generic-lens or generic-optics with 'audioDescriptionNames' instead"  #-}

-- | The names of the CaptionDescriptions used as caption sources for this output.
--
-- /Note:/ Consider using 'captionDescriptionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oCaptionDescriptionNames :: Lens.Lens' Output (Core.Maybe [Core.Text])
oCaptionDescriptionNames = Lens.field @"captionDescriptionNames"
{-# INLINEABLE oCaptionDescriptionNames #-}
{-# DEPRECATED captionDescriptionNames "Use generic-lens or generic-optics with 'captionDescriptionNames' instead"  #-}

-- | The name used to identify an output.
--
-- /Note:/ Consider using 'outputName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOutputName :: Lens.Lens' Output (Core.Maybe Core.Text)
oOutputName = Lens.field @"outputName"
{-# INLINEABLE oOutputName #-}
{-# DEPRECATED outputName "Use generic-lens or generic-optics with 'outputName' instead"  #-}

-- | The name of the VideoDescription used as the source for this output.
--
-- /Note:/ Consider using 'videoDescriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oVideoDescriptionName :: Lens.Lens' Output (Core.Maybe Core.Text)
oVideoDescriptionName = Lens.field @"videoDescriptionName"
{-# INLINEABLE oVideoDescriptionName #-}
{-# DEPRECATED videoDescriptionName "Use generic-lens or generic-optics with 'videoDescriptionName' instead"  #-}

instance Core.FromJSON Output where
        toJSON Output{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("outputSettings" Core..= outputSettings),
                  ("audioDescriptionNames" Core..=) Core.<$> audioDescriptionNames,
                  ("captionDescriptionNames" Core..=) Core.<$>
                    captionDescriptionNames,
                  ("outputName" Core..=) Core.<$> outputName,
                  ("videoDescriptionName" Core..=) Core.<$> videoDescriptionName])

instance Core.FromJSON Output where
        parseJSON
          = Core.withObject "Output" Core.$
              \ x ->
                Output' Core.<$>
                  (x Core..: "outputSettings") Core.<*>
                    x Core..:? "audioDescriptionNames"
                    Core.<*> x Core..:? "captionDescriptionNames"
                    Core.<*> x Core..:? "outputName"
                    Core.<*> x Core..:? "videoDescriptionName"
