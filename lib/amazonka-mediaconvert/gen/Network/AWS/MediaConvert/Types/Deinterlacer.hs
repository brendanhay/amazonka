{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Deinterlacer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Deinterlacer
  ( Deinterlacer (..)
  -- * Smart constructor
  , mkDeinterlacer
  -- * Lenses
  , dAlgorithm
  , dControl
  , dMode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.DeinterlaceAlgorithm as Types
import qualified Network.AWS.MediaConvert.Types.DeinterlacerControl as Types
import qualified Network.AWS.MediaConvert.Types.DeinterlacerMode as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for deinterlacer
--
-- /See:/ 'mkDeinterlacer' smart constructor.
data Deinterlacer = Deinterlacer'
  { algorithm :: Core.Maybe Types.DeinterlaceAlgorithm
    -- ^ Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your source file includes a ticker, such as a scrolling headline at the bottom of the frame.
  , control :: Core.Maybe Types.DeinterlacerControl
    -- ^ - When set to NORMAL (default), the deinterlacer does not convert frames that are tagged  in metadata as progressive. It will only convert those that are tagged as some other type. - When set to FORCE_ALL_FRAMES, the deinterlacer converts every frame to progressive - even those that are already tagged as progressive. Turn Force mode on only if there is  a good chance that the metadata has tagged frames as progressive when they are not  progressive. Do not turn on otherwise; processing frames that are already progressive  into progressive will probably result in lower quality video.
  , mode :: Core.Maybe Types.DeinterlacerMode
    -- ^ Use Deinterlacer (DeinterlaceMode) to choose how the service will do deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced to progressive. - Inverse telecine converts Hard Telecine 29.97i to progressive 23.976p. - Adaptive auto-detects and converts to progressive.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Deinterlacer' value with any optional fields omitted.
mkDeinterlacer
    :: Deinterlacer
mkDeinterlacer
  = Deinterlacer'{algorithm = Core.Nothing, control = Core.Nothing,
                  mode = Core.Nothing}

-- | Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your source file includes a ticker, such as a scrolling headline at the bottom of the frame.
--
-- /Note:/ Consider using 'algorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAlgorithm :: Lens.Lens' Deinterlacer (Core.Maybe Types.DeinterlaceAlgorithm)
dAlgorithm = Lens.field @"algorithm"
{-# INLINEABLE dAlgorithm #-}
{-# DEPRECATED algorithm "Use generic-lens or generic-optics with 'algorithm' instead"  #-}

-- | - When set to NORMAL (default), the deinterlacer does not convert frames that are tagged  in metadata as progressive. It will only convert those that are tagged as some other type. - When set to FORCE_ALL_FRAMES, the deinterlacer converts every frame to progressive - even those that are already tagged as progressive. Turn Force mode on only if there is  a good chance that the metadata has tagged frames as progressive when they are not  progressive. Do not turn on otherwise; processing frames that are already progressive  into progressive will probably result in lower quality video.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dControl :: Lens.Lens' Deinterlacer (Core.Maybe Types.DeinterlacerControl)
dControl = Lens.field @"control"
{-# INLINEABLE dControl #-}
{-# DEPRECATED control "Use generic-lens or generic-optics with 'control' instead"  #-}

-- | Use Deinterlacer (DeinterlaceMode) to choose how the service will do deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced to progressive. - Inverse telecine converts Hard Telecine 29.97i to progressive 23.976p. - Adaptive auto-detects and converts to progressive.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMode :: Lens.Lens' Deinterlacer (Core.Maybe Types.DeinterlacerMode)
dMode = Lens.field @"mode"
{-# INLINEABLE dMode #-}
{-# DEPRECATED mode "Use generic-lens or generic-optics with 'mode' instead"  #-}

instance Core.FromJSON Deinterlacer where
        toJSON Deinterlacer{..}
          = Core.object
              (Core.catMaybes
                 [("algorithm" Core..=) Core.<$> algorithm,
                  ("control" Core..=) Core.<$> control,
                  ("mode" Core..=) Core.<$> mode])

instance Core.FromJSON Deinterlacer where
        parseJSON
          = Core.withObject "Deinterlacer" Core.$
              \ x ->
                Deinterlacer' Core.<$>
                  (x Core..:? "algorithm") Core.<*> x Core..:? "control" Core.<*>
                    x Core..:? "mode"
