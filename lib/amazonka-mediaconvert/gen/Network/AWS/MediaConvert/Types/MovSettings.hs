{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MovSettings
  ( MovSettings (..),

    -- * Smart constructor
    mkMovSettings,

    -- * Lenses
    msClapAtom,
    msCslgAtom,
    msMpeg2FourCCControl,
    msPaddingControl,
    msReference,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.MovClapAtom as Types
import qualified Network.AWS.MediaConvert.Types.MovCslgAtom as Types
import qualified Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl as Types
import qualified Network.AWS.MediaConvert.Types.MovPaddingControl as Types
import qualified Network.AWS.MediaConvert.Types.MovReference as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for MOV Container.
--
-- /See:/ 'mkMovSettings' smart constructor.
data MovSettings = MovSettings'
  { -- | When enabled, include 'clap' atom if appropriate for the video output settings.
    clapAtom :: Core.Maybe Types.MovClapAtom,
    -- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
    cslgAtom :: Core.Maybe Types.MovCslgAtom,
    -- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file using XDCAM fourcc codes. This increases compatibility with Apple editors and players, but may decrease compatibility with other players. Only applicable when the video codec is MPEG2.
    mpeg2FourCCControl :: Core.Maybe Types.MovMpeg2FourCCControl,
    -- | To make this output compatible with Omenon, keep the default value, OMNEON. Unless you need Omneon compatibility, set this value to NONE. When you keep the default value, OMNEON, MediaConvert increases the length of the edit list atom. This might cause file rejections when a recipient of the output file doesn't expct this extra padding.
    paddingControl :: Core.Maybe Types.MovPaddingControl,
    -- | Always keep the default value (SELF_CONTAINED) for this setting.
    reference :: Core.Maybe Types.MovReference
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MovSettings' value with any optional fields omitted.
mkMovSettings ::
  MovSettings
mkMovSettings =
  MovSettings'
    { clapAtom = Core.Nothing,
      cslgAtom = Core.Nothing,
      mpeg2FourCCControl = Core.Nothing,
      paddingControl = Core.Nothing,
      reference = Core.Nothing
    }

-- | When enabled, include 'clap' atom if appropriate for the video output settings.
--
-- /Note:/ Consider using 'clapAtom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msClapAtom :: Lens.Lens' MovSettings (Core.Maybe Types.MovClapAtom)
msClapAtom = Lens.field @"clapAtom"
{-# DEPRECATED msClapAtom "Use generic-lens or generic-optics with 'clapAtom' instead." #-}

-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
--
-- /Note:/ Consider using 'cslgAtom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msCslgAtom :: Lens.Lens' MovSettings (Core.Maybe Types.MovCslgAtom)
msCslgAtom = Lens.field @"cslgAtom"
{-# DEPRECATED msCslgAtom "Use generic-lens or generic-optics with 'cslgAtom' instead." #-}

-- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file using XDCAM fourcc codes. This increases compatibility with Apple editors and players, but may decrease compatibility with other players. Only applicable when the video codec is MPEG2.
--
-- /Note:/ Consider using 'mpeg2FourCCControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msMpeg2FourCCControl :: Lens.Lens' MovSettings (Core.Maybe Types.MovMpeg2FourCCControl)
msMpeg2FourCCControl = Lens.field @"mpeg2FourCCControl"
{-# DEPRECATED msMpeg2FourCCControl "Use generic-lens or generic-optics with 'mpeg2FourCCControl' instead." #-}

-- | To make this output compatible with Omenon, keep the default value, OMNEON. Unless you need Omneon compatibility, set this value to NONE. When you keep the default value, OMNEON, MediaConvert increases the length of the edit list atom. This might cause file rejections when a recipient of the output file doesn't expct this extra padding.
--
-- /Note:/ Consider using 'paddingControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPaddingControl :: Lens.Lens' MovSettings (Core.Maybe Types.MovPaddingControl)
msPaddingControl = Lens.field @"paddingControl"
{-# DEPRECATED msPaddingControl "Use generic-lens or generic-optics with 'paddingControl' instead." #-}

-- | Always keep the default value (SELF_CONTAINED) for this setting.
--
-- /Note:/ Consider using 'reference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msReference :: Lens.Lens' MovSettings (Core.Maybe Types.MovReference)
msReference = Lens.field @"reference"
{-# DEPRECATED msReference "Use generic-lens or generic-optics with 'reference' instead." #-}

instance Core.FromJSON MovSettings where
  toJSON MovSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("clapAtom" Core..=) Core.<$> clapAtom,
            ("cslgAtom" Core..=) Core.<$> cslgAtom,
            ("mpeg2FourCCControl" Core..=) Core.<$> mpeg2FourCCControl,
            ("paddingControl" Core..=) Core.<$> paddingControl,
            ("reference" Core..=) Core.<$> reference
          ]
      )

instance Core.FromJSON MovSettings where
  parseJSON =
    Core.withObject "MovSettings" Core.$
      \x ->
        MovSettings'
          Core.<$> (x Core..:? "clapAtom")
          Core.<*> (x Core..:? "cslgAtom")
          Core.<*> (x Core..:? "mpeg2FourCCControl")
          Core.<*> (x Core..:? "paddingControl")
          Core.<*> (x Core..:? "reference")
