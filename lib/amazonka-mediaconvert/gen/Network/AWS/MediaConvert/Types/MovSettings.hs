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
    msReference,
    msCslgAtom,
    msMpeg2FourCCControl,
    msPaddingControl,
    msClapAtom,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.MovClapAtom
import Network.AWS.MediaConvert.Types.MovCslgAtom
import Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl
import Network.AWS.MediaConvert.Types.MovPaddingControl
import Network.AWS.MediaConvert.Types.MovReference
import qualified Network.AWS.Prelude as Lude

-- | Settings for MOV Container.
--
-- /See:/ 'mkMovSettings' smart constructor.
data MovSettings = MovSettings'
  { -- | Always keep the default value (SELF_CONTAINED) for this setting.
    reference :: Lude.Maybe MovReference,
    -- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
    cslgAtom :: Lude.Maybe MovCslgAtom,
    -- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file using XDCAM fourcc codes. This increases compatibility with Apple editors and players, but may decrease compatibility with other players. Only applicable when the video codec is MPEG2.
    mpeg2FourCCControl :: Lude.Maybe MovMpeg2FourCCControl,
    -- | To make this output compatible with Omenon, keep the default value, OMNEON. Unless you need Omneon compatibility, set this value to NONE. When you keep the default value, OMNEON, MediaConvert increases the length of the edit list atom. This might cause file rejections when a recipient of the output file doesn't expct this extra padding.
    paddingControl :: Lude.Maybe MovPaddingControl,
    -- | When enabled, include 'clap' atom if appropriate for the video output settings.
    clapAtom :: Lude.Maybe MovClapAtom
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MovSettings' with the minimum fields required to make a request.
--
-- * 'reference' - Always keep the default value (SELF_CONTAINED) for this setting.
-- * 'cslgAtom' - When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
-- * 'mpeg2FourCCControl' - When set to XDCAM, writes MPEG2 video streams into the QuickTime file using XDCAM fourcc codes. This increases compatibility with Apple editors and players, but may decrease compatibility with other players. Only applicable when the video codec is MPEG2.
-- * 'paddingControl' - To make this output compatible with Omenon, keep the default value, OMNEON. Unless you need Omneon compatibility, set this value to NONE. When you keep the default value, OMNEON, MediaConvert increases the length of the edit list atom. This might cause file rejections when a recipient of the output file doesn't expct this extra padding.
-- * 'clapAtom' - When enabled, include 'clap' atom if appropriate for the video output settings.
mkMovSettings ::
  MovSettings
mkMovSettings =
  MovSettings'
    { reference = Lude.Nothing,
      cslgAtom = Lude.Nothing,
      mpeg2FourCCControl = Lude.Nothing,
      paddingControl = Lude.Nothing,
      clapAtom = Lude.Nothing
    }

-- | Always keep the default value (SELF_CONTAINED) for this setting.
--
-- /Note:/ Consider using 'reference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msReference :: Lens.Lens' MovSettings (Lude.Maybe MovReference)
msReference = Lens.lens (reference :: MovSettings -> Lude.Maybe MovReference) (\s a -> s {reference = a} :: MovSettings)
{-# DEPRECATED msReference "Use generic-lens or generic-optics with 'reference' instead." #-}

-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
--
-- /Note:/ Consider using 'cslgAtom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msCslgAtom :: Lens.Lens' MovSettings (Lude.Maybe MovCslgAtom)
msCslgAtom = Lens.lens (cslgAtom :: MovSettings -> Lude.Maybe MovCslgAtom) (\s a -> s {cslgAtom = a} :: MovSettings)
{-# DEPRECATED msCslgAtom "Use generic-lens or generic-optics with 'cslgAtom' instead." #-}

-- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file using XDCAM fourcc codes. This increases compatibility with Apple editors and players, but may decrease compatibility with other players. Only applicable when the video codec is MPEG2.
--
-- /Note:/ Consider using 'mpeg2FourCCControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msMpeg2FourCCControl :: Lens.Lens' MovSettings (Lude.Maybe MovMpeg2FourCCControl)
msMpeg2FourCCControl = Lens.lens (mpeg2FourCCControl :: MovSettings -> Lude.Maybe MovMpeg2FourCCControl) (\s a -> s {mpeg2FourCCControl = a} :: MovSettings)
{-# DEPRECATED msMpeg2FourCCControl "Use generic-lens or generic-optics with 'mpeg2FourCCControl' instead." #-}

-- | To make this output compatible with Omenon, keep the default value, OMNEON. Unless you need Omneon compatibility, set this value to NONE. When you keep the default value, OMNEON, MediaConvert increases the length of the edit list atom. This might cause file rejections when a recipient of the output file doesn't expct this extra padding.
--
-- /Note:/ Consider using 'paddingControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPaddingControl :: Lens.Lens' MovSettings (Lude.Maybe MovPaddingControl)
msPaddingControl = Lens.lens (paddingControl :: MovSettings -> Lude.Maybe MovPaddingControl) (\s a -> s {paddingControl = a} :: MovSettings)
{-# DEPRECATED msPaddingControl "Use generic-lens or generic-optics with 'paddingControl' instead." #-}

-- | When enabled, include 'clap' atom if appropriate for the video output settings.
--
-- /Note:/ Consider using 'clapAtom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msClapAtom :: Lens.Lens' MovSettings (Lude.Maybe MovClapAtom)
msClapAtom = Lens.lens (clapAtom :: MovSettings -> Lude.Maybe MovClapAtom) (\s a -> s {clapAtom = a} :: MovSettings)
{-# DEPRECATED msClapAtom "Use generic-lens or generic-optics with 'clapAtom' instead." #-}

instance Lude.FromJSON MovSettings where
  parseJSON =
    Lude.withObject
      "MovSettings"
      ( \x ->
          MovSettings'
            Lude.<$> (x Lude..:? "reference")
            Lude.<*> (x Lude..:? "cslgAtom")
            Lude.<*> (x Lude..:? "mpeg2FourCCControl")
            Lude.<*> (x Lude..:? "paddingControl")
            Lude.<*> (x Lude..:? "clapAtom")
      )

instance Lude.ToJSON MovSettings where
  toJSON MovSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("reference" Lude..=) Lude.<$> reference,
            ("cslgAtom" Lude..=) Lude.<$> cslgAtom,
            ("mpeg2FourCCControl" Lude..=) Lude.<$> mpeg2FourCCControl,
            ("paddingControl" Lude..=) Lude.<$> paddingControl,
            ("clapAtom" Lude..=) Lude.<$> clapAtom
          ]
      )
