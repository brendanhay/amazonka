-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Deinterlacer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Deinterlacer
  ( Deinterlacer (..),

    -- * Smart constructor
    mkDeinterlacer,

    -- * Lenses
    dControl,
    dMode,
    dAlgorithm,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DeinterlaceAlgorithm
import Network.AWS.MediaConvert.Types.DeinterlacerControl
import Network.AWS.MediaConvert.Types.DeinterlacerMode
import qualified Network.AWS.Prelude as Lude

-- | Settings for deinterlacer
--
-- /See:/ 'mkDeinterlacer' smart constructor.
data Deinterlacer = Deinterlacer'
  { control ::
      Lude.Maybe DeinterlacerControl,
    mode :: Lude.Maybe DeinterlacerMode,
    algorithm :: Lude.Maybe DeinterlaceAlgorithm
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Deinterlacer' with the minimum fields required to make a request.
--
-- * 'algorithm' - Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your source file includes a ticker, such as a scrolling headline at the bottom of the frame.
-- * 'control' - - When set to NORMAL (default), the deinterlacer does not convert frames that are tagged  in metadata as progressive. It will only convert those that are tagged as some other type. - When set to FORCE_ALL_FRAMES, the deinterlacer converts every frame to progressive - even those that are already tagged as progressive. Turn Force mode on only if there is  a good chance that the metadata has tagged frames as progressive when they are not  progressive. Do not turn on otherwise; processing frames that are already progressive  into progressive will probably result in lower quality video.
-- * 'mode' - Use Deinterlacer (DeinterlaceMode) to choose how the service will do deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced to progressive. - Inverse telecine converts Hard Telecine 29.97i to progressive 23.976p. - Adaptive auto-detects and converts to progressive.
mkDeinterlacer ::
  Deinterlacer
mkDeinterlacer =
  Deinterlacer'
    { control = Lude.Nothing,
      mode = Lude.Nothing,
      algorithm = Lude.Nothing
    }

-- | - When set to NORMAL (default), the deinterlacer does not convert frames that are tagged  in metadata as progressive. It will only convert those that are tagged as some other type. - When set to FORCE_ALL_FRAMES, the deinterlacer converts every frame to progressive - even those that are already tagged as progressive. Turn Force mode on only if there is  a good chance that the metadata has tagged frames as progressive when they are not  progressive. Do not turn on otherwise; processing frames that are already progressive  into progressive will probably result in lower quality video.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dControl :: Lens.Lens' Deinterlacer (Lude.Maybe DeinterlacerControl)
dControl = Lens.lens (control :: Deinterlacer -> Lude.Maybe DeinterlacerControl) (\s a -> s {control = a} :: Deinterlacer)
{-# DEPRECATED dControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | Use Deinterlacer (DeinterlaceMode) to choose how the service will do deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced to progressive. - Inverse telecine converts Hard Telecine 29.97i to progressive 23.976p. - Adaptive auto-detects and converts to progressive.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMode :: Lens.Lens' Deinterlacer (Lude.Maybe DeinterlacerMode)
dMode = Lens.lens (mode :: Deinterlacer -> Lude.Maybe DeinterlacerMode) (\s a -> s {mode = a} :: Deinterlacer)
{-# DEPRECATED dMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your source file includes a ticker, such as a scrolling headline at the bottom of the frame.
--
-- /Note:/ Consider using 'algorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAlgorithm :: Lens.Lens' Deinterlacer (Lude.Maybe DeinterlaceAlgorithm)
dAlgorithm = Lens.lens (algorithm :: Deinterlacer -> Lude.Maybe DeinterlaceAlgorithm) (\s a -> s {algorithm = a} :: Deinterlacer)
{-# DEPRECATED dAlgorithm "Use generic-lens or generic-optics with 'algorithm' instead." #-}

instance Lude.FromJSON Deinterlacer where
  parseJSON =
    Lude.withObject
      "Deinterlacer"
      ( \x ->
          Deinterlacer'
            Lude.<$> (x Lude..:? "control")
            Lude.<*> (x Lude..:? "mode")
            Lude.<*> (x Lude..:? "algorithm")
      )

instance Lude.ToJSON Deinterlacer where
  toJSON Deinterlacer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("control" Lude..=) Lude.<$> control,
            ("mode" Lude..=) Lude.<$> mode,
            ("algorithm" Lude..=) Lude.<$> algorithm
          ]
      )
