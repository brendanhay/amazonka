{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoPreprocessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoPreprocessor
  ( VideoPreprocessor (..),

    -- * Smart constructor
    mkVideoPreprocessor,

    -- * Lenses
    vpTimecodeBurnin,
    vpDolbyVision,
    vpColorCorrector,
    vpDeinterlacer,
    vpNoiseReducer,
    vpImageInserter,
    vpPartnerWatermarking,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.ColorCorrector
import Network.AWS.MediaConvert.Types.Deinterlacer
import Network.AWS.MediaConvert.Types.DolbyVision
import Network.AWS.MediaConvert.Types.ImageInserter
import Network.AWS.MediaConvert.Types.NoiseReducer
import Network.AWS.MediaConvert.Types.PartnerWatermarking
import Network.AWS.MediaConvert.Types.TimecodeBurnin
import qualified Network.AWS.Prelude as Lude

-- | Find additional transcoding features under Preprocessors (VideoPreprocessors). Enable the features at each output individually. These features are disabled by default.
--
-- /See:/ 'mkVideoPreprocessor' smart constructor.
data VideoPreprocessor = VideoPreprocessor'
  { timecodeBurnin ::
      Lude.Maybe TimecodeBurnin,
    dolbyVision :: Lude.Maybe DolbyVision,
    colorCorrector :: Lude.Maybe ColorCorrector,
    deinterlacer :: Lude.Maybe Deinterlacer,
    noiseReducer :: Lude.Maybe NoiseReducer,
    imageInserter :: Lude.Maybe ImageInserter,
    partnerWatermarking :: Lude.Maybe PartnerWatermarking
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VideoPreprocessor' with the minimum fields required to make a request.
--
-- * 'colorCorrector' - Enable the Color corrector (ColorCorrector) feature if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
-- * 'deinterlacer' - Use Deinterlacer (Deinterlacer) to produce smoother motion and a clearer picture.
-- * 'dolbyVision' - Enable Dolby Vision feature to produce Dolby Vision compatible video output.
-- * 'imageInserter' - Enable the Image inserter (ImageInserter) feature to include a graphic overlay on your video. Enable or disable this feature for each output individually. This setting is disabled by default.
-- * 'noiseReducer' - Enable the Noise reducer (NoiseReducer) feature to remove noise from your video output if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
-- * 'partnerWatermarking' - If you work with a third party video watermarking partner, use the group of settings that correspond with your watermarking partner to include watermarks in your output.
-- * 'timecodeBurnin' - Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and specified prefix into the output.
mkVideoPreprocessor ::
  VideoPreprocessor
mkVideoPreprocessor =
  VideoPreprocessor'
    { timecodeBurnin = Lude.Nothing,
      dolbyVision = Lude.Nothing,
      colorCorrector = Lude.Nothing,
      deinterlacer = Lude.Nothing,
      noiseReducer = Lude.Nothing,
      imageInserter = Lude.Nothing,
      partnerWatermarking = Lude.Nothing
    }

-- | Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and specified prefix into the output.
--
-- /Note:/ Consider using 'timecodeBurnin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpTimecodeBurnin :: Lens.Lens' VideoPreprocessor (Lude.Maybe TimecodeBurnin)
vpTimecodeBurnin = Lens.lens (timecodeBurnin :: VideoPreprocessor -> Lude.Maybe TimecodeBurnin) (\s a -> s {timecodeBurnin = a} :: VideoPreprocessor)
{-# DEPRECATED vpTimecodeBurnin "Use generic-lens or generic-optics with 'timecodeBurnin' instead." #-}

-- | Enable Dolby Vision feature to produce Dolby Vision compatible video output.
--
-- /Note:/ Consider using 'dolbyVision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpDolbyVision :: Lens.Lens' VideoPreprocessor (Lude.Maybe DolbyVision)
vpDolbyVision = Lens.lens (dolbyVision :: VideoPreprocessor -> Lude.Maybe DolbyVision) (\s a -> s {dolbyVision = a} :: VideoPreprocessor)
{-# DEPRECATED vpDolbyVision "Use generic-lens or generic-optics with 'dolbyVision' instead." #-}

-- | Enable the Color corrector (ColorCorrector) feature if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
--
-- /Note:/ Consider using 'colorCorrector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpColorCorrector :: Lens.Lens' VideoPreprocessor (Lude.Maybe ColorCorrector)
vpColorCorrector = Lens.lens (colorCorrector :: VideoPreprocessor -> Lude.Maybe ColorCorrector) (\s a -> s {colorCorrector = a} :: VideoPreprocessor)
{-# DEPRECATED vpColorCorrector "Use generic-lens or generic-optics with 'colorCorrector' instead." #-}

-- | Use Deinterlacer (Deinterlacer) to produce smoother motion and a clearer picture.
--
-- /Note:/ Consider using 'deinterlacer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpDeinterlacer :: Lens.Lens' VideoPreprocessor (Lude.Maybe Deinterlacer)
vpDeinterlacer = Lens.lens (deinterlacer :: VideoPreprocessor -> Lude.Maybe Deinterlacer) (\s a -> s {deinterlacer = a} :: VideoPreprocessor)
{-# DEPRECATED vpDeinterlacer "Use generic-lens or generic-optics with 'deinterlacer' instead." #-}

-- | Enable the Noise reducer (NoiseReducer) feature to remove noise from your video output if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
--
-- /Note:/ Consider using 'noiseReducer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpNoiseReducer :: Lens.Lens' VideoPreprocessor (Lude.Maybe NoiseReducer)
vpNoiseReducer = Lens.lens (noiseReducer :: VideoPreprocessor -> Lude.Maybe NoiseReducer) (\s a -> s {noiseReducer = a} :: VideoPreprocessor)
{-# DEPRECATED vpNoiseReducer "Use generic-lens or generic-optics with 'noiseReducer' instead." #-}

-- | Enable the Image inserter (ImageInserter) feature to include a graphic overlay on your video. Enable or disable this feature for each output individually. This setting is disabled by default.
--
-- /Note:/ Consider using 'imageInserter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpImageInserter :: Lens.Lens' VideoPreprocessor (Lude.Maybe ImageInserter)
vpImageInserter = Lens.lens (imageInserter :: VideoPreprocessor -> Lude.Maybe ImageInserter) (\s a -> s {imageInserter = a} :: VideoPreprocessor)
{-# DEPRECATED vpImageInserter "Use generic-lens or generic-optics with 'imageInserter' instead." #-}

-- | If you work with a third party video watermarking partner, use the group of settings that correspond with your watermarking partner to include watermarks in your output.
--
-- /Note:/ Consider using 'partnerWatermarking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpPartnerWatermarking :: Lens.Lens' VideoPreprocessor (Lude.Maybe PartnerWatermarking)
vpPartnerWatermarking = Lens.lens (partnerWatermarking :: VideoPreprocessor -> Lude.Maybe PartnerWatermarking) (\s a -> s {partnerWatermarking = a} :: VideoPreprocessor)
{-# DEPRECATED vpPartnerWatermarking "Use generic-lens or generic-optics with 'partnerWatermarking' instead." #-}

instance Lude.FromJSON VideoPreprocessor where
  parseJSON =
    Lude.withObject
      "VideoPreprocessor"
      ( \x ->
          VideoPreprocessor'
            Lude.<$> (x Lude..:? "timecodeBurnin")
            Lude.<*> (x Lude..:? "dolbyVision")
            Lude.<*> (x Lude..:? "colorCorrector")
            Lude.<*> (x Lude..:? "deinterlacer")
            Lude.<*> (x Lude..:? "noiseReducer")
            Lude.<*> (x Lude..:? "imageInserter")
            Lude.<*> (x Lude..:? "partnerWatermarking")
      )

instance Lude.ToJSON VideoPreprocessor where
  toJSON VideoPreprocessor' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("timecodeBurnin" Lude..=) Lude.<$> timecodeBurnin,
            ("dolbyVision" Lude..=) Lude.<$> dolbyVision,
            ("colorCorrector" Lude..=) Lude.<$> colorCorrector,
            ("deinterlacer" Lude..=) Lude.<$> deinterlacer,
            ("noiseReducer" Lude..=) Lude.<$> noiseReducer,
            ("imageInserter" Lude..=) Lude.<$> imageInserter,
            ("partnerWatermarking" Lude..=) Lude.<$> partnerWatermarking
          ]
      )
