{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Hdr10Metadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Hdr10Metadata
  ( Hdr10Metadata (..),

    -- * Smart constructor
    mkHdr10Metadata,

    -- * Lenses
    hmRedPrimaryX,
    hmBluePrimaryX,
    hmMaxFrameAverageLightLevel,
    hmWhitePointY,
    hmMaxContentLightLevel,
    hmWhitePointX,
    hmBluePrimaryY,
    hmGreenPrimaryY,
    hmGreenPrimaryX,
    hmMinLuminance,
    hmRedPrimaryY,
    hmMaxLuminance,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Use these settings to specify static color calibration metadata, as defined by SMPTE ST 2086. These values don't affect the pixel values that are encoded in the video stream. They are intended to help the downstream video player display content in a way that reflects the intentions of the the content creator.
--
-- /See:/ 'mkHdr10Metadata' smart constructor.
data Hdr10Metadata = Hdr10Metadata'
  { -- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
    redPrimaryX :: Lude.Maybe Lude.Natural,
    -- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
    bluePrimaryX :: Lude.Maybe Lude.Natural,
    -- | Maximum average light level of any frame in the coded video sequence, in units of candelas per square meter. This setting doesn't have a default value; you must specify a value that is suitable for the content.
    maxFrameAverageLightLevel :: Lude.Maybe Lude.Natural,
    -- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
    whitePointY :: Lude.Maybe Lude.Natural,
    -- | Maximum light level among all samples in the coded video sequence, in units of candelas per square meter.  This setting doesn't have a default value; you must specify a value that is suitable for the content.
    maxContentLightLevel :: Lude.Maybe Lude.Natural,
    -- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
    whitePointX :: Lude.Maybe Lude.Natural,
    -- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
    bluePrimaryY :: Lude.Maybe Lude.Natural,
    -- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
    greenPrimaryY :: Lude.Maybe Lude.Natural,
    -- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
    greenPrimaryX :: Lude.Maybe Lude.Natural,
    -- | Nominal minimum mastering display luminance in units of of 0.0001 candelas per square meter
    minLuminance :: Lude.Maybe Lude.Natural,
    -- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
    redPrimaryY :: Lude.Maybe Lude.Natural,
    -- | Nominal maximum mastering display luminance in units of of 0.0001 candelas per square meter.
    maxLuminance :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Hdr10Metadata' with the minimum fields required to make a request.
--
-- * 'redPrimaryX' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
-- * 'bluePrimaryX' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
-- * 'maxFrameAverageLightLevel' - Maximum average light level of any frame in the coded video sequence, in units of candelas per square meter. This setting doesn't have a default value; you must specify a value that is suitable for the content.
-- * 'whitePointY' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
-- * 'maxContentLightLevel' - Maximum light level among all samples in the coded video sequence, in units of candelas per square meter.  This setting doesn't have a default value; you must specify a value that is suitable for the content.
-- * 'whitePointX' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
-- * 'bluePrimaryY' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
-- * 'greenPrimaryY' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
-- * 'greenPrimaryX' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
-- * 'minLuminance' - Nominal minimum mastering display luminance in units of of 0.0001 candelas per square meter
-- * 'redPrimaryY' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
-- * 'maxLuminance' - Nominal maximum mastering display luminance in units of of 0.0001 candelas per square meter.
mkHdr10Metadata ::
  Hdr10Metadata
mkHdr10Metadata =
  Hdr10Metadata'
    { redPrimaryX = Lude.Nothing,
      bluePrimaryX = Lude.Nothing,
      maxFrameAverageLightLevel = Lude.Nothing,
      whitePointY = Lude.Nothing,
      maxContentLightLevel = Lude.Nothing,
      whitePointX = Lude.Nothing,
      bluePrimaryY = Lude.Nothing,
      greenPrimaryY = Lude.Nothing,
      greenPrimaryX = Lude.Nothing,
      minLuminance = Lude.Nothing,
      redPrimaryY = Lude.Nothing,
      maxLuminance = Lude.Nothing
    }

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'redPrimaryX' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmRedPrimaryX :: Lens.Lens' Hdr10Metadata (Lude.Maybe Lude.Natural)
hmRedPrimaryX = Lens.lens (redPrimaryX :: Hdr10Metadata -> Lude.Maybe Lude.Natural) (\s a -> s {redPrimaryX = a} :: Hdr10Metadata)
{-# DEPRECATED hmRedPrimaryX "Use generic-lens or generic-optics with 'redPrimaryX' instead." #-}

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'bluePrimaryX' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmBluePrimaryX :: Lens.Lens' Hdr10Metadata (Lude.Maybe Lude.Natural)
hmBluePrimaryX = Lens.lens (bluePrimaryX :: Hdr10Metadata -> Lude.Maybe Lude.Natural) (\s a -> s {bluePrimaryX = a} :: Hdr10Metadata)
{-# DEPRECATED hmBluePrimaryX "Use generic-lens or generic-optics with 'bluePrimaryX' instead." #-}

-- | Maximum average light level of any frame in the coded video sequence, in units of candelas per square meter. This setting doesn't have a default value; you must specify a value that is suitable for the content.
--
-- /Note:/ Consider using 'maxFrameAverageLightLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmMaxFrameAverageLightLevel :: Lens.Lens' Hdr10Metadata (Lude.Maybe Lude.Natural)
hmMaxFrameAverageLightLevel = Lens.lens (maxFrameAverageLightLevel :: Hdr10Metadata -> Lude.Maybe Lude.Natural) (\s a -> s {maxFrameAverageLightLevel = a} :: Hdr10Metadata)
{-# DEPRECATED hmMaxFrameAverageLightLevel "Use generic-lens or generic-optics with 'maxFrameAverageLightLevel' instead." #-}

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'whitePointY' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmWhitePointY :: Lens.Lens' Hdr10Metadata (Lude.Maybe Lude.Natural)
hmWhitePointY = Lens.lens (whitePointY :: Hdr10Metadata -> Lude.Maybe Lude.Natural) (\s a -> s {whitePointY = a} :: Hdr10Metadata)
{-# DEPRECATED hmWhitePointY "Use generic-lens or generic-optics with 'whitePointY' instead." #-}

-- | Maximum light level among all samples in the coded video sequence, in units of candelas per square meter.  This setting doesn't have a default value; you must specify a value that is suitable for the content.
--
-- /Note:/ Consider using 'maxContentLightLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmMaxContentLightLevel :: Lens.Lens' Hdr10Metadata (Lude.Maybe Lude.Natural)
hmMaxContentLightLevel = Lens.lens (maxContentLightLevel :: Hdr10Metadata -> Lude.Maybe Lude.Natural) (\s a -> s {maxContentLightLevel = a} :: Hdr10Metadata)
{-# DEPRECATED hmMaxContentLightLevel "Use generic-lens or generic-optics with 'maxContentLightLevel' instead." #-}

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'whitePointX' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmWhitePointX :: Lens.Lens' Hdr10Metadata (Lude.Maybe Lude.Natural)
hmWhitePointX = Lens.lens (whitePointX :: Hdr10Metadata -> Lude.Maybe Lude.Natural) (\s a -> s {whitePointX = a} :: Hdr10Metadata)
{-# DEPRECATED hmWhitePointX "Use generic-lens or generic-optics with 'whitePointX' instead." #-}

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'bluePrimaryY' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmBluePrimaryY :: Lens.Lens' Hdr10Metadata (Lude.Maybe Lude.Natural)
hmBluePrimaryY = Lens.lens (bluePrimaryY :: Hdr10Metadata -> Lude.Maybe Lude.Natural) (\s a -> s {bluePrimaryY = a} :: Hdr10Metadata)
{-# DEPRECATED hmBluePrimaryY "Use generic-lens or generic-optics with 'bluePrimaryY' instead." #-}

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'greenPrimaryY' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmGreenPrimaryY :: Lens.Lens' Hdr10Metadata (Lude.Maybe Lude.Natural)
hmGreenPrimaryY = Lens.lens (greenPrimaryY :: Hdr10Metadata -> Lude.Maybe Lude.Natural) (\s a -> s {greenPrimaryY = a} :: Hdr10Metadata)
{-# DEPRECATED hmGreenPrimaryY "Use generic-lens or generic-optics with 'greenPrimaryY' instead." #-}

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'greenPrimaryX' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmGreenPrimaryX :: Lens.Lens' Hdr10Metadata (Lude.Maybe Lude.Natural)
hmGreenPrimaryX = Lens.lens (greenPrimaryX :: Hdr10Metadata -> Lude.Maybe Lude.Natural) (\s a -> s {greenPrimaryX = a} :: Hdr10Metadata)
{-# DEPRECATED hmGreenPrimaryX "Use generic-lens or generic-optics with 'greenPrimaryX' instead." #-}

-- | Nominal minimum mastering display luminance in units of of 0.0001 candelas per square meter
--
-- /Note:/ Consider using 'minLuminance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmMinLuminance :: Lens.Lens' Hdr10Metadata (Lude.Maybe Lude.Natural)
hmMinLuminance = Lens.lens (minLuminance :: Hdr10Metadata -> Lude.Maybe Lude.Natural) (\s a -> s {minLuminance = a} :: Hdr10Metadata)
{-# DEPRECATED hmMinLuminance "Use generic-lens or generic-optics with 'minLuminance' instead." #-}

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- /Note:/ Consider using 'redPrimaryY' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmRedPrimaryY :: Lens.Lens' Hdr10Metadata (Lude.Maybe Lude.Natural)
hmRedPrimaryY = Lens.lens (redPrimaryY :: Hdr10Metadata -> Lude.Maybe Lude.Natural) (\s a -> s {redPrimaryY = a} :: Hdr10Metadata)
{-# DEPRECATED hmRedPrimaryY "Use generic-lens or generic-optics with 'redPrimaryY' instead." #-}

-- | Nominal maximum mastering display luminance in units of of 0.0001 candelas per square meter.
--
-- /Note:/ Consider using 'maxLuminance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmMaxLuminance :: Lens.Lens' Hdr10Metadata (Lude.Maybe Lude.Natural)
hmMaxLuminance = Lens.lens (maxLuminance :: Hdr10Metadata -> Lude.Maybe Lude.Natural) (\s a -> s {maxLuminance = a} :: Hdr10Metadata)
{-# DEPRECATED hmMaxLuminance "Use generic-lens or generic-optics with 'maxLuminance' instead." #-}

instance Lude.FromJSON Hdr10Metadata where
  parseJSON =
    Lude.withObject
      "Hdr10Metadata"
      ( \x ->
          Hdr10Metadata'
            Lude.<$> (x Lude..:? "redPrimaryX")
            Lude.<*> (x Lude..:? "bluePrimaryX")
            Lude.<*> (x Lude..:? "maxFrameAverageLightLevel")
            Lude.<*> (x Lude..:? "whitePointY")
            Lude.<*> (x Lude..:? "maxContentLightLevel")
            Lude.<*> (x Lude..:? "whitePointX")
            Lude.<*> (x Lude..:? "bluePrimaryY")
            Lude.<*> (x Lude..:? "greenPrimaryY")
            Lude.<*> (x Lude..:? "greenPrimaryX")
            Lude.<*> (x Lude..:? "minLuminance")
            Lude.<*> (x Lude..:? "redPrimaryY")
            Lude.<*> (x Lude..:? "maxLuminance")
      )

instance Lude.ToJSON Hdr10Metadata where
  toJSON Hdr10Metadata' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("redPrimaryX" Lude..=) Lude.<$> redPrimaryX,
            ("bluePrimaryX" Lude..=) Lude.<$> bluePrimaryX,
            ("maxFrameAverageLightLevel" Lude..=)
              Lude.<$> maxFrameAverageLightLevel,
            ("whitePointY" Lude..=) Lude.<$> whitePointY,
            ("maxContentLightLevel" Lude..=) Lude.<$> maxContentLightLevel,
            ("whitePointX" Lude..=) Lude.<$> whitePointX,
            ("bluePrimaryY" Lude..=) Lude.<$> bluePrimaryY,
            ("greenPrimaryY" Lude..=) Lude.<$> greenPrimaryY,
            ("greenPrimaryX" Lude..=) Lude.<$> greenPrimaryX,
            ("minLuminance" Lude..=) Lude.<$> minLuminance,
            ("redPrimaryY" Lude..=) Lude.<$> redPrimaryY,
            ("maxLuminance" Lude..=) Lude.<$> maxLuminance
          ]
      )
