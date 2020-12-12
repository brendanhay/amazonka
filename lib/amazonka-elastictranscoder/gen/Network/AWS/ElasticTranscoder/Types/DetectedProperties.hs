{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.DetectedProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.DetectedProperties
  ( DetectedProperties (..),

    -- * Smart constructor
    mkDetectedProperties,

    -- * Lenses
    dpHeight,
    dpFrameRate,
    dpFileSize,
    dpWidth,
    dpDurationMillis,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The detected properties of the input file. Elastic Transcoder identifies these values from the input file.
--
-- /See:/ 'mkDetectedProperties' smart constructor.
data DetectedProperties = DetectedProperties'
  { height ::
      Lude.Maybe Lude.Int,
    frameRate :: Lude.Maybe Lude.Text,
    fileSize :: Lude.Maybe Lude.Integer,
    width :: Lude.Maybe Lude.Int,
    durationMillis :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetectedProperties' with the minimum fields required to make a request.
--
-- * 'durationMillis' - The detected duration of the input file, in milliseconds.
-- * 'fileSize' - The detected file size of the input file, in bytes.
-- * 'frameRate' - The detected frame rate of the input file, in frames per second.
-- * 'height' - The detected height of the input file, in pixels.
-- * 'width' - The detected width of the input file, in pixels.
mkDetectedProperties ::
  DetectedProperties
mkDetectedProperties =
  DetectedProperties'
    { height = Lude.Nothing,
      frameRate = Lude.Nothing,
      fileSize = Lude.Nothing,
      width = Lude.Nothing,
      durationMillis = Lude.Nothing
    }

-- | The detected height of the input file, in pixels.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpHeight :: Lens.Lens' DetectedProperties (Lude.Maybe Lude.Int)
dpHeight = Lens.lens (height :: DetectedProperties -> Lude.Maybe Lude.Int) (\s a -> s {height = a} :: DetectedProperties)
{-# DEPRECATED dpHeight "Use generic-lens or generic-optics with 'height' instead." #-}

-- | The detected frame rate of the input file, in frames per second.
--
-- /Note:/ Consider using 'frameRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpFrameRate :: Lens.Lens' DetectedProperties (Lude.Maybe Lude.Text)
dpFrameRate = Lens.lens (frameRate :: DetectedProperties -> Lude.Maybe Lude.Text) (\s a -> s {frameRate = a} :: DetectedProperties)
{-# DEPRECATED dpFrameRate "Use generic-lens or generic-optics with 'frameRate' instead." #-}

-- | The detected file size of the input file, in bytes.
--
-- /Note:/ Consider using 'fileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpFileSize :: Lens.Lens' DetectedProperties (Lude.Maybe Lude.Integer)
dpFileSize = Lens.lens (fileSize :: DetectedProperties -> Lude.Maybe Lude.Integer) (\s a -> s {fileSize = a} :: DetectedProperties)
{-# DEPRECATED dpFileSize "Use generic-lens or generic-optics with 'fileSize' instead." #-}

-- | The detected width of the input file, in pixels.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpWidth :: Lens.Lens' DetectedProperties (Lude.Maybe Lude.Int)
dpWidth = Lens.lens (width :: DetectedProperties -> Lude.Maybe Lude.Int) (\s a -> s {width = a} :: DetectedProperties)
{-# DEPRECATED dpWidth "Use generic-lens or generic-optics with 'width' instead." #-}

-- | The detected duration of the input file, in milliseconds.
--
-- /Note:/ Consider using 'durationMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDurationMillis :: Lens.Lens' DetectedProperties (Lude.Maybe Lude.Integer)
dpDurationMillis = Lens.lens (durationMillis :: DetectedProperties -> Lude.Maybe Lude.Integer) (\s a -> s {durationMillis = a} :: DetectedProperties)
{-# DEPRECATED dpDurationMillis "Use generic-lens or generic-optics with 'durationMillis' instead." #-}

instance Lude.FromJSON DetectedProperties where
  parseJSON =
    Lude.withObject
      "DetectedProperties"
      ( \x ->
          DetectedProperties'
            Lude.<$> (x Lude..:? "Height")
            Lude.<*> (x Lude..:? "FrameRate")
            Lude.<*> (x Lude..:? "FileSize")
            Lude.<*> (x Lude..:? "Width")
            Lude.<*> (x Lude..:? "DurationMillis")
      )

instance Lude.ToJSON DetectedProperties where
  toJSON DetectedProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Height" Lude..=) Lude.<$> height,
            ("FrameRate" Lude..=) Lude.<$> frameRate,
            ("FileSize" Lude..=) Lude.<$> fileSize,
            ("Width" Lude..=) Lude.<$> width,
            ("DurationMillis" Lude..=) Lude.<$> durationMillis
          ]
      )
