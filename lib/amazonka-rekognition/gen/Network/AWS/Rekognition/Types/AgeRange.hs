{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.AgeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.AgeRange
  ( AgeRange (..),

    -- * Smart constructor
    mkAgeRange,

    -- * Lenses
    arLow,
    arHigh,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Structure containing the estimated age range, in years, for a face.
--
-- Amazon Rekognition estimates an age range for faces detected in the input image. Estimated age ranges can overlap. A face of a 5-year-old might have an estimated range of 4-6, while the face of a 6-year-old might have an estimated range of 4-8.
--
-- /See:/ 'mkAgeRange' smart constructor.
data AgeRange = AgeRange'
  { -- | The lowest estimated age.
    low :: Lude.Maybe Lude.Natural,
    -- | The highest estimated age.
    high :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AgeRange' with the minimum fields required to make a request.
--
-- * 'low' - The lowest estimated age.
-- * 'high' - The highest estimated age.
mkAgeRange ::
  AgeRange
mkAgeRange = AgeRange' {low = Lude.Nothing, high = Lude.Nothing}

-- | The lowest estimated age.
--
-- /Note:/ Consider using 'low' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arLow :: Lens.Lens' AgeRange (Lude.Maybe Lude.Natural)
arLow = Lens.lens (low :: AgeRange -> Lude.Maybe Lude.Natural) (\s a -> s {low = a} :: AgeRange)
{-# DEPRECATED arLow "Use generic-lens or generic-optics with 'low' instead." #-}

-- | The highest estimated age.
--
-- /Note:/ Consider using 'high' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arHigh :: Lens.Lens' AgeRange (Lude.Maybe Lude.Natural)
arHigh = Lens.lens (high :: AgeRange -> Lude.Maybe Lude.Natural) (\s a -> s {high = a} :: AgeRange)
{-# DEPRECATED arHigh "Use generic-lens or generic-optics with 'high' instead." #-}

instance Lude.FromJSON AgeRange where
  parseJSON =
    Lude.withObject
      "AgeRange"
      ( \x ->
          AgeRange' Lude.<$> (x Lude..:? "Low") Lude.<*> (x Lude..:? "High")
      )
