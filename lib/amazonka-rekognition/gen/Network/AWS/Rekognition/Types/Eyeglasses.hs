{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Eyeglasses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Eyeglasses
  ( Eyeglasses (..),

    -- * Smart constructor
    mkEyeglasses,

    -- * Lenses
    eyeValue,
    eyeConfidence,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.
--
-- /See:/ 'mkEyeglasses' smart constructor.
data Eyeglasses = Eyeglasses'
  { value :: Lude.Maybe Lude.Bool,
    confidence :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Eyeglasses' with the minimum fields required to make a request.
--
-- * 'confidence' - Level of confidence in the determination.
-- * 'value' - Boolean value that indicates whether the face is wearing eye glasses or not.
mkEyeglasses ::
  Eyeglasses
mkEyeglasses =
  Eyeglasses' {value = Lude.Nothing, confidence = Lude.Nothing}

-- | Boolean value that indicates whether the face is wearing eye glasses or not.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eyeValue :: Lens.Lens' Eyeglasses (Lude.Maybe Lude.Bool)
eyeValue = Lens.lens (value :: Eyeglasses -> Lude.Maybe Lude.Bool) (\s a -> s {value = a} :: Eyeglasses)
{-# DEPRECATED eyeValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eyeConfidence :: Lens.Lens' Eyeglasses (Lude.Maybe Lude.Double)
eyeConfidence = Lens.lens (confidence :: Eyeglasses -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: Eyeglasses)
{-# DEPRECATED eyeConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

instance Lude.FromJSON Eyeglasses where
  parseJSON =
    Lude.withObject
      "Eyeglasses"
      ( \x ->
          Eyeglasses'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Confidence")
      )
