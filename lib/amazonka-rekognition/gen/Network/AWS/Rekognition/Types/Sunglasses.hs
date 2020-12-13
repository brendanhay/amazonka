{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Sunglasses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Sunglasses
  ( Sunglasses (..),

    -- * Smart constructor
    mkSunglasses,

    -- * Lenses
    sValue,
    sConfidence,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.
--
-- /See:/ 'mkSunglasses' smart constructor.
data Sunglasses = Sunglasses'
  { -- | Boolean value that indicates whether the face is wearing sunglasses or not.
    value :: Lude.Maybe Lude.Bool,
    -- | Level of confidence in the determination.
    confidence :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Sunglasses' with the minimum fields required to make a request.
--
-- * 'value' - Boolean value that indicates whether the face is wearing sunglasses or not.
-- * 'confidence' - Level of confidence in the determination.
mkSunglasses ::
  Sunglasses
mkSunglasses =
  Sunglasses' {value = Lude.Nothing, confidence = Lude.Nothing}

-- | Boolean value that indicates whether the face is wearing sunglasses or not.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sValue :: Lens.Lens' Sunglasses (Lude.Maybe Lude.Bool)
sValue = Lens.lens (value :: Sunglasses -> Lude.Maybe Lude.Bool) (\s a -> s {value = a} :: Sunglasses)
{-# DEPRECATED sValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConfidence :: Lens.Lens' Sunglasses (Lude.Maybe Lude.Double)
sConfidence = Lens.lens (confidence :: Sunglasses -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: Sunglasses)
{-# DEPRECATED sConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

instance Lude.FromJSON Sunglasses where
  parseJSON =
    Lude.withObject
      "Sunglasses"
      ( \x ->
          Sunglasses'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Confidence")
      )
