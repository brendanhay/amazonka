{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Smile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Smile
  ( Smile (..),

    -- * Smart constructor
    mkSmile,

    -- * Lenses
    sfValue,
    sfConfidence,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether or not the face is smiling, and the confidence level in the determination.
--
-- /See:/ 'mkSmile' smart constructor.
data Smile = Smile'
  { -- | Boolean value that indicates whether the face is smiling or not.
    value :: Lude.Maybe Lude.Bool,
    -- | Level of confidence in the determination.
    confidence :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Smile' with the minimum fields required to make a request.
--
-- * 'value' - Boolean value that indicates whether the face is smiling or not.
-- * 'confidence' - Level of confidence in the determination.
mkSmile ::
  Smile
mkSmile = Smile' {value = Lude.Nothing, confidence = Lude.Nothing}

-- | Boolean value that indicates whether the face is smiling or not.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfValue :: Lens.Lens' Smile (Lude.Maybe Lude.Bool)
sfValue = Lens.lens (value :: Smile -> Lude.Maybe Lude.Bool) (\s a -> s {value = a} :: Smile)
{-# DEPRECATED sfValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfConfidence :: Lens.Lens' Smile (Lude.Maybe Lude.Double)
sfConfidence = Lens.lens (confidence :: Smile -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: Smile)
{-# DEPRECATED sfConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

instance Lude.FromJSON Smile where
  parseJSON =
    Lude.withObject
      "Smile"
      ( \x ->
          Smile'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Confidence")
      )
