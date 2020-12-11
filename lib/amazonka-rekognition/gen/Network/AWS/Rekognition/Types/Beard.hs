-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Beard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Beard
  ( Beard (..),

    -- * Smart constructor
    mkBeard,

    -- * Lenses
    bValue,
    bConfidence,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether or not the face has a beard, and the confidence level in the determination.
--
-- /See:/ 'mkBeard' smart constructor.
data Beard = Beard'
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

-- | Creates a value of 'Beard' with the minimum fields required to make a request.
--
-- * 'confidence' - Level of confidence in the determination.
-- * 'value' - Boolean value that indicates whether the face has beard or not.
mkBeard ::
  Beard
mkBeard = Beard' {value = Lude.Nothing, confidence = Lude.Nothing}

-- | Boolean value that indicates whether the face has beard or not.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bValue :: Lens.Lens' Beard (Lude.Maybe Lude.Bool)
bValue = Lens.lens (value :: Beard -> Lude.Maybe Lude.Bool) (\s a -> s {value = a} :: Beard)
{-# DEPRECATED bValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bConfidence :: Lens.Lens' Beard (Lude.Maybe Lude.Double)
bConfidence = Lens.lens (confidence :: Beard -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: Beard)
{-# DEPRECATED bConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

instance Lude.FromJSON Beard where
  parseJSON =
    Lude.withObject
      "Beard"
      ( \x ->
          Beard'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Confidence")
      )
