{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.EyeOpen
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.EyeOpen
  ( EyeOpen (..),

    -- * Smart constructor
    mkEyeOpen,

    -- * Lenses
    eoValue,
    eoConfidence,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether or not the eyes on the face are open, and the confidence level in the determination.
--
-- /See:/ 'mkEyeOpen' smart constructor.
data EyeOpen = EyeOpen'
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

-- | Creates a value of 'EyeOpen' with the minimum fields required to make a request.
--
-- * 'confidence' - Level of confidence in the determination.
-- * 'value' - Boolean value that indicates whether the eyes on the face are open.
mkEyeOpen ::
  EyeOpen
mkEyeOpen =
  EyeOpen' {value = Lude.Nothing, confidence = Lude.Nothing}

-- | Boolean value that indicates whether the eyes on the face are open.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoValue :: Lens.Lens' EyeOpen (Lude.Maybe Lude.Bool)
eoValue = Lens.lens (value :: EyeOpen -> Lude.Maybe Lude.Bool) (\s a -> s {value = a} :: EyeOpen)
{-# DEPRECATED eoValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoConfidence :: Lens.Lens' EyeOpen (Lude.Maybe Lude.Double)
eoConfidence = Lens.lens (confidence :: EyeOpen -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: EyeOpen)
{-# DEPRECATED eoConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

instance Lude.FromJSON EyeOpen where
  parseJSON =
    Lude.withObject
      "EyeOpen"
      ( \x ->
          EyeOpen'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Confidence")
      )
