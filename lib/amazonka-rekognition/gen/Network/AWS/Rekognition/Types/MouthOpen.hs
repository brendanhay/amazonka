{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.MouthOpen
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.MouthOpen
  ( MouthOpen (..),

    -- * Smart constructor
    mkMouthOpen,

    -- * Lenses
    moValue,
    moConfidence,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether or not the mouth on the face is open, and the confidence level in the determination.
--
-- /See:/ 'mkMouthOpen' smart constructor.
data MouthOpen = MouthOpen'
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

-- | Creates a value of 'MouthOpen' with the minimum fields required to make a request.
--
-- * 'confidence' - Level of confidence in the determination.
-- * 'value' - Boolean value that indicates whether the mouth on the face is open or not.
mkMouthOpen ::
  MouthOpen
mkMouthOpen =
  MouthOpen' {value = Lude.Nothing, confidence = Lude.Nothing}

-- | Boolean value that indicates whether the mouth on the face is open or not.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
moValue :: Lens.Lens' MouthOpen (Lude.Maybe Lude.Bool)
moValue = Lens.lens (value :: MouthOpen -> Lude.Maybe Lude.Bool) (\s a -> s {value = a} :: MouthOpen)
{-# DEPRECATED moValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
moConfidence :: Lens.Lens' MouthOpen (Lude.Maybe Lude.Double)
moConfidence = Lens.lens (confidence :: MouthOpen -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: MouthOpen)
{-# DEPRECATED moConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

instance Lude.FromJSON MouthOpen where
  parseJSON =
    Lude.withObject
      "MouthOpen"
      ( \x ->
          MouthOpen'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Confidence")
      )
