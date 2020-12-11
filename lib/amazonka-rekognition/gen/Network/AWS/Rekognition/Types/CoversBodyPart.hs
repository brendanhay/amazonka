-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.CoversBodyPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CoversBodyPart
  ( CoversBodyPart (..),

    -- * Smart constructor
    mkCoversBodyPart,

    -- * Lenses
    cbpValue,
    cbpConfidence,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an item of Personal Protective Equipment covering a corresponding body part. For more information, see 'DetectProtectiveEquipment' .
--
-- /See:/ 'mkCoversBodyPart' smart constructor.
data CoversBodyPart = CoversBodyPart'
  { value ::
      Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'CoversBodyPart' with the minimum fields required to make a request.
--
-- * 'confidence' - The confidence that Amazon Rekognition has in the value of @Value@ .
-- * 'value' - True if the PPE covers the corresponding body part, otherwise false.
mkCoversBodyPart ::
  CoversBodyPart
mkCoversBodyPart =
  CoversBodyPart' {value = Lude.Nothing, confidence = Lude.Nothing}

-- | True if the PPE covers the corresponding body part, otherwise false.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpValue :: Lens.Lens' CoversBodyPart (Lude.Maybe Lude.Bool)
cbpValue = Lens.lens (value :: CoversBodyPart -> Lude.Maybe Lude.Bool) (\s a -> s {value = a} :: CoversBodyPart)
{-# DEPRECATED cbpValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The confidence that Amazon Rekognition has in the value of @Value@ .
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpConfidence :: Lens.Lens' CoversBodyPart (Lude.Maybe Lude.Double)
cbpConfidence = Lens.lens (confidence :: CoversBodyPart -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: CoversBodyPart)
{-# DEPRECATED cbpConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

instance Lude.FromJSON CoversBodyPart where
  parseJSON =
    Lude.withObject
      "CoversBodyPart"
      ( \x ->
          CoversBodyPart'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Confidence")
      )
