{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Gender
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Gender
  ( Gender (..),

    -- * Smart constructor
    mkGender,

    -- * Lenses
    gValue,
    gConfidence,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.GenderType

-- | The predicted gender of a detected face.
--
-- Amazon Rekognition makes gender binary (male/female) predictions based on the physical appearance of a face in a particular image. This kind of prediction is not designed to categorize a person’s gender identity, and you shouldn't use Amazon Rekognition to make such a determination. For example, a male actor wearing a long-haired wig and earrings for a role might be predicted as female.
-- Using Amazon Rekognition to make gender binary predictions is best suited for use cases where aggregate gender distribution statistics need to be analyzed without identifying specific users. For example, the percentage of female users compared to male users on a social media platform.
-- We don't recommend using gender binary predictions to make decisions that impact  an individual's rights, privacy, or access to services.
--
-- /See:/ 'mkGender' smart constructor.
data Gender = Gender'
  { value :: Lude.Maybe GenderType,
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

-- | Creates a value of 'Gender' with the minimum fields required to make a request.
--
-- * 'confidence' - Level of confidence in the prediction.
-- * 'value' - The predicted gender of the face.
mkGender ::
  Gender
mkGender = Gender' {value = Lude.Nothing, confidence = Lude.Nothing}

-- | The predicted gender of the face.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gValue :: Lens.Lens' Gender (Lude.Maybe GenderType)
gValue = Lens.lens (value :: Gender -> Lude.Maybe GenderType) (\s a -> s {value = a} :: Gender)
{-# DEPRECATED gValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Level of confidence in the prediction.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gConfidence :: Lens.Lens' Gender (Lude.Maybe Lude.Double)
gConfidence = Lens.lens (confidence :: Gender -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: Gender)
{-# DEPRECATED gConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

instance Lude.FromJSON Gender where
  parseJSON =
    Lude.withObject
      "Gender"
      ( \x ->
          Gender'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Confidence")
      )
