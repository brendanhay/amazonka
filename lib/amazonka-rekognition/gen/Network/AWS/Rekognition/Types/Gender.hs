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
    gConfidence,
    gValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.GenderType as Types

-- | The predicted gender of a detected face.
--
-- Amazon Rekognition makes gender binary (male/female) predictions based on the physical appearance of a face in a particular image. This kind of prediction is not designed to categorize a person’s gender identity, and you shouldn't use Amazon Rekognition to make such a determination. For example, a male actor wearing a long-haired wig and earrings for a role might be predicted as female.
-- Using Amazon Rekognition to make gender binary predictions is best suited for use cases where aggregate gender distribution statistics need to be analyzed without identifying specific users. For example, the percentage of female users compared to male users on a social media platform.
-- We don't recommend using gender binary predictions to make decisions that impact  an individual's rights, privacy, or access to services.
--
-- /See:/ 'mkGender' smart constructor.
data Gender = Gender'
  { -- | Level of confidence in the prediction.
    confidence :: Core.Maybe Core.Double,
    -- | The predicted gender of the face.
    value :: Core.Maybe Types.GenderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Gender' value with any optional fields omitted.
mkGender ::
  Gender
mkGender = Gender' {confidence = Core.Nothing, value = Core.Nothing}

-- | Level of confidence in the prediction.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gConfidence :: Lens.Lens' Gender (Core.Maybe Core.Double)
gConfidence = Lens.field @"confidence"
{-# DEPRECATED gConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | The predicted gender of the face.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gValue :: Lens.Lens' Gender (Core.Maybe Types.GenderType)
gValue = Lens.field @"value"
{-# DEPRECATED gValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Gender where
  parseJSON =
    Core.withObject "Gender" Core.$
      \x ->
        Gender'
          Core.<$> (x Core..:? "Confidence") Core.<*> (x Core..:? "Value")
