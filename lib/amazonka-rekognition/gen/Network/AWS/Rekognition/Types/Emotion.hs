{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Emotion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Emotion
  ( Emotion (..),

    -- * Smart constructor
    mkEmotion,

    -- * Lenses
    eConfidence,
    eType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.EmotionName as Types

-- | The emotions that appear to be expressed on the face, and the confidence level in the determination. The API is only making a determination of the physical appearance of a person's face. It is not a determination of the person’s internal emotional state and should not be used in such a way. For example, a person pretending to have a sad face might not be sad emotionally.
--
-- /See:/ 'mkEmotion' smart constructor.
data Emotion = Emotion'
  { -- | Level of confidence in the determination.
    confidence :: Core.Maybe Core.Double,
    -- | Type of emotion detected.
    type' :: Core.Maybe Types.EmotionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Emotion' value with any optional fields omitted.
mkEmotion ::
  Emotion
mkEmotion =
  Emotion' {confidence = Core.Nothing, type' = Core.Nothing}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eConfidence :: Lens.Lens' Emotion (Core.Maybe Core.Double)
eConfidence = Lens.field @"confidence"
{-# DEPRECATED eConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | Type of emotion detected.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eType :: Lens.Lens' Emotion (Core.Maybe Types.EmotionName)
eType = Lens.field @"type'"
{-# DEPRECATED eType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Emotion where
  parseJSON =
    Core.withObject "Emotion" Core.$
      \x ->
        Emotion'
          Core.<$> (x Core..:? "Confidence") Core.<*> (x Core..:? "Type")
