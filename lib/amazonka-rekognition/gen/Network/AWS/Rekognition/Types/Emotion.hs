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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.EmotionName

-- | The emotions that appear to be expressed on the face, and the confidence level in the determination. The API is only making a determination of the physical appearance of a person's face. It is not a determination of the person’s internal emotional state and should not be used in such a way. For example, a person pretending to have a sad face might not be sad emotionally.
--
-- /See:/ 'mkEmotion' smart constructor.
data Emotion = Emotion'
  { confidence :: Lude.Maybe Lude.Double,
    type' :: Lude.Maybe EmotionName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Emotion' with the minimum fields required to make a request.
--
-- * 'confidence' - Level of confidence in the determination.
-- * 'type'' - Type of emotion detected.
mkEmotion ::
  Emotion
mkEmotion =
  Emotion' {confidence = Lude.Nothing, type' = Lude.Nothing}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eConfidence :: Lens.Lens' Emotion (Lude.Maybe Lude.Double)
eConfidence = Lens.lens (confidence :: Emotion -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: Emotion)
{-# DEPRECATED eConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | Type of emotion detected.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eType :: Lens.Lens' Emotion (Lude.Maybe EmotionName)
eType = Lens.lens (type' :: Emotion -> Lude.Maybe EmotionName) (\s a -> s {type' = a} :: Emotion)
{-# DEPRECATED eType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Emotion where
  parseJSON =
    Lude.withObject
      "Emotion"
      ( \x ->
          Emotion'
            Lude.<$> (x Lude..:? "Confidence") Lude.<*> (x Lude..:? "Type")
      )
