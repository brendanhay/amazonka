{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Emotion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Emotion where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.EmotionName

-- | The emotions that appear to be expressed on the face, and the confidence
-- level in the determination. The API is only making a determination of
-- the physical appearance of a person\'s face. It is not a determination
-- of the person’s internal emotional state and should not be used in such
-- a way. For example, a person pretending to have a sad face might not be
-- sad emotionally.
--
-- /See:/ 'newEmotion' smart constructor.
data Emotion = Emotion'
  { -- | Level of confidence in the determination.
    confidence :: Core.Maybe Core.Double,
    -- | Type of emotion detected.
    type' :: Core.Maybe EmotionName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Emotion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'emotion_confidence' - Level of confidence in the determination.
--
-- 'type'', 'emotion_type' - Type of emotion detected.
newEmotion ::
  Emotion
newEmotion =
  Emotion'
    { confidence = Core.Nothing,
      type' = Core.Nothing
    }

-- | Level of confidence in the determination.
emotion_confidence :: Lens.Lens' Emotion (Core.Maybe Core.Double)
emotion_confidence = Lens.lens (\Emotion' {confidence} -> confidence) (\s@Emotion' {} a -> s {confidence = a} :: Emotion)

-- | Type of emotion detected.
emotion_type :: Lens.Lens' Emotion (Core.Maybe EmotionName)
emotion_type = Lens.lens (\Emotion' {type'} -> type') (\s@Emotion' {} a -> s {type' = a} :: Emotion)

instance Core.FromJSON Emotion where
  parseJSON =
    Core.withObject
      "Emotion"
      ( \x ->
          Emotion'
            Core.<$> (x Core..:? "Confidence")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable Emotion

instance Core.NFData Emotion
