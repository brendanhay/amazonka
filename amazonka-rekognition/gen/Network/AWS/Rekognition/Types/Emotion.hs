{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    confidence :: Prelude.Maybe Prelude.Double,
    -- | Type of emotion detected.
    type' :: Prelude.Maybe EmotionName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { confidence = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Level of confidence in the determination.
emotion_confidence :: Lens.Lens' Emotion (Prelude.Maybe Prelude.Double)
emotion_confidence = Lens.lens (\Emotion' {confidence} -> confidence) (\s@Emotion' {} a -> s {confidence = a} :: Emotion)

-- | Type of emotion detected.
emotion_type :: Lens.Lens' Emotion (Prelude.Maybe EmotionName)
emotion_type = Lens.lens (\Emotion' {type'} -> type') (\s@Emotion' {} a -> s {type' = a} :: Emotion)

instance Prelude.FromJSON Emotion where
  parseJSON =
    Prelude.withObject
      "Emotion"
      ( \x ->
          Emotion'
            Prelude.<$> (x Prelude..:? "Confidence")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable Emotion

instance Prelude.NFData Emotion
