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
-- Module      : Amazonka.Rekognition.Types.Emotion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.Emotion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.EmotionName

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON Emotion where
  parseJSON =
    Data.withObject
      "Emotion"
      ( \x ->
          Emotion'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Emotion where
  hashWithSalt _salt Emotion' {..} =
    _salt `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Emotion where
  rnf Emotion' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf type'
