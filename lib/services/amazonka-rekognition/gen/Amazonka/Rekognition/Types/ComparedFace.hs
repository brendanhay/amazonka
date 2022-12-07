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
-- Module      : Amazonka.Rekognition.Types.ComparedFace
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ComparedFace where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.BoundingBox
import Amazonka.Rekognition.Types.Emotion
import Amazonka.Rekognition.Types.ImageQuality
import Amazonka.Rekognition.Types.Landmark
import Amazonka.Rekognition.Types.Pose
import Amazonka.Rekognition.Types.Smile

-- | Provides face metadata for target image faces that are analyzed by
-- @CompareFaces@ and @RecognizeCelebrities@.
--
-- /See:/ 'newComparedFace' smart constructor.
data ComparedFace = ComparedFace'
  { -- | Identifies face image brightness and sharpness.
    quality :: Prelude.Maybe ImageQuality,
    -- | Indicates the pose of the face as determined by its pitch, roll, and
    -- yaw.
    pose :: Prelude.Maybe Pose,
    -- | Level of confidence that what the bounding box contains is a face.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | Bounding box of the face.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | An array of facial landmarks.
    landmarks :: Prelude.Maybe [Landmark],
    -- | The emotions that appear to be expressed on the face, and the confidence
    -- level in the determination. Valid values include \"Happy\", \"Sad\",
    -- \"Angry\", \"Confused\", \"Disgusted\", \"Surprised\", \"Calm\",
    -- \"Unknown\", and \"Fear\".
    emotions :: Prelude.Maybe [Emotion],
    -- | Indicates whether or not the face is smiling, and the confidence level
    -- in the determination.
    smile :: Prelude.Maybe Smile
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComparedFace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quality', 'comparedFace_quality' - Identifies face image brightness and sharpness.
--
-- 'pose', 'comparedFace_pose' - Indicates the pose of the face as determined by its pitch, roll, and
-- yaw.
--
-- 'confidence', 'comparedFace_confidence' - Level of confidence that what the bounding box contains is a face.
--
-- 'boundingBox', 'comparedFace_boundingBox' - Bounding box of the face.
--
-- 'landmarks', 'comparedFace_landmarks' - An array of facial landmarks.
--
-- 'emotions', 'comparedFace_emotions' - The emotions that appear to be expressed on the face, and the confidence
-- level in the determination. Valid values include \"Happy\", \"Sad\",
-- \"Angry\", \"Confused\", \"Disgusted\", \"Surprised\", \"Calm\",
-- \"Unknown\", and \"Fear\".
--
-- 'smile', 'comparedFace_smile' - Indicates whether or not the face is smiling, and the confidence level
-- in the determination.
newComparedFace ::
  ComparedFace
newComparedFace =
  ComparedFace'
    { quality = Prelude.Nothing,
      pose = Prelude.Nothing,
      confidence = Prelude.Nothing,
      boundingBox = Prelude.Nothing,
      landmarks = Prelude.Nothing,
      emotions = Prelude.Nothing,
      smile = Prelude.Nothing
    }

-- | Identifies face image brightness and sharpness.
comparedFace_quality :: Lens.Lens' ComparedFace (Prelude.Maybe ImageQuality)
comparedFace_quality = Lens.lens (\ComparedFace' {quality} -> quality) (\s@ComparedFace' {} a -> s {quality = a} :: ComparedFace)

-- | Indicates the pose of the face as determined by its pitch, roll, and
-- yaw.
comparedFace_pose :: Lens.Lens' ComparedFace (Prelude.Maybe Pose)
comparedFace_pose = Lens.lens (\ComparedFace' {pose} -> pose) (\s@ComparedFace' {} a -> s {pose = a} :: ComparedFace)

-- | Level of confidence that what the bounding box contains is a face.
comparedFace_confidence :: Lens.Lens' ComparedFace (Prelude.Maybe Prelude.Double)
comparedFace_confidence = Lens.lens (\ComparedFace' {confidence} -> confidence) (\s@ComparedFace' {} a -> s {confidence = a} :: ComparedFace)

-- | Bounding box of the face.
comparedFace_boundingBox :: Lens.Lens' ComparedFace (Prelude.Maybe BoundingBox)
comparedFace_boundingBox = Lens.lens (\ComparedFace' {boundingBox} -> boundingBox) (\s@ComparedFace' {} a -> s {boundingBox = a} :: ComparedFace)

-- | An array of facial landmarks.
comparedFace_landmarks :: Lens.Lens' ComparedFace (Prelude.Maybe [Landmark])
comparedFace_landmarks = Lens.lens (\ComparedFace' {landmarks} -> landmarks) (\s@ComparedFace' {} a -> s {landmarks = a} :: ComparedFace) Prelude.. Lens.mapping Lens.coerced

-- | The emotions that appear to be expressed on the face, and the confidence
-- level in the determination. Valid values include \"Happy\", \"Sad\",
-- \"Angry\", \"Confused\", \"Disgusted\", \"Surprised\", \"Calm\",
-- \"Unknown\", and \"Fear\".
comparedFace_emotions :: Lens.Lens' ComparedFace (Prelude.Maybe [Emotion])
comparedFace_emotions = Lens.lens (\ComparedFace' {emotions} -> emotions) (\s@ComparedFace' {} a -> s {emotions = a} :: ComparedFace) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether or not the face is smiling, and the confidence level
-- in the determination.
comparedFace_smile :: Lens.Lens' ComparedFace (Prelude.Maybe Smile)
comparedFace_smile = Lens.lens (\ComparedFace' {smile} -> smile) (\s@ComparedFace' {} a -> s {smile = a} :: ComparedFace)

instance Data.FromJSON ComparedFace where
  parseJSON =
    Data.withObject
      "ComparedFace"
      ( \x ->
          ComparedFace'
            Prelude.<$> (x Data..:? "Quality")
            Prelude.<*> (x Data..:? "Pose")
            Prelude.<*> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "BoundingBox")
            Prelude.<*> (x Data..:? "Landmarks" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Emotions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Smile")
      )

instance Prelude.Hashable ComparedFace where
  hashWithSalt _salt ComparedFace' {..} =
    _salt `Prelude.hashWithSalt` quality
      `Prelude.hashWithSalt` pose
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` boundingBox
      `Prelude.hashWithSalt` landmarks
      `Prelude.hashWithSalt` emotions
      `Prelude.hashWithSalt` smile

instance Prelude.NFData ComparedFace where
  rnf ComparedFace' {..} =
    Prelude.rnf quality
      `Prelude.seq` Prelude.rnf pose
      `Prelude.seq` Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf boundingBox
      `Prelude.seq` Prelude.rnf landmarks
      `Prelude.seq` Prelude.rnf emotions
      `Prelude.seq` Prelude.rnf smile
