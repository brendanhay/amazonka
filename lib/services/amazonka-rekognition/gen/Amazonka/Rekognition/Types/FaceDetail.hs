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
-- Module      : Amazonka.Rekognition.Types.FaceDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.FaceDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.AgeRange
import Amazonka.Rekognition.Types.Beard
import Amazonka.Rekognition.Types.BoundingBox
import Amazonka.Rekognition.Types.Emotion
import Amazonka.Rekognition.Types.EyeOpen
import Amazonka.Rekognition.Types.Eyeglasses
import Amazonka.Rekognition.Types.Gender
import Amazonka.Rekognition.Types.ImageQuality
import Amazonka.Rekognition.Types.Landmark
import Amazonka.Rekognition.Types.MouthOpen
import Amazonka.Rekognition.Types.Mustache
import Amazonka.Rekognition.Types.Pose
import Amazonka.Rekognition.Types.Smile
import Amazonka.Rekognition.Types.Sunglasses

-- | Structure containing attributes of the face that the algorithm detected.
--
-- A @FaceDetail@ object contains either the default facial attributes or
-- all facial attributes. The default attributes are @BoundingBox@,
-- @Confidence@, @Landmarks@, @Pose@, and @Quality@.
--
-- GetFaceDetection is the only Amazon Rekognition Video stored video
-- operation that can return a @FaceDetail@ object with all attributes. To
-- specify which attributes to return, use the @FaceAttributes@ input
-- parameter for StartFaceDetection. The following Amazon Rekognition Video
-- operations return only the default attributes. The corresponding Start
-- operations don\'t have a @FaceAttributes@ input parameter.
--
-- -   GetCelebrityRecognition
--
-- -   GetPersonTracking
--
-- -   GetFaceSearch
--
-- The Amazon Rekognition Image DetectFaces and IndexFaces operations can
-- return all facial attributes. To specify which attributes to return, use
-- the @Attributes@ input parameter for @DetectFaces@. For @IndexFaces@,
-- use the @DetectAttributes@ input parameter.
--
-- /See:/ 'newFaceDetail' smart constructor.
data FaceDetail = FaceDetail'
  { -- | The estimated age range, in years, for the face. Low represents the
    -- lowest estimated age and High represents the highest estimated age.
    ageRange :: Prelude.Maybe AgeRange,
    -- | Indicates whether or not the face is wearing sunglasses, and the
    -- confidence level in the determination.
    sunglasses :: Prelude.Maybe Sunglasses,
    -- | Indicates whether or not the mouth on the face is open, and the
    -- confidence level in the determination.
    mouthOpen :: Prelude.Maybe MouthOpen,
    -- | Bounding box of the face. Default attribute.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | The emotions that appear to be expressed on the face, and the confidence
    -- level in the determination. The API is only making a determination of
    -- the physical appearance of a person\'s face. It is not a determination
    -- of the person’s internal emotional state and should not be used in such
    -- a way. For example, a person pretending to have a sad face might not be
    -- sad emotionally.
    emotions :: Prelude.Maybe [Emotion],
    -- | Indicates whether or not the eyes on the face are open, and the
    -- confidence level in the determination.
    eyesOpen :: Prelude.Maybe EyeOpen,
    -- | Indicates the pose of the face as determined by its pitch, roll, and
    -- yaw. Default attribute.
    pose :: Prelude.Maybe Pose,
    -- | Confidence level that the bounding box contains a face (and not a
    -- different object such as a tree). Default attribute.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The predicted gender of a detected face.
    gender :: Prelude.Maybe Gender,
    -- | Identifies image brightness and sharpness. Default attribute.
    quality :: Prelude.Maybe ImageQuality,
    -- | Indicates whether or not the face is wearing eye glasses, and the
    -- confidence level in the determination.
    eyeglasses :: Prelude.Maybe Eyeglasses,
    -- | Indicates whether or not the face has a beard, and the confidence level
    -- in the determination.
    beard :: Prelude.Maybe Beard,
    -- | Indicates whether or not the face has a mustache, and the confidence
    -- level in the determination.
    mustache :: Prelude.Maybe Mustache,
    -- | Indicates whether or not the face is smiling, and the confidence level
    -- in the determination.
    smile :: Prelude.Maybe Smile,
    -- | Indicates the location of landmarks on the face. Default attribute.
    landmarks :: Prelude.Maybe [Landmark]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FaceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ageRange', 'faceDetail_ageRange' - The estimated age range, in years, for the face. Low represents the
-- lowest estimated age and High represents the highest estimated age.
--
-- 'sunglasses', 'faceDetail_sunglasses' - Indicates whether or not the face is wearing sunglasses, and the
-- confidence level in the determination.
--
-- 'mouthOpen', 'faceDetail_mouthOpen' - Indicates whether or not the mouth on the face is open, and the
-- confidence level in the determination.
--
-- 'boundingBox', 'faceDetail_boundingBox' - Bounding box of the face. Default attribute.
--
-- 'emotions', 'faceDetail_emotions' - The emotions that appear to be expressed on the face, and the confidence
-- level in the determination. The API is only making a determination of
-- the physical appearance of a person\'s face. It is not a determination
-- of the person’s internal emotional state and should not be used in such
-- a way. For example, a person pretending to have a sad face might not be
-- sad emotionally.
--
-- 'eyesOpen', 'faceDetail_eyesOpen' - Indicates whether or not the eyes on the face are open, and the
-- confidence level in the determination.
--
-- 'pose', 'faceDetail_pose' - Indicates the pose of the face as determined by its pitch, roll, and
-- yaw. Default attribute.
--
-- 'confidence', 'faceDetail_confidence' - Confidence level that the bounding box contains a face (and not a
-- different object such as a tree). Default attribute.
--
-- 'gender', 'faceDetail_gender' - The predicted gender of a detected face.
--
-- 'quality', 'faceDetail_quality' - Identifies image brightness and sharpness. Default attribute.
--
-- 'eyeglasses', 'faceDetail_eyeglasses' - Indicates whether or not the face is wearing eye glasses, and the
-- confidence level in the determination.
--
-- 'beard', 'faceDetail_beard' - Indicates whether or not the face has a beard, and the confidence level
-- in the determination.
--
-- 'mustache', 'faceDetail_mustache' - Indicates whether or not the face has a mustache, and the confidence
-- level in the determination.
--
-- 'smile', 'faceDetail_smile' - Indicates whether or not the face is smiling, and the confidence level
-- in the determination.
--
-- 'landmarks', 'faceDetail_landmarks' - Indicates the location of landmarks on the face. Default attribute.
newFaceDetail ::
  FaceDetail
newFaceDetail =
  FaceDetail'
    { ageRange = Prelude.Nothing,
      sunglasses = Prelude.Nothing,
      mouthOpen = Prelude.Nothing,
      boundingBox = Prelude.Nothing,
      emotions = Prelude.Nothing,
      eyesOpen = Prelude.Nothing,
      pose = Prelude.Nothing,
      confidence = Prelude.Nothing,
      gender = Prelude.Nothing,
      quality = Prelude.Nothing,
      eyeglasses = Prelude.Nothing,
      beard = Prelude.Nothing,
      mustache = Prelude.Nothing,
      smile = Prelude.Nothing,
      landmarks = Prelude.Nothing
    }

-- | The estimated age range, in years, for the face. Low represents the
-- lowest estimated age and High represents the highest estimated age.
faceDetail_ageRange :: Lens.Lens' FaceDetail (Prelude.Maybe AgeRange)
faceDetail_ageRange = Lens.lens (\FaceDetail' {ageRange} -> ageRange) (\s@FaceDetail' {} a -> s {ageRange = a} :: FaceDetail)

-- | Indicates whether or not the face is wearing sunglasses, and the
-- confidence level in the determination.
faceDetail_sunglasses :: Lens.Lens' FaceDetail (Prelude.Maybe Sunglasses)
faceDetail_sunglasses = Lens.lens (\FaceDetail' {sunglasses} -> sunglasses) (\s@FaceDetail' {} a -> s {sunglasses = a} :: FaceDetail)

-- | Indicates whether or not the mouth on the face is open, and the
-- confidence level in the determination.
faceDetail_mouthOpen :: Lens.Lens' FaceDetail (Prelude.Maybe MouthOpen)
faceDetail_mouthOpen = Lens.lens (\FaceDetail' {mouthOpen} -> mouthOpen) (\s@FaceDetail' {} a -> s {mouthOpen = a} :: FaceDetail)

-- | Bounding box of the face. Default attribute.
faceDetail_boundingBox :: Lens.Lens' FaceDetail (Prelude.Maybe BoundingBox)
faceDetail_boundingBox = Lens.lens (\FaceDetail' {boundingBox} -> boundingBox) (\s@FaceDetail' {} a -> s {boundingBox = a} :: FaceDetail)

-- | The emotions that appear to be expressed on the face, and the confidence
-- level in the determination. The API is only making a determination of
-- the physical appearance of a person\'s face. It is not a determination
-- of the person’s internal emotional state and should not be used in such
-- a way. For example, a person pretending to have a sad face might not be
-- sad emotionally.
faceDetail_emotions :: Lens.Lens' FaceDetail (Prelude.Maybe [Emotion])
faceDetail_emotions = Lens.lens (\FaceDetail' {emotions} -> emotions) (\s@FaceDetail' {} a -> s {emotions = a} :: FaceDetail) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether or not the eyes on the face are open, and the
-- confidence level in the determination.
faceDetail_eyesOpen :: Lens.Lens' FaceDetail (Prelude.Maybe EyeOpen)
faceDetail_eyesOpen = Lens.lens (\FaceDetail' {eyesOpen} -> eyesOpen) (\s@FaceDetail' {} a -> s {eyesOpen = a} :: FaceDetail)

-- | Indicates the pose of the face as determined by its pitch, roll, and
-- yaw. Default attribute.
faceDetail_pose :: Lens.Lens' FaceDetail (Prelude.Maybe Pose)
faceDetail_pose = Lens.lens (\FaceDetail' {pose} -> pose) (\s@FaceDetail' {} a -> s {pose = a} :: FaceDetail)

-- | Confidence level that the bounding box contains a face (and not a
-- different object such as a tree). Default attribute.
faceDetail_confidence :: Lens.Lens' FaceDetail (Prelude.Maybe Prelude.Double)
faceDetail_confidence = Lens.lens (\FaceDetail' {confidence} -> confidence) (\s@FaceDetail' {} a -> s {confidence = a} :: FaceDetail)

-- | The predicted gender of a detected face.
faceDetail_gender :: Lens.Lens' FaceDetail (Prelude.Maybe Gender)
faceDetail_gender = Lens.lens (\FaceDetail' {gender} -> gender) (\s@FaceDetail' {} a -> s {gender = a} :: FaceDetail)

-- | Identifies image brightness and sharpness. Default attribute.
faceDetail_quality :: Lens.Lens' FaceDetail (Prelude.Maybe ImageQuality)
faceDetail_quality = Lens.lens (\FaceDetail' {quality} -> quality) (\s@FaceDetail' {} a -> s {quality = a} :: FaceDetail)

-- | Indicates whether or not the face is wearing eye glasses, and the
-- confidence level in the determination.
faceDetail_eyeglasses :: Lens.Lens' FaceDetail (Prelude.Maybe Eyeglasses)
faceDetail_eyeglasses = Lens.lens (\FaceDetail' {eyeglasses} -> eyeglasses) (\s@FaceDetail' {} a -> s {eyeglasses = a} :: FaceDetail)

-- | Indicates whether or not the face has a beard, and the confidence level
-- in the determination.
faceDetail_beard :: Lens.Lens' FaceDetail (Prelude.Maybe Beard)
faceDetail_beard = Lens.lens (\FaceDetail' {beard} -> beard) (\s@FaceDetail' {} a -> s {beard = a} :: FaceDetail)

-- | Indicates whether or not the face has a mustache, and the confidence
-- level in the determination.
faceDetail_mustache :: Lens.Lens' FaceDetail (Prelude.Maybe Mustache)
faceDetail_mustache = Lens.lens (\FaceDetail' {mustache} -> mustache) (\s@FaceDetail' {} a -> s {mustache = a} :: FaceDetail)

-- | Indicates whether or not the face is smiling, and the confidence level
-- in the determination.
faceDetail_smile :: Lens.Lens' FaceDetail (Prelude.Maybe Smile)
faceDetail_smile = Lens.lens (\FaceDetail' {smile} -> smile) (\s@FaceDetail' {} a -> s {smile = a} :: FaceDetail)

-- | Indicates the location of landmarks on the face. Default attribute.
faceDetail_landmarks :: Lens.Lens' FaceDetail (Prelude.Maybe [Landmark])
faceDetail_landmarks = Lens.lens (\FaceDetail' {landmarks} -> landmarks) (\s@FaceDetail' {} a -> s {landmarks = a} :: FaceDetail) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON FaceDetail where
  parseJSON =
    Core.withObject
      "FaceDetail"
      ( \x ->
          FaceDetail'
            Prelude.<$> (x Core..:? "AgeRange")
            Prelude.<*> (x Core..:? "Sunglasses")
            Prelude.<*> (x Core..:? "MouthOpen")
            Prelude.<*> (x Core..:? "BoundingBox")
            Prelude.<*> (x Core..:? "Emotions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "EyesOpen")
            Prelude.<*> (x Core..:? "Pose")
            Prelude.<*> (x Core..:? "Confidence")
            Prelude.<*> (x Core..:? "Gender")
            Prelude.<*> (x Core..:? "Quality")
            Prelude.<*> (x Core..:? "Eyeglasses")
            Prelude.<*> (x Core..:? "Beard")
            Prelude.<*> (x Core..:? "Mustache")
            Prelude.<*> (x Core..:? "Smile")
            Prelude.<*> (x Core..:? "Landmarks" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable FaceDetail where
  hashWithSalt salt' FaceDetail' {..} =
    salt' `Prelude.hashWithSalt` landmarks
      `Prelude.hashWithSalt` smile
      `Prelude.hashWithSalt` mustache
      `Prelude.hashWithSalt` beard
      `Prelude.hashWithSalt` eyeglasses
      `Prelude.hashWithSalt` quality
      `Prelude.hashWithSalt` gender
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` pose
      `Prelude.hashWithSalt` eyesOpen
      `Prelude.hashWithSalt` emotions
      `Prelude.hashWithSalt` boundingBox
      `Prelude.hashWithSalt` mouthOpen
      `Prelude.hashWithSalt` sunglasses
      `Prelude.hashWithSalt` ageRange

instance Prelude.NFData FaceDetail where
  rnf FaceDetail' {..} =
    Prelude.rnf ageRange
      `Prelude.seq` Prelude.rnf landmarks
      `Prelude.seq` Prelude.rnf smile
      `Prelude.seq` Prelude.rnf mustache
      `Prelude.seq` Prelude.rnf beard
      `Prelude.seq` Prelude.rnf eyeglasses
      `Prelude.seq` Prelude.rnf quality
      `Prelude.seq` Prelude.rnf gender
      `Prelude.seq` Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf pose
      `Prelude.seq` Prelude.rnf eyesOpen
      `Prelude.seq` Prelude.rnf emotions
      `Prelude.seq` Prelude.rnf boundingBox
      `Prelude.seq` Prelude.rnf mouthOpen
      `Prelude.seq` Prelude.rnf sunglasses
