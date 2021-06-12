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
-- Module      : Network.AWS.Rekognition.Types.FaceDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.AgeRange
import Network.AWS.Rekognition.Types.Beard
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.Emotion
import Network.AWS.Rekognition.Types.EyeOpen
import Network.AWS.Rekognition.Types.Eyeglasses
import Network.AWS.Rekognition.Types.Gender
import Network.AWS.Rekognition.Types.ImageQuality
import Network.AWS.Rekognition.Types.Landmark
import Network.AWS.Rekognition.Types.MouthOpen
import Network.AWS.Rekognition.Types.Mustache
import Network.AWS.Rekognition.Types.Pose
import Network.AWS.Rekognition.Types.Smile
import Network.AWS.Rekognition.Types.Sunglasses

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
  { -- | Indicates whether or not the face is wearing sunglasses, and the
    -- confidence level in the determination.
    sunglasses :: Core.Maybe Sunglasses,
    -- | The estimated age range, in years, for the face. Low represents the
    -- lowest estimated age and High represents the highest estimated age.
    ageRange :: Core.Maybe AgeRange,
    -- | Indicates the pose of the face as determined by its pitch, roll, and
    -- yaw. Default attribute.
    pose :: Core.Maybe Pose,
    -- | Indicates the location of landmarks on the face. Default attribute.
    landmarks :: Core.Maybe [Landmark],
    -- | Indicates whether or not the face has a beard, and the confidence level
    -- in the determination.
    beard :: Core.Maybe Beard,
    -- | The emotions that appear to be expressed on the face, and the confidence
    -- level in the determination. The API is only making a determination of
    -- the physical appearance of a person\'s face. It is not a determination
    -- of the person’s internal emotional state and should not be used in such
    -- a way. For example, a person pretending to have a sad face might not be
    -- sad emotionally.
    emotions :: Core.Maybe [Emotion],
    -- | Indicates whether or not the face is wearing eye glasses, and the
    -- confidence level in the determination.
    eyeglasses :: Core.Maybe Eyeglasses,
    -- | The predicted gender of a detected face.
    gender :: Core.Maybe Gender,
    -- | Bounding box of the face. Default attribute.
    boundingBox :: Core.Maybe BoundingBox,
    -- | Indicates whether or not the mouth on the face is open, and the
    -- confidence level in the determination.
    mouthOpen :: Core.Maybe MouthOpen,
    -- | Confidence level that the bounding box contains a face (and not a
    -- different object such as a tree). Default attribute.
    confidence :: Core.Maybe Core.Double,
    -- | Indicates whether or not the face is smiling, and the confidence level
    -- in the determination.
    smile :: Core.Maybe Smile,
    -- | Indicates whether or not the eyes on the face are open, and the
    -- confidence level in the determination.
    eyesOpen :: Core.Maybe EyeOpen,
    -- | Indicates whether or not the face has a mustache, and the confidence
    -- level in the determination.
    mustache :: Core.Maybe Mustache,
    -- | Identifies image brightness and sharpness. Default attribute.
    quality :: Core.Maybe ImageQuality
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FaceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sunglasses', 'faceDetail_sunglasses' - Indicates whether or not the face is wearing sunglasses, and the
-- confidence level in the determination.
--
-- 'ageRange', 'faceDetail_ageRange' - The estimated age range, in years, for the face. Low represents the
-- lowest estimated age and High represents the highest estimated age.
--
-- 'pose', 'faceDetail_pose' - Indicates the pose of the face as determined by its pitch, roll, and
-- yaw. Default attribute.
--
-- 'landmarks', 'faceDetail_landmarks' - Indicates the location of landmarks on the face. Default attribute.
--
-- 'beard', 'faceDetail_beard' - Indicates whether or not the face has a beard, and the confidence level
-- in the determination.
--
-- 'emotions', 'faceDetail_emotions' - The emotions that appear to be expressed on the face, and the confidence
-- level in the determination. The API is only making a determination of
-- the physical appearance of a person\'s face. It is not a determination
-- of the person’s internal emotional state and should not be used in such
-- a way. For example, a person pretending to have a sad face might not be
-- sad emotionally.
--
-- 'eyeglasses', 'faceDetail_eyeglasses' - Indicates whether or not the face is wearing eye glasses, and the
-- confidence level in the determination.
--
-- 'gender', 'faceDetail_gender' - The predicted gender of a detected face.
--
-- 'boundingBox', 'faceDetail_boundingBox' - Bounding box of the face. Default attribute.
--
-- 'mouthOpen', 'faceDetail_mouthOpen' - Indicates whether or not the mouth on the face is open, and the
-- confidence level in the determination.
--
-- 'confidence', 'faceDetail_confidence' - Confidence level that the bounding box contains a face (and not a
-- different object such as a tree). Default attribute.
--
-- 'smile', 'faceDetail_smile' - Indicates whether or not the face is smiling, and the confidence level
-- in the determination.
--
-- 'eyesOpen', 'faceDetail_eyesOpen' - Indicates whether or not the eyes on the face are open, and the
-- confidence level in the determination.
--
-- 'mustache', 'faceDetail_mustache' - Indicates whether or not the face has a mustache, and the confidence
-- level in the determination.
--
-- 'quality', 'faceDetail_quality' - Identifies image brightness and sharpness. Default attribute.
newFaceDetail ::
  FaceDetail
newFaceDetail =
  FaceDetail'
    { sunglasses = Core.Nothing,
      ageRange = Core.Nothing,
      pose = Core.Nothing,
      landmarks = Core.Nothing,
      beard = Core.Nothing,
      emotions = Core.Nothing,
      eyeglasses = Core.Nothing,
      gender = Core.Nothing,
      boundingBox = Core.Nothing,
      mouthOpen = Core.Nothing,
      confidence = Core.Nothing,
      smile = Core.Nothing,
      eyesOpen = Core.Nothing,
      mustache = Core.Nothing,
      quality = Core.Nothing
    }

-- | Indicates whether or not the face is wearing sunglasses, and the
-- confidence level in the determination.
faceDetail_sunglasses :: Lens.Lens' FaceDetail (Core.Maybe Sunglasses)
faceDetail_sunglasses = Lens.lens (\FaceDetail' {sunglasses} -> sunglasses) (\s@FaceDetail' {} a -> s {sunglasses = a} :: FaceDetail)

-- | The estimated age range, in years, for the face. Low represents the
-- lowest estimated age and High represents the highest estimated age.
faceDetail_ageRange :: Lens.Lens' FaceDetail (Core.Maybe AgeRange)
faceDetail_ageRange = Lens.lens (\FaceDetail' {ageRange} -> ageRange) (\s@FaceDetail' {} a -> s {ageRange = a} :: FaceDetail)

-- | Indicates the pose of the face as determined by its pitch, roll, and
-- yaw. Default attribute.
faceDetail_pose :: Lens.Lens' FaceDetail (Core.Maybe Pose)
faceDetail_pose = Lens.lens (\FaceDetail' {pose} -> pose) (\s@FaceDetail' {} a -> s {pose = a} :: FaceDetail)

-- | Indicates the location of landmarks on the face. Default attribute.
faceDetail_landmarks :: Lens.Lens' FaceDetail (Core.Maybe [Landmark])
faceDetail_landmarks = Lens.lens (\FaceDetail' {landmarks} -> landmarks) (\s@FaceDetail' {} a -> s {landmarks = a} :: FaceDetail) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether or not the face has a beard, and the confidence level
-- in the determination.
faceDetail_beard :: Lens.Lens' FaceDetail (Core.Maybe Beard)
faceDetail_beard = Lens.lens (\FaceDetail' {beard} -> beard) (\s@FaceDetail' {} a -> s {beard = a} :: FaceDetail)

-- | The emotions that appear to be expressed on the face, and the confidence
-- level in the determination. The API is only making a determination of
-- the physical appearance of a person\'s face. It is not a determination
-- of the person’s internal emotional state and should not be used in such
-- a way. For example, a person pretending to have a sad face might not be
-- sad emotionally.
faceDetail_emotions :: Lens.Lens' FaceDetail (Core.Maybe [Emotion])
faceDetail_emotions = Lens.lens (\FaceDetail' {emotions} -> emotions) (\s@FaceDetail' {} a -> s {emotions = a} :: FaceDetail) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether or not the face is wearing eye glasses, and the
-- confidence level in the determination.
faceDetail_eyeglasses :: Lens.Lens' FaceDetail (Core.Maybe Eyeglasses)
faceDetail_eyeglasses = Lens.lens (\FaceDetail' {eyeglasses} -> eyeglasses) (\s@FaceDetail' {} a -> s {eyeglasses = a} :: FaceDetail)

-- | The predicted gender of a detected face.
faceDetail_gender :: Lens.Lens' FaceDetail (Core.Maybe Gender)
faceDetail_gender = Lens.lens (\FaceDetail' {gender} -> gender) (\s@FaceDetail' {} a -> s {gender = a} :: FaceDetail)

-- | Bounding box of the face. Default attribute.
faceDetail_boundingBox :: Lens.Lens' FaceDetail (Core.Maybe BoundingBox)
faceDetail_boundingBox = Lens.lens (\FaceDetail' {boundingBox} -> boundingBox) (\s@FaceDetail' {} a -> s {boundingBox = a} :: FaceDetail)

-- | Indicates whether or not the mouth on the face is open, and the
-- confidence level in the determination.
faceDetail_mouthOpen :: Lens.Lens' FaceDetail (Core.Maybe MouthOpen)
faceDetail_mouthOpen = Lens.lens (\FaceDetail' {mouthOpen} -> mouthOpen) (\s@FaceDetail' {} a -> s {mouthOpen = a} :: FaceDetail)

-- | Confidence level that the bounding box contains a face (and not a
-- different object such as a tree). Default attribute.
faceDetail_confidence :: Lens.Lens' FaceDetail (Core.Maybe Core.Double)
faceDetail_confidence = Lens.lens (\FaceDetail' {confidence} -> confidence) (\s@FaceDetail' {} a -> s {confidence = a} :: FaceDetail)

-- | Indicates whether or not the face is smiling, and the confidence level
-- in the determination.
faceDetail_smile :: Lens.Lens' FaceDetail (Core.Maybe Smile)
faceDetail_smile = Lens.lens (\FaceDetail' {smile} -> smile) (\s@FaceDetail' {} a -> s {smile = a} :: FaceDetail)

-- | Indicates whether or not the eyes on the face are open, and the
-- confidence level in the determination.
faceDetail_eyesOpen :: Lens.Lens' FaceDetail (Core.Maybe EyeOpen)
faceDetail_eyesOpen = Lens.lens (\FaceDetail' {eyesOpen} -> eyesOpen) (\s@FaceDetail' {} a -> s {eyesOpen = a} :: FaceDetail)

-- | Indicates whether or not the face has a mustache, and the confidence
-- level in the determination.
faceDetail_mustache :: Lens.Lens' FaceDetail (Core.Maybe Mustache)
faceDetail_mustache = Lens.lens (\FaceDetail' {mustache} -> mustache) (\s@FaceDetail' {} a -> s {mustache = a} :: FaceDetail)

-- | Identifies image brightness and sharpness. Default attribute.
faceDetail_quality :: Lens.Lens' FaceDetail (Core.Maybe ImageQuality)
faceDetail_quality = Lens.lens (\FaceDetail' {quality} -> quality) (\s@FaceDetail' {} a -> s {quality = a} :: FaceDetail)

instance Core.FromJSON FaceDetail where
  parseJSON =
    Core.withObject
      "FaceDetail"
      ( \x ->
          FaceDetail'
            Core.<$> (x Core..:? "Sunglasses")
            Core.<*> (x Core..:? "AgeRange")
            Core.<*> (x Core..:? "Pose")
            Core.<*> (x Core..:? "Landmarks" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Beard")
            Core.<*> (x Core..:? "Emotions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Eyeglasses")
            Core.<*> (x Core..:? "Gender")
            Core.<*> (x Core..:? "BoundingBox")
            Core.<*> (x Core..:? "MouthOpen")
            Core.<*> (x Core..:? "Confidence")
            Core.<*> (x Core..:? "Smile")
            Core.<*> (x Core..:? "EyesOpen")
            Core.<*> (x Core..:? "Mustache")
            Core.<*> (x Core..:? "Quality")
      )

instance Core.Hashable FaceDetail

instance Core.NFData FaceDetail
