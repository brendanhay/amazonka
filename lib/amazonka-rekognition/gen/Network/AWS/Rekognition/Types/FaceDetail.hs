{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceDetail
  ( FaceDetail (..),

    -- * Smart constructor
    mkFaceDetail,

    -- * Lenses
    fdAgeRange,
    fdSunglasses,
    fdMouthOpen,
    fdBoundingBox,
    fdEmotions,
    fdEyesOpen,
    fdPose,
    fdConfidence,
    fdGender,
    fdQuality,
    fdEyeglasses,
    fdBeard,
    fdMustache,
    fdSmile,
    fdLandmarks,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
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
-- A @FaceDetail@ object contains either the default facial attributes or all facial attributes. The default attributes are @BoundingBox@ , @Confidence@ , @Landmarks@ , @Pose@ , and @Quality@ .
-- 'GetFaceDetection' is the only Amazon Rekognition Video stored video operation that can return a @FaceDetail@ object with all attributes. To specify which attributes to return, use the @FaceAttributes@ input parameter for 'StartFaceDetection' . The following Amazon Rekognition Video operations return only the default attributes. The corresponding Start operations don't have a @FaceAttributes@ input parameter.
--
--     * GetCelebrityRecognition
--
--
--     * GetPersonTracking
--
--
--     * GetFaceSearch
--
--
-- The Amazon Rekognition Image 'DetectFaces' and 'IndexFaces' operations can return all facial attributes. To specify which attributes to return, use the @Attributes@ input parameter for @DetectFaces@ . For @IndexFaces@ , use the @DetectAttributes@ input parameter.
--
-- /See:/ 'mkFaceDetail' smart constructor.
data FaceDetail = FaceDetail'
  { ageRange :: Lude.Maybe AgeRange,
    sunglasses :: Lude.Maybe Sunglasses,
    mouthOpen :: Lude.Maybe MouthOpen,
    boundingBox :: Lude.Maybe BoundingBox,
    emotions :: Lude.Maybe [Emotion],
    eyesOpen :: Lude.Maybe EyeOpen,
    pose :: Lude.Maybe Pose,
    confidence :: Lude.Maybe Lude.Double,
    gender :: Lude.Maybe Gender,
    quality :: Lude.Maybe ImageQuality,
    eyeglasses :: Lude.Maybe Eyeglasses,
    beard :: Lude.Maybe Beard,
    mustache :: Lude.Maybe Mustache,
    smile :: Lude.Maybe Smile,
    landmarks :: Lude.Maybe [Landmark]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FaceDetail' with the minimum fields required to make a request.
--
-- * 'ageRange' - The estimated age range, in years, for the face. Low represents the lowest estimated age and High represents the highest estimated age.
-- * 'beard' - Indicates whether or not the face has a beard, and the confidence level in the determination.
-- * 'boundingBox' - Bounding box of the face. Default attribute.
-- * 'confidence' - Confidence level that the bounding box contains a face (and not a different object such as a tree). Default attribute.
-- * 'emotions' - The emotions that appear to be expressed on the face, and the confidence level in the determination. The API is only making a determination of the physical appearance of a person's face. It is not a determination of the person’s internal emotional state and should not be used in such a way. For example, a person pretending to have a sad face might not be sad emotionally.
-- * 'eyeglasses' - Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.
-- * 'eyesOpen' - Indicates whether or not the eyes on the face are open, and the confidence level in the determination.
-- * 'gender' - The predicted gender of a detected face.
-- * 'landmarks' - Indicates the location of landmarks on the face. Default attribute.
-- * 'mouthOpen' - Indicates whether or not the mouth on the face is open, and the confidence level in the determination.
-- * 'mustache' - Indicates whether or not the face has a mustache, and the confidence level in the determination.
-- * 'pose' - Indicates the pose of the face as determined by its pitch, roll, and yaw. Default attribute.
-- * 'quality' - Identifies image brightness and sharpness. Default attribute.
-- * 'smile' - Indicates whether or not the face is smiling, and the confidence level in the determination.
-- * 'sunglasses' - Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.
mkFaceDetail ::
  FaceDetail
mkFaceDetail =
  FaceDetail'
    { ageRange = Lude.Nothing,
      sunglasses = Lude.Nothing,
      mouthOpen = Lude.Nothing,
      boundingBox = Lude.Nothing,
      emotions = Lude.Nothing,
      eyesOpen = Lude.Nothing,
      pose = Lude.Nothing,
      confidence = Lude.Nothing,
      gender = Lude.Nothing,
      quality = Lude.Nothing,
      eyeglasses = Lude.Nothing,
      beard = Lude.Nothing,
      mustache = Lude.Nothing,
      smile = Lude.Nothing,
      landmarks = Lude.Nothing
    }

-- | The estimated age range, in years, for the face. Low represents the lowest estimated age and High represents the highest estimated age.
--
-- /Note:/ Consider using 'ageRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdAgeRange :: Lens.Lens' FaceDetail (Lude.Maybe AgeRange)
fdAgeRange = Lens.lens (ageRange :: FaceDetail -> Lude.Maybe AgeRange) (\s a -> s {ageRange = a} :: FaceDetail)
{-# DEPRECATED fdAgeRange "Use generic-lens or generic-optics with 'ageRange' instead." #-}

-- | Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.
--
-- /Note:/ Consider using 'sunglasses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdSunglasses :: Lens.Lens' FaceDetail (Lude.Maybe Sunglasses)
fdSunglasses = Lens.lens (sunglasses :: FaceDetail -> Lude.Maybe Sunglasses) (\s a -> s {sunglasses = a} :: FaceDetail)
{-# DEPRECATED fdSunglasses "Use generic-lens or generic-optics with 'sunglasses' instead." #-}

-- | Indicates whether or not the mouth on the face is open, and the confidence level in the determination.
--
-- /Note:/ Consider using 'mouthOpen' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdMouthOpen :: Lens.Lens' FaceDetail (Lude.Maybe MouthOpen)
fdMouthOpen = Lens.lens (mouthOpen :: FaceDetail -> Lude.Maybe MouthOpen) (\s a -> s {mouthOpen = a} :: FaceDetail)
{-# DEPRECATED fdMouthOpen "Use generic-lens or generic-optics with 'mouthOpen' instead." #-}

-- | Bounding box of the face. Default attribute.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdBoundingBox :: Lens.Lens' FaceDetail (Lude.Maybe BoundingBox)
fdBoundingBox = Lens.lens (boundingBox :: FaceDetail -> Lude.Maybe BoundingBox) (\s a -> s {boundingBox = a} :: FaceDetail)
{-# DEPRECATED fdBoundingBox "Use generic-lens or generic-optics with 'boundingBox' instead." #-}

-- | The emotions that appear to be expressed on the face, and the confidence level in the determination. The API is only making a determination of the physical appearance of a person's face. It is not a determination of the person’s internal emotional state and should not be used in such a way. For example, a person pretending to have a sad face might not be sad emotionally.
--
-- /Note:/ Consider using 'emotions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdEmotions :: Lens.Lens' FaceDetail (Lude.Maybe [Emotion])
fdEmotions = Lens.lens (emotions :: FaceDetail -> Lude.Maybe [Emotion]) (\s a -> s {emotions = a} :: FaceDetail)
{-# DEPRECATED fdEmotions "Use generic-lens or generic-optics with 'emotions' instead." #-}

-- | Indicates whether or not the eyes on the face are open, and the confidence level in the determination.
--
-- /Note:/ Consider using 'eyesOpen' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdEyesOpen :: Lens.Lens' FaceDetail (Lude.Maybe EyeOpen)
fdEyesOpen = Lens.lens (eyesOpen :: FaceDetail -> Lude.Maybe EyeOpen) (\s a -> s {eyesOpen = a} :: FaceDetail)
{-# DEPRECATED fdEyesOpen "Use generic-lens or generic-optics with 'eyesOpen' instead." #-}

-- | Indicates the pose of the face as determined by its pitch, roll, and yaw. Default attribute.
--
-- /Note:/ Consider using 'pose' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdPose :: Lens.Lens' FaceDetail (Lude.Maybe Pose)
fdPose = Lens.lens (pose :: FaceDetail -> Lude.Maybe Pose) (\s a -> s {pose = a} :: FaceDetail)
{-# DEPRECATED fdPose "Use generic-lens or generic-optics with 'pose' instead." #-}

-- | Confidence level that the bounding box contains a face (and not a different object such as a tree). Default attribute.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdConfidence :: Lens.Lens' FaceDetail (Lude.Maybe Lude.Double)
fdConfidence = Lens.lens (confidence :: FaceDetail -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: FaceDetail)
{-# DEPRECATED fdConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | The predicted gender of a detected face.
--
-- /Note:/ Consider using 'gender' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdGender :: Lens.Lens' FaceDetail (Lude.Maybe Gender)
fdGender = Lens.lens (gender :: FaceDetail -> Lude.Maybe Gender) (\s a -> s {gender = a} :: FaceDetail)
{-# DEPRECATED fdGender "Use generic-lens or generic-optics with 'gender' instead." #-}

-- | Identifies image brightness and sharpness. Default attribute.
--
-- /Note:/ Consider using 'quality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdQuality :: Lens.Lens' FaceDetail (Lude.Maybe ImageQuality)
fdQuality = Lens.lens (quality :: FaceDetail -> Lude.Maybe ImageQuality) (\s a -> s {quality = a} :: FaceDetail)
{-# DEPRECATED fdQuality "Use generic-lens or generic-optics with 'quality' instead." #-}

-- | Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.
--
-- /Note:/ Consider using 'eyeglasses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdEyeglasses :: Lens.Lens' FaceDetail (Lude.Maybe Eyeglasses)
fdEyeglasses = Lens.lens (eyeglasses :: FaceDetail -> Lude.Maybe Eyeglasses) (\s a -> s {eyeglasses = a} :: FaceDetail)
{-# DEPRECATED fdEyeglasses "Use generic-lens or generic-optics with 'eyeglasses' instead." #-}

-- | Indicates whether or not the face has a beard, and the confidence level in the determination.
--
-- /Note:/ Consider using 'beard' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdBeard :: Lens.Lens' FaceDetail (Lude.Maybe Beard)
fdBeard = Lens.lens (beard :: FaceDetail -> Lude.Maybe Beard) (\s a -> s {beard = a} :: FaceDetail)
{-# DEPRECATED fdBeard "Use generic-lens or generic-optics with 'beard' instead." #-}

-- | Indicates whether or not the face has a mustache, and the confidence level in the determination.
--
-- /Note:/ Consider using 'mustache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdMustache :: Lens.Lens' FaceDetail (Lude.Maybe Mustache)
fdMustache = Lens.lens (mustache :: FaceDetail -> Lude.Maybe Mustache) (\s a -> s {mustache = a} :: FaceDetail)
{-# DEPRECATED fdMustache "Use generic-lens or generic-optics with 'mustache' instead." #-}

-- | Indicates whether or not the face is smiling, and the confidence level in the determination.
--
-- /Note:/ Consider using 'smile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdSmile :: Lens.Lens' FaceDetail (Lude.Maybe Smile)
fdSmile = Lens.lens (smile :: FaceDetail -> Lude.Maybe Smile) (\s a -> s {smile = a} :: FaceDetail)
{-# DEPRECATED fdSmile "Use generic-lens or generic-optics with 'smile' instead." #-}

-- | Indicates the location of landmarks on the face. Default attribute.
--
-- /Note:/ Consider using 'landmarks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdLandmarks :: Lens.Lens' FaceDetail (Lude.Maybe [Landmark])
fdLandmarks = Lens.lens (landmarks :: FaceDetail -> Lude.Maybe [Landmark]) (\s a -> s {landmarks = a} :: FaceDetail)
{-# DEPRECATED fdLandmarks "Use generic-lens or generic-optics with 'landmarks' instead." #-}

instance Lude.FromJSON FaceDetail where
  parseJSON =
    Lude.withObject
      "FaceDetail"
      ( \x ->
          FaceDetail'
            Lude.<$> (x Lude..:? "AgeRange")
            Lude.<*> (x Lude..:? "Sunglasses")
            Lude.<*> (x Lude..:? "MouthOpen")
            Lude.<*> (x Lude..:? "BoundingBox")
            Lude.<*> (x Lude..:? "Emotions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EyesOpen")
            Lude.<*> (x Lude..:? "Pose")
            Lude.<*> (x Lude..:? "Confidence")
            Lude.<*> (x Lude..:? "Gender")
            Lude.<*> (x Lude..:? "Quality")
            Lude.<*> (x Lude..:? "Eyeglasses")
            Lude.<*> (x Lude..:? "Beard")
            Lude.<*> (x Lude..:? "Mustache")
            Lude.<*> (x Lude..:? "Smile")
            Lude.<*> (x Lude..:? "Landmarks" Lude..!= Lude.mempty)
      )
