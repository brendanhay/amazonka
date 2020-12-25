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
    fdBeard,
    fdBoundingBox,
    fdConfidence,
    fdEmotions,
    fdEyeglasses,
    fdEyesOpen,
    fdGender,
    fdLandmarks,
    fdMouthOpen,
    fdMustache,
    fdPose,
    fdQuality,
    fdSmile,
    fdSunglasses,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.AgeRange as Types
import qualified Network.AWS.Rekognition.Types.Beard as Types
import qualified Network.AWS.Rekognition.Types.BoundingBox as Types
import qualified Network.AWS.Rekognition.Types.Emotion as Types
import qualified Network.AWS.Rekognition.Types.EyeOpen as Types
import qualified Network.AWS.Rekognition.Types.Eyeglasses as Types
import qualified Network.AWS.Rekognition.Types.Gender as Types
import qualified Network.AWS.Rekognition.Types.ImageQuality as Types
import qualified Network.AWS.Rekognition.Types.Landmark as Types
import qualified Network.AWS.Rekognition.Types.MouthOpen as Types
import qualified Network.AWS.Rekognition.Types.Mustache as Types
import qualified Network.AWS.Rekognition.Types.Pose as Types
import qualified Network.AWS.Rekognition.Types.Smile as Types
import qualified Network.AWS.Rekognition.Types.Sunglasses as Types

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
  { -- | The estimated age range, in years, for the face. Low represents the lowest estimated age and High represents the highest estimated age.
    ageRange :: Core.Maybe Types.AgeRange,
    -- | Indicates whether or not the face has a beard, and the confidence level in the determination.
    beard :: Core.Maybe Types.Beard,
    -- | Bounding box of the face. Default attribute.
    boundingBox :: Core.Maybe Types.BoundingBox,
    -- | Confidence level that the bounding box contains a face (and not a different object such as a tree). Default attribute.
    confidence :: Core.Maybe Core.Double,
    -- | The emotions that appear to be expressed on the face, and the confidence level in the determination. The API is only making a determination of the physical appearance of a person's face. It is not a determination of the person’s internal emotional state and should not be used in such a way. For example, a person pretending to have a sad face might not be sad emotionally.
    emotions :: Core.Maybe [Types.Emotion],
    -- | Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.
    eyeglasses :: Core.Maybe Types.Eyeglasses,
    -- | Indicates whether or not the eyes on the face are open, and the confidence level in the determination.
    eyesOpen :: Core.Maybe Types.EyeOpen,
    -- | The predicted gender of a detected face.
    gender :: Core.Maybe Types.Gender,
    -- | Indicates the location of landmarks on the face. Default attribute.
    landmarks :: Core.Maybe [Types.Landmark],
    -- | Indicates whether or not the mouth on the face is open, and the confidence level in the determination.
    mouthOpen :: Core.Maybe Types.MouthOpen,
    -- | Indicates whether or not the face has a mustache, and the confidence level in the determination.
    mustache :: Core.Maybe Types.Mustache,
    -- | Indicates the pose of the face as determined by its pitch, roll, and yaw. Default attribute.
    pose :: Core.Maybe Types.Pose,
    -- | Identifies image brightness and sharpness. Default attribute.
    quality :: Core.Maybe Types.ImageQuality,
    -- | Indicates whether or not the face is smiling, and the confidence level in the determination.
    smile :: Core.Maybe Types.Smile,
    -- | Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.
    sunglasses :: Core.Maybe Types.Sunglasses
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FaceDetail' value with any optional fields omitted.
mkFaceDetail ::
  FaceDetail
mkFaceDetail =
  FaceDetail'
    { ageRange = Core.Nothing,
      beard = Core.Nothing,
      boundingBox = Core.Nothing,
      confidence = Core.Nothing,
      emotions = Core.Nothing,
      eyeglasses = Core.Nothing,
      eyesOpen = Core.Nothing,
      gender = Core.Nothing,
      landmarks = Core.Nothing,
      mouthOpen = Core.Nothing,
      mustache = Core.Nothing,
      pose = Core.Nothing,
      quality = Core.Nothing,
      smile = Core.Nothing,
      sunglasses = Core.Nothing
    }

-- | The estimated age range, in years, for the face. Low represents the lowest estimated age and High represents the highest estimated age.
--
-- /Note:/ Consider using 'ageRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdAgeRange :: Lens.Lens' FaceDetail (Core.Maybe Types.AgeRange)
fdAgeRange = Lens.field @"ageRange"
{-# DEPRECATED fdAgeRange "Use generic-lens or generic-optics with 'ageRange' instead." #-}

-- | Indicates whether or not the face has a beard, and the confidence level in the determination.
--
-- /Note:/ Consider using 'beard' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdBeard :: Lens.Lens' FaceDetail (Core.Maybe Types.Beard)
fdBeard = Lens.field @"beard"
{-# DEPRECATED fdBeard "Use generic-lens or generic-optics with 'beard' instead." #-}

-- | Bounding box of the face. Default attribute.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdBoundingBox :: Lens.Lens' FaceDetail (Core.Maybe Types.BoundingBox)
fdBoundingBox = Lens.field @"boundingBox"
{-# DEPRECATED fdBoundingBox "Use generic-lens or generic-optics with 'boundingBox' instead." #-}

-- | Confidence level that the bounding box contains a face (and not a different object such as a tree). Default attribute.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdConfidence :: Lens.Lens' FaceDetail (Core.Maybe Core.Double)
fdConfidence = Lens.field @"confidence"
{-# DEPRECATED fdConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | The emotions that appear to be expressed on the face, and the confidence level in the determination. The API is only making a determination of the physical appearance of a person's face. It is not a determination of the person’s internal emotional state and should not be used in such a way. For example, a person pretending to have a sad face might not be sad emotionally.
--
-- /Note:/ Consider using 'emotions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdEmotions :: Lens.Lens' FaceDetail (Core.Maybe [Types.Emotion])
fdEmotions = Lens.field @"emotions"
{-# DEPRECATED fdEmotions "Use generic-lens or generic-optics with 'emotions' instead." #-}

-- | Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.
--
-- /Note:/ Consider using 'eyeglasses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdEyeglasses :: Lens.Lens' FaceDetail (Core.Maybe Types.Eyeglasses)
fdEyeglasses = Lens.field @"eyeglasses"
{-# DEPRECATED fdEyeglasses "Use generic-lens or generic-optics with 'eyeglasses' instead." #-}

-- | Indicates whether or not the eyes on the face are open, and the confidence level in the determination.
--
-- /Note:/ Consider using 'eyesOpen' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdEyesOpen :: Lens.Lens' FaceDetail (Core.Maybe Types.EyeOpen)
fdEyesOpen = Lens.field @"eyesOpen"
{-# DEPRECATED fdEyesOpen "Use generic-lens or generic-optics with 'eyesOpen' instead." #-}

-- | The predicted gender of a detected face.
--
-- /Note:/ Consider using 'gender' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdGender :: Lens.Lens' FaceDetail (Core.Maybe Types.Gender)
fdGender = Lens.field @"gender"
{-# DEPRECATED fdGender "Use generic-lens or generic-optics with 'gender' instead." #-}

-- | Indicates the location of landmarks on the face. Default attribute.
--
-- /Note:/ Consider using 'landmarks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdLandmarks :: Lens.Lens' FaceDetail (Core.Maybe [Types.Landmark])
fdLandmarks = Lens.field @"landmarks"
{-# DEPRECATED fdLandmarks "Use generic-lens or generic-optics with 'landmarks' instead." #-}

-- | Indicates whether or not the mouth on the face is open, and the confidence level in the determination.
--
-- /Note:/ Consider using 'mouthOpen' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdMouthOpen :: Lens.Lens' FaceDetail (Core.Maybe Types.MouthOpen)
fdMouthOpen = Lens.field @"mouthOpen"
{-# DEPRECATED fdMouthOpen "Use generic-lens or generic-optics with 'mouthOpen' instead." #-}

-- | Indicates whether or not the face has a mustache, and the confidence level in the determination.
--
-- /Note:/ Consider using 'mustache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdMustache :: Lens.Lens' FaceDetail (Core.Maybe Types.Mustache)
fdMustache = Lens.field @"mustache"
{-# DEPRECATED fdMustache "Use generic-lens or generic-optics with 'mustache' instead." #-}

-- | Indicates the pose of the face as determined by its pitch, roll, and yaw. Default attribute.
--
-- /Note:/ Consider using 'pose' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdPose :: Lens.Lens' FaceDetail (Core.Maybe Types.Pose)
fdPose = Lens.field @"pose"
{-# DEPRECATED fdPose "Use generic-lens or generic-optics with 'pose' instead." #-}

-- | Identifies image brightness and sharpness. Default attribute.
--
-- /Note:/ Consider using 'quality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdQuality :: Lens.Lens' FaceDetail (Core.Maybe Types.ImageQuality)
fdQuality = Lens.field @"quality"
{-# DEPRECATED fdQuality "Use generic-lens or generic-optics with 'quality' instead." #-}

-- | Indicates whether or not the face is smiling, and the confidence level in the determination.
--
-- /Note:/ Consider using 'smile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdSmile :: Lens.Lens' FaceDetail (Core.Maybe Types.Smile)
fdSmile = Lens.field @"smile"
{-# DEPRECATED fdSmile "Use generic-lens or generic-optics with 'smile' instead." #-}

-- | Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.
--
-- /Note:/ Consider using 'sunglasses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdSunglasses :: Lens.Lens' FaceDetail (Core.Maybe Types.Sunglasses)
fdSunglasses = Lens.field @"sunglasses"
{-# DEPRECATED fdSunglasses "Use generic-lens or generic-optics with 'sunglasses' instead." #-}

instance Core.FromJSON FaceDetail where
  parseJSON =
    Core.withObject "FaceDetail" Core.$
      \x ->
        FaceDetail'
          Core.<$> (x Core..:? "AgeRange")
          Core.<*> (x Core..:? "Beard")
          Core.<*> (x Core..:? "BoundingBox")
          Core.<*> (x Core..:? "Confidence")
          Core.<*> (x Core..:? "Emotions")
          Core.<*> (x Core..:? "Eyeglasses")
          Core.<*> (x Core..:? "EyesOpen")
          Core.<*> (x Core..:? "Gender")
          Core.<*> (x Core..:? "Landmarks")
          Core.<*> (x Core..:? "MouthOpen")
          Core.<*> (x Core..:? "Mustache")
          Core.<*> (x Core..:? "Pose")
          Core.<*> (x Core..:? "Quality")
          Core.<*> (x Core..:? "Smile")
          Core.<*> (x Core..:? "Sunglasses")
