{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.SearchFacesByImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a given input image, first detects the largest face in the image, and then searches the specified collection for matching faces. The operation compares the features of the input face with faces in the specified collection. 
--
-- You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. 
-- The response returns an array of faces that match, ordered by similarity score with the highest similarity first. More specifically, it is an array of metadata for each face match found. Along with the metadata, the response also includes a @similarity@ indicating how similar the face is to the input face. In the response, the operation also returns the bounding box (and a confidence level that the bounding box contains a face) of the face that Amazon Rekognition used for the input image. 
-- For an example, Searching for a Face Using an Image in the Amazon Rekognition Developer Guide.
-- The @QualityFilter@ input parameter allows you to filter out detected faces that don’t meet a required quality bar. The quality bar is based on a variety of common use cases. Use @QualityFilter@ to set the quality bar for filtering by specifying @LOW@ , @MEDIUM@ , or @HIGH@ . If you do not want to filter detected faces, specify @NONE@ . The default value is @NONE@ .
-- This operation requires permissions to perform the @rekognition:SearchFacesByImage@ action.
module Network.AWS.Rekognition.SearchFacesByImage
    (
    -- * Creating a request
      SearchFacesByImage (..)
    , mkSearchFacesByImage
    -- ** Request lenses
    , sfbiCollectionId
    , sfbiImage
    , sfbiFaceMatchThreshold
    , sfbiMaxFaces
    , sfbiQualityFilter

    -- * Destructuring the response
    , SearchFacesByImageResponse (..)
    , mkSearchFacesByImageResponse
    -- ** Response lenses
    , sfbirrsFaceMatches
    , sfbirrsFaceModelVersion
    , sfbirrsSearchedFaceBoundingBox
    , sfbirrsSearchedFaceConfidence
    , sfbirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchFacesByImage' smart constructor.
data SearchFacesByImage = SearchFacesByImage'
  { collectionId :: Types.CollectionId
    -- ^ ID of the collection to search.
  , image :: Types.Image
    -- ^ The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported. 
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
  , faceMatchThreshold :: Core.Maybe Core.Double
    -- ^ (Optional) Specifies the minimum confidence in the face match to return. For example, don't return any matches where confidence in matches is less than 70%. The default value is 80%.
  , maxFaces :: Core.Maybe Core.Natural
    -- ^ Maximum number of faces to return. The operation returns the maximum number of faces with the highest confidence in the match.
  , qualityFilter :: Core.Maybe Types.QualityFilter
    -- ^ A filter that specifies a quality bar for how much filtering is done to identify faces. Filtered faces aren't searched for in the collection. If you specify @AUTO@ , Amazon Rekognition chooses the quality bar. If you specify @LOW@ , @MEDIUM@ , or @HIGH@ , filtering removes all faces that don’t meet the chosen quality bar. The quality bar is based on a variety of common use cases. Low-quality detections can occur for a number of reasons. Some examples are an object that's misidentified as a face, a face that's too blurry, or a face with a pose that's too extreme to use. If you specify @NONE@ , no filtering is performed. The default value is @NONE@ . 
--
-- To use quality filtering, the collection you are using must be associated with version 3 of the face model or higher.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchFacesByImage' value with any optional fields omitted.
mkSearchFacesByImage
    :: Types.CollectionId -- ^ 'collectionId'
    -> Types.Image -- ^ 'image'
    -> SearchFacesByImage
mkSearchFacesByImage collectionId image
  = SearchFacesByImage'{collectionId, image,
                        faceMatchThreshold = Core.Nothing, maxFaces = Core.Nothing,
                        qualityFilter = Core.Nothing}

-- | ID of the collection to search.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbiCollectionId :: Lens.Lens' SearchFacesByImage Types.CollectionId
sfbiCollectionId = Lens.field @"collectionId"
{-# INLINEABLE sfbiCollectionId #-}
{-# DEPRECATED collectionId "Use generic-lens or generic-optics with 'collectionId' instead"  #-}

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported. 
--
-- If you are using an AWS SDK to call Amazon Rekognition, you might not need to base64-encode image bytes passed using the @Bytes@ field. For more information, see Images in the Amazon Rekognition developer guide.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbiImage :: Lens.Lens' SearchFacesByImage Types.Image
sfbiImage = Lens.field @"image"
{-# INLINEABLE sfbiImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | (Optional) Specifies the minimum confidence in the face match to return. For example, don't return any matches where confidence in matches is less than 70%. The default value is 80%.
--
-- /Note:/ Consider using 'faceMatchThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbiFaceMatchThreshold :: Lens.Lens' SearchFacesByImage (Core.Maybe Core.Double)
sfbiFaceMatchThreshold = Lens.field @"faceMatchThreshold"
{-# INLINEABLE sfbiFaceMatchThreshold #-}
{-# DEPRECATED faceMatchThreshold "Use generic-lens or generic-optics with 'faceMatchThreshold' instead"  #-}

-- | Maximum number of faces to return. The operation returns the maximum number of faces with the highest confidence in the match.
--
-- /Note:/ Consider using 'maxFaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbiMaxFaces :: Lens.Lens' SearchFacesByImage (Core.Maybe Core.Natural)
sfbiMaxFaces = Lens.field @"maxFaces"
{-# INLINEABLE sfbiMaxFaces #-}
{-# DEPRECATED maxFaces "Use generic-lens or generic-optics with 'maxFaces' instead"  #-}

-- | A filter that specifies a quality bar for how much filtering is done to identify faces. Filtered faces aren't searched for in the collection. If you specify @AUTO@ , Amazon Rekognition chooses the quality bar. If you specify @LOW@ , @MEDIUM@ , or @HIGH@ , filtering removes all faces that don’t meet the chosen quality bar. The quality bar is based on a variety of common use cases. Low-quality detections can occur for a number of reasons. Some examples are an object that's misidentified as a face, a face that's too blurry, or a face with a pose that's too extreme to use. If you specify @NONE@ , no filtering is performed. The default value is @NONE@ . 
--
-- To use quality filtering, the collection you are using must be associated with version 3 of the face model or higher.
--
-- /Note:/ Consider using 'qualityFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbiQualityFilter :: Lens.Lens' SearchFacesByImage (Core.Maybe Types.QualityFilter)
sfbiQualityFilter = Lens.field @"qualityFilter"
{-# INLINEABLE sfbiQualityFilter #-}
{-# DEPRECATED qualityFilter "Use generic-lens or generic-optics with 'qualityFilter' instead"  #-}

instance Core.ToQuery SearchFacesByImage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SearchFacesByImage where
        toHeaders SearchFacesByImage{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.SearchFacesByImage")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SearchFacesByImage where
        toJSON SearchFacesByImage{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CollectionId" Core..= collectionId),
                  Core.Just ("Image" Core..= image),
                  ("FaceMatchThreshold" Core..=) Core.<$> faceMatchThreshold,
                  ("MaxFaces" Core..=) Core.<$> maxFaces,
                  ("QualityFilter" Core..=) Core.<$> qualityFilter])

instance Core.AWSRequest SearchFacesByImage where
        type Rs SearchFacesByImage = SearchFacesByImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SearchFacesByImageResponse' Core.<$>
                   (x Core..:? "FaceMatches") Core.<*> x Core..:? "FaceModelVersion"
                     Core.<*> x Core..:? "SearchedFaceBoundingBox"
                     Core.<*> x Core..:? "SearchedFaceConfidence"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSearchFacesByImageResponse' smart constructor.
data SearchFacesByImageResponse = SearchFacesByImageResponse'
  { faceMatches :: Core.Maybe [Types.FaceMatch]
    -- ^ An array of faces that match the input face, along with the confidence in the match.
  , faceModelVersion :: Core.Maybe Core.Text
    -- ^ Version number of the face detection model associated with the input collection (@CollectionId@ ).
  , searchedFaceBoundingBox :: Core.Maybe Types.BoundingBox
    -- ^ The bounding box around the face in the input image that Amazon Rekognition used for the search.
  , searchedFaceConfidence :: Core.Maybe Core.Double
    -- ^ The level of confidence that the @searchedFaceBoundingBox@ , contains a face.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchFacesByImageResponse' value with any optional fields omitted.
mkSearchFacesByImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SearchFacesByImageResponse
mkSearchFacesByImageResponse responseStatus
  = SearchFacesByImageResponse'{faceMatches = Core.Nothing,
                                faceModelVersion = Core.Nothing,
                                searchedFaceBoundingBox = Core.Nothing,
                                searchedFaceConfidence = Core.Nothing, responseStatus}

-- | An array of faces that match the input face, along with the confidence in the match.
--
-- /Note:/ Consider using 'faceMatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbirrsFaceMatches :: Lens.Lens' SearchFacesByImageResponse (Core.Maybe [Types.FaceMatch])
sfbirrsFaceMatches = Lens.field @"faceMatches"
{-# INLINEABLE sfbirrsFaceMatches #-}
{-# DEPRECATED faceMatches "Use generic-lens or generic-optics with 'faceMatches' instead"  #-}

-- | Version number of the face detection model associated with the input collection (@CollectionId@ ).
--
-- /Note:/ Consider using 'faceModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbirrsFaceModelVersion :: Lens.Lens' SearchFacesByImageResponse (Core.Maybe Core.Text)
sfbirrsFaceModelVersion = Lens.field @"faceModelVersion"
{-# INLINEABLE sfbirrsFaceModelVersion #-}
{-# DEPRECATED faceModelVersion "Use generic-lens or generic-optics with 'faceModelVersion' instead"  #-}

-- | The bounding box around the face in the input image that Amazon Rekognition used for the search.
--
-- /Note:/ Consider using 'searchedFaceBoundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbirrsSearchedFaceBoundingBox :: Lens.Lens' SearchFacesByImageResponse (Core.Maybe Types.BoundingBox)
sfbirrsSearchedFaceBoundingBox = Lens.field @"searchedFaceBoundingBox"
{-# INLINEABLE sfbirrsSearchedFaceBoundingBox #-}
{-# DEPRECATED searchedFaceBoundingBox "Use generic-lens or generic-optics with 'searchedFaceBoundingBox' instead"  #-}

-- | The level of confidence that the @searchedFaceBoundingBox@ , contains a face.
--
-- /Note:/ Consider using 'searchedFaceConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbirrsSearchedFaceConfidence :: Lens.Lens' SearchFacesByImageResponse (Core.Maybe Core.Double)
sfbirrsSearchedFaceConfidence = Lens.field @"searchedFaceConfidence"
{-# INLINEABLE sfbirrsSearchedFaceConfidence #-}
{-# DEPRECATED searchedFaceConfidence "Use generic-lens or generic-optics with 'searchedFaceConfidence' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfbirrsResponseStatus :: Lens.Lens' SearchFacesByImageResponse Core.Int
sfbirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sfbirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
