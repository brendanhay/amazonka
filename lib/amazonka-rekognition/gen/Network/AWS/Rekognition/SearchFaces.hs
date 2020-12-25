{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.SearchFaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a given input face ID, searches for matching faces in the collection the face belongs to. You get a face ID when you add a face to the collection using the 'IndexFaces' operation. The operation compares the features of the input face with faces in the specified collection.
--
-- The operation response returns an array of faces that match, ordered by similarity score with the highest similarity first. More specifically, it is an array of metadata for each face match that is found. Along with the metadata, the response also includes a @confidence@ value for each face match, indicating the confidence that the specific face matches the input face.
-- For an example, see Searching for a Face Using Its Face ID in the Amazon Rekognition Developer Guide.
-- This operation requires permissions to perform the @rekognition:SearchFaces@ action.
module Network.AWS.Rekognition.SearchFaces
  ( -- * Creating a request
    SearchFaces (..),
    mkSearchFaces,

    -- ** Request lenses
    sfCollectionId,
    sfFaceId,
    sfFaceMatchThreshold,
    sfMaxFaces,

    -- * Destructuring the response
    SearchFacesResponse (..),
    mkSearchFacesResponse,

    -- ** Response lenses
    sfrrsFaceMatches,
    sfrrsFaceModelVersion,
    sfrrsSearchedFaceId,
    sfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchFaces' smart constructor.
data SearchFaces = SearchFaces'
  { -- | ID of the collection the face belongs to.
    collectionId :: Types.CollectionId,
    -- | ID of a face to find matches for in the collection.
    faceId :: Types.FaceId,
    -- | Optional value specifying the minimum confidence in the face match to return. For example, don't return any matches where confidence in matches is less than 70%. The default value is 80%.
    faceMatchThreshold :: Core.Maybe Core.Double,
    -- | Maximum number of faces to return. The operation returns the maximum number of faces with the highest confidence in the match.
    maxFaces :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchFaces' value with any optional fields omitted.
mkSearchFaces ::
  -- | 'collectionId'
  Types.CollectionId ->
  -- | 'faceId'
  Types.FaceId ->
  SearchFaces
mkSearchFaces collectionId faceId =
  SearchFaces'
    { collectionId,
      faceId,
      faceMatchThreshold = Core.Nothing,
      maxFaces = Core.Nothing
    }

-- | ID of the collection the face belongs to.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCollectionId :: Lens.Lens' SearchFaces Types.CollectionId
sfCollectionId = Lens.field @"collectionId"
{-# DEPRECATED sfCollectionId "Use generic-lens or generic-optics with 'collectionId' instead." #-}

-- | ID of a face to find matches for in the collection.
--
-- /Note:/ Consider using 'faceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfFaceId :: Lens.Lens' SearchFaces Types.FaceId
sfFaceId = Lens.field @"faceId"
{-# DEPRECATED sfFaceId "Use generic-lens or generic-optics with 'faceId' instead." #-}

-- | Optional value specifying the minimum confidence in the face match to return. For example, don't return any matches where confidence in matches is less than 70%. The default value is 80%.
--
-- /Note:/ Consider using 'faceMatchThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfFaceMatchThreshold :: Lens.Lens' SearchFaces (Core.Maybe Core.Double)
sfFaceMatchThreshold = Lens.field @"faceMatchThreshold"
{-# DEPRECATED sfFaceMatchThreshold "Use generic-lens or generic-optics with 'faceMatchThreshold' instead." #-}

-- | Maximum number of faces to return. The operation returns the maximum number of faces with the highest confidence in the match.
--
-- /Note:/ Consider using 'maxFaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfMaxFaces :: Lens.Lens' SearchFaces (Core.Maybe Core.Natural)
sfMaxFaces = Lens.field @"maxFaces"
{-# DEPRECATED sfMaxFaces "Use generic-lens or generic-optics with 'maxFaces' instead." #-}

instance Core.FromJSON SearchFaces where
  toJSON SearchFaces {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CollectionId" Core..= collectionId),
            Core.Just ("FaceId" Core..= faceId),
            ("FaceMatchThreshold" Core..=) Core.<$> faceMatchThreshold,
            ("MaxFaces" Core..=) Core.<$> maxFaces
          ]
      )

instance Core.AWSRequest SearchFaces where
  type Rs SearchFaces = SearchFacesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "RekognitionService.SearchFaces")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchFacesResponse'
            Core.<$> (x Core..:? "FaceMatches")
            Core.<*> (x Core..:? "FaceModelVersion")
            Core.<*> (x Core..:? "SearchedFaceId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSearchFacesResponse' smart constructor.
data SearchFacesResponse = SearchFacesResponse'
  { -- | An array of faces that matched the input face, along with the confidence in the match.
    faceMatches :: Core.Maybe [Types.FaceMatch],
    -- | Version number of the face detection model associated with the input collection (@CollectionId@ ).
    faceModelVersion :: Core.Maybe Types.String,
    -- | ID of the face that was searched for matches in a collection.
    searchedFaceId :: Core.Maybe Types.FaceId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchFacesResponse' value with any optional fields omitted.
mkSearchFacesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SearchFacesResponse
mkSearchFacesResponse responseStatus =
  SearchFacesResponse'
    { faceMatches = Core.Nothing,
      faceModelVersion = Core.Nothing,
      searchedFaceId = Core.Nothing,
      responseStatus
    }

-- | An array of faces that matched the input face, along with the confidence in the match.
--
-- /Note:/ Consider using 'faceMatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrrsFaceMatches :: Lens.Lens' SearchFacesResponse (Core.Maybe [Types.FaceMatch])
sfrrsFaceMatches = Lens.field @"faceMatches"
{-# DEPRECATED sfrrsFaceMatches "Use generic-lens or generic-optics with 'faceMatches' instead." #-}

-- | Version number of the face detection model associated with the input collection (@CollectionId@ ).
--
-- /Note:/ Consider using 'faceModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrrsFaceModelVersion :: Lens.Lens' SearchFacesResponse (Core.Maybe Types.String)
sfrrsFaceModelVersion = Lens.field @"faceModelVersion"
{-# DEPRECATED sfrrsFaceModelVersion "Use generic-lens or generic-optics with 'faceModelVersion' instead." #-}

-- | ID of the face that was searched for matches in a collection.
--
-- /Note:/ Consider using 'searchedFaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrrsSearchedFaceId :: Lens.Lens' SearchFacesResponse (Core.Maybe Types.FaceId)
sfrrsSearchedFaceId = Lens.field @"searchedFaceId"
{-# DEPRECATED sfrrsSearchedFaceId "Use generic-lens or generic-optics with 'searchedFaceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrrsResponseStatus :: Lens.Lens' SearchFacesResponse Core.Int
sfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
