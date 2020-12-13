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
    sfFaceId,
    sfFaceMatchThreshold,
    sfCollectionId,
    sfMaxFaces,

    -- * Destructuring the response
    SearchFacesResponse (..),
    mkSearchFacesResponse,

    -- ** Response lenses
    sfrsFaceMatches,
    sfrsFaceModelVersion,
    sfrsSearchedFaceId,
    sfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchFaces' smart constructor.
data SearchFaces = SearchFaces'
  { -- | ID of a face to find matches for in the collection.
    faceId :: Lude.Text,
    -- | Optional value specifying the minimum confidence in the face match to return. For example, don't return any matches where confidence in matches is less than 70%. The default value is 80%.
    faceMatchThreshold :: Lude.Maybe Lude.Double,
    -- | ID of the collection the face belongs to.
    collectionId :: Lude.Text,
    -- | Maximum number of faces to return. The operation returns the maximum number of faces with the highest confidence in the match.
    maxFaces :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchFaces' with the minimum fields required to make a request.
--
-- * 'faceId' - ID of a face to find matches for in the collection.
-- * 'faceMatchThreshold' - Optional value specifying the minimum confidence in the face match to return. For example, don't return any matches where confidence in matches is less than 70%. The default value is 80%.
-- * 'collectionId' - ID of the collection the face belongs to.
-- * 'maxFaces' - Maximum number of faces to return. The operation returns the maximum number of faces with the highest confidence in the match.
mkSearchFaces ::
  -- | 'faceId'
  Lude.Text ->
  -- | 'collectionId'
  Lude.Text ->
  SearchFaces
mkSearchFaces pFaceId_ pCollectionId_ =
  SearchFaces'
    { faceId = pFaceId_,
      faceMatchThreshold = Lude.Nothing,
      collectionId = pCollectionId_,
      maxFaces = Lude.Nothing
    }

-- | ID of a face to find matches for in the collection.
--
-- /Note:/ Consider using 'faceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfFaceId :: Lens.Lens' SearchFaces Lude.Text
sfFaceId = Lens.lens (faceId :: SearchFaces -> Lude.Text) (\s a -> s {faceId = a} :: SearchFaces)
{-# DEPRECATED sfFaceId "Use generic-lens or generic-optics with 'faceId' instead." #-}

-- | Optional value specifying the minimum confidence in the face match to return. For example, don't return any matches where confidence in matches is less than 70%. The default value is 80%.
--
-- /Note:/ Consider using 'faceMatchThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfFaceMatchThreshold :: Lens.Lens' SearchFaces (Lude.Maybe Lude.Double)
sfFaceMatchThreshold = Lens.lens (faceMatchThreshold :: SearchFaces -> Lude.Maybe Lude.Double) (\s a -> s {faceMatchThreshold = a} :: SearchFaces)
{-# DEPRECATED sfFaceMatchThreshold "Use generic-lens or generic-optics with 'faceMatchThreshold' instead." #-}

-- | ID of the collection the face belongs to.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCollectionId :: Lens.Lens' SearchFaces Lude.Text
sfCollectionId = Lens.lens (collectionId :: SearchFaces -> Lude.Text) (\s a -> s {collectionId = a} :: SearchFaces)
{-# DEPRECATED sfCollectionId "Use generic-lens or generic-optics with 'collectionId' instead." #-}

-- | Maximum number of faces to return. The operation returns the maximum number of faces with the highest confidence in the match.
--
-- /Note:/ Consider using 'maxFaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfMaxFaces :: Lens.Lens' SearchFaces (Lude.Maybe Lude.Natural)
sfMaxFaces = Lens.lens (maxFaces :: SearchFaces -> Lude.Maybe Lude.Natural) (\s a -> s {maxFaces = a} :: SearchFaces)
{-# DEPRECATED sfMaxFaces "Use generic-lens or generic-optics with 'maxFaces' instead." #-}

instance Lude.AWSRequest SearchFaces where
  type Rs SearchFaces = SearchFacesResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchFacesResponse'
            Lude.<$> (x Lude..?> "FaceMatches" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "FaceModelVersion")
            Lude.<*> (x Lude..?> "SearchedFaceId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchFaces where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.SearchFaces" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SearchFaces where
  toJSON SearchFaces' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FaceId" Lude..= faceId),
            ("FaceMatchThreshold" Lude..=) Lude.<$> faceMatchThreshold,
            Lude.Just ("CollectionId" Lude..= collectionId),
            ("MaxFaces" Lude..=) Lude.<$> maxFaces
          ]
      )

instance Lude.ToPath SearchFaces where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchFaces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchFacesResponse' smart constructor.
data SearchFacesResponse = SearchFacesResponse'
  { -- | An array of faces that matched the input face, along with the confidence in the match.
    faceMatches :: Lude.Maybe [FaceMatch],
    -- | Version number of the face detection model associated with the input collection (@CollectionId@ ).
    faceModelVersion :: Lude.Maybe Lude.Text,
    -- | ID of the face that was searched for matches in a collection.
    searchedFaceId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchFacesResponse' with the minimum fields required to make a request.
--
-- * 'faceMatches' - An array of faces that matched the input face, along with the confidence in the match.
-- * 'faceModelVersion' - Version number of the face detection model associated with the input collection (@CollectionId@ ).
-- * 'searchedFaceId' - ID of the face that was searched for matches in a collection.
-- * 'responseStatus' - The response status code.
mkSearchFacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchFacesResponse
mkSearchFacesResponse pResponseStatus_ =
  SearchFacesResponse'
    { faceMatches = Lude.Nothing,
      faceModelVersion = Lude.Nothing,
      searchedFaceId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of faces that matched the input face, along with the confidence in the match.
--
-- /Note:/ Consider using 'faceMatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrsFaceMatches :: Lens.Lens' SearchFacesResponse (Lude.Maybe [FaceMatch])
sfrsFaceMatches = Lens.lens (faceMatches :: SearchFacesResponse -> Lude.Maybe [FaceMatch]) (\s a -> s {faceMatches = a} :: SearchFacesResponse)
{-# DEPRECATED sfrsFaceMatches "Use generic-lens or generic-optics with 'faceMatches' instead." #-}

-- | Version number of the face detection model associated with the input collection (@CollectionId@ ).
--
-- /Note:/ Consider using 'faceModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrsFaceModelVersion :: Lens.Lens' SearchFacesResponse (Lude.Maybe Lude.Text)
sfrsFaceModelVersion = Lens.lens (faceModelVersion :: SearchFacesResponse -> Lude.Maybe Lude.Text) (\s a -> s {faceModelVersion = a} :: SearchFacesResponse)
{-# DEPRECATED sfrsFaceModelVersion "Use generic-lens or generic-optics with 'faceModelVersion' instead." #-}

-- | ID of the face that was searched for matches in a collection.
--
-- /Note:/ Consider using 'searchedFaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrsSearchedFaceId :: Lens.Lens' SearchFacesResponse (Lude.Maybe Lude.Text)
sfrsSearchedFaceId = Lens.lens (searchedFaceId :: SearchFacesResponse -> Lude.Maybe Lude.Text) (\s a -> s {searchedFaceId = a} :: SearchFacesResponse)
{-# DEPRECATED sfrsSearchedFaceId "Use generic-lens or generic-optics with 'searchedFaceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrsResponseStatus :: Lens.Lens' SearchFacesResponse Lude.Int
sfrsResponseStatus = Lens.lens (responseStatus :: SearchFacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchFacesResponse)
{-# DEPRECATED sfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
