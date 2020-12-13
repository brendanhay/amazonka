{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.ListFaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata for faces in the specified collection. This metadata includes information such as the bounding box coordinates, the confidence (that the bounding box contains a face), and face ID. For an example, see Listing Faces in a Collection in the Amazon Rekognition Developer Guide.
--
-- This operation requires permissions to perform the @rekognition:ListFaces@ action.
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.ListFaces
  ( -- * Creating a request
    ListFaces (..),
    mkListFaces,

    -- ** Request lenses
    lfCollectionId,
    lfNextToken,
    lfMaxResults,

    -- * Destructuring the response
    ListFacesResponse (..),
    mkListFacesResponse,

    -- ** Response lenses
    lfrsFaceModelVersion,
    lfrsNextToken,
    lfrsFaces,
    lfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListFaces' smart constructor.
data ListFaces = ListFaces'
  { -- | ID of the collection from which to list the faces.
    collectionId :: Lude.Text,
    -- | If the previous response was incomplete (because there is more data to retrieve), Amazon Rekognition returns a pagination token in the response. You can use this pagination token to retrieve the next set of faces.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Maximum number of faces to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFaces' with the minimum fields required to make a request.
--
-- * 'collectionId' - ID of the collection from which to list the faces.
-- * 'nextToken' - If the previous response was incomplete (because there is more data to retrieve), Amazon Rekognition returns a pagination token in the response. You can use this pagination token to retrieve the next set of faces.
-- * 'maxResults' - Maximum number of faces to return.
mkListFaces ::
  -- | 'collectionId'
  Lude.Text ->
  ListFaces
mkListFaces pCollectionId_ =
  ListFaces'
    { collectionId = pCollectionId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | ID of the collection from which to list the faces.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfCollectionId :: Lens.Lens' ListFaces Lude.Text
lfCollectionId = Lens.lens (collectionId :: ListFaces -> Lude.Text) (\s a -> s {collectionId = a} :: ListFaces)
{-# DEPRECATED lfCollectionId "Use generic-lens or generic-optics with 'collectionId' instead." #-}

-- | If the previous response was incomplete (because there is more data to retrieve), Amazon Rekognition returns a pagination token in the response. You can use this pagination token to retrieve the next set of faces.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfNextToken :: Lens.Lens' ListFaces (Lude.Maybe Lude.Text)
lfNextToken = Lens.lens (nextToken :: ListFaces -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFaces)
{-# DEPRECATED lfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of faces to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMaxResults :: Lens.Lens' ListFaces (Lude.Maybe Lude.Natural)
lfMaxResults = Lens.lens (maxResults :: ListFaces -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListFaces)
{-# DEPRECATED lfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListFaces where
  page rq rs
    | Page.stop (rs Lens.^. lfrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lfrsFaces) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lfNextToken Lens..~ rs Lens.^. lfrsNextToken

instance Lude.AWSRequest ListFaces where
  type Rs ListFaces = ListFacesResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListFacesResponse'
            Lude.<$> (x Lude..?> "FaceModelVersion")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Faces" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFaces where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.ListFaces" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListFaces where
  toJSON ListFaces' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CollectionId" Lude..= collectionId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListFaces where
  toPath = Lude.const "/"

instance Lude.ToQuery ListFaces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListFacesResponse' smart constructor.
data ListFacesResponse = ListFacesResponse'
  { -- | Version number of the face detection model associated with the input collection (@CollectionId@ ).
    faceModelVersion :: Lude.Maybe Lude.Text,
    -- | If the response is truncated, Amazon Rekognition returns this token that you can use in the subsequent request to retrieve the next set of faces.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array of @Face@ objects.
    faces :: Lude.Maybe [Face],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFacesResponse' with the minimum fields required to make a request.
--
-- * 'faceModelVersion' - Version number of the face detection model associated with the input collection (@CollectionId@ ).
-- * 'nextToken' - If the response is truncated, Amazon Rekognition returns this token that you can use in the subsequent request to retrieve the next set of faces.
-- * 'faces' - An array of @Face@ objects.
-- * 'responseStatus' - The response status code.
mkListFacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFacesResponse
mkListFacesResponse pResponseStatus_ =
  ListFacesResponse'
    { faceModelVersion = Lude.Nothing,
      nextToken = Lude.Nothing,
      faces = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Version number of the face detection model associated with the input collection (@CollectionId@ ).
--
-- /Note:/ Consider using 'faceModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsFaceModelVersion :: Lens.Lens' ListFacesResponse (Lude.Maybe Lude.Text)
lfrsFaceModelVersion = Lens.lens (faceModelVersion :: ListFacesResponse -> Lude.Maybe Lude.Text) (\s a -> s {faceModelVersion = a} :: ListFacesResponse)
{-# DEPRECATED lfrsFaceModelVersion "Use generic-lens or generic-optics with 'faceModelVersion' instead." #-}

-- | If the response is truncated, Amazon Rekognition returns this token that you can use in the subsequent request to retrieve the next set of faces.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsNextToken :: Lens.Lens' ListFacesResponse (Lude.Maybe Lude.Text)
lfrsNextToken = Lens.lens (nextToken :: ListFacesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFacesResponse)
{-# DEPRECATED lfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @Face@ objects.
--
-- /Note:/ Consider using 'faces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsFaces :: Lens.Lens' ListFacesResponse (Lude.Maybe [Face])
lfrsFaces = Lens.lens (faces :: ListFacesResponse -> Lude.Maybe [Face]) (\s a -> s {faces = a} :: ListFacesResponse)
{-# DEPRECATED lfrsFaces "Use generic-lens or generic-optics with 'faces' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsResponseStatus :: Lens.Lens' ListFacesResponse Lude.Int
lfrsResponseStatus = Lens.lens (responseStatus :: ListFacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFacesResponse)
{-# DEPRECATED lfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
