{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.ListCollections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of collection IDs in your account. If the result is truncated, the response also provides a @NextToken@ that you can use in the subsequent request to fetch the next set of collection IDs.
--
-- For an example, see Listing Collections in the Amazon Rekognition Developer Guide.
-- This operation requires permissions to perform the @rekognition:ListCollections@ action.
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.ListCollections
  ( -- * Creating a request
    ListCollections (..),
    mkListCollections,

    -- ** Request lenses
    lcNextToken,
    lcMaxResults,

    -- * Destructuring the response
    ListCollectionsResponse (..),
    mkListCollectionsResponse,

    -- ** Response lenses
    lcrsCollectionIds,
    lcrsNextToken,
    lcrsFaceModelVersions,
    lcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListCollections' smart constructor.
data ListCollections = ListCollections'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCollections' with the minimum fields required to make a request.
--
-- * 'maxResults' - Maximum number of collection IDs to return.
-- * 'nextToken' - Pagination token from the previous response.
mkListCollections ::
  ListCollections
mkListCollections =
  ListCollections'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Pagination token from the previous response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListCollections (Lude.Maybe Lude.Text)
lcNextToken = Lens.lens (nextToken :: ListCollections -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCollections)
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of collection IDs to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListCollections (Lude.Maybe Lude.Natural)
lcMaxResults = Lens.lens (maxResults :: ListCollections -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListCollections)
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListCollections where
  page rq rs
    | Page.stop (rs Lens.^. lcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcrsCollectionIds) = Lude.Nothing
    | Page.stop (rs Lens.^. lcrsFaceModelVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcNextToken Lens..~ rs Lens.^. lcrsNextToken

instance Lude.AWSRequest ListCollections where
  type Rs ListCollections = ListCollectionsResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCollectionsResponse'
            Lude.<$> (x Lude..?> "CollectionIds" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "FaceModelVersions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCollections where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.ListCollections" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListCollections where
  toJSON ListCollections' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListCollections where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCollections where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListCollectionsResponse' smart constructor.
data ListCollectionsResponse = ListCollectionsResponse'
  { collectionIds ::
      Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    faceModelVersions :: Lude.Maybe [Lude.Text],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCollectionsResponse' with the minimum fields required to make a request.
--
-- * 'collectionIds' - An array of collection IDs.
-- * 'faceModelVersions' - Version numbers of the face detection models associated with the collections in the array @CollectionIds@ . For example, the value of @FaceModelVersions[2]@ is the version number for the face detection model used by the collection in @CollectionId[2]@ .
-- * 'nextToken' - If the result is truncated, the response provides a @NextToken@ that you can use in the subsequent request to fetch the next set of collection IDs.
-- * 'responseStatus' - The response status code.
mkListCollectionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCollectionsResponse
mkListCollectionsResponse pResponseStatus_ =
  ListCollectionsResponse'
    { collectionIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      faceModelVersions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of collection IDs.
--
-- /Note:/ Consider using 'collectionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsCollectionIds :: Lens.Lens' ListCollectionsResponse (Lude.Maybe [Lude.Text])
lcrsCollectionIds = Lens.lens (collectionIds :: ListCollectionsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {collectionIds = a} :: ListCollectionsResponse)
{-# DEPRECATED lcrsCollectionIds "Use generic-lens or generic-optics with 'collectionIds' instead." #-}

-- | If the result is truncated, the response provides a @NextToken@ that you can use in the subsequent request to fetch the next set of collection IDs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsNextToken :: Lens.Lens' ListCollectionsResponse (Lude.Maybe Lude.Text)
lcrsNextToken = Lens.lens (nextToken :: ListCollectionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCollectionsResponse)
{-# DEPRECATED lcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Version numbers of the face detection models associated with the collections in the array @CollectionIds@ . For example, the value of @FaceModelVersions[2]@ is the version number for the face detection model used by the collection in @CollectionId[2]@ .
--
-- /Note:/ Consider using 'faceModelVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsFaceModelVersions :: Lens.Lens' ListCollectionsResponse (Lude.Maybe [Lude.Text])
lcrsFaceModelVersions = Lens.lens (faceModelVersions :: ListCollectionsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {faceModelVersions = a} :: ListCollectionsResponse)
{-# DEPRECATED lcrsFaceModelVersions "Use generic-lens or generic-optics with 'faceModelVersions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsResponseStatus :: Lens.Lens' ListCollectionsResponse Lude.Int
lcrsResponseStatus = Lens.lens (responseStatus :: ListCollectionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCollectionsResponse)
{-# DEPRECATED lcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
