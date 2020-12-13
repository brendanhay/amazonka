{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DescribeCollection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified collection. You can use @DescribeCollection@ to get information, such as the number of faces indexed into a collection and the version of the model used by the collection for face detection.
--
-- For more information, see Describing a Collection in the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.DescribeCollection
  ( -- * Creating a request
    DescribeCollection (..),
    mkDescribeCollection,

    -- ** Request lenses
    dcCollectionId,

    -- * Destructuring the response
    DescribeCollectionResponse (..),
    mkDescribeCollectionResponse,

    -- ** Response lenses
    drsFaceModelVersion,
    drsFaceCount,
    drsCreationTimestamp,
    drsCollectionARN,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCollection' smart constructor.
newtype DescribeCollection = DescribeCollection'
  { -- | The ID of the collection to describe.
    collectionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCollection' with the minimum fields required to make a request.
--
-- * 'collectionId' - The ID of the collection to describe.
mkDescribeCollection ::
  -- | 'collectionId'
  Lude.Text ->
  DescribeCollection
mkDescribeCollection pCollectionId_ =
  DescribeCollection' {collectionId = pCollectionId_}

-- | The ID of the collection to describe.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCollectionId :: Lens.Lens' DescribeCollection Lude.Text
dcCollectionId = Lens.lens (collectionId :: DescribeCollection -> Lude.Text) (\s a -> s {collectionId = a} :: DescribeCollection)
{-# DEPRECATED dcCollectionId "Use generic-lens or generic-optics with 'collectionId' instead." #-}

instance Lude.AWSRequest DescribeCollection where
  type Rs DescribeCollection = DescribeCollectionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCollectionResponse'
            Lude.<$> (x Lude..?> "FaceModelVersion")
            Lude.<*> (x Lude..?> "FaceCount")
            Lude.<*> (x Lude..?> "CreationTimestamp")
            Lude.<*> (x Lude..?> "CollectionARN")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCollection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.DescribeCollection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCollection where
  toJSON DescribeCollection' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("CollectionId" Lude..= collectionId)])

instance Lude.ToPath DescribeCollection where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCollection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCollectionResponse' smart constructor.
data DescribeCollectionResponse = DescribeCollectionResponse'
  { -- | The version of the face model that's used by the collection for face detection.
    --
    -- For more information, see Model Versioning in the Amazon Rekognition Developer Guide.
    faceModelVersion :: Lude.Maybe Lude.Text,
    -- | The number of faces that are indexed into the collection. To index faces into a collection, use 'IndexFaces' .
    faceCount :: Lude.Maybe Lude.Natural,
    -- | The number of milliseconds since the Unix epoch time until the creation of the collection. The Unix epoch time is 00:00:00 Coordinated Universal Time (UTC), Thursday, 1 January 1970.
    creationTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the collection.
    collectionARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCollectionResponse' with the minimum fields required to make a request.
--
-- * 'faceModelVersion' - The version of the face model that's used by the collection for face detection.
--
-- For more information, see Model Versioning in the Amazon Rekognition Developer Guide.
-- * 'faceCount' - The number of faces that are indexed into the collection. To index faces into a collection, use 'IndexFaces' .
-- * 'creationTimestamp' - The number of milliseconds since the Unix epoch time until the creation of the collection. The Unix epoch time is 00:00:00 Coordinated Universal Time (UTC), Thursday, 1 January 1970.
-- * 'collectionARN' - The Amazon Resource Name (ARN) of the collection.
-- * 'responseStatus' - The response status code.
mkDescribeCollectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCollectionResponse
mkDescribeCollectionResponse pResponseStatus_ =
  DescribeCollectionResponse'
    { faceModelVersion = Lude.Nothing,
      faceCount = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      collectionARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The version of the face model that's used by the collection for face detection.
--
-- For more information, see Model Versioning in the Amazon Rekognition Developer Guide.
--
-- /Note:/ Consider using 'faceModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsFaceModelVersion :: Lens.Lens' DescribeCollectionResponse (Lude.Maybe Lude.Text)
drsFaceModelVersion = Lens.lens (faceModelVersion :: DescribeCollectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {faceModelVersion = a} :: DescribeCollectionResponse)
{-# DEPRECATED drsFaceModelVersion "Use generic-lens or generic-optics with 'faceModelVersion' instead." #-}

-- | The number of faces that are indexed into the collection. To index faces into a collection, use 'IndexFaces' .
--
-- /Note:/ Consider using 'faceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsFaceCount :: Lens.Lens' DescribeCollectionResponse (Lude.Maybe Lude.Natural)
drsFaceCount = Lens.lens (faceCount :: DescribeCollectionResponse -> Lude.Maybe Lude.Natural) (\s a -> s {faceCount = a} :: DescribeCollectionResponse)
{-# DEPRECATED drsFaceCount "Use generic-lens or generic-optics with 'faceCount' instead." #-}

-- | The number of milliseconds since the Unix epoch time until the creation of the collection. The Unix epoch time is 00:00:00 Coordinated Universal Time (UTC), Thursday, 1 January 1970.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCreationTimestamp :: Lens.Lens' DescribeCollectionResponse (Lude.Maybe Lude.Timestamp)
drsCreationTimestamp = Lens.lens (creationTimestamp :: DescribeCollectionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimestamp = a} :: DescribeCollectionResponse)
{-# DEPRECATED drsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The Amazon Resource Name (ARN) of the collection.
--
-- /Note:/ Consider using 'collectionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCollectionARN :: Lens.Lens' DescribeCollectionResponse (Lude.Maybe Lude.Text)
drsCollectionARN = Lens.lens (collectionARN :: DescribeCollectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {collectionARN = a} :: DescribeCollectionResponse)
{-# DEPRECATED drsCollectionARN "Use generic-lens or generic-optics with 'collectionARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeCollectionResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeCollectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCollectionResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
