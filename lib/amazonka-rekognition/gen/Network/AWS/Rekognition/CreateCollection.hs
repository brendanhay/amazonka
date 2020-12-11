{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.CreateCollection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a collection in an AWS Region. You can add faces to the collection using the 'IndexFaces' operation.
--
-- For example, you might create collections, one for each of your application users. A user can then index faces using the @IndexFaces@ operation and persist results in a specific collection. Then, a user can search the collection for faces in the user-specific container.
-- When you create a collection, it is associated with the latest version of the face model version.
-- This operation requires permissions to perform the @rekognition:CreateCollection@ action.
module Network.AWS.Rekognition.CreateCollection
  ( -- * Creating a request
    CreateCollection (..),
    mkCreateCollection,

    -- ** Request lenses
    ccCollectionId,

    -- * Destructuring the response
    CreateCollectionResponse (..),
    mkCreateCollectionResponse,

    -- ** Response lenses
    ccrsFaceModelVersion,
    ccrsCollectionARN,
    ccrsStatusCode,
    ccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCollection' smart constructor.
newtype CreateCollection = CreateCollection'
  { collectionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCollection' with the minimum fields required to make a request.
--
-- * 'collectionId' - ID for the collection that you are creating.
mkCreateCollection ::
  -- | 'collectionId'
  Lude.Text ->
  CreateCollection
mkCreateCollection pCollectionId_ =
  CreateCollection' {collectionId = pCollectionId_}

-- | ID for the collection that you are creating.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCollectionId :: Lens.Lens' CreateCollection Lude.Text
ccCollectionId = Lens.lens (collectionId :: CreateCollection -> Lude.Text) (\s a -> s {collectionId = a} :: CreateCollection)
{-# DEPRECATED ccCollectionId "Use generic-lens or generic-optics with 'collectionId' instead." #-}

instance Lude.AWSRequest CreateCollection where
  type Rs CreateCollection = CreateCollectionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCollectionResponse'
            Lude.<$> (x Lude..?> "FaceModelVersion")
            Lude.<*> (x Lude..?> "CollectionArn")
            Lude.<*> (x Lude..?> "StatusCode")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCollection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.CreateCollection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCollection where
  toJSON CreateCollection' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("CollectionId" Lude..= collectionId)])

instance Lude.ToPath CreateCollection where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCollection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCollectionResponse' smart constructor.
data CreateCollectionResponse = CreateCollectionResponse'
  { faceModelVersion ::
      Lude.Maybe Lude.Text,
    collectionARN :: Lude.Maybe Lude.Text,
    statusCode :: Lude.Maybe Lude.Natural,
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

-- | Creates a value of 'CreateCollectionResponse' with the minimum fields required to make a request.
--
-- * 'collectionARN' - Amazon Resource Name (ARN) of the collection. You can use this to manage permissions on your resources.
-- * 'faceModelVersion' - Version number of the face detection model associated with the collection you are creating.
-- * 'responseStatus' - The response status code.
-- * 'statusCode' - HTTP status code indicating the result of the operation.
mkCreateCollectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCollectionResponse
mkCreateCollectionResponse pResponseStatus_ =
  CreateCollectionResponse'
    { faceModelVersion = Lude.Nothing,
      collectionARN = Lude.Nothing,
      statusCode = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Version number of the face detection model associated with the collection you are creating.
--
-- /Note:/ Consider using 'faceModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsFaceModelVersion :: Lens.Lens' CreateCollectionResponse (Lude.Maybe Lude.Text)
ccrsFaceModelVersion = Lens.lens (faceModelVersion :: CreateCollectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {faceModelVersion = a} :: CreateCollectionResponse)
{-# DEPRECATED ccrsFaceModelVersion "Use generic-lens or generic-optics with 'faceModelVersion' instead." #-}

-- | Amazon Resource Name (ARN) of the collection. You can use this to manage permissions on your resources.
--
-- /Note:/ Consider using 'collectionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsCollectionARN :: Lens.Lens' CreateCollectionResponse (Lude.Maybe Lude.Text)
ccrsCollectionARN = Lens.lens (collectionARN :: CreateCollectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {collectionARN = a} :: CreateCollectionResponse)
{-# DEPRECATED ccrsCollectionARN "Use generic-lens or generic-optics with 'collectionARN' instead." #-}

-- | HTTP status code indicating the result of the operation.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsStatusCode :: Lens.Lens' CreateCollectionResponse (Lude.Maybe Lude.Natural)
ccrsStatusCode = Lens.lens (statusCode :: CreateCollectionResponse -> Lude.Maybe Lude.Natural) (\s a -> s {statusCode = a} :: CreateCollectionResponse)
{-# DEPRECATED ccrsStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateCollectionResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateCollectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCollectionResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
