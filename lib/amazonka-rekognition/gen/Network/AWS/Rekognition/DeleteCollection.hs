{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DeleteCollection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified collection. Note that this operation removes all faces in the collection. For an example, see 'delete-collection-procedure' .
--
-- This operation requires permissions to perform the @rekognition:DeleteCollection@ action.
module Network.AWS.Rekognition.DeleteCollection
  ( -- * Creating a request
    DeleteCollection (..),
    mkDeleteCollection,

    -- ** Request lenses
    dCollectionId,

    -- * Destructuring the response
    DeleteCollectionResponse (..),
    mkDeleteCollectionResponse,

    -- ** Response lenses
    dcrsStatusCode,
    dcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCollection' smart constructor.
newtype DeleteCollection = DeleteCollection'
  { -- | ID of the collection to delete.
    collectionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCollection' with the minimum fields required to make a request.
--
-- * 'collectionId' - ID of the collection to delete.
mkDeleteCollection ::
  -- | 'collectionId'
  Lude.Text ->
  DeleteCollection
mkDeleteCollection pCollectionId_ =
  DeleteCollection' {collectionId = pCollectionId_}

-- | ID of the collection to delete.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCollectionId :: Lens.Lens' DeleteCollection Lude.Text
dCollectionId = Lens.lens (collectionId :: DeleteCollection -> Lude.Text) (\s a -> s {collectionId = a} :: DeleteCollection)
{-# DEPRECATED dCollectionId "Use generic-lens or generic-optics with 'collectionId' instead." #-}

instance Lude.AWSRequest DeleteCollection where
  type Rs DeleteCollection = DeleteCollectionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteCollectionResponse'
            Lude.<$> (x Lude..?> "StatusCode") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCollection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.DeleteCollection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCollection where
  toJSON DeleteCollection' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("CollectionId" Lude..= collectionId)])

instance Lude.ToPath DeleteCollection where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCollection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCollectionResponse' smart constructor.
data DeleteCollectionResponse = DeleteCollectionResponse'
  { -- | HTTP status code that indicates the result of the operation.
    statusCode :: Lude.Maybe Lude.Natural,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCollectionResponse' with the minimum fields required to make a request.
--
-- * 'statusCode' - HTTP status code that indicates the result of the operation.
-- * 'responseStatus' - The response status code.
mkDeleteCollectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCollectionResponse
mkDeleteCollectionResponse pResponseStatus_ =
  DeleteCollectionResponse'
    { statusCode = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | HTTP status code that indicates the result of the operation.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsStatusCode :: Lens.Lens' DeleteCollectionResponse (Lude.Maybe Lude.Natural)
dcrsStatusCode = Lens.lens (statusCode :: DeleteCollectionResponse -> Lude.Maybe Lude.Natural) (\s a -> s {statusCode = a} :: DeleteCollectionResponse)
{-# DEPRECATED dcrsStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DeleteCollectionResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DeleteCollectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCollectionResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
