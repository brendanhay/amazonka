{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DeleteFaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes faces from a collection. You specify a collection ID and an array of face IDs to remove from the collection.
--
-- This operation requires permissions to perform the @rekognition:DeleteFaces@ action.
module Network.AWS.Rekognition.DeleteFaces
  ( -- * Creating a request
    DeleteFaces (..),
    mkDeleteFaces,

    -- ** Request lenses
    dfCollectionId,
    dfFaceIds,

    -- * Destructuring the response
    DeleteFacesResponse (..),
    mkDeleteFacesResponse,

    -- ** Response lenses
    dfsrsDeletedFaces,
    dfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFaces' smart constructor.
data DeleteFaces = DeleteFaces'
  { -- | Collection from which to remove the specific faces.
    collectionId :: Lude.Text,
    -- | An array of face IDs to delete.
    faceIds :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFaces' with the minimum fields required to make a request.
--
-- * 'collectionId' - Collection from which to remove the specific faces.
-- * 'faceIds' - An array of face IDs to delete.
mkDeleteFaces ::
  -- | 'collectionId'
  Lude.Text ->
  -- | 'faceIds'
  Lude.NonEmpty Lude.Text ->
  DeleteFaces
mkDeleteFaces pCollectionId_ pFaceIds_ =
  DeleteFaces' {collectionId = pCollectionId_, faceIds = pFaceIds_}

-- | Collection from which to remove the specific faces.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfCollectionId :: Lens.Lens' DeleteFaces Lude.Text
dfCollectionId = Lens.lens (collectionId :: DeleteFaces -> Lude.Text) (\s a -> s {collectionId = a} :: DeleteFaces)
{-# DEPRECATED dfCollectionId "Use generic-lens or generic-optics with 'collectionId' instead." #-}

-- | An array of face IDs to delete.
--
-- /Note:/ Consider using 'faceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFaceIds :: Lens.Lens' DeleteFaces (Lude.NonEmpty Lude.Text)
dfFaceIds = Lens.lens (faceIds :: DeleteFaces -> Lude.NonEmpty Lude.Text) (\s a -> s {faceIds = a} :: DeleteFaces)
{-# DEPRECATED dfFaceIds "Use generic-lens or generic-optics with 'faceIds' instead." #-}

instance Lude.AWSRequest DeleteFaces where
  type Rs DeleteFaces = DeleteFacesResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteFacesResponse'
            Lude.<$> (x Lude..?> "DeletedFaces") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteFaces where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.DeleteFaces" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteFaces where
  toJSON DeleteFaces' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CollectionId" Lude..= collectionId),
            Lude.Just ("FaceIds" Lude..= faceIds)
          ]
      )

instance Lude.ToPath DeleteFaces where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteFaces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFacesResponse' smart constructor.
data DeleteFacesResponse = DeleteFacesResponse'
  { -- | An array of strings (face IDs) of the faces that were deleted.
    deletedFaces :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFacesResponse' with the minimum fields required to make a request.
--
-- * 'deletedFaces' - An array of strings (face IDs) of the faces that were deleted.
-- * 'responseStatus' - The response status code.
mkDeleteFacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteFacesResponse
mkDeleteFacesResponse pResponseStatus_ =
  DeleteFacesResponse'
    { deletedFaces = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of strings (face IDs) of the faces that were deleted.
--
-- /Note:/ Consider using 'deletedFaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsDeletedFaces :: Lens.Lens' DeleteFacesResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
dfsrsDeletedFaces = Lens.lens (deletedFaces :: DeleteFacesResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {deletedFaces = a} :: DeleteFacesResponse)
{-# DEPRECATED dfsrsDeletedFaces "Use generic-lens or generic-optics with 'deletedFaces' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsResponseStatus :: Lens.Lens' DeleteFacesResponse Lude.Int
dfsrsResponseStatus = Lens.lens (responseStatus :: DeleteFacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteFacesResponse)
{-# DEPRECATED dfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
