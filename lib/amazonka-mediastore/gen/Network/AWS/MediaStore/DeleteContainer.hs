{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.DeleteContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified container. Before you make a @DeleteContainer@ request, delete any objects in the container or in any folders in the container. You can delete only empty containers.
module Network.AWS.MediaStore.DeleteContainer
  ( -- * Creating a request
    DeleteContainer (..),
    mkDeleteContainer,

    -- ** Request lenses
    dContainerName,

    -- * Destructuring the response
    DeleteContainerResponse (..),
    mkDeleteContainerResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteContainer' smart constructor.
newtype DeleteContainer = DeleteContainer'
  { -- | The name of the container to delete.
    containerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteContainer' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container to delete.
mkDeleteContainer ::
  -- | 'containerName'
  Lude.Text ->
  DeleteContainer
mkDeleteContainer pContainerName_ =
  DeleteContainer' {containerName = pContainerName_}

-- | The name of the container to delete.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dContainerName :: Lens.Lens' DeleteContainer Lude.Text
dContainerName = Lens.lens (containerName :: DeleteContainer -> Lude.Text) (\s a -> s {containerName = a} :: DeleteContainer)
{-# DEPRECATED dContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Lude.AWSRequest DeleteContainer where
  type Rs DeleteContainer = DeleteContainerResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteContainerResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteContainer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.DeleteContainer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteContainer where
  toJSON DeleteContainer' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ContainerName" Lude..= containerName)]
      )

instance Lude.ToPath DeleteContainer where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteContainer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteContainerResponse' smart constructor.
newtype DeleteContainerResponse = DeleteContainerResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteContainerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteContainerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteContainerResponse
mkDeleteContainerResponse pResponseStatus_ =
  DeleteContainerResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteContainerResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteContainerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteContainerResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
