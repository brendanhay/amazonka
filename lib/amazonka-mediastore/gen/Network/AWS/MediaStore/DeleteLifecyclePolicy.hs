{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.DeleteLifecyclePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an object lifecycle policy from a container. It takes up to 20 minutes for the change to take effect.
module Network.AWS.MediaStore.DeleteLifecyclePolicy
  ( -- * Creating a request
    DeleteLifecyclePolicy (..),
    mkDeleteLifecyclePolicy,

    -- ** Request lenses
    dlpContainerName,

    -- * Destructuring the response
    DeleteLifecyclePolicyResponse (..),
    mkDeleteLifecyclePolicyResponse,

    -- ** Response lenses
    dlprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLifecyclePolicy' smart constructor.
newtype DeleteLifecyclePolicy = DeleteLifecyclePolicy'
  { -- | The name of the container that holds the object lifecycle policy.
    containerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLifecyclePolicy' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container that holds the object lifecycle policy.
mkDeleteLifecyclePolicy ::
  -- | 'containerName'
  Lude.Text ->
  DeleteLifecyclePolicy
mkDeleteLifecyclePolicy pContainerName_ =
  DeleteLifecyclePolicy' {containerName = pContainerName_}

-- | The name of the container that holds the object lifecycle policy.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlpContainerName :: Lens.Lens' DeleteLifecyclePolicy Lude.Text
dlpContainerName = Lens.lens (containerName :: DeleteLifecyclePolicy -> Lude.Text) (\s a -> s {containerName = a} :: DeleteLifecyclePolicy)
{-# DEPRECATED dlpContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Lude.AWSRequest DeleteLifecyclePolicy where
  type Rs DeleteLifecyclePolicy = DeleteLifecyclePolicyResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteLifecyclePolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLifecyclePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.DeleteLifecyclePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteLifecyclePolicy where
  toJSON DeleteLifecyclePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ContainerName" Lude..= containerName)]
      )

instance Lude.ToPath DeleteLifecyclePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLifecyclePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteLifecyclePolicyResponse' smart constructor.
newtype DeleteLifecyclePolicyResponse = DeleteLifecyclePolicyResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLifecyclePolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteLifecyclePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLifecyclePolicyResponse
mkDeleteLifecyclePolicyResponse pResponseStatus_ =
  DeleteLifecyclePolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprsResponseStatus :: Lens.Lens' DeleteLifecyclePolicyResponse Lude.Int
dlprsResponseStatus = Lens.lens (responseStatus :: DeleteLifecyclePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLifecyclePolicyResponse)
{-# DEPRECATED dlprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
