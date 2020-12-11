{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.DeleteContainerPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the access policy that is associated with the specified container.
module Network.AWS.MediaStore.DeleteContainerPolicy
  ( -- * Creating a request
    DeleteContainerPolicy (..),
    mkDeleteContainerPolicy,

    -- ** Request lenses
    delContainerName,

    -- * Destructuring the response
    DeleteContainerPolicyResponse (..),
    mkDeleteContainerPolicyResponse,

    -- ** Response lenses
    dcprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteContainerPolicy' smart constructor.
newtype DeleteContainerPolicy = DeleteContainerPolicy'
  { containerName ::
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

-- | Creates a value of 'DeleteContainerPolicy' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container that holds the policy.
mkDeleteContainerPolicy ::
  -- | 'containerName'
  Lude.Text ->
  DeleteContainerPolicy
mkDeleteContainerPolicy pContainerName_ =
  DeleteContainerPolicy' {containerName = pContainerName_}

-- | The name of the container that holds the policy.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delContainerName :: Lens.Lens' DeleteContainerPolicy Lude.Text
delContainerName = Lens.lens (containerName :: DeleteContainerPolicy -> Lude.Text) (\s a -> s {containerName = a} :: DeleteContainerPolicy)
{-# DEPRECATED delContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Lude.AWSRequest DeleteContainerPolicy where
  type Rs DeleteContainerPolicy = DeleteContainerPolicyResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteContainerPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteContainerPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.DeleteContainerPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteContainerPolicy where
  toJSON DeleteContainerPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ContainerName" Lude..= containerName)]
      )

instance Lude.ToPath DeleteContainerPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteContainerPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteContainerPolicyResponse' smart constructor.
newtype DeleteContainerPolicyResponse = DeleteContainerPolicyResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteContainerPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteContainerPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteContainerPolicyResponse
mkDeleteContainerPolicyResponse pResponseStatus_ =
  DeleteContainerPolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsResponseStatus :: Lens.Lens' DeleteContainerPolicyResponse Lude.Int
dcprsResponseStatus = Lens.lens (responseStatus :: DeleteContainerPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteContainerPolicyResponse)
{-# DEPRECATED dcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
