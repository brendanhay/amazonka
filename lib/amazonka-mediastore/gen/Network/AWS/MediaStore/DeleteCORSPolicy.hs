{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.DeleteCORSPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the cross-origin resource sharing (CORS) configuration information that is set for the container.
--
-- To use this operation, you must have permission to perform the @MediaStore:DeleteCorsPolicy@ action. The container owner has this permission by default and can grant this permission to others.
module Network.AWS.MediaStore.DeleteCORSPolicy
  ( -- * Creating a request
    DeleteCORSPolicy (..),
    mkDeleteCORSPolicy,

    -- ** Request lenses
    dcpContainerName,

    -- * Destructuring the response
    DeleteCORSPolicyResponse (..),
    mkDeleteCORSPolicyResponse,

    -- ** Response lenses
    dcorsprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCORSPolicy' smart constructor.
newtype DeleteCORSPolicy = DeleteCORSPolicy'
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

-- | Creates a value of 'DeleteCORSPolicy' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container to remove the policy from.
mkDeleteCORSPolicy ::
  -- | 'containerName'
  Lude.Text ->
  DeleteCORSPolicy
mkDeleteCORSPolicy pContainerName_ =
  DeleteCORSPolicy' {containerName = pContainerName_}

-- | The name of the container to remove the policy from.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpContainerName :: Lens.Lens' DeleteCORSPolicy Lude.Text
dcpContainerName = Lens.lens (containerName :: DeleteCORSPolicy -> Lude.Text) (\s a -> s {containerName = a} :: DeleteCORSPolicy)
{-# DEPRECATED dcpContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Lude.AWSRequest DeleteCORSPolicy where
  type Rs DeleteCORSPolicy = DeleteCORSPolicyResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteCORSPolicyResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCORSPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.DeleteCorsPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCORSPolicy where
  toJSON DeleteCORSPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ContainerName" Lude..= containerName)]
      )

instance Lude.ToPath DeleteCORSPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCORSPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCORSPolicyResponse' smart constructor.
newtype DeleteCORSPolicyResponse = DeleteCORSPolicyResponse'
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

-- | Creates a value of 'DeleteCORSPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteCORSPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCORSPolicyResponse
mkDeleteCORSPolicyResponse pResponseStatus_ =
  DeleteCORSPolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcorsprsResponseStatus :: Lens.Lens' DeleteCORSPolicyResponse Lude.Int
dcorsprsResponseStatus = Lens.lens (responseStatus :: DeleteCORSPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCORSPolicyResponse)
{-# DEPRECATED dcorsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
