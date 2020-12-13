{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.DeleteMetricPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the metric policy that is associated with the specified container. If there is no metric policy associated with the container, MediaStore doesn't send metrics to CloudWatch.
module Network.AWS.MediaStore.DeleteMetricPolicy
  ( -- * Creating a request
    DeleteMetricPolicy (..),
    mkDeleteMetricPolicy,

    -- ** Request lenses
    dmpContainerName,

    -- * Destructuring the response
    DeleteMetricPolicyResponse (..),
    mkDeleteMetricPolicyResponse,

    -- ** Response lenses
    dmprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteMetricPolicy' smart constructor.
newtype DeleteMetricPolicy = DeleteMetricPolicy'
  { -- | The name of the container that is associated with the metric policy that you want to delete.
    containerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMetricPolicy' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container that is associated with the metric policy that you want to delete.
mkDeleteMetricPolicy ::
  -- | 'containerName'
  Lude.Text ->
  DeleteMetricPolicy
mkDeleteMetricPolicy pContainerName_ =
  DeleteMetricPolicy' {containerName = pContainerName_}

-- | The name of the container that is associated with the metric policy that you want to delete.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpContainerName :: Lens.Lens' DeleteMetricPolicy Lude.Text
dmpContainerName = Lens.lens (containerName :: DeleteMetricPolicy -> Lude.Text) (\s a -> s {containerName = a} :: DeleteMetricPolicy)
{-# DEPRECATED dmpContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Lude.AWSRequest DeleteMetricPolicy where
  type Rs DeleteMetricPolicy = DeleteMetricPolicyResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteMetricPolicyResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteMetricPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.DeleteMetricPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteMetricPolicy where
  toJSON DeleteMetricPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ContainerName" Lude..= containerName)]
      )

instance Lude.ToPath DeleteMetricPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteMetricPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteMetricPolicyResponse' smart constructor.
newtype DeleteMetricPolicyResponse = DeleteMetricPolicyResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMetricPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteMetricPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteMetricPolicyResponse
mkDeleteMetricPolicyResponse pResponseStatus_ =
  DeleteMetricPolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsResponseStatus :: Lens.Lens' DeleteMetricPolicyResponse Lude.Int
dmprsResponseStatus = Lens.lens (responseStatus :: DeleteMetricPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteMetricPolicyResponse)
{-# DEPRECATED dmprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
