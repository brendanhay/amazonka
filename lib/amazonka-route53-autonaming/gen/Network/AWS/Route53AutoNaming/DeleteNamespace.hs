{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.DeleteNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a namespace from the current account. If the namespace still contains one or more services, the request fails.
module Network.AWS.Route53AutoNaming.DeleteNamespace
  ( -- * Creating a request
    DeleteNamespace (..),
    mkDeleteNamespace,

    -- ** Request lenses
    dnId,

    -- * Destructuring the response
    DeleteNamespaceResponse (..),
    mkDeleteNamespaceResponse,

    -- ** Response lenses
    dnrsOperationId,
    dnrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkDeleteNamespace' smart constructor.
newtype DeleteNamespace = DeleteNamespace' {id :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNamespace' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the namespace that you want to delete.
mkDeleteNamespace ::
  -- | 'id'
  Lude.Text ->
  DeleteNamespace
mkDeleteNamespace pId_ = DeleteNamespace' {id = pId_}

-- | The ID of the namespace that you want to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnId :: Lens.Lens' DeleteNamespace Lude.Text
dnId = Lens.lens (id :: DeleteNamespace -> Lude.Text) (\s a -> s {id = a} :: DeleteNamespace)
{-# DEPRECATED dnId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteNamespace where
  type Rs DeleteNamespace = DeleteNamespaceResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteNamespaceResponse'
            Lude.<$> (x Lude..?> "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteNamespace where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53AutoNaming_v20170314.DeleteNamespace" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteNamespace where
  toJSON DeleteNamespace' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Id" Lude..= id)])

instance Lude.ToPath DeleteNamespace where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteNamespace where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteNamespaceResponse' smart constructor.
data DeleteNamespaceResponse = DeleteNamespaceResponse'
  { operationId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DeleteNamespaceResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
-- * 'responseStatus' - The response status code.
mkDeleteNamespaceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteNamespaceResponse
mkDeleteNamespaceResponse pResponseStatus_ =
  DeleteNamespaceResponse'
    { operationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnrsOperationId :: Lens.Lens' DeleteNamespaceResponse (Lude.Maybe Lude.Text)
dnrsOperationId = Lens.lens (operationId :: DeleteNamespaceResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: DeleteNamespaceResponse)
{-# DEPRECATED dnrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnrsResponseStatus :: Lens.Lens' DeleteNamespaceResponse Lude.Int
dnrsResponseStatus = Lens.lens (responseStatus :: DeleteNamespaceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteNamespaceResponse)
{-# DEPRECATED dnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
