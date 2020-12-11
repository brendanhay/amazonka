{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified stack. After the stack is deleted, the application streaming environment provided by the stack is no longer available to users. Also, any reservations made for application streaming sessions for the stack are released.
module Network.AWS.AppStream.DeleteStack
  ( -- * Creating a request
    DeleteStack (..),
    mkDeleteStack,

    -- ** Request lenses
    dsName,

    -- * Destructuring the response
    DeleteStackResponse (..),
    mkDeleteStackResponse,

    -- ** Response lenses
    dsrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteStack' smart constructor.
newtype DeleteStack = DeleteStack' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStack' with the minimum fields required to make a request.
--
-- * 'name' - The name of the stack.
mkDeleteStack ::
  -- | 'name'
  Lude.Text ->
  DeleteStack
mkDeleteStack pName_ = DeleteStack' {name = pName_}

-- | The name of the stack.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsName :: Lens.Lens' DeleteStack Lude.Text
dsName = Lens.lens (name :: DeleteStack -> Lude.Text) (\s a -> s {name = a} :: DeleteStack)
{-# DEPRECATED dsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteStack where
  type Rs DeleteStack = DeleteStackResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteStackResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteStack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.DeleteStack" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteStack where
  toJSON DeleteStack' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteStack where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteStack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteStackResponse' smart constructor.
newtype DeleteStackResponse = DeleteStackResponse'
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

-- | Creates a value of 'DeleteStackResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteStackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteStackResponse
mkDeleteStackResponse pResponseStatus_ =
  DeleteStackResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DeleteStackResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DeleteStackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteStackResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
