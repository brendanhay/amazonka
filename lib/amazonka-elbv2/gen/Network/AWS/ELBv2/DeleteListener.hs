{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DeleteListener
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified listener.
--
-- Alternatively, your listener is deleted when you delete the load balancer to which it is attached.
module Network.AWS.ELBv2.DeleteListener
  ( -- * Creating a request
    DeleteListener (..),
    mkDeleteListener,

    -- ** Request lenses
    dlListenerARN,

    -- * Destructuring the response
    DeleteListenerResponse (..),
    mkDeleteListenerResponse,

    -- ** Response lenses
    dlrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteListener' smart constructor.
newtype DeleteListener = DeleteListener' {listenerARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteListener' with the minimum fields required to make a request.
--
-- * 'listenerARN' - The Amazon Resource Name (ARN) of the listener.
mkDeleteListener ::
  -- | 'listenerARN'
  Lude.Text ->
  DeleteListener
mkDeleteListener pListenerARN_ =
  DeleteListener' {listenerARN = pListenerARN_}

-- | The Amazon Resource Name (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlListenerARN :: Lens.Lens' DeleteListener Lude.Text
dlListenerARN = Lens.lens (listenerARN :: DeleteListener -> Lude.Text) (\s a -> s {listenerARN = a} :: DeleteListener)
{-# DEPRECATED dlListenerARN "Use generic-lens or generic-optics with 'listenerARN' instead." #-}

instance Lude.AWSRequest DeleteListener where
  type Rs DeleteListener = DeleteListenerResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "DeleteListenerResult"
      ( \s h x ->
          DeleteListenerResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteListener where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteListener where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteListener where
  toQuery DeleteListener' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteListener" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "ListenerArn" Lude.=: listenerARN
      ]

-- | /See:/ 'mkDeleteListenerResponse' smart constructor.
newtype DeleteListenerResponse = DeleteListenerResponse'
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

-- | Creates a value of 'DeleteListenerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteListenerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteListenerResponse
mkDeleteListenerResponse pResponseStatus_ =
  DeleteListenerResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsResponseStatus :: Lens.Lens' DeleteListenerResponse Lude.Int
dlrsResponseStatus = Lens.lens (responseStatus :: DeleteListenerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteListenerResponse)
{-# DEPRECATED dlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
