{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteTapePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a custom tape pool. A custom tape pool can only be deleted if there are no tapes in the pool and if there are no automatic tape creation policies that reference the custom tape pool.
module Network.AWS.StorageGateway.DeleteTapePool
  ( -- * Creating a request
    DeleteTapePool (..),
    mkDeleteTapePool,

    -- ** Request lenses
    dtpPoolARN,

    -- * Destructuring the response
    DeleteTapePoolResponse (..),
    mkDeleteTapePoolResponse,

    -- ** Response lenses
    dtprsPoolARN,
    dtprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkDeleteTapePool' smart constructor.
newtype DeleteTapePool = DeleteTapePool' {poolARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTapePool' with the minimum fields required to make a request.
--
-- * 'poolARN' - The Amazon Resource Name (ARN) of the custom tape pool to delete.
mkDeleteTapePool ::
  -- | 'poolARN'
  Lude.Text ->
  DeleteTapePool
mkDeleteTapePool pPoolARN_ = DeleteTapePool' {poolARN = pPoolARN_}

-- | The Amazon Resource Name (ARN) of the custom tape pool to delete.
--
-- /Note:/ Consider using 'poolARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpPoolARN :: Lens.Lens' DeleteTapePool Lude.Text
dtpPoolARN = Lens.lens (poolARN :: DeleteTapePool -> Lude.Text) (\s a -> s {poolARN = a} :: DeleteTapePool)
{-# DEPRECATED dtpPoolARN "Use generic-lens or generic-optics with 'poolARN' instead." #-}

instance Lude.AWSRequest DeleteTapePool where
  type Rs DeleteTapePool = DeleteTapePoolResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteTapePoolResponse'
            Lude.<$> (x Lude..?> "PoolARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTapePool where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.DeleteTapePool" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTapePool where
  toJSON DeleteTapePool' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("PoolARN" Lude..= poolARN)])

instance Lude.ToPath DeleteTapePool where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTapePool where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTapePoolResponse' smart constructor.
data DeleteTapePoolResponse = DeleteTapePoolResponse'
  { poolARN ::
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

-- | Creates a value of 'DeleteTapePoolResponse' with the minimum fields required to make a request.
--
-- * 'poolARN' - The Amazon Resource Name (ARN) of the custom tape pool being deleted.
-- * 'responseStatus' - The response status code.
mkDeleteTapePoolResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTapePoolResponse
mkDeleteTapePoolResponse pResponseStatus_ =
  DeleteTapePoolResponse'
    { poolARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the custom tape pool being deleted.
--
-- /Note:/ Consider using 'poolARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtprsPoolARN :: Lens.Lens' DeleteTapePoolResponse (Lude.Maybe Lude.Text)
dtprsPoolARN = Lens.lens (poolARN :: DeleteTapePoolResponse -> Lude.Maybe Lude.Text) (\s a -> s {poolARN = a} :: DeleteTapePoolResponse)
{-# DEPRECATED dtprsPoolARN "Use generic-lens or generic-optics with 'poolARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtprsResponseStatus :: Lens.Lens' DeleteTapePoolResponse Lude.Int
dtprsResponseStatus = Lens.lens (responseStatus :: DeleteTapePoolResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTapePoolResponse)
{-# DEPRECATED dtprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
