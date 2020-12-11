{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.DeleteWorkerBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteWorkerBlock@ operation allows you to reinstate a blocked Worker to work on your HITs. This operation reverses the effects of the CreateWorkerBlock operation. You need the Worker ID to use this operation. If the Worker ID is missing or invalid, this operation fails and returns the message “WorkerId is invalid.” If the specified Worker is not blocked, this operation returns successfully.
module Network.AWS.MechanicalTurk.DeleteWorkerBlock
  ( -- * Creating a request
    DeleteWorkerBlock (..),
    mkDeleteWorkerBlock,

    -- ** Request lenses
    dwbReason,
    dwbWorkerId,

    -- * Destructuring the response
    DeleteWorkerBlockResponse (..),
    mkDeleteWorkerBlockResponse,

    -- ** Response lenses
    dwbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteWorkerBlock' smart constructor.
data DeleteWorkerBlock = DeleteWorkerBlock'
  { reason ::
      Lude.Maybe Lude.Text,
    workerId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteWorkerBlock' with the minimum fields required to make a request.
--
-- * 'reason' - A message that explains the reason for unblocking the Worker. The Worker does not see this message.
-- * 'workerId' - The ID of the Worker to unblock.
mkDeleteWorkerBlock ::
  -- | 'workerId'
  Lude.Text ->
  DeleteWorkerBlock
mkDeleteWorkerBlock pWorkerId_ =
  DeleteWorkerBlock' {reason = Lude.Nothing, workerId = pWorkerId_}

-- | A message that explains the reason for unblocking the Worker. The Worker does not see this message.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbReason :: Lens.Lens' DeleteWorkerBlock (Lude.Maybe Lude.Text)
dwbReason = Lens.lens (reason :: DeleteWorkerBlock -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: DeleteWorkerBlock)
{-# DEPRECATED dwbReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The ID of the Worker to unblock.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbWorkerId :: Lens.Lens' DeleteWorkerBlock Lude.Text
dwbWorkerId = Lens.lens (workerId :: DeleteWorkerBlock -> Lude.Text) (\s a -> s {workerId = a} :: DeleteWorkerBlock)
{-# DEPRECATED dwbWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

instance Lude.AWSRequest DeleteWorkerBlock where
  type Rs DeleteWorkerBlock = DeleteWorkerBlockResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteWorkerBlockResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteWorkerBlock where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.DeleteWorkerBlock" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteWorkerBlock where
  toJSON DeleteWorkerBlock' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Reason" Lude..=) Lude.<$> reason,
            Lude.Just ("WorkerId" Lude..= workerId)
          ]
      )

instance Lude.ToPath DeleteWorkerBlock where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteWorkerBlock where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteWorkerBlockResponse' smart constructor.
newtype DeleteWorkerBlockResponse = DeleteWorkerBlockResponse'
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

-- | Creates a value of 'DeleteWorkerBlockResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteWorkerBlockResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteWorkerBlockResponse
mkDeleteWorkerBlockResponse pResponseStatus_ =
  DeleteWorkerBlockResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbrsResponseStatus :: Lens.Lens' DeleteWorkerBlockResponse Lude.Int
dwbrsResponseStatus = Lens.lens (responseStatus :: DeleteWorkerBlockResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteWorkerBlockResponse)
{-# DEPRECATED dwbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
