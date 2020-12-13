{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.CreateWorkerBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateWorkerBlock@ operation allows you to prevent a Worker from working on your HITs. For example, you can block a Worker who is producing poor quality work. You can block up to 100,000 Workers.
module Network.AWS.MechanicalTurk.CreateWorkerBlock
  ( -- * Creating a request
    CreateWorkerBlock (..),
    mkCreateWorkerBlock,

    -- ** Request lenses
    cwbReason,
    cwbWorkerId,

    -- * Destructuring the response
    CreateWorkerBlockResponse (..),
    mkCreateWorkerBlockResponse,

    -- ** Response lenses
    cwbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateWorkerBlock' smart constructor.
data CreateWorkerBlock = CreateWorkerBlock'
  { -- | A message explaining the reason for blocking the Worker. This parameter enables you to keep track of your Workers. The Worker does not see this message.
    reason :: Lude.Text,
    -- | The ID of the Worker to block.
    workerId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateWorkerBlock' with the minimum fields required to make a request.
--
-- * 'reason' - A message explaining the reason for blocking the Worker. This parameter enables you to keep track of your Workers. The Worker does not see this message.
-- * 'workerId' - The ID of the Worker to block.
mkCreateWorkerBlock ::
  -- | 'reason'
  Lude.Text ->
  -- | 'workerId'
  Lude.Text ->
  CreateWorkerBlock
mkCreateWorkerBlock pReason_ pWorkerId_ =
  CreateWorkerBlock' {reason = pReason_, workerId = pWorkerId_}

-- | A message explaining the reason for blocking the Worker. This parameter enables you to keep track of your Workers. The Worker does not see this message.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwbReason :: Lens.Lens' CreateWorkerBlock Lude.Text
cwbReason = Lens.lens (reason :: CreateWorkerBlock -> Lude.Text) (\s a -> s {reason = a} :: CreateWorkerBlock)
{-# DEPRECATED cwbReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The ID of the Worker to block.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwbWorkerId :: Lens.Lens' CreateWorkerBlock Lude.Text
cwbWorkerId = Lens.lens (workerId :: CreateWorkerBlock -> Lude.Text) (\s a -> s {workerId = a} :: CreateWorkerBlock)
{-# DEPRECATED cwbWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

instance Lude.AWSRequest CreateWorkerBlock where
  type Rs CreateWorkerBlock = CreateWorkerBlockResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateWorkerBlockResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateWorkerBlock where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.CreateWorkerBlock" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateWorkerBlock where
  toJSON CreateWorkerBlock' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Reason" Lude..= reason),
            Lude.Just ("WorkerId" Lude..= workerId)
          ]
      )

instance Lude.ToPath CreateWorkerBlock where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateWorkerBlock where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateWorkerBlockResponse' smart constructor.
newtype CreateWorkerBlockResponse = CreateWorkerBlockResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateWorkerBlockResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateWorkerBlockResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateWorkerBlockResponse
mkCreateWorkerBlockResponse pResponseStatus_ =
  CreateWorkerBlockResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwbrsResponseStatus :: Lens.Lens' CreateWorkerBlockResponse Lude.Int
cwbrsResponseStatus = Lens.lens (responseStatus :: CreateWorkerBlockResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateWorkerBlockResponse)
{-# DEPRECATED cwbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
