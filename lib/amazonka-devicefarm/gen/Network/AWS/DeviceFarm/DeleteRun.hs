{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the run, given the run ARN.
--
-- Deleting this resource does not stop an in-progress run.
module Network.AWS.DeviceFarm.DeleteRun
  ( -- * Creating a request
    DeleteRun (..),
    mkDeleteRun,

    -- ** Request lenses
    drArn,

    -- * Destructuring the response
    DeleteRunResponse (..),
    mkDeleteRunResponse,

    -- ** Response lenses
    drrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the delete run operation.
--
-- /See:/ 'mkDeleteRun' smart constructor.
newtype DeleteRun = DeleteRun'
  { -- | The Amazon Resource Name (ARN) for the run to delete.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRun' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) for the run to delete.
mkDeleteRun ::
  -- | 'arn'
  Lude.Text ->
  DeleteRun
mkDeleteRun pArn_ = DeleteRun' {arn = pArn_}

-- | The Amazon Resource Name (ARN) for the run to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drArn :: Lens.Lens' DeleteRun Lude.Text
drArn = Lens.lens (arn :: DeleteRun -> Lude.Text) (\s a -> s {arn = a} :: DeleteRun)
{-# DEPRECATED drArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest DeleteRun where
  type Rs DeleteRun = DeleteRunResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteRunResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.DeleteRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRun where
  toJSON DeleteRun' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath DeleteRun where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRun where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a delete run request.
--
-- /See:/ 'mkDeleteRunResponse' smart constructor.
newtype DeleteRunResponse = DeleteRunResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRunResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRunResponse
mkDeleteRunResponse pResponseStatus_ =
  DeleteRunResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DeleteRunResponse Lude.Int
drrsResponseStatus = Lens.lens (responseStatus :: DeleteRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRunResponse)
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
