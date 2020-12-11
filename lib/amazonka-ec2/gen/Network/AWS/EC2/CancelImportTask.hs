{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelImportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an in-process import virtual machine or import snapshot task.
module Network.AWS.EC2.CancelImportTask
  ( -- * Creating a request
    CancelImportTask (..),
    mkCancelImportTask,

    -- ** Request lenses
    citCancelReason,
    citImportTaskId,
    citDryRun,

    -- * Destructuring the response
    CancelImportTaskResponse (..),
    mkCancelImportTaskResponse,

    -- ** Response lenses
    citrsState,
    citrsImportTaskId,
    citrsPreviousState,
    citrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelImportTask' smart constructor.
data CancelImportTask = CancelImportTask'
  { cancelReason ::
      Lude.Maybe Lude.Text,
    importTaskId :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelImportTask' with the minimum fields required to make a request.
--
-- * 'cancelReason' - The reason for canceling the task.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'importTaskId' - The ID of the import image or import snapshot task to be canceled.
mkCancelImportTask ::
  CancelImportTask
mkCancelImportTask =
  CancelImportTask'
    { cancelReason = Lude.Nothing,
      importTaskId = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The reason for canceling the task.
--
-- /Note:/ Consider using 'cancelReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citCancelReason :: Lens.Lens' CancelImportTask (Lude.Maybe Lude.Text)
citCancelReason = Lens.lens (cancelReason :: CancelImportTask -> Lude.Maybe Lude.Text) (\s a -> s {cancelReason = a} :: CancelImportTask)
{-# DEPRECATED citCancelReason "Use generic-lens or generic-optics with 'cancelReason' instead." #-}

-- | The ID of the import image or import snapshot task to be canceled.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citImportTaskId :: Lens.Lens' CancelImportTask (Lude.Maybe Lude.Text)
citImportTaskId = Lens.lens (importTaskId :: CancelImportTask -> Lude.Maybe Lude.Text) (\s a -> s {importTaskId = a} :: CancelImportTask)
{-# DEPRECATED citImportTaskId "Use generic-lens or generic-optics with 'importTaskId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citDryRun :: Lens.Lens' CancelImportTask (Lude.Maybe Lude.Bool)
citDryRun = Lens.lens (dryRun :: CancelImportTask -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CancelImportTask)
{-# DEPRECATED citDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CancelImportTask where
  type Rs CancelImportTask = CancelImportTaskResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CancelImportTaskResponse'
            Lude.<$> (x Lude..@? "state")
            Lude.<*> (x Lude..@? "importTaskId")
            Lude.<*> (x Lude..@? "previousState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelImportTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CancelImportTask where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelImportTask where
  toQuery CancelImportTask' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CancelImportTask" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "CancelReason" Lude.=: cancelReason,
        "ImportTaskId" Lude.=: importTaskId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCancelImportTaskResponse' smart constructor.
data CancelImportTaskResponse = CancelImportTaskResponse'
  { state ::
      Lude.Maybe Lude.Text,
    importTaskId :: Lude.Maybe Lude.Text,
    previousState :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CancelImportTaskResponse' with the minimum fields required to make a request.
--
-- * 'importTaskId' - The ID of the task being canceled.
-- * 'previousState' - The current state of the task being canceled.
-- * 'responseStatus' - The response status code.
-- * 'state' - The current state of the task being canceled.
mkCancelImportTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelImportTaskResponse
mkCancelImportTaskResponse pResponseStatus_ =
  CancelImportTaskResponse'
    { state = Lude.Nothing,
      importTaskId = Lude.Nothing,
      previousState = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current state of the task being canceled.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citrsState :: Lens.Lens' CancelImportTaskResponse (Lude.Maybe Lude.Text)
citrsState = Lens.lens (state :: CancelImportTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: CancelImportTaskResponse)
{-# DEPRECATED citrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the task being canceled.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citrsImportTaskId :: Lens.Lens' CancelImportTaskResponse (Lude.Maybe Lude.Text)
citrsImportTaskId = Lens.lens (importTaskId :: CancelImportTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {importTaskId = a} :: CancelImportTaskResponse)
{-# DEPRECATED citrsImportTaskId "Use generic-lens or generic-optics with 'importTaskId' instead." #-}

-- | The current state of the task being canceled.
--
-- /Note:/ Consider using 'previousState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citrsPreviousState :: Lens.Lens' CancelImportTaskResponse (Lude.Maybe Lude.Text)
citrsPreviousState = Lens.lens (previousState :: CancelImportTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {previousState = a} :: CancelImportTaskResponse)
{-# DEPRECATED citrsPreviousState "Use generic-lens or generic-optics with 'previousState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citrsResponseStatus :: Lens.Lens' CancelImportTaskResponse Lude.Int
citrsResponseStatus = Lens.lens (responseStatus :: CancelImportTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelImportTaskResponse)
{-# DEPRECATED citrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
