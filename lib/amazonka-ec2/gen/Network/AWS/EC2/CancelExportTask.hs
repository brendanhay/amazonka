{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an active export task. The request removes all artifacts of the export, including any partially-created Amazon S3 objects. If the export task is complete or is in the process of transferring the final disk image, the command fails and returns an error.
module Network.AWS.EC2.CancelExportTask
  ( -- * Creating a request
    CancelExportTask (..),
    mkCancelExportTask,

    -- ** Request lenses
    cetExportTaskId,

    -- * Destructuring the response
    CancelExportTaskResponse (..),
    mkCancelExportTaskResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelExportTask' smart constructor.
newtype CancelExportTask = CancelExportTask'
  { exportTaskId ::
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

-- | Creates a value of 'CancelExportTask' with the minimum fields required to make a request.
--
-- * 'exportTaskId' - The ID of the export task. This is the ID returned by @CreateInstanceExportTask@ .
mkCancelExportTask ::
  -- | 'exportTaskId'
  Lude.Text ->
  CancelExportTask
mkCancelExportTask pExportTaskId_ =
  CancelExportTask' {exportTaskId = pExportTaskId_}

-- | The ID of the export task. This is the ID returned by @CreateInstanceExportTask@ .
--
-- /Note:/ Consider using 'exportTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetExportTaskId :: Lens.Lens' CancelExportTask Lude.Text
cetExportTaskId = Lens.lens (exportTaskId :: CancelExportTask -> Lude.Text) (\s a -> s {exportTaskId = a} :: CancelExportTask)
{-# DEPRECATED cetExportTaskId "Use generic-lens or generic-optics with 'exportTaskId' instead." #-}

instance Lude.AWSRequest CancelExportTask where
  type Rs CancelExportTask = CancelExportTaskResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull CancelExportTaskResponse'

instance Lude.ToHeaders CancelExportTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CancelExportTask where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelExportTask where
  toQuery CancelExportTask' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CancelExportTask" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ExportTaskId" Lude.=: exportTaskId
      ]

-- | /See:/ 'mkCancelExportTaskResponse' smart constructor.
data CancelExportTaskResponse = CancelExportTaskResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelExportTaskResponse' with the minimum fields required to make a request.
mkCancelExportTaskResponse ::
  CancelExportTaskResponse
mkCancelExportTaskResponse = CancelExportTaskResponse'
