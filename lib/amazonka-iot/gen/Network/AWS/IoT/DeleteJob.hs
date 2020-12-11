{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a job and its related job executions.
--
-- Deleting a job may take time, depending on the number of job executions created for the job and various other factors. While the job is being deleted, the status of the job will be shown as "DELETION_IN_PROGRESS". Attempting to delete or cancel a job whose status is already "DELETION_IN_PROGRESS" will result in an error.
-- Only 10 jobs may have status "DELETION_IN_PROGRESS" at the same time, or a LimitExceededException will occur.
module Network.AWS.IoT.DeleteJob
  ( -- * Creating a request
    DeleteJob (..),
    mkDeleteJob,

    -- ** Request lenses
    djForce,
    djNamespaceId,
    djJobId,

    -- * Destructuring the response
    DeleteJobResponse (..),
    mkDeleteJobResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteJob' smart constructor.
data DeleteJob = DeleteJob'
  { force :: Lude.Maybe Lude.Bool,
    namespaceId :: Lude.Maybe Lude.Text,
    jobId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteJob' with the minimum fields required to make a request.
--
-- * 'force' - (Optional) When true, you can delete a job which is "IN_PROGRESS". Otherwise, you can only delete a job which is in a terminal state ("COMPLETED" or "CANCELED") or an exception will occur. The default is false.
-- * 'jobId' - The ID of the job to be deleted.
--
-- After a job deletion is completed, you may reuse this jobId when you create a new job. However, this is not recommended, and you must ensure that your devices are not using the jobId to refer to the deleted job.
-- * 'namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
mkDeleteJob ::
  -- | 'jobId'
  Lude.Text ->
  DeleteJob
mkDeleteJob pJobId_ =
  DeleteJob'
    { force = Lude.Nothing,
      namespaceId = Lude.Nothing,
      jobId = pJobId_
    }

-- | (Optional) When true, you can delete a job which is "IN_PROGRESS". Otherwise, you can only delete a job which is in a terminal state ("COMPLETED" or "CANCELED") or an exception will occur. The default is false.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djForce :: Lens.Lens' DeleteJob (Lude.Maybe Lude.Bool)
djForce = Lens.lens (force :: DeleteJob -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: DeleteJob)
{-# DEPRECATED djForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djNamespaceId :: Lens.Lens' DeleteJob (Lude.Maybe Lude.Text)
djNamespaceId = Lens.lens (namespaceId :: DeleteJob -> Lude.Maybe Lude.Text) (\s a -> s {namespaceId = a} :: DeleteJob)
{-# DEPRECATED djNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

-- | The ID of the job to be deleted.
--
-- After a job deletion is completed, you may reuse this jobId when you create a new job. However, this is not recommended, and you must ensure that your devices are not using the jobId to refer to the deleted job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djJobId :: Lens.Lens' DeleteJob Lude.Text
djJobId = Lens.lens (jobId :: DeleteJob -> Lude.Text) (\s a -> s {jobId = a} :: DeleteJob)
{-# DEPRECATED djJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest DeleteJob where
  type Rs DeleteJob = DeleteJobResponse
  request = Req.delete ioTService
  response = Res.receiveNull DeleteJobResponse'

instance Lude.ToHeaders DeleteJob where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteJob where
  toPath DeleteJob' {..} = Lude.mconcat ["/jobs/", Lude.toBS jobId]

instance Lude.ToQuery DeleteJob where
  toQuery DeleteJob' {..} =
    Lude.mconcat
      ["force" Lude.=: force, "namespaceId" Lude.=: namespaceId]

-- | /See:/ 'mkDeleteJobResponse' smart constructor.
data DeleteJobResponse = DeleteJobResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteJobResponse' with the minimum fields required to make a request.
mkDeleteJobResponse ::
  DeleteJobResponse
mkDeleteJobResponse = DeleteJobResponse'
