{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteJobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a job execution.
module Network.AWS.IoT.DeleteJobExecution
  ( -- * Creating a request
    DeleteJobExecution (..),
    mkDeleteJobExecution,

    -- ** Request lenses
    djeJobId,
    djeForce,
    djeNamespaceId,
    djeExecutionNumber,
    djeThingName,

    -- * Destructuring the response
    DeleteJobExecutionResponse (..),
    mkDeleteJobExecutionResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteJobExecution' smart constructor.
data DeleteJobExecution = DeleteJobExecution'
  { -- | The ID of the job whose execution on a particular device will be deleted.
    jobId :: Lude.Text,
    -- | (Optional) When true, you can delete a job execution which is "IN_PROGRESS". Otherwise, you can only delete a job execution which is in a terminal state ("SUCCEEDED", "FAILED", "REJECTED", "REMOVED" or "CANCELED") or an exception will occur. The default is false.
    force :: Lude.Maybe Lude.Bool,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
    -- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
    namespaceId :: Lude.Maybe Lude.Text,
    -- | The ID of the job execution to be deleted. The @executionNumber@ refers to the execution of a particular job on a particular device.
    --
    -- Note that once a job execution is deleted, the @executionNumber@ may be reused by IoT, so be sure you get and use the correct value here.
    executionNumber :: Lude.Integer,
    -- | The name of the thing whose job execution will be deleted.
    thingName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteJobExecution' with the minimum fields required to make a request.
--
-- * 'jobId' - The ID of the job whose execution on a particular device will be deleted.
-- * 'force' - (Optional) When true, you can delete a job execution which is "IN_PROGRESS". Otherwise, you can only delete a job execution which is in a terminal state ("SUCCEEDED", "FAILED", "REJECTED", "REMOVED" or "CANCELED") or an exception will occur. The default is false.
-- * 'namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
-- * 'executionNumber' - The ID of the job execution to be deleted. The @executionNumber@ refers to the execution of a particular job on a particular device.
--
-- Note that once a job execution is deleted, the @executionNumber@ may be reused by IoT, so be sure you get and use the correct value here.
-- * 'thingName' - The name of the thing whose job execution will be deleted.
mkDeleteJobExecution ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'executionNumber'
  Lude.Integer ->
  -- | 'thingName'
  Lude.Text ->
  DeleteJobExecution
mkDeleteJobExecution pJobId_ pExecutionNumber_ pThingName_ =
  DeleteJobExecution'
    { jobId = pJobId_,
      force = Lude.Nothing,
      namespaceId = Lude.Nothing,
      executionNumber = pExecutionNumber_,
      thingName = pThingName_
    }

-- | The ID of the job whose execution on a particular device will be deleted.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeJobId :: Lens.Lens' DeleteJobExecution Lude.Text
djeJobId = Lens.lens (jobId :: DeleteJobExecution -> Lude.Text) (\s a -> s {jobId = a} :: DeleteJobExecution)
{-# DEPRECATED djeJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | (Optional) When true, you can delete a job execution which is "IN_PROGRESS". Otherwise, you can only delete a job execution which is in a terminal state ("SUCCEEDED", "FAILED", "REJECTED", "REMOVED" or "CANCELED") or an exception will occur. The default is false.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeForce :: Lens.Lens' DeleteJobExecution (Lude.Maybe Lude.Bool)
djeForce = Lens.lens (force :: DeleteJobExecution -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: DeleteJobExecution)
{-# DEPRECATED djeForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeNamespaceId :: Lens.Lens' DeleteJobExecution (Lude.Maybe Lude.Text)
djeNamespaceId = Lens.lens (namespaceId :: DeleteJobExecution -> Lude.Maybe Lude.Text) (\s a -> s {namespaceId = a} :: DeleteJobExecution)
{-# DEPRECATED djeNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

-- | The ID of the job execution to be deleted. The @executionNumber@ refers to the execution of a particular job on a particular device.
--
-- Note that once a job execution is deleted, the @executionNumber@ may be reused by IoT, so be sure you get and use the correct value here.
--
-- /Note:/ Consider using 'executionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeExecutionNumber :: Lens.Lens' DeleteJobExecution Lude.Integer
djeExecutionNumber = Lens.lens (executionNumber :: DeleteJobExecution -> Lude.Integer) (\s a -> s {executionNumber = a} :: DeleteJobExecution)
{-# DEPRECATED djeExecutionNumber "Use generic-lens or generic-optics with 'executionNumber' instead." #-}

-- | The name of the thing whose job execution will be deleted.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeThingName :: Lens.Lens' DeleteJobExecution Lude.Text
djeThingName = Lens.lens (thingName :: DeleteJobExecution -> Lude.Text) (\s a -> s {thingName = a} :: DeleteJobExecution)
{-# DEPRECATED djeThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest DeleteJobExecution where
  type Rs DeleteJobExecution = DeleteJobExecutionResponse
  request = Req.delete ioTService
  response = Res.receiveNull DeleteJobExecutionResponse'

instance Lude.ToHeaders DeleteJobExecution where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteJobExecution where
  toPath DeleteJobExecution' {..} =
    Lude.mconcat
      [ "/things/",
        Lude.toBS thingName,
        "/jobs/",
        Lude.toBS jobId,
        "/executionNumber/",
        Lude.toBS executionNumber
      ]

instance Lude.ToQuery DeleteJobExecution where
  toQuery DeleteJobExecution' {..} =
    Lude.mconcat
      ["force" Lude.=: force, "namespaceId" Lude.=: namespaceId]

-- | /See:/ 'mkDeleteJobExecutionResponse' smart constructor.
data DeleteJobExecutionResponse = DeleteJobExecutionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteJobExecutionResponse' with the minimum fields required to make a request.
mkDeleteJobExecutionResponse ::
  DeleteJobExecutionResponse
mkDeleteJobExecutionResponse = DeleteJobExecutionResponse'
