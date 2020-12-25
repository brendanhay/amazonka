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
    djeThingName,
    djeExecutionNumber,
    djeForce,
    djeNamespaceId,

    -- * Destructuring the response
    DeleteJobExecutionResponse (..),
    mkDeleteJobExecutionResponse,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteJobExecution' smart constructor.
data DeleteJobExecution = DeleteJobExecution'
  { -- | The ID of the job whose execution on a particular device will be deleted.
    jobId :: Types.JobId,
    -- | The name of the thing whose job execution will be deleted.
    thingName :: Types.ThingName,
    -- | The ID of the job execution to be deleted. The @executionNumber@ refers to the execution of a particular job on a particular device.
    --
    -- Note that once a job execution is deleted, the @executionNumber@ may be reused by IoT, so be sure you get and use the correct value here.
    executionNumber :: Core.Integer,
    -- | (Optional) When true, you can delete a job execution which is "IN_PROGRESS". Otherwise, you can only delete a job execution which is in a terminal state ("SUCCEEDED", "FAILED", "REJECTED", "REMOVED" or "CANCELED") or an exception will occur. The default is false.
    force :: Core.Maybe Core.Bool,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
    -- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
    namespaceId :: Core.Maybe Types.NamespaceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteJobExecution' value with any optional fields omitted.
mkDeleteJobExecution ::
  -- | 'jobId'
  Types.JobId ->
  -- | 'thingName'
  Types.ThingName ->
  -- | 'executionNumber'
  Core.Integer ->
  DeleteJobExecution
mkDeleteJobExecution jobId thingName executionNumber =
  DeleteJobExecution'
    { jobId,
      thingName,
      executionNumber,
      force = Core.Nothing,
      namespaceId = Core.Nothing
    }

-- | The ID of the job whose execution on a particular device will be deleted.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeJobId :: Lens.Lens' DeleteJobExecution Types.JobId
djeJobId = Lens.field @"jobId"
{-# DEPRECATED djeJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name of the thing whose job execution will be deleted.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeThingName :: Lens.Lens' DeleteJobExecution Types.ThingName
djeThingName = Lens.field @"thingName"
{-# DEPRECATED djeThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The ID of the job execution to be deleted. The @executionNumber@ refers to the execution of a particular job on a particular device.
--
-- Note that once a job execution is deleted, the @executionNumber@ may be reused by IoT, so be sure you get and use the correct value here.
--
-- /Note:/ Consider using 'executionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeExecutionNumber :: Lens.Lens' DeleteJobExecution Core.Integer
djeExecutionNumber = Lens.field @"executionNumber"
{-# DEPRECATED djeExecutionNumber "Use generic-lens or generic-optics with 'executionNumber' instead." #-}

-- | (Optional) When true, you can delete a job execution which is "IN_PROGRESS". Otherwise, you can only delete a job execution which is in a terminal state ("SUCCEEDED", "FAILED", "REJECTED", "REMOVED" or "CANCELED") or an exception will occur. The default is false.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeForce :: Lens.Lens' DeleteJobExecution (Core.Maybe Core.Bool)
djeForce = Lens.field @"force"
{-# DEPRECATED djeForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djeNamespaceId :: Lens.Lens' DeleteJobExecution (Core.Maybe Types.NamespaceId)
djeNamespaceId = Lens.field @"namespaceId"
{-# DEPRECATED djeNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

instance Core.AWSRequest DeleteJobExecution where
  type Rs DeleteJobExecution = DeleteJobExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/things/" Core.<> (Core.toText thingName) Core.<> ("/jobs/")
                Core.<> (Core.toText jobId)
                Core.<> ("/executionNumber/")
                Core.<> (Core.toText executionNumber)
            ),
        Core._rqQuery =
          Core.toQueryValue "force" Core.<$> force
            Core.<> (Core.toQueryValue "namespaceId" Core.<$> namespaceId),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteJobExecutionResponse'

-- | /See:/ 'mkDeleteJobExecutionResponse' smart constructor.
data DeleteJobExecutionResponse = DeleteJobExecutionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteJobExecutionResponse' value with any optional fields omitted.
mkDeleteJobExecutionResponse ::
  DeleteJobExecutionResponse
mkDeleteJobExecutionResponse = DeleteJobExecutionResponse'
