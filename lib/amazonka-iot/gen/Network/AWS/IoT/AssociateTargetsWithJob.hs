{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.AssociateTargetsWithJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a group with a continuous job. The following criteria must be met:
--
--
--     * The job must have been created with the @targetSelection@ field set to "CONTINUOUS".
--
--
--     * The job status must currently be "IN_PROGRESS".
--
--
--     * The total number of targets associated with a job must not exceed 100.
module Network.AWS.IoT.AssociateTargetsWithJob
  ( -- * Creating a request
    AssociateTargetsWithJob (..),
    mkAssociateTargetsWithJob,

    -- ** Request lenses
    atwjTargets,
    atwjJobId,
    atwjComment,
    atwjNamespaceId,

    -- * Destructuring the response
    AssociateTargetsWithJobResponse (..),
    mkAssociateTargetsWithJobResponse,

    -- ** Response lenses
    atwjrrsDescription,
    atwjrrsJobArn,
    atwjrrsJobId,
    atwjrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateTargetsWithJob' smart constructor.
data AssociateTargetsWithJob = AssociateTargetsWithJob'
  { -- | A list of thing group ARNs that define the targets of the job.
    targets :: Core.NonEmpty Types.TargetArn,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Types.JobId,
    -- | An optional comment string describing why the job was associated with the targets.
    comment :: Core.Maybe Types.Comment,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
    -- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
    namespaceId :: Core.Maybe Types.NamespaceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTargetsWithJob' value with any optional fields omitted.
mkAssociateTargetsWithJob ::
  -- | 'targets'
  Core.NonEmpty Types.TargetArn ->
  -- | 'jobId'
  Types.JobId ->
  AssociateTargetsWithJob
mkAssociateTargetsWithJob targets jobId =
  AssociateTargetsWithJob'
    { targets,
      jobId,
      comment = Core.Nothing,
      namespaceId = Core.Nothing
    }

-- | A list of thing group ARNs that define the targets of the job.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjTargets :: Lens.Lens' AssociateTargetsWithJob (Core.NonEmpty Types.TargetArn)
atwjTargets = Lens.field @"targets"
{-# DEPRECATED atwjTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjJobId :: Lens.Lens' AssociateTargetsWithJob Types.JobId
atwjJobId = Lens.field @"jobId"
{-# DEPRECATED atwjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | An optional comment string describing why the job was associated with the targets.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjComment :: Lens.Lens' AssociateTargetsWithJob (Core.Maybe Types.Comment)
atwjComment = Lens.field @"comment"
{-# DEPRECATED atwjComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjNamespaceId :: Lens.Lens' AssociateTargetsWithJob (Core.Maybe Types.NamespaceId)
atwjNamespaceId = Lens.field @"namespaceId"
{-# DEPRECATED atwjNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

instance Core.FromJSON AssociateTargetsWithJob where
  toJSON AssociateTargetsWithJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("targets" Core..= targets),
            ("comment" Core..=) Core.<$> comment
          ]
      )

instance Core.AWSRequest AssociateTargetsWithJob where
  type Rs AssociateTargetsWithJob = AssociateTargetsWithJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/jobs/" Core.<> (Core.toText jobId) Core.<> ("/targets")),
        Core._rqQuery =
          Core.toQueryValue "namespaceId" Core.<$> namespaceId,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateTargetsWithJobResponse'
            Core.<$> (x Core..:? "description")
            Core.<*> (x Core..:? "jobArn")
            Core.<*> (x Core..:? "jobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateTargetsWithJobResponse' smart constructor.
data AssociateTargetsWithJobResponse = AssociateTargetsWithJobResponse'
  { -- | A short text description of the job.
    description :: Core.Maybe Types.Description,
    -- | An ARN identifying the job.
    jobArn :: Core.Maybe Types.JobArn,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Core.Maybe Types.JobId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTargetsWithJobResponse' value with any optional fields omitted.
mkAssociateTargetsWithJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateTargetsWithJobResponse
mkAssociateTargetsWithJobResponse responseStatus =
  AssociateTargetsWithJobResponse'
    { description = Core.Nothing,
      jobArn = Core.Nothing,
      jobId = Core.Nothing,
      responseStatus
    }

-- | A short text description of the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjrrsDescription :: Lens.Lens' AssociateTargetsWithJobResponse (Core.Maybe Types.Description)
atwjrrsDescription = Lens.field @"description"
{-# DEPRECATED atwjrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | An ARN identifying the job.
--
-- /Note:/ Consider using 'jobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjrrsJobArn :: Lens.Lens' AssociateTargetsWithJobResponse (Core.Maybe Types.JobArn)
atwjrrsJobArn = Lens.field @"jobArn"
{-# DEPRECATED atwjrrsJobArn "Use generic-lens or generic-optics with 'jobArn' instead." #-}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjrrsJobId :: Lens.Lens' AssociateTargetsWithJobResponse (Core.Maybe Types.JobId)
atwjrrsJobId = Lens.field @"jobId"
{-# DEPRECATED atwjrrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atwjrrsResponseStatus :: Lens.Lens' AssociateTargetsWithJobResponse Core.Int
atwjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED atwjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
