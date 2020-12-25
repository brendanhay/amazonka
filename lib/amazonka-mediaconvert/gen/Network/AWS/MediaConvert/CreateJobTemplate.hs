{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.CreateJobTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new job template. For information about job templates see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
module Network.AWS.MediaConvert.CreateJobTemplate
  ( -- * Creating a request
    CreateJobTemplate (..),
    mkCreateJobTemplate,

    -- ** Request lenses
    cjtSettings,
    cjtName,
    cjtAccelerationSettings,
    cjtCategory,
    cjtDescription,
    cjtHopDestinations,
    cjtPriority,
    cjtQueue,
    cjtStatusUpdateInterval,
    cjtTags,

    -- * Destructuring the response
    CreateJobTemplateResponse (..),
    mkCreateJobTemplateResponse,

    -- ** Response lenses
    cjtrrsJobTemplate,
    cjtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateJobTemplate' smart constructor.
data CreateJobTemplate = CreateJobTemplate'
  { -- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
    settings :: Types.JobTemplateSettings,
    -- | The name of the job template you are creating.
    name :: Core.Text,
    -- | Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
    accelerationSettings :: Core.Maybe Types.AccelerationSettings,
    -- | Optional. A category for the job template you are creating
    category :: Core.Maybe Core.Text,
    -- | Optional. A description of the job template you are creating.
    description :: Core.Maybe Core.Text,
    -- | Optional. Use queue hopping to avoid overly long waits in the backlog of the queue that you submit your job to. Specify an alternate queue and the maximum time that your job will wait in the initial queue before hopping. For more information about this feature, see the AWS Elemental MediaConvert User Guide.
    hopDestinations :: Core.Maybe [Types.HopDestination],
    -- | Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
    priority :: Core.Maybe Core.Int,
    -- | Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
    queue :: Core.Maybe Core.Text,
    -- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
    statusUpdateInterval :: Core.Maybe Types.StatusUpdateInterval,
    -- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJobTemplate' value with any optional fields omitted.
mkCreateJobTemplate ::
  -- | 'settings'
  Types.JobTemplateSettings ->
  -- | 'name'
  Core.Text ->
  CreateJobTemplate
mkCreateJobTemplate settings name =
  CreateJobTemplate'
    { settings,
      name,
      accelerationSettings = Core.Nothing,
      category = Core.Nothing,
      description = Core.Nothing,
      hopDestinations = Core.Nothing,
      priority = Core.Nothing,
      queue = Core.Nothing,
      statusUpdateInterval = Core.Nothing,
      tags = Core.Nothing
    }

-- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtSettings :: Lens.Lens' CreateJobTemplate Types.JobTemplateSettings
cjtSettings = Lens.field @"settings"
{-# DEPRECATED cjtSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | The name of the job template you are creating.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtName :: Lens.Lens' CreateJobTemplate Core.Text
cjtName = Lens.field @"name"
{-# DEPRECATED cjtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
--
-- /Note:/ Consider using 'accelerationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtAccelerationSettings :: Lens.Lens' CreateJobTemplate (Core.Maybe Types.AccelerationSettings)
cjtAccelerationSettings = Lens.field @"accelerationSettings"
{-# DEPRECATED cjtAccelerationSettings "Use generic-lens or generic-optics with 'accelerationSettings' instead." #-}

-- | Optional. A category for the job template you are creating
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtCategory :: Lens.Lens' CreateJobTemplate (Core.Maybe Core.Text)
cjtCategory = Lens.field @"category"
{-# DEPRECATED cjtCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | Optional. A description of the job template you are creating.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtDescription :: Lens.Lens' CreateJobTemplate (Core.Maybe Core.Text)
cjtDescription = Lens.field @"description"
{-# DEPRECATED cjtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Optional. Use queue hopping to avoid overly long waits in the backlog of the queue that you submit your job to. Specify an alternate queue and the maximum time that your job will wait in the initial queue before hopping. For more information about this feature, see the AWS Elemental MediaConvert User Guide.
--
-- /Note:/ Consider using 'hopDestinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtHopDestinations :: Lens.Lens' CreateJobTemplate (Core.Maybe [Types.HopDestination])
cjtHopDestinations = Lens.field @"hopDestinations"
{-# DEPRECATED cjtHopDestinations "Use generic-lens or generic-optics with 'hopDestinations' instead." #-}

-- | Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtPriority :: Lens.Lens' CreateJobTemplate (Core.Maybe Core.Int)
cjtPriority = Lens.field @"priority"
{-# DEPRECATED cjtPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtQueue :: Lens.Lens' CreateJobTemplate (Core.Maybe Core.Text)
cjtQueue = Lens.field @"queue"
{-# DEPRECATED cjtQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- /Note:/ Consider using 'statusUpdateInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtStatusUpdateInterval :: Lens.Lens' CreateJobTemplate (Core.Maybe Types.StatusUpdateInterval)
cjtStatusUpdateInterval = Lens.field @"statusUpdateInterval"
{-# DEPRECATED cjtStatusUpdateInterval "Use generic-lens or generic-optics with 'statusUpdateInterval' instead." #-}

-- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtTags :: Lens.Lens' CreateJobTemplate (Core.Maybe (Core.HashMap Core.Text Core.Text))
cjtTags = Lens.field @"tags"
{-# DEPRECATED cjtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateJobTemplate where
  toJSON CreateJobTemplate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("settings" Core..= settings),
            Core.Just ("name" Core..= name),
            ("accelerationSettings" Core..=) Core.<$> accelerationSettings,
            ("category" Core..=) Core.<$> category,
            ("description" Core..=) Core.<$> description,
            ("hopDestinations" Core..=) Core.<$> hopDestinations,
            ("priority" Core..=) Core.<$> priority,
            ("queue" Core..=) Core.<$> queue,
            ("statusUpdateInterval" Core..=) Core.<$> statusUpdateInterval,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateJobTemplate where
  type Rs CreateJobTemplate = CreateJobTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2017-08-29/jobTemplates",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobTemplateResponse'
            Core.<$> (x Core..:? "jobTemplate") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateJobTemplateResponse' smart constructor.
data CreateJobTemplateResponse = CreateJobTemplateResponse'
  { -- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
    jobTemplate :: Core.Maybe Types.JobTemplate,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateJobTemplateResponse' value with any optional fields omitted.
mkCreateJobTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateJobTemplateResponse
mkCreateJobTemplateResponse responseStatus =
  CreateJobTemplateResponse'
    { jobTemplate = Core.Nothing,
      responseStatus
    }

-- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
--
-- /Note:/ Consider using 'jobTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtrrsJobTemplate :: Lens.Lens' CreateJobTemplateResponse (Core.Maybe Types.JobTemplate)
cjtrrsJobTemplate = Lens.field @"jobTemplate"
{-# DEPRECATED cjtrrsJobTemplate "Use generic-lens or generic-optics with 'jobTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtrrsResponseStatus :: Lens.Lens' CreateJobTemplateResponse Core.Int
cjtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cjtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
