{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.CreateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new transcoding job. For information about jobs and job settings, see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
module Network.AWS.MediaConvert.CreateJob
    (
    -- * Creating a request
      CreateJob (..)
    , mkCreateJob
    -- ** Request lenses
    , cjRole
    , cjSettings
    , cjAccelerationSettings
    , cjBillingTagsSource
    , cjClientRequestToken
    , cjHopDestinations
    , cjJobTemplate
    , cjPriority
    , cjQueue
    , cjSimulateReservedQueue
    , cjStatusUpdateInterval
    , cjTags
    , cjUserMetadata

    -- * Destructuring the response
    , CreateJobResponse (..)
    , mkCreateJobResponse
    -- ** Response lenses
    , cjrrsJob
    , cjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateJob' smart constructor.
data CreateJob = CreateJob'
  { role' :: Core.Text
    -- ^ Required. The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html.
  , settings :: Types.JobSettings
    -- ^ JobSettings contains all the transcode settings for a job.
  , accelerationSettings :: Core.Maybe Types.AccelerationSettings
    -- ^ Optional. Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
  , billingTagsSource :: Core.Maybe Types.BillingTagsSource
    -- ^ Optional. Choose a tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up. Any transcoding outputs that don't have an associated tag will appear in your billing report unsorted. If you don't choose a valid value for this field, your job outputs will appear on the billing report unsorted.
  , clientRequestToken :: Core.Maybe Core.Text
    -- ^ Optional. Idempotency token for CreateJob operation.
  , hopDestinations :: Core.Maybe [Types.HopDestination]
    -- ^ Optional. Use queue hopping to avoid overly long waits in the backlog of the queue that you submit your job to. Specify an alternate queue and the maximum time that your job will wait in the initial queue before hopping. For more information about this feature, see the AWS Elemental MediaConvert User Guide.
  , jobTemplate :: Core.Maybe Core.Text
    -- ^ Optional. When you create a job, you can either specify a job template or specify the transcoding settings individually.
  , priority :: Core.Maybe Core.Int
    -- ^ Optional. Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
  , queue :: Core.Maybe Core.Text
    -- ^ Optional. When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html.
  , simulateReservedQueue :: Core.Maybe Types.SimulateReservedQueue
    -- ^ Optional. Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
  , statusUpdateInterval :: Core.Maybe Types.StatusUpdateInterval
    -- ^ Optional. Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Optional. The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.  Use standard AWS tags on your job for automatic integration with AWS services and for custom integrations and workflows.
  , userMetadata :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Optional. User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.  Use only for existing integrations or workflows that rely on job metadata tags. Otherwise, we recommend that you use standard AWS tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJob' value with any optional fields omitted.
mkCreateJob
    :: Core.Text -- ^ 'role\''
    -> Types.JobSettings -- ^ 'settings'
    -> CreateJob
mkCreateJob role' settings
  = CreateJob'{role', settings, accelerationSettings = Core.Nothing,
               billingTagsSource = Core.Nothing,
               clientRequestToken = Core.Nothing, hopDestinations = Core.Nothing,
               jobTemplate = Core.Nothing, priority = Core.Nothing,
               queue = Core.Nothing, simulateReservedQueue = Core.Nothing,
               statusUpdateInterval = Core.Nothing, tags = Core.Nothing,
               userMetadata = Core.Nothing}

-- | Required. The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjRole :: Lens.Lens' CreateJob Core.Text
cjRole = Lens.field @"role'"
{-# INLINEABLE cjRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

-- | JobSettings contains all the transcode settings for a job.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjSettings :: Lens.Lens' CreateJob Types.JobSettings
cjSettings = Lens.field @"settings"
{-# INLINEABLE cjSettings #-}
{-# DEPRECATED settings "Use generic-lens or generic-optics with 'settings' instead"  #-}

-- | Optional. Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
--
-- /Note:/ Consider using 'accelerationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjAccelerationSettings :: Lens.Lens' CreateJob (Core.Maybe Types.AccelerationSettings)
cjAccelerationSettings = Lens.field @"accelerationSettings"
{-# INLINEABLE cjAccelerationSettings #-}
{-# DEPRECATED accelerationSettings "Use generic-lens or generic-optics with 'accelerationSettings' instead"  #-}

-- | Optional. Choose a tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up. Any transcoding outputs that don't have an associated tag will appear in your billing report unsorted. If you don't choose a valid value for this field, your job outputs will appear on the billing report unsorted.
--
-- /Note:/ Consider using 'billingTagsSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjBillingTagsSource :: Lens.Lens' CreateJob (Core.Maybe Types.BillingTagsSource)
cjBillingTagsSource = Lens.field @"billingTagsSource"
{-# INLINEABLE cjBillingTagsSource #-}
{-# DEPRECATED billingTagsSource "Use generic-lens or generic-optics with 'billingTagsSource' instead"  #-}

-- | Optional. Idempotency token for CreateJob operation.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjClientRequestToken :: Lens.Lens' CreateJob (Core.Maybe Core.Text)
cjClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE cjClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | Optional. Use queue hopping to avoid overly long waits in the backlog of the queue that you submit your job to. Specify an alternate queue and the maximum time that your job will wait in the initial queue before hopping. For more information about this feature, see the AWS Elemental MediaConvert User Guide.
--
-- /Note:/ Consider using 'hopDestinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjHopDestinations :: Lens.Lens' CreateJob (Core.Maybe [Types.HopDestination])
cjHopDestinations = Lens.field @"hopDestinations"
{-# INLINEABLE cjHopDestinations #-}
{-# DEPRECATED hopDestinations "Use generic-lens or generic-optics with 'hopDestinations' instead"  #-}

-- | Optional. When you create a job, you can either specify a job template or specify the transcoding settings individually.
--
-- /Note:/ Consider using 'jobTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjJobTemplate :: Lens.Lens' CreateJob (Core.Maybe Core.Text)
cjJobTemplate = Lens.field @"jobTemplate"
{-# INLINEABLE cjJobTemplate #-}
{-# DEPRECATED jobTemplate "Use generic-lens or generic-optics with 'jobTemplate' instead"  #-}

-- | Optional. Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjPriority :: Lens.Lens' CreateJob (Core.Maybe Core.Int)
cjPriority = Lens.field @"priority"
{-# INLINEABLE cjPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | Optional. When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjQueue :: Lens.Lens' CreateJob (Core.Maybe Core.Text)
cjQueue = Lens.field @"queue"
{-# INLINEABLE cjQueue #-}
{-# DEPRECATED queue "Use generic-lens or generic-optics with 'queue' instead"  #-}

-- | Optional. Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
--
-- /Note:/ Consider using 'simulateReservedQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjSimulateReservedQueue :: Lens.Lens' CreateJob (Core.Maybe Types.SimulateReservedQueue)
cjSimulateReservedQueue = Lens.field @"simulateReservedQueue"
{-# INLINEABLE cjSimulateReservedQueue #-}
{-# DEPRECATED simulateReservedQueue "Use generic-lens or generic-optics with 'simulateReservedQueue' instead"  #-}

-- | Optional. Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- /Note:/ Consider using 'statusUpdateInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjStatusUpdateInterval :: Lens.Lens' CreateJob (Core.Maybe Types.StatusUpdateInterval)
cjStatusUpdateInterval = Lens.field @"statusUpdateInterval"
{-# INLINEABLE cjStatusUpdateInterval #-}
{-# DEPRECATED statusUpdateInterval "Use generic-lens or generic-optics with 'statusUpdateInterval' instead"  #-}

-- | Optional. The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.  Use standard AWS tags on your job for automatic integration with AWS services and for custom integrations and workflows.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjTags :: Lens.Lens' CreateJob (Core.Maybe (Core.HashMap Core.Text Core.Text))
cjTags = Lens.field @"tags"
{-# INLINEABLE cjTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Optional. User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.  Use only for existing integrations or workflows that rely on job metadata tags. Otherwise, we recommend that you use standard AWS tags.
--
-- /Note:/ Consider using 'userMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjUserMetadata :: Lens.Lens' CreateJob (Core.Maybe (Core.HashMap Core.Text Core.Text))
cjUserMetadata = Lens.field @"userMetadata"
{-# INLINEABLE cjUserMetadata #-}
{-# DEPRECATED userMetadata "Use generic-lens or generic-optics with 'userMetadata' instead"  #-}

instance Core.ToQuery CreateJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateJob where
        toHeaders CreateJob{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateJob where
        toJSON CreateJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("role" Core..= role'),
                  Core.Just ("settings" Core..= settings),
                  ("accelerationSettings" Core..=) Core.<$> accelerationSettings,
                  ("billingTagsSource" Core..=) Core.<$> billingTagsSource,
                  ("clientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("hopDestinations" Core..=) Core.<$> hopDestinations,
                  ("jobTemplate" Core..=) Core.<$> jobTemplate,
                  ("priority" Core..=) Core.<$> priority,
                  ("queue" Core..=) Core.<$> queue,
                  ("simulateReservedQueue" Core..=) Core.<$> simulateReservedQueue,
                  ("statusUpdateInterval" Core..=) Core.<$> statusUpdateInterval,
                  ("tags" Core..=) Core.<$> tags,
                  ("userMetadata" Core..=) Core.<$> userMetadata])

instance Core.AWSRequest CreateJob where
        type Rs CreateJob = CreateJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/2017-08-29/jobs",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateJobResponse' Core.<$>
                   (x Core..:? "job") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { job :: Core.Maybe Types.Job
    -- ^ Each job converts an input file into an output file or files. For more information, see the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateJobResponse' value with any optional fields omitted.
mkCreateJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateJobResponse
mkCreateJobResponse responseStatus
  = CreateJobResponse'{job = Core.Nothing, responseStatus}

-- | Each job converts an input file into an output file or files. For more information, see the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
--
-- /Note:/ Consider using 'job' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsJob :: Lens.Lens' CreateJobResponse (Core.Maybe Types.Job)
cjrrsJob = Lens.field @"job"
{-# INLINEABLE cjrrsJob #-}
{-# DEPRECATED job "Use generic-lens or generic-optics with 'job' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsResponseStatus :: Lens.Lens' CreateJobResponse Core.Int
cjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
