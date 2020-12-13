{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateJob (..),
    mkCreateJob,

    -- ** Request lenses
    cjJobTemplate,
    cjAccelerationSettings,
    cjPriority,
    cjStatusUpdateInterval,
    cjSettings,
    cjHopDestinations,
    cjSimulateReservedQueue,
    cjQueue,
    cjUserMetadata,
    cjRole,
    cjBillingTagsSource,
    cjClientRequestToken,
    cjTags,

    -- * Destructuring the response
    CreateJobResponse (..),
    mkCreateJobResponse,

    -- ** Response lenses
    cjrsJob,
    cjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateJob' smart constructor.
data CreateJob = CreateJob'
  { -- | Optional. When you create a job, you can either specify a job template or specify the transcoding settings individually.
    jobTemplate :: Lude.Maybe Lude.Text,
    -- | Optional. Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
    accelerationSettings :: Lude.Maybe AccelerationSettings,
    -- | Optional. Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
    priority :: Lude.Maybe Lude.Int,
    -- | Optional. Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
    statusUpdateInterval :: Lude.Maybe StatusUpdateInterval,
    -- | JobSettings contains all the transcode settings for a job.
    settings :: JobSettings,
    -- | Optional. Use queue hopping to avoid overly long waits in the backlog of the queue that you submit your job to. Specify an alternate queue and the maximum time that your job will wait in the initial queue before hopping. For more information about this feature, see the AWS Elemental MediaConvert User Guide.
    hopDestinations :: Lude.Maybe [HopDestination],
    -- | Optional. Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
    simulateReservedQueue :: Lude.Maybe SimulateReservedQueue,
    -- | Optional. When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html.
    queue :: Lude.Maybe Lude.Text,
    -- | Optional. User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.  Use only for existing integrations or workflows that rely on job metadata tags. Otherwise, we recommend that you use standard AWS tags.
    userMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Required. The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html.
    role' :: Lude.Text,
    -- | Optional. Choose a tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up. Any transcoding outputs that don't have an associated tag will appear in your billing report unsorted. If you don't choose a valid value for this field, your job outputs will appear on the billing report unsorted.
    billingTagsSource :: Lude.Maybe BillingTagsSource,
    -- | Optional. Idempotency token for CreateJob operation.
    clientRequestToken :: Lude.Maybe Lude.Text,
    -- | Optional. The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.  Use standard AWS tags on your job for automatic integration with AWS services and for custom integrations and workflows.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateJob' with the minimum fields required to make a request.
--
-- * 'jobTemplate' - Optional. When you create a job, you can either specify a job template or specify the transcoding settings individually.
-- * 'accelerationSettings' - Optional. Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
-- * 'priority' - Optional. Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
-- * 'statusUpdateInterval' - Optional. Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
-- * 'settings' - JobSettings contains all the transcode settings for a job.
-- * 'hopDestinations' - Optional. Use queue hopping to avoid overly long waits in the backlog of the queue that you submit your job to. Specify an alternate queue and the maximum time that your job will wait in the initial queue before hopping. For more information about this feature, see the AWS Elemental MediaConvert User Guide.
-- * 'simulateReservedQueue' - Optional. Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
-- * 'queue' - Optional. When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html.
-- * 'userMetadata' - Optional. User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.  Use only for existing integrations or workflows that rely on job metadata tags. Otherwise, we recommend that you use standard AWS tags.
-- * 'role'' - Required. The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html.
-- * 'billingTagsSource' - Optional. Choose a tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up. Any transcoding outputs that don't have an associated tag will appear in your billing report unsorted. If you don't choose a valid value for this field, your job outputs will appear on the billing report unsorted.
-- * 'clientRequestToken' - Optional. Idempotency token for CreateJob operation.
-- * 'tags' - Optional. The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.  Use standard AWS tags on your job for automatic integration with AWS services and for custom integrations and workflows.
mkCreateJob ::
  -- | 'settings'
  JobSettings ->
  -- | 'role''
  Lude.Text ->
  CreateJob
mkCreateJob pSettings_ pRole_ =
  CreateJob'
    { jobTemplate = Lude.Nothing,
      accelerationSettings = Lude.Nothing,
      priority = Lude.Nothing,
      statusUpdateInterval = Lude.Nothing,
      settings = pSettings_,
      hopDestinations = Lude.Nothing,
      simulateReservedQueue = Lude.Nothing,
      queue = Lude.Nothing,
      userMetadata = Lude.Nothing,
      role' = pRole_,
      billingTagsSource = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Optional. When you create a job, you can either specify a job template or specify the transcoding settings individually.
--
-- /Note:/ Consider using 'jobTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjJobTemplate :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjJobTemplate = Lens.lens (jobTemplate :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {jobTemplate = a} :: CreateJob)
{-# DEPRECATED cjJobTemplate "Use generic-lens or generic-optics with 'jobTemplate' instead." #-}

-- | Optional. Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
--
-- /Note:/ Consider using 'accelerationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjAccelerationSettings :: Lens.Lens' CreateJob (Lude.Maybe AccelerationSettings)
cjAccelerationSettings = Lens.lens (accelerationSettings :: CreateJob -> Lude.Maybe AccelerationSettings) (\s a -> s {accelerationSettings = a} :: CreateJob)
{-# DEPRECATED cjAccelerationSettings "Use generic-lens or generic-optics with 'accelerationSettings' instead." #-}

-- | Optional. Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjPriority :: Lens.Lens' CreateJob (Lude.Maybe Lude.Int)
cjPriority = Lens.lens (priority :: CreateJob -> Lude.Maybe Lude.Int) (\s a -> s {priority = a} :: CreateJob)
{-# DEPRECATED cjPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Optional. Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- /Note:/ Consider using 'statusUpdateInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjStatusUpdateInterval :: Lens.Lens' CreateJob (Lude.Maybe StatusUpdateInterval)
cjStatusUpdateInterval = Lens.lens (statusUpdateInterval :: CreateJob -> Lude.Maybe StatusUpdateInterval) (\s a -> s {statusUpdateInterval = a} :: CreateJob)
{-# DEPRECATED cjStatusUpdateInterval "Use generic-lens or generic-optics with 'statusUpdateInterval' instead." #-}

-- | JobSettings contains all the transcode settings for a job.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjSettings :: Lens.Lens' CreateJob JobSettings
cjSettings = Lens.lens (settings :: CreateJob -> JobSettings) (\s a -> s {settings = a} :: CreateJob)
{-# DEPRECATED cjSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | Optional. Use queue hopping to avoid overly long waits in the backlog of the queue that you submit your job to. Specify an alternate queue and the maximum time that your job will wait in the initial queue before hopping. For more information about this feature, see the AWS Elemental MediaConvert User Guide.
--
-- /Note:/ Consider using 'hopDestinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjHopDestinations :: Lens.Lens' CreateJob (Lude.Maybe [HopDestination])
cjHopDestinations = Lens.lens (hopDestinations :: CreateJob -> Lude.Maybe [HopDestination]) (\s a -> s {hopDestinations = a} :: CreateJob)
{-# DEPRECATED cjHopDestinations "Use generic-lens or generic-optics with 'hopDestinations' instead." #-}

-- | Optional. Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
--
-- /Note:/ Consider using 'simulateReservedQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjSimulateReservedQueue :: Lens.Lens' CreateJob (Lude.Maybe SimulateReservedQueue)
cjSimulateReservedQueue = Lens.lens (simulateReservedQueue :: CreateJob -> Lude.Maybe SimulateReservedQueue) (\s a -> s {simulateReservedQueue = a} :: CreateJob)
{-# DEPRECATED cjSimulateReservedQueue "Use generic-lens or generic-optics with 'simulateReservedQueue' instead." #-}

-- | Optional. When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjQueue :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjQueue = Lens.lens (queue :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {queue = a} :: CreateJob)
{-# DEPRECATED cjQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | Optional. User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.  Use only for existing integrations or workflows that rely on job metadata tags. Otherwise, we recommend that you use standard AWS tags.
--
-- /Note:/ Consider using 'userMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjUserMetadata :: Lens.Lens' CreateJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cjUserMetadata = Lens.lens (userMetadata :: CreateJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {userMetadata = a} :: CreateJob)
{-# DEPRECATED cjUserMetadata "Use generic-lens or generic-optics with 'userMetadata' instead." #-}

-- | Required. The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjRole :: Lens.Lens' CreateJob Lude.Text
cjRole = Lens.lens (role' :: CreateJob -> Lude.Text) (\s a -> s {role' = a} :: CreateJob)
{-# DEPRECATED cjRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | Optional. Choose a tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up. Any transcoding outputs that don't have an associated tag will appear in your billing report unsorted. If you don't choose a valid value for this field, your job outputs will appear on the billing report unsorted.
--
-- /Note:/ Consider using 'billingTagsSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjBillingTagsSource :: Lens.Lens' CreateJob (Lude.Maybe BillingTagsSource)
cjBillingTagsSource = Lens.lens (billingTagsSource :: CreateJob -> Lude.Maybe BillingTagsSource) (\s a -> s {billingTagsSource = a} :: CreateJob)
{-# DEPRECATED cjBillingTagsSource "Use generic-lens or generic-optics with 'billingTagsSource' instead." #-}

-- | Optional. Idempotency token for CreateJob operation.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjClientRequestToken :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjClientRequestToken = Lens.lens (clientRequestToken :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateJob)
{-# DEPRECATED cjClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Optional. The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.  Use standard AWS tags on your job for automatic integration with AWS services and for custom integrations and workflows.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjTags :: Lens.Lens' CreateJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cjTags = Lens.lens (tags :: CreateJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateJob)
{-# DEPRECATED cjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateJob where
  type Rs CreateJob = CreateJobResponse
  request = Req.postJSON mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Lude.<$> (x Lude..?> "job") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("jobTemplate" Lude..=) Lude.<$> jobTemplate,
            ("accelerationSettings" Lude..=) Lude.<$> accelerationSettings,
            ("priority" Lude..=) Lude.<$> priority,
            ("statusUpdateInterval" Lude..=) Lude.<$> statusUpdateInterval,
            Lude.Just ("settings" Lude..= settings),
            ("hopDestinations" Lude..=) Lude.<$> hopDestinations,
            ("simulateReservedQueue" Lude..=) Lude.<$> simulateReservedQueue,
            ("queue" Lude..=) Lude.<$> queue,
            ("userMetadata" Lude..=) Lude.<$> userMetadata,
            Lude.Just ("role" Lude..= role'),
            ("billingTagsSource" Lude..=) Lude.<$> billingTagsSource,
            ("clientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateJob where
  toPath = Lude.const "/2017-08-29/jobs"

instance Lude.ToQuery CreateJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { -- | Each job converts an input file into an output file or files. For more information, see the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
    job :: Lude.Maybe Job,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateJobResponse' with the minimum fields required to make a request.
--
-- * 'job' - Each job converts an input file into an output file or files. For more information, see the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
-- * 'responseStatus' - The response status code.
mkCreateJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateJobResponse
mkCreateJobResponse pResponseStatus_ =
  CreateJobResponse'
    { job = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Each job converts an input file into an output file or files. For more information, see the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
--
-- /Note:/ Consider using 'job' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsJob :: Lens.Lens' CreateJobResponse (Lude.Maybe Job)
cjrsJob = Lens.lens (job :: CreateJobResponse -> Lude.Maybe Job) (\s a -> s {job = a} :: CreateJobResponse)
{-# DEPRECATED cjrsJob "Use generic-lens or generic-optics with 'job' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsResponseStatus :: Lens.Lens' CreateJobResponse Lude.Int
cjrsResponseStatus = Lens.lens (responseStatus :: CreateJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateJobResponse)
{-# DEPRECATED cjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
