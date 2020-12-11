{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cjtAccelerationSettings,
    cjtPriority,
    cjtStatusUpdateInterval,
    cjtCategory,
    cjtHopDestinations,
    cjtQueue,
    cjtDescription,
    cjtTags,
    cjtSettings,
    cjtName,

    -- * Destructuring the response
    CreateJobTemplateResponse (..),
    mkCreateJobTemplateResponse,

    -- ** Response lenses
    cjtrsJobTemplate,
    cjtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateJobTemplate' smart constructor.
data CreateJobTemplate = CreateJobTemplate'
  { accelerationSettings ::
      Lude.Maybe AccelerationSettings,
    priority :: Lude.Maybe Lude.Int,
    statusUpdateInterval :: Lude.Maybe StatusUpdateInterval,
    category :: Lude.Maybe Lude.Text,
    hopDestinations :: Lude.Maybe [HopDestination],
    queue :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    settings :: JobTemplateSettings,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateJobTemplate' with the minimum fields required to make a request.
--
-- * 'accelerationSettings' - Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
-- * 'category' - Optional. A category for the job template you are creating
-- * 'description' - Optional. A description of the job template you are creating.
-- * 'hopDestinations' - Optional. Use queue hopping to avoid overly long waits in the backlog of the queue that you submit your job to. Specify an alternate queue and the maximum time that your job will wait in the initial queue before hopping. For more information about this feature, see the AWS Elemental MediaConvert User Guide.
-- * 'name' - The name of the job template you are creating.
-- * 'priority' - Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
-- * 'queue' - Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
-- * 'settings' - JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
-- * 'statusUpdateInterval' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
-- * 'tags' - The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
mkCreateJobTemplate ::
  -- | 'settings'
  JobTemplateSettings ->
  -- | 'name'
  Lude.Text ->
  CreateJobTemplate
mkCreateJobTemplate pSettings_ pName_ =
  CreateJobTemplate'
    { accelerationSettings = Lude.Nothing,
      priority = Lude.Nothing,
      statusUpdateInterval = Lude.Nothing,
      category = Lude.Nothing,
      hopDestinations = Lude.Nothing,
      queue = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      settings = pSettings_,
      name = pName_
    }

-- | Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
--
-- /Note:/ Consider using 'accelerationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtAccelerationSettings :: Lens.Lens' CreateJobTemplate (Lude.Maybe AccelerationSettings)
cjtAccelerationSettings = Lens.lens (accelerationSettings :: CreateJobTemplate -> Lude.Maybe AccelerationSettings) (\s a -> s {accelerationSettings = a} :: CreateJobTemplate)
{-# DEPRECATED cjtAccelerationSettings "Use generic-lens or generic-optics with 'accelerationSettings' instead." #-}

-- | Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtPriority :: Lens.Lens' CreateJobTemplate (Lude.Maybe Lude.Int)
cjtPriority = Lens.lens (priority :: CreateJobTemplate -> Lude.Maybe Lude.Int) (\s a -> s {priority = a} :: CreateJobTemplate)
{-# DEPRECATED cjtPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- /Note:/ Consider using 'statusUpdateInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtStatusUpdateInterval :: Lens.Lens' CreateJobTemplate (Lude.Maybe StatusUpdateInterval)
cjtStatusUpdateInterval = Lens.lens (statusUpdateInterval :: CreateJobTemplate -> Lude.Maybe StatusUpdateInterval) (\s a -> s {statusUpdateInterval = a} :: CreateJobTemplate)
{-# DEPRECATED cjtStatusUpdateInterval "Use generic-lens or generic-optics with 'statusUpdateInterval' instead." #-}

-- | Optional. A category for the job template you are creating
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtCategory :: Lens.Lens' CreateJobTemplate (Lude.Maybe Lude.Text)
cjtCategory = Lens.lens (category :: CreateJobTemplate -> Lude.Maybe Lude.Text) (\s a -> s {category = a} :: CreateJobTemplate)
{-# DEPRECATED cjtCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | Optional. Use queue hopping to avoid overly long waits in the backlog of the queue that you submit your job to. Specify an alternate queue and the maximum time that your job will wait in the initial queue before hopping. For more information about this feature, see the AWS Elemental MediaConvert User Guide.
--
-- /Note:/ Consider using 'hopDestinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtHopDestinations :: Lens.Lens' CreateJobTemplate (Lude.Maybe [HopDestination])
cjtHopDestinations = Lens.lens (hopDestinations :: CreateJobTemplate -> Lude.Maybe [HopDestination]) (\s a -> s {hopDestinations = a} :: CreateJobTemplate)
{-# DEPRECATED cjtHopDestinations "Use generic-lens or generic-optics with 'hopDestinations' instead." #-}

-- | Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtQueue :: Lens.Lens' CreateJobTemplate (Lude.Maybe Lude.Text)
cjtQueue = Lens.lens (queue :: CreateJobTemplate -> Lude.Maybe Lude.Text) (\s a -> s {queue = a} :: CreateJobTemplate)
{-# DEPRECATED cjtQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | Optional. A description of the job template you are creating.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtDescription :: Lens.Lens' CreateJobTemplate (Lude.Maybe Lude.Text)
cjtDescription = Lens.lens (description :: CreateJobTemplate -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateJobTemplate)
{-# DEPRECATED cjtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtTags :: Lens.Lens' CreateJobTemplate (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cjtTags = Lens.lens (tags :: CreateJobTemplate -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateJobTemplate)
{-# DEPRECATED cjtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtSettings :: Lens.Lens' CreateJobTemplate JobTemplateSettings
cjtSettings = Lens.lens (settings :: CreateJobTemplate -> JobTemplateSettings) (\s a -> s {settings = a} :: CreateJobTemplate)
{-# DEPRECATED cjtSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | The name of the job template you are creating.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtName :: Lens.Lens' CreateJobTemplate Lude.Text
cjtName = Lens.lens (name :: CreateJobTemplate -> Lude.Text) (\s a -> s {name = a} :: CreateJobTemplate)
{-# DEPRECATED cjtName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateJobTemplate where
  type Rs CreateJobTemplate = CreateJobTemplateResponse
  request = Req.postJSON mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateJobTemplateResponse'
            Lude.<$> (x Lude..?> "jobTemplate") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateJobTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateJobTemplate where
  toJSON CreateJobTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("accelerationSettings" Lude..=) Lude.<$> accelerationSettings,
            ("priority" Lude..=) Lude.<$> priority,
            ("statusUpdateInterval" Lude..=) Lude.<$> statusUpdateInterval,
            ("category" Lude..=) Lude.<$> category,
            ("hopDestinations" Lude..=) Lude.<$> hopDestinations,
            ("queue" Lude..=) Lude.<$> queue,
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("settings" Lude..= settings),
            Lude.Just ("name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateJobTemplate where
  toPath = Lude.const "/2017-08-29/jobTemplates"

instance Lude.ToQuery CreateJobTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateJobTemplateResponse' smart constructor.
data CreateJobTemplateResponse = CreateJobTemplateResponse'
  { jobTemplate ::
      Lude.Maybe JobTemplate,
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

-- | Creates a value of 'CreateJobTemplateResponse' with the minimum fields required to make a request.
--
-- * 'jobTemplate' - A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
-- * 'responseStatus' - The response status code.
mkCreateJobTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateJobTemplateResponse
mkCreateJobTemplateResponse pResponseStatus_ =
  CreateJobTemplateResponse'
    { jobTemplate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
--
-- /Note:/ Consider using 'jobTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtrsJobTemplate :: Lens.Lens' CreateJobTemplateResponse (Lude.Maybe JobTemplate)
cjtrsJobTemplate = Lens.lens (jobTemplate :: CreateJobTemplateResponse -> Lude.Maybe JobTemplate) (\s a -> s {jobTemplate = a} :: CreateJobTemplateResponse)
{-# DEPRECATED cjtrsJobTemplate "Use generic-lens or generic-optics with 'jobTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjtrsResponseStatus :: Lens.Lens' CreateJobTemplateResponse Lude.Int
cjtrsResponseStatus = Lens.lens (responseStatus :: CreateJobTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateJobTemplateResponse)
{-# DEPRECATED cjtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
