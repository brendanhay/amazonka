{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.UpdateJobTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify one of your existing job templates.
module Network.AWS.MediaConvert.UpdateJobTemplate
  ( -- * Creating a request
    UpdateJobTemplate (..),
    mkUpdateJobTemplate,

    -- ** Request lenses
    ujtAccelerationSettings,
    ujtPriority,
    ujtStatusUpdateInterval,
    ujtSettings,
    ujtCategory,
    ujtHopDestinations,
    ujtQueue,
    ujtName,
    ujtDescription,

    -- * Destructuring the response
    UpdateJobTemplateResponse (..),
    mkUpdateJobTemplateResponse,

    -- ** Response lenses
    ujtrsJobTemplate,
    ujtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateJobTemplate' smart constructor.
data UpdateJobTemplate = UpdateJobTemplate'
  { -- | Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
    accelerationSettings :: Lude.Maybe AccelerationSettings,
    -- | Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
    priority :: Lude.Maybe Lude.Int,
    -- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
    statusUpdateInterval :: Lude.Maybe StatusUpdateInterval,
    -- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
    settings :: Lude.Maybe JobTemplateSettings,
    -- | The new category for the job template, if you are changing it.
    category :: Lude.Maybe Lude.Text,
    -- | Optional list of hop destinations.
    hopDestinations :: Lude.Maybe [HopDestination],
    -- | The new queue for the job template, if you are changing it.
    queue :: Lude.Maybe Lude.Text,
    -- | The name of the job template you are modifying
    name :: Lude.Text,
    -- | The new description for the job template, if you are changing it.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJobTemplate' with the minimum fields required to make a request.
--
-- * 'accelerationSettings' - Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
-- * 'priority' - Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
-- * 'statusUpdateInterval' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
-- * 'settings' - JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
-- * 'category' - The new category for the job template, if you are changing it.
-- * 'hopDestinations' - Optional list of hop destinations.
-- * 'queue' - The new queue for the job template, if you are changing it.
-- * 'name' - The name of the job template you are modifying
-- * 'description' - The new description for the job template, if you are changing it.
mkUpdateJobTemplate ::
  -- | 'name'
  Lude.Text ->
  UpdateJobTemplate
mkUpdateJobTemplate pName_ =
  UpdateJobTemplate'
    { accelerationSettings = Lude.Nothing,
      priority = Lude.Nothing,
      statusUpdateInterval = Lude.Nothing,
      settings = Lude.Nothing,
      category = Lude.Nothing,
      hopDestinations = Lude.Nothing,
      queue = Lude.Nothing,
      name = pName_,
      description = Lude.Nothing
    }

-- | Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
--
-- /Note:/ Consider using 'accelerationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujtAccelerationSettings :: Lens.Lens' UpdateJobTemplate (Lude.Maybe AccelerationSettings)
ujtAccelerationSettings = Lens.lens (accelerationSettings :: UpdateJobTemplate -> Lude.Maybe AccelerationSettings) (\s a -> s {accelerationSettings = a} :: UpdateJobTemplate)
{-# DEPRECATED ujtAccelerationSettings "Use generic-lens or generic-optics with 'accelerationSettings' instead." #-}

-- | Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujtPriority :: Lens.Lens' UpdateJobTemplate (Lude.Maybe Lude.Int)
ujtPriority = Lens.lens (priority :: UpdateJobTemplate -> Lude.Maybe Lude.Int) (\s a -> s {priority = a} :: UpdateJobTemplate)
{-# DEPRECATED ujtPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- /Note:/ Consider using 'statusUpdateInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujtStatusUpdateInterval :: Lens.Lens' UpdateJobTemplate (Lude.Maybe StatusUpdateInterval)
ujtStatusUpdateInterval = Lens.lens (statusUpdateInterval :: UpdateJobTemplate -> Lude.Maybe StatusUpdateInterval) (\s a -> s {statusUpdateInterval = a} :: UpdateJobTemplate)
{-# DEPRECATED ujtStatusUpdateInterval "Use generic-lens or generic-optics with 'statusUpdateInterval' instead." #-}

-- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujtSettings :: Lens.Lens' UpdateJobTemplate (Lude.Maybe JobTemplateSettings)
ujtSettings = Lens.lens (settings :: UpdateJobTemplate -> Lude.Maybe JobTemplateSettings) (\s a -> s {settings = a} :: UpdateJobTemplate)
{-# DEPRECATED ujtSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | The new category for the job template, if you are changing it.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujtCategory :: Lens.Lens' UpdateJobTemplate (Lude.Maybe Lude.Text)
ujtCategory = Lens.lens (category :: UpdateJobTemplate -> Lude.Maybe Lude.Text) (\s a -> s {category = a} :: UpdateJobTemplate)
{-# DEPRECATED ujtCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | Optional list of hop destinations.
--
-- /Note:/ Consider using 'hopDestinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujtHopDestinations :: Lens.Lens' UpdateJobTemplate (Lude.Maybe [HopDestination])
ujtHopDestinations = Lens.lens (hopDestinations :: UpdateJobTemplate -> Lude.Maybe [HopDestination]) (\s a -> s {hopDestinations = a} :: UpdateJobTemplate)
{-# DEPRECATED ujtHopDestinations "Use generic-lens or generic-optics with 'hopDestinations' instead." #-}

-- | The new queue for the job template, if you are changing it.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujtQueue :: Lens.Lens' UpdateJobTemplate (Lude.Maybe Lude.Text)
ujtQueue = Lens.lens (queue :: UpdateJobTemplate -> Lude.Maybe Lude.Text) (\s a -> s {queue = a} :: UpdateJobTemplate)
{-# DEPRECATED ujtQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | The name of the job template you are modifying
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujtName :: Lens.Lens' UpdateJobTemplate Lude.Text
ujtName = Lens.lens (name :: UpdateJobTemplate -> Lude.Text) (\s a -> s {name = a} :: UpdateJobTemplate)
{-# DEPRECATED ujtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The new description for the job template, if you are changing it.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujtDescription :: Lens.Lens' UpdateJobTemplate (Lude.Maybe Lude.Text)
ujtDescription = Lens.lens (description :: UpdateJobTemplate -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateJobTemplate)
{-# DEPRECATED ujtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateJobTemplate where
  type Rs UpdateJobTemplate = UpdateJobTemplateResponse
  request = Req.putJSON mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateJobTemplateResponse'
            Lude.<$> (x Lude..?> "jobTemplate") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateJobTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateJobTemplate where
  toJSON UpdateJobTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("accelerationSettings" Lude..=) Lude.<$> accelerationSettings,
            ("priority" Lude..=) Lude.<$> priority,
            ("statusUpdateInterval" Lude..=) Lude.<$> statusUpdateInterval,
            ("settings" Lude..=) Lude.<$> settings,
            ("category" Lude..=) Lude.<$> category,
            ("hopDestinations" Lude..=) Lude.<$> hopDestinations,
            ("queue" Lude..=) Lude.<$> queue,
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateJobTemplate where
  toPath UpdateJobTemplate' {..} =
    Lude.mconcat ["/2017-08-29/jobTemplates/", Lude.toBS name]

instance Lude.ToQuery UpdateJobTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateJobTemplateResponse' smart constructor.
data UpdateJobTemplateResponse = UpdateJobTemplateResponse'
  { -- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
    jobTemplate :: Lude.Maybe JobTemplate,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJobTemplateResponse' with the minimum fields required to make a request.
--
-- * 'jobTemplate' - A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
-- * 'responseStatus' - The response status code.
mkUpdateJobTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateJobTemplateResponse
mkUpdateJobTemplateResponse pResponseStatus_ =
  UpdateJobTemplateResponse'
    { jobTemplate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
--
-- /Note:/ Consider using 'jobTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujtrsJobTemplate :: Lens.Lens' UpdateJobTemplateResponse (Lude.Maybe JobTemplate)
ujtrsJobTemplate = Lens.lens (jobTemplate :: UpdateJobTemplateResponse -> Lude.Maybe JobTemplate) (\s a -> s {jobTemplate = a} :: UpdateJobTemplateResponse)
{-# DEPRECATED ujtrsJobTemplate "Use generic-lens or generic-optics with 'jobTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujtrsResponseStatus :: Lens.Lens' UpdateJobTemplateResponse Lude.Int
ujtrsResponseStatus = Lens.lens (responseStatus :: UpdateJobTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateJobTemplateResponse)
{-# DEPRECATED ujtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
