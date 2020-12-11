-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobTemplate
  ( JobTemplate (..),

    -- * Smart constructor
    mkJobTemplate,

    -- * Lenses
    jtAccelerationSettings,
    jtLastUpdated,
    jtPriority,
    jtStatusUpdateInterval,
    jtARN,
    jtCreatedAt,
    jtCategory,
    jtHopDestinations,
    jtQueue,
    jtType,
    jtDescription,
    jtSettings,
    jtName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AccelerationSettings
import Network.AWS.MediaConvert.Types.HopDestination
import Network.AWS.MediaConvert.Types.JobTemplateSettings
import Network.AWS.MediaConvert.Types.StatusUpdateInterval
import Network.AWS.MediaConvert.Types.Type
import qualified Network.AWS.Prelude as Lude

-- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
--
-- /See:/ 'mkJobTemplate' smart constructor.
data JobTemplate = JobTemplate'
  { accelerationSettings ::
      Lude.Maybe AccelerationSettings,
    lastUpdated :: Lude.Maybe Lude.Timestamp,
    priority :: Lude.Maybe Lude.Int,
    statusUpdateInterval :: Lude.Maybe StatusUpdateInterval,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    category :: Lude.Maybe Lude.Text,
    hopDestinations :: Lude.Maybe [HopDestination],
    queue :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Type,
    description :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'JobTemplate' with the minimum fields required to make a request.
--
-- * 'accelerationSettings' - Accelerated transcoding can significantly speed up jobs with long, visually complex content.
-- * 'arn' - An identifier for this resource that is unique within all of AWS.
-- * 'category' - An optional category you create to organize your job templates.
-- * 'createdAt' - The timestamp in epoch seconds for Job template creation.
-- * 'description' - An optional description you create for each job template.
-- * 'hopDestinations' - Optional list of hop destinations.
-- * 'lastUpdated' - The timestamp in epoch seconds when the Job template was last updated.
-- * 'name' - A name you create for each job template. Each name must be unique within your account.
-- * 'priority' - Relative priority on the job.
-- * 'queue' - Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
-- * 'settings' - JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
-- * 'statusUpdateInterval' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
-- * 'type'' - A job template can be of two types: system or custom. System or built-in job templates can't be modified or deleted by the user.
mkJobTemplate ::
  -- | 'settings'
  JobTemplateSettings ->
  -- | 'name'
  Lude.Text ->
  JobTemplate
mkJobTemplate pSettings_ pName_ =
  JobTemplate'
    { accelerationSettings = Lude.Nothing,
      lastUpdated = Lude.Nothing,
      priority = Lude.Nothing,
      statusUpdateInterval = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      category = Lude.Nothing,
      hopDestinations = Lude.Nothing,
      queue = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing,
      settings = pSettings_,
      name = pName_
    }

-- | Accelerated transcoding can significantly speed up jobs with long, visually complex content.
--
-- /Note:/ Consider using 'accelerationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtAccelerationSettings :: Lens.Lens' JobTemplate (Lude.Maybe AccelerationSettings)
jtAccelerationSettings = Lens.lens (accelerationSettings :: JobTemplate -> Lude.Maybe AccelerationSettings) (\s a -> s {accelerationSettings = a} :: JobTemplate)
{-# DEPRECATED jtAccelerationSettings "Use generic-lens or generic-optics with 'accelerationSettings' instead." #-}

-- | The timestamp in epoch seconds when the Job template was last updated.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtLastUpdated :: Lens.Lens' JobTemplate (Lude.Maybe Lude.Timestamp)
jtLastUpdated = Lens.lens (lastUpdated :: JobTemplate -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdated = a} :: JobTemplate)
{-# DEPRECATED jtLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | Relative priority on the job.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtPriority :: Lens.Lens' JobTemplate (Lude.Maybe Lude.Int)
jtPriority = Lens.lens (priority :: JobTemplate -> Lude.Maybe Lude.Int) (\s a -> s {priority = a} :: JobTemplate)
{-# DEPRECATED jtPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- /Note:/ Consider using 'statusUpdateInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtStatusUpdateInterval :: Lens.Lens' JobTemplate (Lude.Maybe StatusUpdateInterval)
jtStatusUpdateInterval = Lens.lens (statusUpdateInterval :: JobTemplate -> Lude.Maybe StatusUpdateInterval) (\s a -> s {statusUpdateInterval = a} :: JobTemplate)
{-# DEPRECATED jtStatusUpdateInterval "Use generic-lens or generic-optics with 'statusUpdateInterval' instead." #-}

-- | An identifier for this resource that is unique within all of AWS.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtARN :: Lens.Lens' JobTemplate (Lude.Maybe Lude.Text)
jtARN = Lens.lens (arn :: JobTemplate -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: JobTemplate)
{-# DEPRECATED jtARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp in epoch seconds for Job template creation.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtCreatedAt :: Lens.Lens' JobTemplate (Lude.Maybe Lude.Timestamp)
jtCreatedAt = Lens.lens (createdAt :: JobTemplate -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: JobTemplate)
{-# DEPRECATED jtCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | An optional category you create to organize your job templates.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtCategory :: Lens.Lens' JobTemplate (Lude.Maybe Lude.Text)
jtCategory = Lens.lens (category :: JobTemplate -> Lude.Maybe Lude.Text) (\s a -> s {category = a} :: JobTemplate)
{-# DEPRECATED jtCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | Optional list of hop destinations.
--
-- /Note:/ Consider using 'hopDestinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtHopDestinations :: Lens.Lens' JobTemplate (Lude.Maybe [HopDestination])
jtHopDestinations = Lens.lens (hopDestinations :: JobTemplate -> Lude.Maybe [HopDestination]) (\s a -> s {hopDestinations = a} :: JobTemplate)
{-# DEPRECATED jtHopDestinations "Use generic-lens or generic-optics with 'hopDestinations' instead." #-}

-- | Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtQueue :: Lens.Lens' JobTemplate (Lude.Maybe Lude.Text)
jtQueue = Lens.lens (queue :: JobTemplate -> Lude.Maybe Lude.Text) (\s a -> s {queue = a} :: JobTemplate)
{-# DEPRECATED jtQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | A job template can be of two types: system or custom. System or built-in job templates can't be modified or deleted by the user.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtType :: Lens.Lens' JobTemplate (Lude.Maybe Type)
jtType = Lens.lens (type' :: JobTemplate -> Lude.Maybe Type) (\s a -> s {type' = a} :: JobTemplate)
{-# DEPRECATED jtType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | An optional description you create for each job template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtDescription :: Lens.Lens' JobTemplate (Lude.Maybe Lude.Text)
jtDescription = Lens.lens (description :: JobTemplate -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: JobTemplate)
{-# DEPRECATED jtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtSettings :: Lens.Lens' JobTemplate JobTemplateSettings
jtSettings = Lens.lens (settings :: JobTemplate -> JobTemplateSettings) (\s a -> s {settings = a} :: JobTemplate)
{-# DEPRECATED jtSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | A name you create for each job template. Each name must be unique within your account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtName :: Lens.Lens' JobTemplate Lude.Text
jtName = Lens.lens (name :: JobTemplate -> Lude.Text) (\s a -> s {name = a} :: JobTemplate)
{-# DEPRECATED jtName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON JobTemplate where
  parseJSON =
    Lude.withObject
      "JobTemplate"
      ( \x ->
          JobTemplate'
            Lude.<$> (x Lude..:? "accelerationSettings")
            Lude.<*> (x Lude..:? "lastUpdated")
            Lude.<*> (x Lude..:? "priority")
            Lude.<*> (x Lude..:? "statusUpdateInterval")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "category")
            Lude.<*> (x Lude..:? "hopDestinations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "queue")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..: "settings")
            Lude.<*> (x Lude..: "name")
      )
