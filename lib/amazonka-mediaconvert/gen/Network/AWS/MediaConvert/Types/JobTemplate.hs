{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    jtSettings,
    jtName,
    jtAccelerationSettings,
    jtArn,
    jtCategory,
    jtCreatedAt,
    jtDescription,
    jtHopDestinations,
    jtLastUpdated,
    jtPriority,
    jtQueue,
    jtStatusUpdateInterval,
    jtType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AccelerationSettings as Types
import qualified Network.AWS.MediaConvert.Types.HopDestination as Types
import qualified Network.AWS.MediaConvert.Types.JobTemplateSettings as Types
import qualified Network.AWS.MediaConvert.Types.StatusUpdateInterval as Types
import qualified Network.AWS.MediaConvert.Types.Type as Types
import qualified Network.AWS.Prelude as Core

-- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
--
-- /See:/ 'mkJobTemplate' smart constructor.
data JobTemplate = JobTemplate'
  { -- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
    settings :: Types.JobTemplateSettings,
    -- | A name you create for each job template. Each name must be unique within your account.
    name :: Core.Text,
    -- | Accelerated transcoding can significantly speed up jobs with long, visually complex content.
    accelerationSettings :: Core.Maybe Types.AccelerationSettings,
    -- | An identifier for this resource that is unique within all of AWS.
    arn :: Core.Maybe Core.Text,
    -- | An optional category you create to organize your job templates.
    category :: Core.Maybe Core.Text,
    -- | The timestamp in epoch seconds for Job template creation.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | An optional description you create for each job template.
    description :: Core.Maybe Core.Text,
    -- | Optional list of hop destinations.
    hopDestinations :: Core.Maybe [Types.HopDestination],
    -- | The timestamp in epoch seconds when the Job template was last updated.
    lastUpdated :: Core.Maybe Core.NominalDiffTime,
    -- | Relative priority on the job.
    priority :: Core.Maybe Core.Int,
    -- | Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
    queue :: Core.Maybe Core.Text,
    -- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
    statusUpdateInterval :: Core.Maybe Types.StatusUpdateInterval,
    -- | A job template can be of two types: system or custom. System or built-in job templates can't be modified or deleted by the user.
    type' :: Core.Maybe Types.Type
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'JobTemplate' value with any optional fields omitted.
mkJobTemplate ::
  -- | 'settings'
  Types.JobTemplateSettings ->
  -- | 'name'
  Core.Text ->
  JobTemplate
mkJobTemplate settings name =
  JobTemplate'
    { settings,
      name,
      accelerationSettings = Core.Nothing,
      arn = Core.Nothing,
      category = Core.Nothing,
      createdAt = Core.Nothing,
      description = Core.Nothing,
      hopDestinations = Core.Nothing,
      lastUpdated = Core.Nothing,
      priority = Core.Nothing,
      queue = Core.Nothing,
      statusUpdateInterval = Core.Nothing,
      type' = Core.Nothing
    }

-- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtSettings :: Lens.Lens' JobTemplate Types.JobTemplateSettings
jtSettings = Lens.field @"settings"
{-# DEPRECATED jtSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | A name you create for each job template. Each name must be unique within your account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtName :: Lens.Lens' JobTemplate Core.Text
jtName = Lens.field @"name"
{-# DEPRECATED jtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Accelerated transcoding can significantly speed up jobs with long, visually complex content.
--
-- /Note:/ Consider using 'accelerationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtAccelerationSettings :: Lens.Lens' JobTemplate (Core.Maybe Types.AccelerationSettings)
jtAccelerationSettings = Lens.field @"accelerationSettings"
{-# DEPRECATED jtAccelerationSettings "Use generic-lens or generic-optics with 'accelerationSettings' instead." #-}

-- | An identifier for this resource that is unique within all of AWS.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtArn :: Lens.Lens' JobTemplate (Core.Maybe Core.Text)
jtArn = Lens.field @"arn"
{-# DEPRECATED jtArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | An optional category you create to organize your job templates.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtCategory :: Lens.Lens' JobTemplate (Core.Maybe Core.Text)
jtCategory = Lens.field @"category"
{-# DEPRECATED jtCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The timestamp in epoch seconds for Job template creation.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtCreatedAt :: Lens.Lens' JobTemplate (Core.Maybe Core.NominalDiffTime)
jtCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED jtCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | An optional description you create for each job template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtDescription :: Lens.Lens' JobTemplate (Core.Maybe Core.Text)
jtDescription = Lens.field @"description"
{-# DEPRECATED jtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Optional list of hop destinations.
--
-- /Note:/ Consider using 'hopDestinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtHopDestinations :: Lens.Lens' JobTemplate (Core.Maybe [Types.HopDestination])
jtHopDestinations = Lens.field @"hopDestinations"
{-# DEPRECATED jtHopDestinations "Use generic-lens or generic-optics with 'hopDestinations' instead." #-}

-- | The timestamp in epoch seconds when the Job template was last updated.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtLastUpdated :: Lens.Lens' JobTemplate (Core.Maybe Core.NominalDiffTime)
jtLastUpdated = Lens.field @"lastUpdated"
{-# DEPRECATED jtLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | Relative priority on the job.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtPriority :: Lens.Lens' JobTemplate (Core.Maybe Core.Int)
jtPriority = Lens.field @"priority"
{-# DEPRECATED jtPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtQueue :: Lens.Lens' JobTemplate (Core.Maybe Core.Text)
jtQueue = Lens.field @"queue"
{-# DEPRECATED jtQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- /Note:/ Consider using 'statusUpdateInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtStatusUpdateInterval :: Lens.Lens' JobTemplate (Core.Maybe Types.StatusUpdateInterval)
jtStatusUpdateInterval = Lens.field @"statusUpdateInterval"
{-# DEPRECATED jtStatusUpdateInterval "Use generic-lens or generic-optics with 'statusUpdateInterval' instead." #-}

-- | A job template can be of two types: system or custom. System or built-in job templates can't be modified or deleted by the user.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtType :: Lens.Lens' JobTemplate (Core.Maybe Types.Type)
jtType = Lens.field @"type'"
{-# DEPRECATED jtType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON JobTemplate where
  parseJSON =
    Core.withObject "JobTemplate" Core.$
      \x ->
        JobTemplate'
          Core.<$> (x Core..: "settings")
          Core.<*> (x Core..: "name")
          Core.<*> (x Core..:? "accelerationSettings")
          Core.<*> (x Core..:? "arn")
          Core.<*> (x Core..:? "category")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "hopDestinations")
          Core.<*> (x Core..:? "lastUpdated")
          Core.<*> (x Core..:? "priority")
          Core.<*> (x Core..:? "queue")
          Core.<*> (x Core..:? "statusUpdateInterval")
          Core.<*> (x Core..:? "type")
