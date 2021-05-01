{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobTemplate where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AccelerationSettings
import Network.AWS.MediaConvert.Types.HopDestination
import Network.AWS.MediaConvert.Types.JobTemplateSettings
import Network.AWS.MediaConvert.Types.StatusUpdateInterval
import Network.AWS.MediaConvert.Types.Type
import qualified Network.AWS.Prelude as Prelude

-- | A job template is a pre-made set of encoding instructions that you can
-- use to quickly create a job.
--
-- /See:/ 'newJobTemplate' smart constructor.
data JobTemplate = JobTemplate'
  { -- | Accelerated transcoding can significantly speed up jobs with long,
    -- visually complex content.
    accelerationSettings :: Prelude.Maybe AccelerationSettings,
    -- | An optional category you create to organize your job templates.
    category :: Prelude.Maybe Prelude.Text,
    -- | An identifier for this resource that is unique within all of AWS.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp in epoch seconds for Job template creation.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | Relative priority on the job.
    priority :: Prelude.Maybe Prelude.Int,
    -- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
    -- CloudWatch Events. Set the interval, in seconds, between status updates.
    -- MediaConvert sends an update at this interval from the time the service
    -- begins processing your job to the time it completes the transcode or
    -- encounters an error.
    statusUpdateInterval :: Prelude.Maybe StatusUpdateInterval,
    -- | The timestamp in epoch seconds when the Job template was last updated.
    lastUpdated :: Prelude.Maybe Prelude.POSIX,
    -- | Optional. The queue that jobs created from this template are assigned
    -- to. If you don\'t specify this, jobs will go to the default queue.
    queue :: Prelude.Maybe Prelude.Text,
    -- | An optional description you create for each job template.
    description :: Prelude.Maybe Prelude.Text,
    -- | A job template can be of two types: system or custom. System or built-in
    -- job templates can\'t be modified or deleted by the user.
    type' :: Prelude.Maybe Type,
    -- | Optional list of hop destinations.
    hopDestinations :: Prelude.Maybe [HopDestination],
    -- | JobTemplateSettings contains all the transcode settings saved in the
    -- template that will be applied to jobs created from it.
    settings :: JobTemplateSettings,
    -- | A name you create for each job template. Each name must be unique within
    -- your account.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'JobTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accelerationSettings', 'jobTemplate_accelerationSettings' - Accelerated transcoding can significantly speed up jobs with long,
-- visually complex content.
--
-- 'category', 'jobTemplate_category' - An optional category you create to organize your job templates.
--
-- 'arn', 'jobTemplate_arn' - An identifier for this resource that is unique within all of AWS.
--
-- 'createdAt', 'jobTemplate_createdAt' - The timestamp in epoch seconds for Job template creation.
--
-- 'priority', 'jobTemplate_priority' - Relative priority on the job.
--
-- 'statusUpdateInterval', 'jobTemplate_statusUpdateInterval' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
-- CloudWatch Events. Set the interval, in seconds, between status updates.
-- MediaConvert sends an update at this interval from the time the service
-- begins processing your job to the time it completes the transcode or
-- encounters an error.
--
-- 'lastUpdated', 'jobTemplate_lastUpdated' - The timestamp in epoch seconds when the Job template was last updated.
--
-- 'queue', 'jobTemplate_queue' - Optional. The queue that jobs created from this template are assigned
-- to. If you don\'t specify this, jobs will go to the default queue.
--
-- 'description', 'jobTemplate_description' - An optional description you create for each job template.
--
-- 'type'', 'jobTemplate_type' - A job template can be of two types: system or custom. System or built-in
-- job templates can\'t be modified or deleted by the user.
--
-- 'hopDestinations', 'jobTemplate_hopDestinations' - Optional list of hop destinations.
--
-- 'settings', 'jobTemplate_settings' - JobTemplateSettings contains all the transcode settings saved in the
-- template that will be applied to jobs created from it.
--
-- 'name', 'jobTemplate_name' - A name you create for each job template. Each name must be unique within
-- your account.
newJobTemplate ::
  -- | 'settings'
  JobTemplateSettings ->
  -- | 'name'
  Prelude.Text ->
  JobTemplate
newJobTemplate pSettings_ pName_ =
  JobTemplate'
    { accelerationSettings =
        Prelude.Nothing,
      category = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      priority = Prelude.Nothing,
      statusUpdateInterval = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      queue = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = Prelude.Nothing,
      hopDestinations = Prelude.Nothing,
      settings = pSettings_,
      name = pName_
    }

-- | Accelerated transcoding can significantly speed up jobs with long,
-- visually complex content.
jobTemplate_accelerationSettings :: Lens.Lens' JobTemplate (Prelude.Maybe AccelerationSettings)
jobTemplate_accelerationSettings = Lens.lens (\JobTemplate' {accelerationSettings} -> accelerationSettings) (\s@JobTemplate' {} a -> s {accelerationSettings = a} :: JobTemplate)

-- | An optional category you create to organize your job templates.
jobTemplate_category :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Text)
jobTemplate_category = Lens.lens (\JobTemplate' {category} -> category) (\s@JobTemplate' {} a -> s {category = a} :: JobTemplate)

-- | An identifier for this resource that is unique within all of AWS.
jobTemplate_arn :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Text)
jobTemplate_arn = Lens.lens (\JobTemplate' {arn} -> arn) (\s@JobTemplate' {} a -> s {arn = a} :: JobTemplate)

-- | The timestamp in epoch seconds for Job template creation.
jobTemplate_createdAt :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.UTCTime)
jobTemplate_createdAt = Lens.lens (\JobTemplate' {createdAt} -> createdAt) (\s@JobTemplate' {} a -> s {createdAt = a} :: JobTemplate) Prelude.. Lens.mapping Prelude._Time

-- | Relative priority on the job.
jobTemplate_priority :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Int)
jobTemplate_priority = Lens.lens (\JobTemplate' {priority} -> priority) (\s@JobTemplate' {} a -> s {priority = a} :: JobTemplate)

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
-- CloudWatch Events. Set the interval, in seconds, between status updates.
-- MediaConvert sends an update at this interval from the time the service
-- begins processing your job to the time it completes the transcode or
-- encounters an error.
jobTemplate_statusUpdateInterval :: Lens.Lens' JobTemplate (Prelude.Maybe StatusUpdateInterval)
jobTemplate_statusUpdateInterval = Lens.lens (\JobTemplate' {statusUpdateInterval} -> statusUpdateInterval) (\s@JobTemplate' {} a -> s {statusUpdateInterval = a} :: JobTemplate)

-- | The timestamp in epoch seconds when the Job template was last updated.
jobTemplate_lastUpdated :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.UTCTime)
jobTemplate_lastUpdated = Lens.lens (\JobTemplate' {lastUpdated} -> lastUpdated) (\s@JobTemplate' {} a -> s {lastUpdated = a} :: JobTemplate) Prelude.. Lens.mapping Prelude._Time

-- | Optional. The queue that jobs created from this template are assigned
-- to. If you don\'t specify this, jobs will go to the default queue.
jobTemplate_queue :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Text)
jobTemplate_queue = Lens.lens (\JobTemplate' {queue} -> queue) (\s@JobTemplate' {} a -> s {queue = a} :: JobTemplate)

-- | An optional description you create for each job template.
jobTemplate_description :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Text)
jobTemplate_description = Lens.lens (\JobTemplate' {description} -> description) (\s@JobTemplate' {} a -> s {description = a} :: JobTemplate)

-- | A job template can be of two types: system or custom. System or built-in
-- job templates can\'t be modified or deleted by the user.
jobTemplate_type :: Lens.Lens' JobTemplate (Prelude.Maybe Type)
jobTemplate_type = Lens.lens (\JobTemplate' {type'} -> type') (\s@JobTemplate' {} a -> s {type' = a} :: JobTemplate)

-- | Optional list of hop destinations.
jobTemplate_hopDestinations :: Lens.Lens' JobTemplate (Prelude.Maybe [HopDestination])
jobTemplate_hopDestinations = Lens.lens (\JobTemplate' {hopDestinations} -> hopDestinations) (\s@JobTemplate' {} a -> s {hopDestinations = a} :: JobTemplate) Prelude.. Lens.mapping Prelude._Coerce

-- | JobTemplateSettings contains all the transcode settings saved in the
-- template that will be applied to jobs created from it.
jobTemplate_settings :: Lens.Lens' JobTemplate JobTemplateSettings
jobTemplate_settings = Lens.lens (\JobTemplate' {settings} -> settings) (\s@JobTemplate' {} a -> s {settings = a} :: JobTemplate)

-- | A name you create for each job template. Each name must be unique within
-- your account.
jobTemplate_name :: Lens.Lens' JobTemplate Prelude.Text
jobTemplate_name = Lens.lens (\JobTemplate' {name} -> name) (\s@JobTemplate' {} a -> s {name = a} :: JobTemplate)

instance Prelude.FromJSON JobTemplate where
  parseJSON =
    Prelude.withObject
      "JobTemplate"
      ( \x ->
          JobTemplate'
            Prelude.<$> (x Prelude..:? "accelerationSettings")
            Prelude.<*> (x Prelude..:? "category")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "priority")
            Prelude.<*> (x Prelude..:? "statusUpdateInterval")
            Prelude.<*> (x Prelude..:? "lastUpdated")
            Prelude.<*> (x Prelude..:? "queue")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "type")
            Prelude.<*> ( x Prelude..:? "hopDestinations"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "settings")
            Prelude.<*> (x Prelude..: "name")
      )

instance Prelude.Hashable JobTemplate

instance Prelude.NFData JobTemplate
