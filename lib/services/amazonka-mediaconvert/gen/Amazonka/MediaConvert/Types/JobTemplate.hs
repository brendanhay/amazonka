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
-- Module      : Amazonka.MediaConvert.Types.JobTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.JobTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AccelerationSettings
import Amazonka.MediaConvert.Types.HopDestination
import Amazonka.MediaConvert.Types.JobTemplateSettings
import Amazonka.MediaConvert.Types.StatusUpdateInterval
import Amazonka.MediaConvert.Types.Type
import qualified Amazonka.Prelude as Prelude

-- | A job template is a pre-made set of encoding instructions that you can
-- use to quickly create a job.
--
-- /See:/ 'newJobTemplate' smart constructor.
data JobTemplate = JobTemplate'
  { -- | Accelerated transcoding can significantly speed up jobs with long,
    -- visually complex content.
    accelerationSettings :: Prelude.Maybe AccelerationSettings,
    -- | An identifier for this resource that is unique within all of AWS.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An optional category you create to organize your job templates.
    category :: Prelude.Maybe Prelude.Text,
    -- | The timestamp in epoch seconds for Job template creation.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | An optional description you create for each job template.
    description :: Prelude.Maybe Prelude.Text,
    -- | Optional list of hop destinations.
    hopDestinations :: Prelude.Maybe [HopDestination],
    -- | The timestamp in epoch seconds when the Job template was last updated.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | Relative priority on the job.
    priority :: Prelude.Maybe Prelude.Int,
    -- | Optional. The queue that jobs created from this template are assigned
    -- to. If you don\'t specify this, jobs will go to the default queue.
    queue :: Prelude.Maybe Prelude.Text,
    -- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
    -- CloudWatch Events. Set the interval, in seconds, between status updates.
    -- MediaConvert sends an update at this interval from the time the service
    -- begins processing your job to the time it completes the transcode or
    -- encounters an error.
    statusUpdateInterval :: Prelude.Maybe StatusUpdateInterval,
    -- | A job template can be of two types: system or custom. System or built-in
    -- job templates can\'t be modified or deleted by the user.
    type' :: Prelude.Maybe Type,
    -- | JobTemplateSettings contains all the transcode settings saved in the
    -- template that will be applied to jobs created from it.
    settings :: JobTemplateSettings,
    -- | A name you create for each job template. Each name must be unique within
    -- your account.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'arn', 'jobTemplate_arn' - An identifier for this resource that is unique within all of AWS.
--
-- 'category', 'jobTemplate_category' - An optional category you create to organize your job templates.
--
-- 'createdAt', 'jobTemplate_createdAt' - The timestamp in epoch seconds for Job template creation.
--
-- 'description', 'jobTemplate_description' - An optional description you create for each job template.
--
-- 'hopDestinations', 'jobTemplate_hopDestinations' - Optional list of hop destinations.
--
-- 'lastUpdated', 'jobTemplate_lastUpdated' - The timestamp in epoch seconds when the Job template was last updated.
--
-- 'priority', 'jobTemplate_priority' - Relative priority on the job.
--
-- 'queue', 'jobTemplate_queue' - Optional. The queue that jobs created from this template are assigned
-- to. If you don\'t specify this, jobs will go to the default queue.
--
-- 'statusUpdateInterval', 'jobTemplate_statusUpdateInterval' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
-- CloudWatch Events. Set the interval, in seconds, between status updates.
-- MediaConvert sends an update at this interval from the time the service
-- begins processing your job to the time it completes the transcode or
-- encounters an error.
--
-- 'type'', 'jobTemplate_type' - A job template can be of two types: system or custom. System or built-in
-- job templates can\'t be modified or deleted by the user.
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
      arn = Prelude.Nothing,
      category = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      hopDestinations = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      priority = Prelude.Nothing,
      queue = Prelude.Nothing,
      statusUpdateInterval = Prelude.Nothing,
      type' = Prelude.Nothing,
      settings = pSettings_,
      name = pName_
    }

-- | Accelerated transcoding can significantly speed up jobs with long,
-- visually complex content.
jobTemplate_accelerationSettings :: Lens.Lens' JobTemplate (Prelude.Maybe AccelerationSettings)
jobTemplate_accelerationSettings = Lens.lens (\JobTemplate' {accelerationSettings} -> accelerationSettings) (\s@JobTemplate' {} a -> s {accelerationSettings = a} :: JobTemplate)

-- | An identifier for this resource that is unique within all of AWS.
jobTemplate_arn :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Text)
jobTemplate_arn = Lens.lens (\JobTemplate' {arn} -> arn) (\s@JobTemplate' {} a -> s {arn = a} :: JobTemplate)

-- | An optional category you create to organize your job templates.
jobTemplate_category :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Text)
jobTemplate_category = Lens.lens (\JobTemplate' {category} -> category) (\s@JobTemplate' {} a -> s {category = a} :: JobTemplate)

-- | The timestamp in epoch seconds for Job template creation.
jobTemplate_createdAt :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.UTCTime)
jobTemplate_createdAt = Lens.lens (\JobTemplate' {createdAt} -> createdAt) (\s@JobTemplate' {} a -> s {createdAt = a} :: JobTemplate) Prelude.. Lens.mapping Data._Time

-- | An optional description you create for each job template.
jobTemplate_description :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Text)
jobTemplate_description = Lens.lens (\JobTemplate' {description} -> description) (\s@JobTemplate' {} a -> s {description = a} :: JobTemplate)

-- | Optional list of hop destinations.
jobTemplate_hopDestinations :: Lens.Lens' JobTemplate (Prelude.Maybe [HopDestination])
jobTemplate_hopDestinations = Lens.lens (\JobTemplate' {hopDestinations} -> hopDestinations) (\s@JobTemplate' {} a -> s {hopDestinations = a} :: JobTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp in epoch seconds when the Job template was last updated.
jobTemplate_lastUpdated :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.UTCTime)
jobTemplate_lastUpdated = Lens.lens (\JobTemplate' {lastUpdated} -> lastUpdated) (\s@JobTemplate' {} a -> s {lastUpdated = a} :: JobTemplate) Prelude.. Lens.mapping Data._Time

-- | Relative priority on the job.
jobTemplate_priority :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Int)
jobTemplate_priority = Lens.lens (\JobTemplate' {priority} -> priority) (\s@JobTemplate' {} a -> s {priority = a} :: JobTemplate)

-- | Optional. The queue that jobs created from this template are assigned
-- to. If you don\'t specify this, jobs will go to the default queue.
jobTemplate_queue :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Text)
jobTemplate_queue = Lens.lens (\JobTemplate' {queue} -> queue) (\s@JobTemplate' {} a -> s {queue = a} :: JobTemplate)

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
-- CloudWatch Events. Set the interval, in seconds, between status updates.
-- MediaConvert sends an update at this interval from the time the service
-- begins processing your job to the time it completes the transcode or
-- encounters an error.
jobTemplate_statusUpdateInterval :: Lens.Lens' JobTemplate (Prelude.Maybe StatusUpdateInterval)
jobTemplate_statusUpdateInterval = Lens.lens (\JobTemplate' {statusUpdateInterval} -> statusUpdateInterval) (\s@JobTemplate' {} a -> s {statusUpdateInterval = a} :: JobTemplate)

-- | A job template can be of two types: system or custom. System or built-in
-- job templates can\'t be modified or deleted by the user.
jobTemplate_type :: Lens.Lens' JobTemplate (Prelude.Maybe Type)
jobTemplate_type = Lens.lens (\JobTemplate' {type'} -> type') (\s@JobTemplate' {} a -> s {type' = a} :: JobTemplate)

-- | JobTemplateSettings contains all the transcode settings saved in the
-- template that will be applied to jobs created from it.
jobTemplate_settings :: Lens.Lens' JobTemplate JobTemplateSettings
jobTemplate_settings = Lens.lens (\JobTemplate' {settings} -> settings) (\s@JobTemplate' {} a -> s {settings = a} :: JobTemplate)

-- | A name you create for each job template. Each name must be unique within
-- your account.
jobTemplate_name :: Lens.Lens' JobTemplate Prelude.Text
jobTemplate_name = Lens.lens (\JobTemplate' {name} -> name) (\s@JobTemplate' {} a -> s {name = a} :: JobTemplate)

instance Data.FromJSON JobTemplate where
  parseJSON =
    Data.withObject
      "JobTemplate"
      ( \x ->
          JobTemplate'
            Prelude.<$> (x Data..:? "accelerationSettings")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "category")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> ( x
                            Data..:? "hopDestinations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "lastUpdated")
            Prelude.<*> (x Data..:? "priority")
            Prelude.<*> (x Data..:? "queue")
            Prelude.<*> (x Data..:? "statusUpdateInterval")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..: "settings")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable JobTemplate where
  hashWithSalt _salt JobTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` accelerationSettings
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` hopDestinations
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` queue
      `Prelude.hashWithSalt` statusUpdateInterval
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` name

instance Prelude.NFData JobTemplate where
  rnf JobTemplate' {..} =
    Prelude.rnf accelerationSettings
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf hopDestinations
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf queue
      `Prelude.seq` Prelude.rnf statusUpdateInterval
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf name
