{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConvert.CreateJobTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new job template. For information about job templates see the
-- User Guide at
-- http:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
module Amazonka.MediaConvert.CreateJobTemplate
  ( -- * Creating a Request
    CreateJobTemplate (..),
    newCreateJobTemplate,

    -- * Request Lenses
    createJobTemplate_tags,
    createJobTemplate_hopDestinations,
    createJobTemplate_statusUpdateInterval,
    createJobTemplate_description,
    createJobTemplate_priority,
    createJobTemplate_category,
    createJobTemplate_accelerationSettings,
    createJobTemplate_queue,
    createJobTemplate_settings,
    createJobTemplate_name,

    -- * Destructuring the Response
    CreateJobTemplateResponse (..),
    newCreateJobTemplateResponse,

    -- * Response Lenses
    createJobTemplateResponse_jobTemplate,
    createJobTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConvert.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateJobTemplate' smart constructor.
data CreateJobTemplate = CreateJobTemplate'
  { -- | The tags that you want to add to the resource. You can tag resources
    -- with a key-value pair or with only a key.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Optional. Use queue hopping to avoid overly long waits in the backlog of
    -- the queue that you submit your job to. Specify an alternate queue and
    -- the maximum time that your job will wait in the initial queue before
    -- hopping. For more information about this feature, see the AWS Elemental
    -- MediaConvert User Guide.
    hopDestinations :: Prelude.Maybe [HopDestination],
    -- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
    -- CloudWatch Events. Set the interval, in seconds, between status updates.
    -- MediaConvert sends an update at this interval from the time the service
    -- begins processing your job to the time it completes the transcode or
    -- encounters an error.
    statusUpdateInterval :: Prelude.Maybe StatusUpdateInterval,
    -- | Optional. A description of the job template you are creating.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specify the relative priority for this job. In any given queue, the
    -- service begins processing the job with the highest value first. When
    -- more than one job has the same priority, the service begins processing
    -- the job that you submitted first. If you don\'t specify a priority, the
    -- service uses the default value 0.
    priority :: Prelude.Maybe Prelude.Int,
    -- | Optional. A category for the job template you are creating
    category :: Prelude.Maybe Prelude.Text,
    -- | Accelerated transcoding can significantly speed up jobs with long,
    -- visually complex content. Outputs that use this feature incur pro-tier
    -- pricing. For information about feature limitations, see the AWS
    -- Elemental MediaConvert User Guide.
    accelerationSettings :: Prelude.Maybe AccelerationSettings,
    -- | Optional. The queue that jobs created from this template are assigned
    -- to. If you don\'t specify this, jobs will go to the default queue.
    queue :: Prelude.Maybe Prelude.Text,
    -- | JobTemplateSettings contains all the transcode settings saved in the
    -- template that will be applied to jobs created from it.
    settings :: JobTemplateSettings,
    -- | The name of the job template you are creating.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJobTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createJobTemplate_tags' - The tags that you want to add to the resource. You can tag resources
-- with a key-value pair or with only a key.
--
-- 'hopDestinations', 'createJobTemplate_hopDestinations' - Optional. Use queue hopping to avoid overly long waits in the backlog of
-- the queue that you submit your job to. Specify an alternate queue and
-- the maximum time that your job will wait in the initial queue before
-- hopping. For more information about this feature, see the AWS Elemental
-- MediaConvert User Guide.
--
-- 'statusUpdateInterval', 'createJobTemplate_statusUpdateInterval' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
-- CloudWatch Events. Set the interval, in seconds, between status updates.
-- MediaConvert sends an update at this interval from the time the service
-- begins processing your job to the time it completes the transcode or
-- encounters an error.
--
-- 'description', 'createJobTemplate_description' - Optional. A description of the job template you are creating.
--
-- 'priority', 'createJobTemplate_priority' - Specify the relative priority for this job. In any given queue, the
-- service begins processing the job with the highest value first. When
-- more than one job has the same priority, the service begins processing
-- the job that you submitted first. If you don\'t specify a priority, the
-- service uses the default value 0.
--
-- 'category', 'createJobTemplate_category' - Optional. A category for the job template you are creating
--
-- 'accelerationSettings', 'createJobTemplate_accelerationSettings' - Accelerated transcoding can significantly speed up jobs with long,
-- visually complex content. Outputs that use this feature incur pro-tier
-- pricing. For information about feature limitations, see the AWS
-- Elemental MediaConvert User Guide.
--
-- 'queue', 'createJobTemplate_queue' - Optional. The queue that jobs created from this template are assigned
-- to. If you don\'t specify this, jobs will go to the default queue.
--
-- 'settings', 'createJobTemplate_settings' - JobTemplateSettings contains all the transcode settings saved in the
-- template that will be applied to jobs created from it.
--
-- 'name', 'createJobTemplate_name' - The name of the job template you are creating.
newCreateJobTemplate ::
  -- | 'settings'
  JobTemplateSettings ->
  -- | 'name'
  Prelude.Text ->
  CreateJobTemplate
newCreateJobTemplate pSettings_ pName_ =
  CreateJobTemplate'
    { tags = Prelude.Nothing,
      hopDestinations = Prelude.Nothing,
      statusUpdateInterval = Prelude.Nothing,
      description = Prelude.Nothing,
      priority = Prelude.Nothing,
      category = Prelude.Nothing,
      accelerationSettings = Prelude.Nothing,
      queue = Prelude.Nothing,
      settings = pSettings_,
      name = pName_
    }

-- | The tags that you want to add to the resource. You can tag resources
-- with a key-value pair or with only a key.
createJobTemplate_tags :: Lens.Lens' CreateJobTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createJobTemplate_tags = Lens.lens (\CreateJobTemplate' {tags} -> tags) (\s@CreateJobTemplate' {} a -> s {tags = a} :: CreateJobTemplate) Prelude.. Lens.mapping Lens.coerced

-- | Optional. Use queue hopping to avoid overly long waits in the backlog of
-- the queue that you submit your job to. Specify an alternate queue and
-- the maximum time that your job will wait in the initial queue before
-- hopping. For more information about this feature, see the AWS Elemental
-- MediaConvert User Guide.
createJobTemplate_hopDestinations :: Lens.Lens' CreateJobTemplate (Prelude.Maybe [HopDestination])
createJobTemplate_hopDestinations = Lens.lens (\CreateJobTemplate' {hopDestinations} -> hopDestinations) (\s@CreateJobTemplate' {} a -> s {hopDestinations = a} :: CreateJobTemplate) Prelude.. Lens.mapping Lens.coerced

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
-- CloudWatch Events. Set the interval, in seconds, between status updates.
-- MediaConvert sends an update at this interval from the time the service
-- begins processing your job to the time it completes the transcode or
-- encounters an error.
createJobTemplate_statusUpdateInterval :: Lens.Lens' CreateJobTemplate (Prelude.Maybe StatusUpdateInterval)
createJobTemplate_statusUpdateInterval = Lens.lens (\CreateJobTemplate' {statusUpdateInterval} -> statusUpdateInterval) (\s@CreateJobTemplate' {} a -> s {statusUpdateInterval = a} :: CreateJobTemplate)

-- | Optional. A description of the job template you are creating.
createJobTemplate_description :: Lens.Lens' CreateJobTemplate (Prelude.Maybe Prelude.Text)
createJobTemplate_description = Lens.lens (\CreateJobTemplate' {description} -> description) (\s@CreateJobTemplate' {} a -> s {description = a} :: CreateJobTemplate)

-- | Specify the relative priority for this job. In any given queue, the
-- service begins processing the job with the highest value first. When
-- more than one job has the same priority, the service begins processing
-- the job that you submitted first. If you don\'t specify a priority, the
-- service uses the default value 0.
createJobTemplate_priority :: Lens.Lens' CreateJobTemplate (Prelude.Maybe Prelude.Int)
createJobTemplate_priority = Lens.lens (\CreateJobTemplate' {priority} -> priority) (\s@CreateJobTemplate' {} a -> s {priority = a} :: CreateJobTemplate)

-- | Optional. A category for the job template you are creating
createJobTemplate_category :: Lens.Lens' CreateJobTemplate (Prelude.Maybe Prelude.Text)
createJobTemplate_category = Lens.lens (\CreateJobTemplate' {category} -> category) (\s@CreateJobTemplate' {} a -> s {category = a} :: CreateJobTemplate)

-- | Accelerated transcoding can significantly speed up jobs with long,
-- visually complex content. Outputs that use this feature incur pro-tier
-- pricing. For information about feature limitations, see the AWS
-- Elemental MediaConvert User Guide.
createJobTemplate_accelerationSettings :: Lens.Lens' CreateJobTemplate (Prelude.Maybe AccelerationSettings)
createJobTemplate_accelerationSettings = Lens.lens (\CreateJobTemplate' {accelerationSettings} -> accelerationSettings) (\s@CreateJobTemplate' {} a -> s {accelerationSettings = a} :: CreateJobTemplate)

-- | Optional. The queue that jobs created from this template are assigned
-- to. If you don\'t specify this, jobs will go to the default queue.
createJobTemplate_queue :: Lens.Lens' CreateJobTemplate (Prelude.Maybe Prelude.Text)
createJobTemplate_queue = Lens.lens (\CreateJobTemplate' {queue} -> queue) (\s@CreateJobTemplate' {} a -> s {queue = a} :: CreateJobTemplate)

-- | JobTemplateSettings contains all the transcode settings saved in the
-- template that will be applied to jobs created from it.
createJobTemplate_settings :: Lens.Lens' CreateJobTemplate JobTemplateSettings
createJobTemplate_settings = Lens.lens (\CreateJobTemplate' {settings} -> settings) (\s@CreateJobTemplate' {} a -> s {settings = a} :: CreateJobTemplate)

-- | The name of the job template you are creating.
createJobTemplate_name :: Lens.Lens' CreateJobTemplate Prelude.Text
createJobTemplate_name = Lens.lens (\CreateJobTemplate' {name} -> name) (\s@CreateJobTemplate' {} a -> s {name = a} :: CreateJobTemplate)

instance Core.AWSRequest CreateJobTemplate where
  type
    AWSResponse CreateJobTemplate =
      CreateJobTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobTemplateResponse'
            Prelude.<$> (x Core..?> "jobTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateJobTemplate where
  hashWithSalt _salt CreateJobTemplate' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` hopDestinations
      `Prelude.hashWithSalt` statusUpdateInterval
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` accelerationSettings
      `Prelude.hashWithSalt` queue
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateJobTemplate where
  rnf CreateJobTemplate' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf hopDestinations
      `Prelude.seq` Prelude.rnf statusUpdateInterval
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf accelerationSettings
      `Prelude.seq` Prelude.rnf queue
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreateJobTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateJobTemplate where
  toJSON CreateJobTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("hopDestinations" Core..=)
              Prelude.<$> hopDestinations,
            ("statusUpdateInterval" Core..=)
              Prelude.<$> statusUpdateInterval,
            ("description" Core..=) Prelude.<$> description,
            ("priority" Core..=) Prelude.<$> priority,
            ("category" Core..=) Prelude.<$> category,
            ("accelerationSettings" Core..=)
              Prelude.<$> accelerationSettings,
            ("queue" Core..=) Prelude.<$> queue,
            Prelude.Just ("settings" Core..= settings),
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateJobTemplate where
  toPath = Prelude.const "/2017-08-29/jobTemplates"

instance Core.ToQuery CreateJobTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateJobTemplateResponse' smart constructor.
data CreateJobTemplateResponse = CreateJobTemplateResponse'
  { -- | A job template is a pre-made set of encoding instructions that you can
    -- use to quickly create a job.
    jobTemplate :: Prelude.Maybe JobTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJobTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobTemplate', 'createJobTemplateResponse_jobTemplate' - A job template is a pre-made set of encoding instructions that you can
-- use to quickly create a job.
--
-- 'httpStatus', 'createJobTemplateResponse_httpStatus' - The response's http status code.
newCreateJobTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateJobTemplateResponse
newCreateJobTemplateResponse pHttpStatus_ =
  CreateJobTemplateResponse'
    { jobTemplate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A job template is a pre-made set of encoding instructions that you can
-- use to quickly create a job.
createJobTemplateResponse_jobTemplate :: Lens.Lens' CreateJobTemplateResponse (Prelude.Maybe JobTemplate)
createJobTemplateResponse_jobTemplate = Lens.lens (\CreateJobTemplateResponse' {jobTemplate} -> jobTemplate) (\s@CreateJobTemplateResponse' {} a -> s {jobTemplate = a} :: CreateJobTemplateResponse)

-- | The response's http status code.
createJobTemplateResponse_httpStatus :: Lens.Lens' CreateJobTemplateResponse Prelude.Int
createJobTemplateResponse_httpStatus = Lens.lens (\CreateJobTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateJobTemplateResponse' {} a -> s {httpStatus = a} :: CreateJobTemplateResponse)

instance Prelude.NFData CreateJobTemplateResponse where
  rnf CreateJobTemplateResponse' {..} =
    Prelude.rnf jobTemplate
      `Prelude.seq` Prelude.rnf httpStatus
