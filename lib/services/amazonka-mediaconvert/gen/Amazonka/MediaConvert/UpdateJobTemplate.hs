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
-- Module      : Amazonka.MediaConvert.UpdateJobTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify one of your existing job templates.
module Amazonka.MediaConvert.UpdateJobTemplate
  ( -- * Creating a Request
    UpdateJobTemplate (..),
    newUpdateJobTemplate,

    -- * Request Lenses
    updateJobTemplate_accelerationSettings,
    updateJobTemplate_category,
    updateJobTemplate_description,
    updateJobTemplate_hopDestinations,
    updateJobTemplate_priority,
    updateJobTemplate_queue,
    updateJobTemplate_settings,
    updateJobTemplate_statusUpdateInterval,
    updateJobTemplate_name,

    -- * Destructuring the Response
    UpdateJobTemplateResponse (..),
    newUpdateJobTemplateResponse,

    -- * Response Lenses
    updateJobTemplateResponse_jobTemplate,
    updateJobTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateJobTemplate' smart constructor.
data UpdateJobTemplate = UpdateJobTemplate'
  { -- | Accelerated transcoding can significantly speed up jobs with long,
    -- visually complex content. Outputs that use this feature incur pro-tier
    -- pricing. For information about feature limitations, see the AWS
    -- Elemental MediaConvert User Guide.
    accelerationSettings :: Prelude.Maybe AccelerationSettings,
    -- | The new category for the job template, if you are changing it.
    category :: Prelude.Maybe Prelude.Text,
    -- | The new description for the job template, if you are changing it.
    description :: Prelude.Maybe Prelude.Text,
    -- | Optional list of hop destinations.
    hopDestinations :: Prelude.Maybe [HopDestination],
    -- | Specify the relative priority for this job. In any given queue, the
    -- service begins processing the job with the highest value first. When
    -- more than one job has the same priority, the service begins processing
    -- the job that you submitted first. If you don\'t specify a priority, the
    -- service uses the default value 0.
    priority :: Prelude.Maybe Prelude.Int,
    -- | The new queue for the job template, if you are changing it.
    queue :: Prelude.Maybe Prelude.Text,
    -- | JobTemplateSettings contains all the transcode settings saved in the
    -- template that will be applied to jobs created from it.
    settings :: Prelude.Maybe JobTemplateSettings,
    -- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
    -- CloudWatch Events. Set the interval, in seconds, between status updates.
    -- MediaConvert sends an update at this interval from the time the service
    -- begins processing your job to the time it completes the transcode or
    -- encounters an error.
    statusUpdateInterval :: Prelude.Maybe StatusUpdateInterval,
    -- | The name of the job template you are modifying
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateJobTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accelerationSettings', 'updateJobTemplate_accelerationSettings' - Accelerated transcoding can significantly speed up jobs with long,
-- visually complex content. Outputs that use this feature incur pro-tier
-- pricing. For information about feature limitations, see the AWS
-- Elemental MediaConvert User Guide.
--
-- 'category', 'updateJobTemplate_category' - The new category for the job template, if you are changing it.
--
-- 'description', 'updateJobTemplate_description' - The new description for the job template, if you are changing it.
--
-- 'hopDestinations', 'updateJobTemplate_hopDestinations' - Optional list of hop destinations.
--
-- 'priority', 'updateJobTemplate_priority' - Specify the relative priority for this job. In any given queue, the
-- service begins processing the job with the highest value first. When
-- more than one job has the same priority, the service begins processing
-- the job that you submitted first. If you don\'t specify a priority, the
-- service uses the default value 0.
--
-- 'queue', 'updateJobTemplate_queue' - The new queue for the job template, if you are changing it.
--
-- 'settings', 'updateJobTemplate_settings' - JobTemplateSettings contains all the transcode settings saved in the
-- template that will be applied to jobs created from it.
--
-- 'statusUpdateInterval', 'updateJobTemplate_statusUpdateInterval' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
-- CloudWatch Events. Set the interval, in seconds, between status updates.
-- MediaConvert sends an update at this interval from the time the service
-- begins processing your job to the time it completes the transcode or
-- encounters an error.
--
-- 'name', 'updateJobTemplate_name' - The name of the job template you are modifying
newUpdateJobTemplate ::
  -- | 'name'
  Prelude.Text ->
  UpdateJobTemplate
newUpdateJobTemplate pName_ =
  UpdateJobTemplate'
    { accelerationSettings =
        Prelude.Nothing,
      category = Prelude.Nothing,
      description = Prelude.Nothing,
      hopDestinations = Prelude.Nothing,
      priority = Prelude.Nothing,
      queue = Prelude.Nothing,
      settings = Prelude.Nothing,
      statusUpdateInterval = Prelude.Nothing,
      name = pName_
    }

-- | Accelerated transcoding can significantly speed up jobs with long,
-- visually complex content. Outputs that use this feature incur pro-tier
-- pricing. For information about feature limitations, see the AWS
-- Elemental MediaConvert User Guide.
updateJobTemplate_accelerationSettings :: Lens.Lens' UpdateJobTemplate (Prelude.Maybe AccelerationSettings)
updateJobTemplate_accelerationSettings = Lens.lens (\UpdateJobTemplate' {accelerationSettings} -> accelerationSettings) (\s@UpdateJobTemplate' {} a -> s {accelerationSettings = a} :: UpdateJobTemplate)

-- | The new category for the job template, if you are changing it.
updateJobTemplate_category :: Lens.Lens' UpdateJobTemplate (Prelude.Maybe Prelude.Text)
updateJobTemplate_category = Lens.lens (\UpdateJobTemplate' {category} -> category) (\s@UpdateJobTemplate' {} a -> s {category = a} :: UpdateJobTemplate)

-- | The new description for the job template, if you are changing it.
updateJobTemplate_description :: Lens.Lens' UpdateJobTemplate (Prelude.Maybe Prelude.Text)
updateJobTemplate_description = Lens.lens (\UpdateJobTemplate' {description} -> description) (\s@UpdateJobTemplate' {} a -> s {description = a} :: UpdateJobTemplate)

-- | Optional list of hop destinations.
updateJobTemplate_hopDestinations :: Lens.Lens' UpdateJobTemplate (Prelude.Maybe [HopDestination])
updateJobTemplate_hopDestinations = Lens.lens (\UpdateJobTemplate' {hopDestinations} -> hopDestinations) (\s@UpdateJobTemplate' {} a -> s {hopDestinations = a} :: UpdateJobTemplate) Prelude.. Lens.mapping Lens.coerced

-- | Specify the relative priority for this job. In any given queue, the
-- service begins processing the job with the highest value first. When
-- more than one job has the same priority, the service begins processing
-- the job that you submitted first. If you don\'t specify a priority, the
-- service uses the default value 0.
updateJobTemplate_priority :: Lens.Lens' UpdateJobTemplate (Prelude.Maybe Prelude.Int)
updateJobTemplate_priority = Lens.lens (\UpdateJobTemplate' {priority} -> priority) (\s@UpdateJobTemplate' {} a -> s {priority = a} :: UpdateJobTemplate)

-- | The new queue for the job template, if you are changing it.
updateJobTemplate_queue :: Lens.Lens' UpdateJobTemplate (Prelude.Maybe Prelude.Text)
updateJobTemplate_queue = Lens.lens (\UpdateJobTemplate' {queue} -> queue) (\s@UpdateJobTemplate' {} a -> s {queue = a} :: UpdateJobTemplate)

-- | JobTemplateSettings contains all the transcode settings saved in the
-- template that will be applied to jobs created from it.
updateJobTemplate_settings :: Lens.Lens' UpdateJobTemplate (Prelude.Maybe JobTemplateSettings)
updateJobTemplate_settings = Lens.lens (\UpdateJobTemplate' {settings} -> settings) (\s@UpdateJobTemplate' {} a -> s {settings = a} :: UpdateJobTemplate)

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
-- CloudWatch Events. Set the interval, in seconds, between status updates.
-- MediaConvert sends an update at this interval from the time the service
-- begins processing your job to the time it completes the transcode or
-- encounters an error.
updateJobTemplate_statusUpdateInterval :: Lens.Lens' UpdateJobTemplate (Prelude.Maybe StatusUpdateInterval)
updateJobTemplate_statusUpdateInterval = Lens.lens (\UpdateJobTemplate' {statusUpdateInterval} -> statusUpdateInterval) (\s@UpdateJobTemplate' {} a -> s {statusUpdateInterval = a} :: UpdateJobTemplate)

-- | The name of the job template you are modifying
updateJobTemplate_name :: Lens.Lens' UpdateJobTemplate Prelude.Text
updateJobTemplate_name = Lens.lens (\UpdateJobTemplate' {name} -> name) (\s@UpdateJobTemplate' {} a -> s {name = a} :: UpdateJobTemplate)

instance Core.AWSRequest UpdateJobTemplate where
  type
    AWSResponse UpdateJobTemplate =
      UpdateJobTemplateResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateJobTemplateResponse'
            Prelude.<$> (x Data..?> "jobTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateJobTemplate where
  hashWithSalt _salt UpdateJobTemplate' {..} =
    _salt `Prelude.hashWithSalt` accelerationSettings
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` hopDestinations
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` queue
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` statusUpdateInterval
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateJobTemplate where
  rnf UpdateJobTemplate' {..} =
    Prelude.rnf accelerationSettings
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf hopDestinations
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf queue
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf statusUpdateInterval
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateJobTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateJobTemplate where
  toJSON UpdateJobTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accelerationSettings" Data..=)
              Prelude.<$> accelerationSettings,
            ("category" Data..=) Prelude.<$> category,
            ("description" Data..=) Prelude.<$> description,
            ("hopDestinations" Data..=)
              Prelude.<$> hopDestinations,
            ("priority" Data..=) Prelude.<$> priority,
            ("queue" Data..=) Prelude.<$> queue,
            ("settings" Data..=) Prelude.<$> settings,
            ("statusUpdateInterval" Data..=)
              Prelude.<$> statusUpdateInterval
          ]
      )

instance Data.ToPath UpdateJobTemplate where
  toPath UpdateJobTemplate' {..} =
    Prelude.mconcat
      ["/2017-08-29/jobTemplates/", Data.toBS name]

instance Data.ToQuery UpdateJobTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateJobTemplateResponse' smart constructor.
data UpdateJobTemplateResponse = UpdateJobTemplateResponse'
  { -- | A job template is a pre-made set of encoding instructions that you can
    -- use to quickly create a job.
    jobTemplate :: Prelude.Maybe JobTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateJobTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobTemplate', 'updateJobTemplateResponse_jobTemplate' - A job template is a pre-made set of encoding instructions that you can
-- use to quickly create a job.
--
-- 'httpStatus', 'updateJobTemplateResponse_httpStatus' - The response's http status code.
newUpdateJobTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateJobTemplateResponse
newUpdateJobTemplateResponse pHttpStatus_ =
  UpdateJobTemplateResponse'
    { jobTemplate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A job template is a pre-made set of encoding instructions that you can
-- use to quickly create a job.
updateJobTemplateResponse_jobTemplate :: Lens.Lens' UpdateJobTemplateResponse (Prelude.Maybe JobTemplate)
updateJobTemplateResponse_jobTemplate = Lens.lens (\UpdateJobTemplateResponse' {jobTemplate} -> jobTemplate) (\s@UpdateJobTemplateResponse' {} a -> s {jobTemplate = a} :: UpdateJobTemplateResponse)

-- | The response's http status code.
updateJobTemplateResponse_httpStatus :: Lens.Lens' UpdateJobTemplateResponse Prelude.Int
updateJobTemplateResponse_httpStatus = Lens.lens (\UpdateJobTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateJobTemplateResponse' {} a -> s {httpStatus = a} :: UpdateJobTemplateResponse)

instance Prelude.NFData UpdateJobTemplateResponse where
  rnf UpdateJobTemplateResponse' {..} =
    Prelude.rnf jobTemplate
      `Prelude.seq` Prelude.rnf httpStatus
