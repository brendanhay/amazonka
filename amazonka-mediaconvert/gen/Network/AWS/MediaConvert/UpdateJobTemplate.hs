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
-- Module      : Network.AWS.MediaConvert.UpdateJobTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify one of your existing job templates.
module Network.AWS.MediaConvert.UpdateJobTemplate
  ( -- * Creating a Request
    UpdateJobTemplate (..),
    newUpdateJobTemplate,

    -- * Request Lenses
    updateJobTemplate_accelerationSettings,
    updateJobTemplate_category,
    updateJobTemplate_priority,
    updateJobTemplate_statusUpdateInterval,
    updateJobTemplate_queue,
    updateJobTemplate_description,
    updateJobTemplate_hopDestinations,
    updateJobTemplate_settings,
    updateJobTemplate_name,

    -- * Destructuring the Response
    UpdateJobTemplateResponse (..),
    newUpdateJobTemplateResponse,

    -- * Response Lenses
    updateJobTemplateResponse_jobTemplate,
    updateJobTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateJobTemplate' smart constructor.
data UpdateJobTemplate = UpdateJobTemplate'
  { -- | Accelerated transcoding can significantly speed up jobs with long,
    -- visually complex content. Outputs that use this feature incur pro-tier
    -- pricing. For information about feature limitations, see the AWS
    -- Elemental MediaConvert User Guide.
    accelerationSettings :: Core.Maybe AccelerationSettings,
    -- | The new category for the job template, if you are changing it.
    category :: Core.Maybe Core.Text,
    -- | Specify the relative priority for this job. In any given queue, the
    -- service begins processing the job with the highest value first. When
    -- more than one job has the same priority, the service begins processing
    -- the job that you submitted first. If you don\'t specify a priority, the
    -- service uses the default value 0.
    priority :: Core.Maybe Core.Int,
    -- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
    -- CloudWatch Events. Set the interval, in seconds, between status updates.
    -- MediaConvert sends an update at this interval from the time the service
    -- begins processing your job to the time it completes the transcode or
    -- encounters an error.
    statusUpdateInterval :: Core.Maybe StatusUpdateInterval,
    -- | The new queue for the job template, if you are changing it.
    queue :: Core.Maybe Core.Text,
    -- | The new description for the job template, if you are changing it.
    description :: Core.Maybe Core.Text,
    -- | Optional list of hop destinations.
    hopDestinations :: Core.Maybe [HopDestination],
    -- | JobTemplateSettings contains all the transcode settings saved in the
    -- template that will be applied to jobs created from it.
    settings :: Core.Maybe JobTemplateSettings,
    -- | The name of the job template you are modifying
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'priority', 'updateJobTemplate_priority' - Specify the relative priority for this job. In any given queue, the
-- service begins processing the job with the highest value first. When
-- more than one job has the same priority, the service begins processing
-- the job that you submitted first. If you don\'t specify a priority, the
-- service uses the default value 0.
--
-- 'statusUpdateInterval', 'updateJobTemplate_statusUpdateInterval' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
-- CloudWatch Events. Set the interval, in seconds, between status updates.
-- MediaConvert sends an update at this interval from the time the service
-- begins processing your job to the time it completes the transcode or
-- encounters an error.
--
-- 'queue', 'updateJobTemplate_queue' - The new queue for the job template, if you are changing it.
--
-- 'description', 'updateJobTemplate_description' - The new description for the job template, if you are changing it.
--
-- 'hopDestinations', 'updateJobTemplate_hopDestinations' - Optional list of hop destinations.
--
-- 'settings', 'updateJobTemplate_settings' - JobTemplateSettings contains all the transcode settings saved in the
-- template that will be applied to jobs created from it.
--
-- 'name', 'updateJobTemplate_name' - The name of the job template you are modifying
newUpdateJobTemplate ::
  -- | 'name'
  Core.Text ->
  UpdateJobTemplate
newUpdateJobTemplate pName_ =
  UpdateJobTemplate'
    { accelerationSettings =
        Core.Nothing,
      category = Core.Nothing,
      priority = Core.Nothing,
      statusUpdateInterval = Core.Nothing,
      queue = Core.Nothing,
      description = Core.Nothing,
      hopDestinations = Core.Nothing,
      settings = Core.Nothing,
      name = pName_
    }

-- | Accelerated transcoding can significantly speed up jobs with long,
-- visually complex content. Outputs that use this feature incur pro-tier
-- pricing. For information about feature limitations, see the AWS
-- Elemental MediaConvert User Guide.
updateJobTemplate_accelerationSettings :: Lens.Lens' UpdateJobTemplate (Core.Maybe AccelerationSettings)
updateJobTemplate_accelerationSettings = Lens.lens (\UpdateJobTemplate' {accelerationSettings} -> accelerationSettings) (\s@UpdateJobTemplate' {} a -> s {accelerationSettings = a} :: UpdateJobTemplate)

-- | The new category for the job template, if you are changing it.
updateJobTemplate_category :: Lens.Lens' UpdateJobTemplate (Core.Maybe Core.Text)
updateJobTemplate_category = Lens.lens (\UpdateJobTemplate' {category} -> category) (\s@UpdateJobTemplate' {} a -> s {category = a} :: UpdateJobTemplate)

-- | Specify the relative priority for this job. In any given queue, the
-- service begins processing the job with the highest value first. When
-- more than one job has the same priority, the service begins processing
-- the job that you submitted first. If you don\'t specify a priority, the
-- service uses the default value 0.
updateJobTemplate_priority :: Lens.Lens' UpdateJobTemplate (Core.Maybe Core.Int)
updateJobTemplate_priority = Lens.lens (\UpdateJobTemplate' {priority} -> priority) (\s@UpdateJobTemplate' {} a -> s {priority = a} :: UpdateJobTemplate)

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
-- CloudWatch Events. Set the interval, in seconds, between status updates.
-- MediaConvert sends an update at this interval from the time the service
-- begins processing your job to the time it completes the transcode or
-- encounters an error.
updateJobTemplate_statusUpdateInterval :: Lens.Lens' UpdateJobTemplate (Core.Maybe StatusUpdateInterval)
updateJobTemplate_statusUpdateInterval = Lens.lens (\UpdateJobTemplate' {statusUpdateInterval} -> statusUpdateInterval) (\s@UpdateJobTemplate' {} a -> s {statusUpdateInterval = a} :: UpdateJobTemplate)

-- | The new queue for the job template, if you are changing it.
updateJobTemplate_queue :: Lens.Lens' UpdateJobTemplate (Core.Maybe Core.Text)
updateJobTemplate_queue = Lens.lens (\UpdateJobTemplate' {queue} -> queue) (\s@UpdateJobTemplate' {} a -> s {queue = a} :: UpdateJobTemplate)

-- | The new description for the job template, if you are changing it.
updateJobTemplate_description :: Lens.Lens' UpdateJobTemplate (Core.Maybe Core.Text)
updateJobTemplate_description = Lens.lens (\UpdateJobTemplate' {description} -> description) (\s@UpdateJobTemplate' {} a -> s {description = a} :: UpdateJobTemplate)

-- | Optional list of hop destinations.
updateJobTemplate_hopDestinations :: Lens.Lens' UpdateJobTemplate (Core.Maybe [HopDestination])
updateJobTemplate_hopDestinations = Lens.lens (\UpdateJobTemplate' {hopDestinations} -> hopDestinations) (\s@UpdateJobTemplate' {} a -> s {hopDestinations = a} :: UpdateJobTemplate) Core.. Lens.mapping Lens._Coerce

-- | JobTemplateSettings contains all the transcode settings saved in the
-- template that will be applied to jobs created from it.
updateJobTemplate_settings :: Lens.Lens' UpdateJobTemplate (Core.Maybe JobTemplateSettings)
updateJobTemplate_settings = Lens.lens (\UpdateJobTemplate' {settings} -> settings) (\s@UpdateJobTemplate' {} a -> s {settings = a} :: UpdateJobTemplate)

-- | The name of the job template you are modifying
updateJobTemplate_name :: Lens.Lens' UpdateJobTemplate Core.Text
updateJobTemplate_name = Lens.lens (\UpdateJobTemplate' {name} -> name) (\s@UpdateJobTemplate' {} a -> s {name = a} :: UpdateJobTemplate)

instance Core.AWSRequest UpdateJobTemplate where
  type
    AWSResponse UpdateJobTemplate =
      UpdateJobTemplateResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateJobTemplateResponse'
            Core.<$> (x Core..?> "jobTemplate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateJobTemplate

instance Core.NFData UpdateJobTemplate

instance Core.ToHeaders UpdateJobTemplate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateJobTemplate where
  toJSON UpdateJobTemplate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("accelerationSettings" Core..=)
              Core.<$> accelerationSettings,
            ("category" Core..=) Core.<$> category,
            ("priority" Core..=) Core.<$> priority,
            ("statusUpdateInterval" Core..=)
              Core.<$> statusUpdateInterval,
            ("queue" Core..=) Core.<$> queue,
            ("description" Core..=) Core.<$> description,
            ("hopDestinations" Core..=) Core.<$> hopDestinations,
            ("settings" Core..=) Core.<$> settings
          ]
      )

instance Core.ToPath UpdateJobTemplate where
  toPath UpdateJobTemplate' {..} =
    Core.mconcat
      ["/2017-08-29/jobTemplates/", Core.toBS name]

instance Core.ToQuery UpdateJobTemplate where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateJobTemplateResponse' smart constructor.
data UpdateJobTemplateResponse = UpdateJobTemplateResponse'
  { -- | A job template is a pre-made set of encoding instructions that you can
    -- use to quickly create a job.
    jobTemplate :: Core.Maybe JobTemplate,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateJobTemplateResponse
newUpdateJobTemplateResponse pHttpStatus_ =
  UpdateJobTemplateResponse'
    { jobTemplate =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A job template is a pre-made set of encoding instructions that you can
-- use to quickly create a job.
updateJobTemplateResponse_jobTemplate :: Lens.Lens' UpdateJobTemplateResponse (Core.Maybe JobTemplate)
updateJobTemplateResponse_jobTemplate = Lens.lens (\UpdateJobTemplateResponse' {jobTemplate} -> jobTemplate) (\s@UpdateJobTemplateResponse' {} a -> s {jobTemplate = a} :: UpdateJobTemplateResponse)

-- | The response's http status code.
updateJobTemplateResponse_httpStatus :: Lens.Lens' UpdateJobTemplateResponse Core.Int
updateJobTemplateResponse_httpStatus = Lens.lens (\UpdateJobTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateJobTemplateResponse' {} a -> s {httpStatus = a} :: UpdateJobTemplateResponse)

instance Core.NFData UpdateJobTemplateResponse
