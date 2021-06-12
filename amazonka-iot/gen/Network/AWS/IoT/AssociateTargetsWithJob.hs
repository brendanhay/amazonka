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
-- Module      : Network.AWS.IoT.AssociateTargetsWithJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a group with a continuous job. The following criteria must be
-- met:
--
-- -   The job must have been created with the @targetSelection@ field set
--     to \"CONTINUOUS\".
--
-- -   The job status must currently be \"IN_PROGRESS\".
--
-- -   The total number of targets associated with a job must not exceed
--     100.
module Network.AWS.IoT.AssociateTargetsWithJob
  ( -- * Creating a Request
    AssociateTargetsWithJob (..),
    newAssociateTargetsWithJob,

    -- * Request Lenses
    associateTargetsWithJob_namespaceId,
    associateTargetsWithJob_comment,
    associateTargetsWithJob_targets,
    associateTargetsWithJob_jobId,

    -- * Destructuring the Response
    AssociateTargetsWithJobResponse (..),
    newAssociateTargetsWithJobResponse,

    -- * Response Lenses
    associateTargetsWithJobResponse_jobArn,
    associateTargetsWithJobResponse_description,
    associateTargetsWithJobResponse_jobId,
    associateTargetsWithJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateTargetsWithJob' smart constructor.
data AssociateTargetsWithJob = AssociateTargetsWithJob'
  { -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs
    -- notifications to MQTT topics that contain the value in the following
    -- format.
    --
    -- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
    --
    -- The @namespaceId@ feature is in public preview.
    namespaceId :: Core.Maybe Core.Text,
    -- | An optional comment string describing why the job was associated with
    -- the targets.
    comment :: Core.Maybe Core.Text,
    -- | A list of thing group ARNs that define the targets of the job.
    targets :: Core.NonEmpty Core.Text,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateTargetsWithJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespaceId', 'associateTargetsWithJob_namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs
-- notifications to MQTT topics that contain the value in the following
-- format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
--
-- 'comment', 'associateTargetsWithJob_comment' - An optional comment string describing why the job was associated with
-- the targets.
--
-- 'targets', 'associateTargetsWithJob_targets' - A list of thing group ARNs that define the targets of the job.
--
-- 'jobId', 'associateTargetsWithJob_jobId' - The unique identifier you assigned to this job when it was created.
newAssociateTargetsWithJob ::
  -- | 'targets'
  Core.NonEmpty Core.Text ->
  -- | 'jobId'
  Core.Text ->
  AssociateTargetsWithJob
newAssociateTargetsWithJob pTargets_ pJobId_ =
  AssociateTargetsWithJob'
    { namespaceId =
        Core.Nothing,
      comment = Core.Nothing,
      targets = Lens._Coerce Lens.# pTargets_,
      jobId = pJobId_
    }

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs
-- notifications to MQTT topics that contain the value in the following
-- format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
associateTargetsWithJob_namespaceId :: Lens.Lens' AssociateTargetsWithJob (Core.Maybe Core.Text)
associateTargetsWithJob_namespaceId = Lens.lens (\AssociateTargetsWithJob' {namespaceId} -> namespaceId) (\s@AssociateTargetsWithJob' {} a -> s {namespaceId = a} :: AssociateTargetsWithJob)

-- | An optional comment string describing why the job was associated with
-- the targets.
associateTargetsWithJob_comment :: Lens.Lens' AssociateTargetsWithJob (Core.Maybe Core.Text)
associateTargetsWithJob_comment = Lens.lens (\AssociateTargetsWithJob' {comment} -> comment) (\s@AssociateTargetsWithJob' {} a -> s {comment = a} :: AssociateTargetsWithJob)

-- | A list of thing group ARNs that define the targets of the job.
associateTargetsWithJob_targets :: Lens.Lens' AssociateTargetsWithJob (Core.NonEmpty Core.Text)
associateTargetsWithJob_targets = Lens.lens (\AssociateTargetsWithJob' {targets} -> targets) (\s@AssociateTargetsWithJob' {} a -> s {targets = a} :: AssociateTargetsWithJob) Core.. Lens._Coerce

-- | The unique identifier you assigned to this job when it was created.
associateTargetsWithJob_jobId :: Lens.Lens' AssociateTargetsWithJob Core.Text
associateTargetsWithJob_jobId = Lens.lens (\AssociateTargetsWithJob' {jobId} -> jobId) (\s@AssociateTargetsWithJob' {} a -> s {jobId = a} :: AssociateTargetsWithJob)

instance Core.AWSRequest AssociateTargetsWithJob where
  type
    AWSResponse AssociateTargetsWithJob =
      AssociateTargetsWithJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateTargetsWithJobResponse'
            Core.<$> (x Core..?> "jobArn")
            Core.<*> (x Core..?> "description")
            Core.<*> (x Core..?> "jobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateTargetsWithJob

instance Core.NFData AssociateTargetsWithJob

instance Core.ToHeaders AssociateTargetsWithJob where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON AssociateTargetsWithJob where
  toJSON AssociateTargetsWithJob' {..} =
    Core.object
      ( Core.catMaybes
          [ ("comment" Core..=) Core.<$> comment,
            Core.Just ("targets" Core..= targets)
          ]
      )

instance Core.ToPath AssociateTargetsWithJob where
  toPath AssociateTargetsWithJob' {..} =
    Core.mconcat
      ["/jobs/", Core.toBS jobId, "/targets"]

instance Core.ToQuery AssociateTargetsWithJob where
  toQuery AssociateTargetsWithJob' {..} =
    Core.mconcat ["namespaceId" Core.=: namespaceId]

-- | /See:/ 'newAssociateTargetsWithJobResponse' smart constructor.
data AssociateTargetsWithJobResponse = AssociateTargetsWithJobResponse'
  { -- | An ARN identifying the job.
    jobArn :: Core.Maybe Core.Text,
    -- | A short text description of the job.
    description :: Core.Maybe Core.Text,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateTargetsWithJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobArn', 'associateTargetsWithJobResponse_jobArn' - An ARN identifying the job.
--
-- 'description', 'associateTargetsWithJobResponse_description' - A short text description of the job.
--
-- 'jobId', 'associateTargetsWithJobResponse_jobId' - The unique identifier you assigned to this job when it was created.
--
-- 'httpStatus', 'associateTargetsWithJobResponse_httpStatus' - The response's http status code.
newAssociateTargetsWithJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateTargetsWithJobResponse
newAssociateTargetsWithJobResponse pHttpStatus_ =
  AssociateTargetsWithJobResponse'
    { jobArn =
        Core.Nothing,
      description = Core.Nothing,
      jobId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An ARN identifying the job.
associateTargetsWithJobResponse_jobArn :: Lens.Lens' AssociateTargetsWithJobResponse (Core.Maybe Core.Text)
associateTargetsWithJobResponse_jobArn = Lens.lens (\AssociateTargetsWithJobResponse' {jobArn} -> jobArn) (\s@AssociateTargetsWithJobResponse' {} a -> s {jobArn = a} :: AssociateTargetsWithJobResponse)

-- | A short text description of the job.
associateTargetsWithJobResponse_description :: Lens.Lens' AssociateTargetsWithJobResponse (Core.Maybe Core.Text)
associateTargetsWithJobResponse_description = Lens.lens (\AssociateTargetsWithJobResponse' {description} -> description) (\s@AssociateTargetsWithJobResponse' {} a -> s {description = a} :: AssociateTargetsWithJobResponse)

-- | The unique identifier you assigned to this job when it was created.
associateTargetsWithJobResponse_jobId :: Lens.Lens' AssociateTargetsWithJobResponse (Core.Maybe Core.Text)
associateTargetsWithJobResponse_jobId = Lens.lens (\AssociateTargetsWithJobResponse' {jobId} -> jobId) (\s@AssociateTargetsWithJobResponse' {} a -> s {jobId = a} :: AssociateTargetsWithJobResponse)

-- | The response's http status code.
associateTargetsWithJobResponse_httpStatus :: Lens.Lens' AssociateTargetsWithJobResponse Core.Int
associateTargetsWithJobResponse_httpStatus = Lens.lens (\AssociateTargetsWithJobResponse' {httpStatus} -> httpStatus) (\s@AssociateTargetsWithJobResponse' {} a -> s {httpStatus = a} :: AssociateTargetsWithJobResponse)

instance Core.NFData AssociateTargetsWithJobResponse
