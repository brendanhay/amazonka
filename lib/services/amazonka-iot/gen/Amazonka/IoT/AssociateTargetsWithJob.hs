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
-- Module      : Amazonka.IoT.AssociateTargetsWithJob
-- Copyright   : (c) 2013-2022 Brendan Hay
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
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions AssociateTargetsWithJob>
-- action.
module Amazonka.IoT.AssociateTargetsWithJob
  ( -- * Creating a Request
    AssociateTargetsWithJob (..),
    newAssociateTargetsWithJob,

    -- * Request Lenses
    associateTargetsWithJob_comment,
    associateTargetsWithJob_namespaceId,
    associateTargetsWithJob_targets,
    associateTargetsWithJob_jobId,

    -- * Destructuring the Response
    AssociateTargetsWithJobResponse (..),
    newAssociateTargetsWithJobResponse,

    -- * Response Lenses
    associateTargetsWithJobResponse_jobId,
    associateTargetsWithJobResponse_description,
    associateTargetsWithJobResponse_jobArn,
    associateTargetsWithJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateTargetsWithJob' smart constructor.
data AssociateTargetsWithJob = AssociateTargetsWithJob'
  { -- | An optional comment string describing why the job was associated with
    -- the targets.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, Amazon Web Services IoT
    -- Core sends jobs notifications to MQTT topics that contain the value in
    -- the following format.
    --
    -- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
    --
    -- The @namespaceId@ feature is in public preview.
    namespaceId :: Prelude.Maybe Prelude.Text,
    -- | A list of thing group ARNs that define the targets of the job.
    targets :: Prelude.NonEmpty Prelude.Text,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTargetsWithJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'associateTargetsWithJob_comment' - An optional comment string describing why the job was associated with
-- the targets.
--
-- 'namespaceId', 'associateTargetsWithJob_namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, Amazon Web Services IoT
-- Core sends jobs notifications to MQTT topics that contain the value in
-- the following format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
--
-- 'targets', 'associateTargetsWithJob_targets' - A list of thing group ARNs that define the targets of the job.
--
-- 'jobId', 'associateTargetsWithJob_jobId' - The unique identifier you assigned to this job when it was created.
newAssociateTargetsWithJob ::
  -- | 'targets'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  AssociateTargetsWithJob
newAssociateTargetsWithJob pTargets_ pJobId_ =
  AssociateTargetsWithJob'
    { comment = Prelude.Nothing,
      namespaceId = Prelude.Nothing,
      targets = Lens.coerced Lens.# pTargets_,
      jobId = pJobId_
    }

-- | An optional comment string describing why the job was associated with
-- the targets.
associateTargetsWithJob_comment :: Lens.Lens' AssociateTargetsWithJob (Prelude.Maybe Prelude.Text)
associateTargetsWithJob_comment = Lens.lens (\AssociateTargetsWithJob' {comment} -> comment) (\s@AssociateTargetsWithJob' {} a -> s {comment = a} :: AssociateTargetsWithJob)

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, Amazon Web Services IoT
-- Core sends jobs notifications to MQTT topics that contain the value in
-- the following format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
associateTargetsWithJob_namespaceId :: Lens.Lens' AssociateTargetsWithJob (Prelude.Maybe Prelude.Text)
associateTargetsWithJob_namespaceId = Lens.lens (\AssociateTargetsWithJob' {namespaceId} -> namespaceId) (\s@AssociateTargetsWithJob' {} a -> s {namespaceId = a} :: AssociateTargetsWithJob)

-- | A list of thing group ARNs that define the targets of the job.
associateTargetsWithJob_targets :: Lens.Lens' AssociateTargetsWithJob (Prelude.NonEmpty Prelude.Text)
associateTargetsWithJob_targets = Lens.lens (\AssociateTargetsWithJob' {targets} -> targets) (\s@AssociateTargetsWithJob' {} a -> s {targets = a} :: AssociateTargetsWithJob) Prelude.. Lens.coerced

-- | The unique identifier you assigned to this job when it was created.
associateTargetsWithJob_jobId :: Lens.Lens' AssociateTargetsWithJob Prelude.Text
associateTargetsWithJob_jobId = Lens.lens (\AssociateTargetsWithJob' {jobId} -> jobId) (\s@AssociateTargetsWithJob' {} a -> s {jobId = a} :: AssociateTargetsWithJob)

instance Core.AWSRequest AssociateTargetsWithJob where
  type
    AWSResponse AssociateTargetsWithJob =
      AssociateTargetsWithJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateTargetsWithJobResponse'
            Prelude.<$> (x Core..?> "jobId")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "jobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateTargetsWithJob where
  hashWithSalt _salt AssociateTargetsWithJob' {..} =
    _salt `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` namespaceId
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData AssociateTargetsWithJob where
  rnf AssociateTargetsWithJob' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf namespaceId
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf jobId

instance Core.ToHeaders AssociateTargetsWithJob where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON AssociateTargetsWithJob where
  toJSON AssociateTargetsWithJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("comment" Core..=) Prelude.<$> comment,
            Prelude.Just ("targets" Core..= targets)
          ]
      )

instance Core.ToPath AssociateTargetsWithJob where
  toPath AssociateTargetsWithJob' {..} =
    Prelude.mconcat
      ["/jobs/", Core.toBS jobId, "/targets"]

instance Core.ToQuery AssociateTargetsWithJob where
  toQuery AssociateTargetsWithJob' {..} =
    Prelude.mconcat ["namespaceId" Core.=: namespaceId]

-- | /See:/ 'newAssociateTargetsWithJobResponse' smart constructor.
data AssociateTargetsWithJobResponse = AssociateTargetsWithJobResponse'
  { -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | A short text description of the job.
    description :: Prelude.Maybe Prelude.Text,
    -- | An ARN identifying the job.
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTargetsWithJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'associateTargetsWithJobResponse_jobId' - The unique identifier you assigned to this job when it was created.
--
-- 'description', 'associateTargetsWithJobResponse_description' - A short text description of the job.
--
-- 'jobArn', 'associateTargetsWithJobResponse_jobArn' - An ARN identifying the job.
--
-- 'httpStatus', 'associateTargetsWithJobResponse_httpStatus' - The response's http status code.
newAssociateTargetsWithJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateTargetsWithJobResponse
newAssociateTargetsWithJobResponse pHttpStatus_ =
  AssociateTargetsWithJobResponse'
    { jobId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier you assigned to this job when it was created.
associateTargetsWithJobResponse_jobId :: Lens.Lens' AssociateTargetsWithJobResponse (Prelude.Maybe Prelude.Text)
associateTargetsWithJobResponse_jobId = Lens.lens (\AssociateTargetsWithJobResponse' {jobId} -> jobId) (\s@AssociateTargetsWithJobResponse' {} a -> s {jobId = a} :: AssociateTargetsWithJobResponse)

-- | A short text description of the job.
associateTargetsWithJobResponse_description :: Lens.Lens' AssociateTargetsWithJobResponse (Prelude.Maybe Prelude.Text)
associateTargetsWithJobResponse_description = Lens.lens (\AssociateTargetsWithJobResponse' {description} -> description) (\s@AssociateTargetsWithJobResponse' {} a -> s {description = a} :: AssociateTargetsWithJobResponse)

-- | An ARN identifying the job.
associateTargetsWithJobResponse_jobArn :: Lens.Lens' AssociateTargetsWithJobResponse (Prelude.Maybe Prelude.Text)
associateTargetsWithJobResponse_jobArn = Lens.lens (\AssociateTargetsWithJobResponse' {jobArn} -> jobArn) (\s@AssociateTargetsWithJobResponse' {} a -> s {jobArn = a} :: AssociateTargetsWithJobResponse)

-- | The response's http status code.
associateTargetsWithJobResponse_httpStatus :: Lens.Lens' AssociateTargetsWithJobResponse Prelude.Int
associateTargetsWithJobResponse_httpStatus = Lens.lens (\AssociateTargetsWithJobResponse' {httpStatus} -> httpStatus) (\s@AssociateTargetsWithJobResponse' {} a -> s {httpStatus = a} :: AssociateTargetsWithJobResponse)

instance
  Prelude.NFData
    AssociateTargetsWithJobResponse
  where
  rnf AssociateTargetsWithJobResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf httpStatus
