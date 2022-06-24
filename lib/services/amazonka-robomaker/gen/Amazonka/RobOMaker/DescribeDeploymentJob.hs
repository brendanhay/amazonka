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
-- Module      : Amazonka.RobOMaker.DescribeDeploymentJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a deployment job.
module Amazonka.RobOMaker.DescribeDeploymentJob
  ( -- * Creating a Request
    DescribeDeploymentJob (..),
    newDescribeDeploymentJob,

    -- * Request Lenses
    describeDeploymentJob_job,

    -- * Destructuring the Response
    DescribeDeploymentJobResponse (..),
    newDescribeDeploymentJobResponse,

    -- * Response Lenses
    describeDeploymentJobResponse_tags,
    describeDeploymentJobResponse_deploymentApplicationConfigs,
    describeDeploymentJobResponse_failureCode,
    describeDeploymentJobResponse_fleet,
    describeDeploymentJobResponse_arn,
    describeDeploymentJobResponse_status,
    describeDeploymentJobResponse_robotDeploymentSummary,
    describeDeploymentJobResponse_deploymentConfig,
    describeDeploymentJobResponse_createdAt,
    describeDeploymentJobResponse_failureReason,
    describeDeploymentJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newDescribeDeploymentJob' smart constructor.
data DescribeDeploymentJob = DescribeDeploymentJob'
  { -- | The Amazon Resource Name (ARN) of the deployment job.
    job :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDeploymentJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'describeDeploymentJob_job' - The Amazon Resource Name (ARN) of the deployment job.
newDescribeDeploymentJob ::
  -- | 'job'
  Prelude.Text ->
  DescribeDeploymentJob
newDescribeDeploymentJob pJob_ =
  DescribeDeploymentJob' {job = pJob_}

-- | The Amazon Resource Name (ARN) of the deployment job.
describeDeploymentJob_job :: Lens.Lens' DescribeDeploymentJob Prelude.Text
describeDeploymentJob_job = Lens.lens (\DescribeDeploymentJob' {job} -> job) (\s@DescribeDeploymentJob' {} a -> s {job = a} :: DescribeDeploymentJob)

instance Core.AWSRequest DescribeDeploymentJob where
  type
    AWSResponse DescribeDeploymentJob =
      DescribeDeploymentJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeploymentJobResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "deploymentApplicationConfigs")
            Prelude.<*> (x Core..?> "failureCode")
            Prelude.<*> (x Core..?> "fleet")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> ( x Core..?> "robotDeploymentSummary"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "deploymentConfig")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (x Core..?> "failureReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDeploymentJob where
  hashWithSalt _salt DescribeDeploymentJob' {..} =
    _salt `Prelude.hashWithSalt` job

instance Prelude.NFData DescribeDeploymentJob where
  rnf DescribeDeploymentJob' {..} = Prelude.rnf job

instance Core.ToHeaders DescribeDeploymentJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDeploymentJob where
  toJSON DescribeDeploymentJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("job" Core..= job)]
      )

instance Core.ToPath DescribeDeploymentJob where
  toPath = Prelude.const "/describeDeploymentJob"

instance Core.ToQuery DescribeDeploymentJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDeploymentJobResponse' smart constructor.
data DescribeDeploymentJobResponse = DescribeDeploymentJobResponse'
  { -- | The list of all tags added to the specified deployment job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The deployment application configuration.
    deploymentApplicationConfigs :: Prelude.Maybe (Prelude.NonEmpty DeploymentApplicationConfig),
    -- | The deployment job failure code.
    failureCode :: Prelude.Maybe DeploymentJobErrorCode,
    -- | The Amazon Resource Name (ARN) of the fleet.
    fleet :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the deployment job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the deployment job.
    status :: Prelude.Maybe DeploymentStatus,
    -- | A list of robot deployment summaries.
    robotDeploymentSummary :: Prelude.Maybe [RobotDeployment],
    -- | The deployment configuration.
    deploymentConfig :: Prelude.Maybe DeploymentConfig,
    -- | The time, in milliseconds since the epoch, when the deployment job was
    -- created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | A short description of the reason why the deployment job failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDeploymentJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeDeploymentJobResponse_tags' - The list of all tags added to the specified deployment job.
--
-- 'deploymentApplicationConfigs', 'describeDeploymentJobResponse_deploymentApplicationConfigs' - The deployment application configuration.
--
-- 'failureCode', 'describeDeploymentJobResponse_failureCode' - The deployment job failure code.
--
-- 'fleet', 'describeDeploymentJobResponse_fleet' - The Amazon Resource Name (ARN) of the fleet.
--
-- 'arn', 'describeDeploymentJobResponse_arn' - The Amazon Resource Name (ARN) of the deployment job.
--
-- 'status', 'describeDeploymentJobResponse_status' - The status of the deployment job.
--
-- 'robotDeploymentSummary', 'describeDeploymentJobResponse_robotDeploymentSummary' - A list of robot deployment summaries.
--
-- 'deploymentConfig', 'describeDeploymentJobResponse_deploymentConfig' - The deployment configuration.
--
-- 'createdAt', 'describeDeploymentJobResponse_createdAt' - The time, in milliseconds since the epoch, when the deployment job was
-- created.
--
-- 'failureReason', 'describeDeploymentJobResponse_failureReason' - A short description of the reason why the deployment job failed.
--
-- 'httpStatus', 'describeDeploymentJobResponse_httpStatus' - The response's http status code.
newDescribeDeploymentJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDeploymentJobResponse
newDescribeDeploymentJobResponse pHttpStatus_ =
  DescribeDeploymentJobResponse'
    { tags =
        Prelude.Nothing,
      deploymentApplicationConfigs =
        Prelude.Nothing,
      failureCode = Prelude.Nothing,
      fleet = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      robotDeploymentSummary = Prelude.Nothing,
      deploymentConfig = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of all tags added to the specified deployment job.
describeDeploymentJobResponse_tags :: Lens.Lens' DescribeDeploymentJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeDeploymentJobResponse_tags = Lens.lens (\DescribeDeploymentJobResponse' {tags} -> tags) (\s@DescribeDeploymentJobResponse' {} a -> s {tags = a} :: DescribeDeploymentJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The deployment application configuration.
describeDeploymentJobResponse_deploymentApplicationConfigs :: Lens.Lens' DescribeDeploymentJobResponse (Prelude.Maybe (Prelude.NonEmpty DeploymentApplicationConfig))
describeDeploymentJobResponse_deploymentApplicationConfigs = Lens.lens (\DescribeDeploymentJobResponse' {deploymentApplicationConfigs} -> deploymentApplicationConfigs) (\s@DescribeDeploymentJobResponse' {} a -> s {deploymentApplicationConfigs = a} :: DescribeDeploymentJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The deployment job failure code.
describeDeploymentJobResponse_failureCode :: Lens.Lens' DescribeDeploymentJobResponse (Prelude.Maybe DeploymentJobErrorCode)
describeDeploymentJobResponse_failureCode = Lens.lens (\DescribeDeploymentJobResponse' {failureCode} -> failureCode) (\s@DescribeDeploymentJobResponse' {} a -> s {failureCode = a} :: DescribeDeploymentJobResponse)

-- | The Amazon Resource Name (ARN) of the fleet.
describeDeploymentJobResponse_fleet :: Lens.Lens' DescribeDeploymentJobResponse (Prelude.Maybe Prelude.Text)
describeDeploymentJobResponse_fleet = Lens.lens (\DescribeDeploymentJobResponse' {fleet} -> fleet) (\s@DescribeDeploymentJobResponse' {} a -> s {fleet = a} :: DescribeDeploymentJobResponse)

-- | The Amazon Resource Name (ARN) of the deployment job.
describeDeploymentJobResponse_arn :: Lens.Lens' DescribeDeploymentJobResponse (Prelude.Maybe Prelude.Text)
describeDeploymentJobResponse_arn = Lens.lens (\DescribeDeploymentJobResponse' {arn} -> arn) (\s@DescribeDeploymentJobResponse' {} a -> s {arn = a} :: DescribeDeploymentJobResponse)

-- | The status of the deployment job.
describeDeploymentJobResponse_status :: Lens.Lens' DescribeDeploymentJobResponse (Prelude.Maybe DeploymentStatus)
describeDeploymentJobResponse_status = Lens.lens (\DescribeDeploymentJobResponse' {status} -> status) (\s@DescribeDeploymentJobResponse' {} a -> s {status = a} :: DescribeDeploymentJobResponse)

-- | A list of robot deployment summaries.
describeDeploymentJobResponse_robotDeploymentSummary :: Lens.Lens' DescribeDeploymentJobResponse (Prelude.Maybe [RobotDeployment])
describeDeploymentJobResponse_robotDeploymentSummary = Lens.lens (\DescribeDeploymentJobResponse' {robotDeploymentSummary} -> robotDeploymentSummary) (\s@DescribeDeploymentJobResponse' {} a -> s {robotDeploymentSummary = a} :: DescribeDeploymentJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The deployment configuration.
describeDeploymentJobResponse_deploymentConfig :: Lens.Lens' DescribeDeploymentJobResponse (Prelude.Maybe DeploymentConfig)
describeDeploymentJobResponse_deploymentConfig = Lens.lens (\DescribeDeploymentJobResponse' {deploymentConfig} -> deploymentConfig) (\s@DescribeDeploymentJobResponse' {} a -> s {deploymentConfig = a} :: DescribeDeploymentJobResponse)

-- | The time, in milliseconds since the epoch, when the deployment job was
-- created.
describeDeploymentJobResponse_createdAt :: Lens.Lens' DescribeDeploymentJobResponse (Prelude.Maybe Prelude.UTCTime)
describeDeploymentJobResponse_createdAt = Lens.lens (\DescribeDeploymentJobResponse' {createdAt} -> createdAt) (\s@DescribeDeploymentJobResponse' {} a -> s {createdAt = a} :: DescribeDeploymentJobResponse) Prelude.. Lens.mapping Core._Time

-- | A short description of the reason why the deployment job failed.
describeDeploymentJobResponse_failureReason :: Lens.Lens' DescribeDeploymentJobResponse (Prelude.Maybe Prelude.Text)
describeDeploymentJobResponse_failureReason = Lens.lens (\DescribeDeploymentJobResponse' {failureReason} -> failureReason) (\s@DescribeDeploymentJobResponse' {} a -> s {failureReason = a} :: DescribeDeploymentJobResponse)

-- | The response's http status code.
describeDeploymentJobResponse_httpStatus :: Lens.Lens' DescribeDeploymentJobResponse Prelude.Int
describeDeploymentJobResponse_httpStatus = Lens.lens (\DescribeDeploymentJobResponse' {httpStatus} -> httpStatus) (\s@DescribeDeploymentJobResponse' {} a -> s {httpStatus = a} :: DescribeDeploymentJobResponse)

instance Prelude.NFData DescribeDeploymentJobResponse where
  rnf DescribeDeploymentJobResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf deploymentApplicationConfigs
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf fleet
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf robotDeploymentSummary
      `Prelude.seq` Prelude.rnf deploymentConfig
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf httpStatus
