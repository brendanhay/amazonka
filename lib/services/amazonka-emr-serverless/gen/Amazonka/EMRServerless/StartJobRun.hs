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
-- Module      : Amazonka.EMRServerless.StartJobRun
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a job run.
module Amazonka.EMRServerless.StartJobRun
  ( -- * Creating a Request
    StartJobRun (..),
    newStartJobRun,

    -- * Request Lenses
    startJobRun_tags,
    startJobRun_name,
    startJobRun_jobDriver,
    startJobRun_configurationOverrides,
    startJobRun_executionTimeoutMinutes,
    startJobRun_applicationId,
    startJobRun_clientToken,
    startJobRun_executionRoleArn,

    -- * Destructuring the Response
    StartJobRunResponse (..),
    newStartJobRunResponse,

    -- * Response Lenses
    startJobRunResponse_httpStatus,
    startJobRunResponse_applicationId,
    startJobRunResponse_jobRunId,
    startJobRunResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMRServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartJobRun' smart constructor.
data StartJobRun = StartJobRun'
  { -- | The tags assigned to the job run.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The optional job run name. This doesn\'t have to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | The job driver for the job run.
    jobDriver :: Prelude.Maybe JobDriver,
    -- | The configuration overrides for the job run.
    configurationOverrides :: Prelude.Maybe ConfigurationOverrides,
    -- | The maximum duration for the job run to run. If the job run runs beyond
    -- this duration, it will be automatically cancelled.
    executionTimeoutMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the application on which to run the job.
    applicationId :: Prelude.Text,
    -- | The client idempotency token of the job run to start. Its value must be
    -- unique for each request.
    clientToken :: Prelude.Text,
    -- | The execution role ARN for the job run.
    executionRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartJobRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'startJobRun_tags' - The tags assigned to the job run.
--
-- 'name', 'startJobRun_name' - The optional job run name. This doesn\'t have to be unique.
--
-- 'jobDriver', 'startJobRun_jobDriver' - The job driver for the job run.
--
-- 'configurationOverrides', 'startJobRun_configurationOverrides' - The configuration overrides for the job run.
--
-- 'executionTimeoutMinutes', 'startJobRun_executionTimeoutMinutes' - The maximum duration for the job run to run. If the job run runs beyond
-- this duration, it will be automatically cancelled.
--
-- 'applicationId', 'startJobRun_applicationId' - The ID of the application on which to run the job.
--
-- 'clientToken', 'startJobRun_clientToken' - The client idempotency token of the job run to start. Its value must be
-- unique for each request.
--
-- 'executionRoleArn', 'startJobRun_executionRoleArn' - The execution role ARN for the job run.
newStartJobRun ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'executionRoleArn'
  Prelude.Text ->
  StartJobRun
newStartJobRun
  pApplicationId_
  pClientToken_
  pExecutionRoleArn_ =
    StartJobRun'
      { tags = Prelude.Nothing,
        name = Prelude.Nothing,
        jobDriver = Prelude.Nothing,
        configurationOverrides = Prelude.Nothing,
        executionTimeoutMinutes = Prelude.Nothing,
        applicationId = pApplicationId_,
        clientToken = pClientToken_,
        executionRoleArn = pExecutionRoleArn_
      }

-- | The tags assigned to the job run.
startJobRun_tags :: Lens.Lens' StartJobRun (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startJobRun_tags = Lens.lens (\StartJobRun' {tags} -> tags) (\s@StartJobRun' {} a -> s {tags = a} :: StartJobRun) Prelude.. Lens.mapping Lens.coerced

-- | The optional job run name. This doesn\'t have to be unique.
startJobRun_name :: Lens.Lens' StartJobRun (Prelude.Maybe Prelude.Text)
startJobRun_name = Lens.lens (\StartJobRun' {name} -> name) (\s@StartJobRun' {} a -> s {name = a} :: StartJobRun)

-- | The job driver for the job run.
startJobRun_jobDriver :: Lens.Lens' StartJobRun (Prelude.Maybe JobDriver)
startJobRun_jobDriver = Lens.lens (\StartJobRun' {jobDriver} -> jobDriver) (\s@StartJobRun' {} a -> s {jobDriver = a} :: StartJobRun)

-- | The configuration overrides for the job run.
startJobRun_configurationOverrides :: Lens.Lens' StartJobRun (Prelude.Maybe ConfigurationOverrides)
startJobRun_configurationOverrides = Lens.lens (\StartJobRun' {configurationOverrides} -> configurationOverrides) (\s@StartJobRun' {} a -> s {configurationOverrides = a} :: StartJobRun)

-- | The maximum duration for the job run to run. If the job run runs beyond
-- this duration, it will be automatically cancelled.
startJobRun_executionTimeoutMinutes :: Lens.Lens' StartJobRun (Prelude.Maybe Prelude.Natural)
startJobRun_executionTimeoutMinutes = Lens.lens (\StartJobRun' {executionTimeoutMinutes} -> executionTimeoutMinutes) (\s@StartJobRun' {} a -> s {executionTimeoutMinutes = a} :: StartJobRun)

-- | The ID of the application on which to run the job.
startJobRun_applicationId :: Lens.Lens' StartJobRun Prelude.Text
startJobRun_applicationId = Lens.lens (\StartJobRun' {applicationId} -> applicationId) (\s@StartJobRun' {} a -> s {applicationId = a} :: StartJobRun)

-- | The client idempotency token of the job run to start. Its value must be
-- unique for each request.
startJobRun_clientToken :: Lens.Lens' StartJobRun Prelude.Text
startJobRun_clientToken = Lens.lens (\StartJobRun' {clientToken} -> clientToken) (\s@StartJobRun' {} a -> s {clientToken = a} :: StartJobRun)

-- | The execution role ARN for the job run.
startJobRun_executionRoleArn :: Lens.Lens' StartJobRun Prelude.Text
startJobRun_executionRoleArn = Lens.lens (\StartJobRun' {executionRoleArn} -> executionRoleArn) (\s@StartJobRun' {} a -> s {executionRoleArn = a} :: StartJobRun)

instance Core.AWSRequest StartJobRun where
  type AWSResponse StartJobRun = StartJobRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartJobRunResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "applicationId")
            Prelude.<*> (x Core..:> "jobRunId")
            Prelude.<*> (x Core..:> "arn")
      )

instance Prelude.Hashable StartJobRun where
  hashWithSalt _salt StartJobRun' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` jobDriver
      `Prelude.hashWithSalt` configurationOverrides
      `Prelude.hashWithSalt` executionTimeoutMinutes
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` executionRoleArn

instance Prelude.NFData StartJobRun where
  rnf StartJobRun' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf jobDriver
      `Prelude.seq` Prelude.rnf configurationOverrides
      `Prelude.seq` Prelude.rnf executionTimeoutMinutes
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf executionRoleArn

instance Core.ToHeaders StartJobRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartJobRun where
  toJSON StartJobRun' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("name" Core..=) Prelude.<$> name,
            ("jobDriver" Core..=) Prelude.<$> jobDriver,
            ("configurationOverrides" Core..=)
              Prelude.<$> configurationOverrides,
            ("executionTimeoutMinutes" Core..=)
              Prelude.<$> executionTimeoutMinutes,
            Prelude.Just ("clientToken" Core..= clientToken),
            Prelude.Just
              ("executionRoleArn" Core..= executionRoleArn)
          ]
      )

instance Core.ToPath StartJobRun where
  toPath StartJobRun' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/jobruns"
      ]

instance Core.ToQuery StartJobRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartJobRunResponse' smart constructor.
data StartJobRunResponse = StartJobRunResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | This output displays the application ID on which the job run was
    -- submitted.
    applicationId :: Prelude.Text,
    -- | The output contains the ID of the started job run.
    jobRunId :: Prelude.Text,
    -- | The output lists the execution role ARN of the job run.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartJobRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startJobRunResponse_httpStatus' - The response's http status code.
--
-- 'applicationId', 'startJobRunResponse_applicationId' - This output displays the application ID on which the job run was
-- submitted.
--
-- 'jobRunId', 'startJobRunResponse_jobRunId' - The output contains the ID of the started job run.
--
-- 'arn', 'startJobRunResponse_arn' - The output lists the execution role ARN of the job run.
newStartJobRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'jobRunId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  StartJobRunResponse
newStartJobRunResponse
  pHttpStatus_
  pApplicationId_
  pJobRunId_
  pArn_ =
    StartJobRunResponse'
      { httpStatus = pHttpStatus_,
        applicationId = pApplicationId_,
        jobRunId = pJobRunId_,
        arn = pArn_
      }

-- | The response's http status code.
startJobRunResponse_httpStatus :: Lens.Lens' StartJobRunResponse Prelude.Int
startJobRunResponse_httpStatus = Lens.lens (\StartJobRunResponse' {httpStatus} -> httpStatus) (\s@StartJobRunResponse' {} a -> s {httpStatus = a} :: StartJobRunResponse)

-- | This output displays the application ID on which the job run was
-- submitted.
startJobRunResponse_applicationId :: Lens.Lens' StartJobRunResponse Prelude.Text
startJobRunResponse_applicationId = Lens.lens (\StartJobRunResponse' {applicationId} -> applicationId) (\s@StartJobRunResponse' {} a -> s {applicationId = a} :: StartJobRunResponse)

-- | The output contains the ID of the started job run.
startJobRunResponse_jobRunId :: Lens.Lens' StartJobRunResponse Prelude.Text
startJobRunResponse_jobRunId = Lens.lens (\StartJobRunResponse' {jobRunId} -> jobRunId) (\s@StartJobRunResponse' {} a -> s {jobRunId = a} :: StartJobRunResponse)

-- | The output lists the execution role ARN of the job run.
startJobRunResponse_arn :: Lens.Lens' StartJobRunResponse Prelude.Text
startJobRunResponse_arn = Lens.lens (\StartJobRunResponse' {arn} -> arn) (\s@StartJobRunResponse' {} a -> s {arn = a} :: StartJobRunResponse)

instance Prelude.NFData StartJobRunResponse where
  rnf StartJobRunResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf jobRunId
      `Prelude.seq` Prelude.rnf arn
