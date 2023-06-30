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
-- Module      : Amazonka.EMRContainers.StartJobRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a job run. A job run is a unit of work, such as a Spark jar,
-- PySpark script, or SparkSQL query, that you submit to Amazon EMR on EKS.
module Amazonka.EMRContainers.StartJobRun
  ( -- * Creating a Request
    StartJobRun (..),
    newStartJobRun,

    -- * Request Lenses
    startJobRun_configurationOverrides,
    startJobRun_executionRoleArn,
    startJobRun_jobDriver,
    startJobRun_jobTemplateId,
    startJobRun_jobTemplateParameters,
    startJobRun_name,
    startJobRun_releaseLabel,
    startJobRun_tags,
    startJobRun_virtualClusterId,
    startJobRun_clientToken,

    -- * Destructuring the Response
    StartJobRunResponse (..),
    newStartJobRunResponse,

    -- * Response Lenses
    startJobRunResponse_arn,
    startJobRunResponse_id,
    startJobRunResponse_name,
    startJobRunResponse_virtualClusterId,
    startJobRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartJobRun' smart constructor.
data StartJobRun = StartJobRun'
  { -- | The configuration overrides for the job run.
    configurationOverrides :: Prelude.Maybe ConfigurationOverrides,
    -- | The execution role ARN for the job run.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The job driver for the job run.
    jobDriver :: Prelude.Maybe JobDriver,
    -- | The job template ID to be used to start the job run.
    jobTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The values of job template parameters to start a job run.
    jobTemplateParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the job run.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EMR release version to use for the job run.
    releaseLabel :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to job runs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The virtual cluster ID for which the job run request is submitted.
    virtualClusterId :: Prelude.Text,
    -- | The client idempotency token of the job run request.
    clientToken :: Prelude.Text
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
-- 'configurationOverrides', 'startJobRun_configurationOverrides' - The configuration overrides for the job run.
--
-- 'executionRoleArn', 'startJobRun_executionRoleArn' - The execution role ARN for the job run.
--
-- 'jobDriver', 'startJobRun_jobDriver' - The job driver for the job run.
--
-- 'jobTemplateId', 'startJobRun_jobTemplateId' - The job template ID to be used to start the job run.
--
-- 'jobTemplateParameters', 'startJobRun_jobTemplateParameters' - The values of job template parameters to start a job run.
--
-- 'name', 'startJobRun_name' - The name of the job run.
--
-- 'releaseLabel', 'startJobRun_releaseLabel' - The Amazon EMR release version to use for the job run.
--
-- 'tags', 'startJobRun_tags' - The tags assigned to job runs.
--
-- 'virtualClusterId', 'startJobRun_virtualClusterId' - The virtual cluster ID for which the job run request is submitted.
--
-- 'clientToken', 'startJobRun_clientToken' - The client idempotency token of the job run request.
newStartJobRun ::
  -- | 'virtualClusterId'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  StartJobRun
newStartJobRun pVirtualClusterId_ pClientToken_ =
  StartJobRun'
    { configurationOverrides =
        Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      jobDriver = Prelude.Nothing,
      jobTemplateId = Prelude.Nothing,
      jobTemplateParameters = Prelude.Nothing,
      name = Prelude.Nothing,
      releaseLabel = Prelude.Nothing,
      tags = Prelude.Nothing,
      virtualClusterId = pVirtualClusterId_,
      clientToken = pClientToken_
    }

-- | The configuration overrides for the job run.
startJobRun_configurationOverrides :: Lens.Lens' StartJobRun (Prelude.Maybe ConfigurationOverrides)
startJobRun_configurationOverrides = Lens.lens (\StartJobRun' {configurationOverrides} -> configurationOverrides) (\s@StartJobRun' {} a -> s {configurationOverrides = a} :: StartJobRun)

-- | The execution role ARN for the job run.
startJobRun_executionRoleArn :: Lens.Lens' StartJobRun (Prelude.Maybe Prelude.Text)
startJobRun_executionRoleArn = Lens.lens (\StartJobRun' {executionRoleArn} -> executionRoleArn) (\s@StartJobRun' {} a -> s {executionRoleArn = a} :: StartJobRun)

-- | The job driver for the job run.
startJobRun_jobDriver :: Lens.Lens' StartJobRun (Prelude.Maybe JobDriver)
startJobRun_jobDriver = Lens.lens (\StartJobRun' {jobDriver} -> jobDriver) (\s@StartJobRun' {} a -> s {jobDriver = a} :: StartJobRun)

-- | The job template ID to be used to start the job run.
startJobRun_jobTemplateId :: Lens.Lens' StartJobRun (Prelude.Maybe Prelude.Text)
startJobRun_jobTemplateId = Lens.lens (\StartJobRun' {jobTemplateId} -> jobTemplateId) (\s@StartJobRun' {} a -> s {jobTemplateId = a} :: StartJobRun)

-- | The values of job template parameters to start a job run.
startJobRun_jobTemplateParameters :: Lens.Lens' StartJobRun (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startJobRun_jobTemplateParameters = Lens.lens (\StartJobRun' {jobTemplateParameters} -> jobTemplateParameters) (\s@StartJobRun' {} a -> s {jobTemplateParameters = a} :: StartJobRun) Prelude.. Lens.mapping Lens.coerced

-- | The name of the job run.
startJobRun_name :: Lens.Lens' StartJobRun (Prelude.Maybe Prelude.Text)
startJobRun_name = Lens.lens (\StartJobRun' {name} -> name) (\s@StartJobRun' {} a -> s {name = a} :: StartJobRun)

-- | The Amazon EMR release version to use for the job run.
startJobRun_releaseLabel :: Lens.Lens' StartJobRun (Prelude.Maybe Prelude.Text)
startJobRun_releaseLabel = Lens.lens (\StartJobRun' {releaseLabel} -> releaseLabel) (\s@StartJobRun' {} a -> s {releaseLabel = a} :: StartJobRun)

-- | The tags assigned to job runs.
startJobRun_tags :: Lens.Lens' StartJobRun (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startJobRun_tags = Lens.lens (\StartJobRun' {tags} -> tags) (\s@StartJobRun' {} a -> s {tags = a} :: StartJobRun) Prelude.. Lens.mapping Lens.coerced

-- | The virtual cluster ID for which the job run request is submitted.
startJobRun_virtualClusterId :: Lens.Lens' StartJobRun Prelude.Text
startJobRun_virtualClusterId = Lens.lens (\StartJobRun' {virtualClusterId} -> virtualClusterId) (\s@StartJobRun' {} a -> s {virtualClusterId = a} :: StartJobRun)

-- | The client idempotency token of the job run request.
startJobRun_clientToken :: Lens.Lens' StartJobRun Prelude.Text
startJobRun_clientToken = Lens.lens (\StartJobRun' {clientToken} -> clientToken) (\s@StartJobRun' {} a -> s {clientToken = a} :: StartJobRun)

instance Core.AWSRequest StartJobRun where
  type AWSResponse StartJobRun = StartJobRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartJobRunResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "virtualClusterId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartJobRun where
  hashWithSalt _salt StartJobRun' {..} =
    _salt
      `Prelude.hashWithSalt` configurationOverrides
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` jobDriver
      `Prelude.hashWithSalt` jobTemplateId
      `Prelude.hashWithSalt` jobTemplateParameters
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` releaseLabel
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` virtualClusterId
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData StartJobRun where
  rnf StartJobRun' {..} =
    Prelude.rnf configurationOverrides
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf jobDriver
      `Prelude.seq` Prelude.rnf jobTemplateId
      `Prelude.seq` Prelude.rnf jobTemplateParameters
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf releaseLabel
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf virtualClusterId
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders StartJobRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartJobRun where
  toJSON StartJobRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("configurationOverrides" Data..=)
              Prelude.<$> configurationOverrides,
            ("executionRoleArn" Data..=)
              Prelude.<$> executionRoleArn,
            ("jobDriver" Data..=) Prelude.<$> jobDriver,
            ("jobTemplateId" Data..=) Prelude.<$> jobTemplateId,
            ("jobTemplateParameters" Data..=)
              Prelude.<$> jobTemplateParameters,
            ("name" Data..=) Prelude.<$> name,
            ("releaseLabel" Data..=) Prelude.<$> releaseLabel,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath StartJobRun where
  toPath StartJobRun' {..} =
    Prelude.mconcat
      [ "/virtualclusters/",
        Data.toBS virtualClusterId,
        "/jobruns"
      ]

instance Data.ToQuery StartJobRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartJobRunResponse' smart constructor.
data StartJobRunResponse = StartJobRunResponse'
  { -- | This output lists the ARN of job run.
    arn :: Prelude.Maybe Prelude.Text,
    -- | This output displays the started job run ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | This output displays the name of the started job run.
    name :: Prelude.Maybe Prelude.Text,
    -- | This output displays the virtual cluster ID for which the job run was
    -- submitted.
    virtualClusterId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'arn', 'startJobRunResponse_arn' - This output lists the ARN of job run.
--
-- 'id', 'startJobRunResponse_id' - This output displays the started job run ID.
--
-- 'name', 'startJobRunResponse_name' - This output displays the name of the started job run.
--
-- 'virtualClusterId', 'startJobRunResponse_virtualClusterId' - This output displays the virtual cluster ID for which the job run was
-- submitted.
--
-- 'httpStatus', 'startJobRunResponse_httpStatus' - The response's http status code.
newStartJobRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartJobRunResponse
newStartJobRunResponse pHttpStatus_ =
  StartJobRunResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      virtualClusterId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This output lists the ARN of job run.
startJobRunResponse_arn :: Lens.Lens' StartJobRunResponse (Prelude.Maybe Prelude.Text)
startJobRunResponse_arn = Lens.lens (\StartJobRunResponse' {arn} -> arn) (\s@StartJobRunResponse' {} a -> s {arn = a} :: StartJobRunResponse)

-- | This output displays the started job run ID.
startJobRunResponse_id :: Lens.Lens' StartJobRunResponse (Prelude.Maybe Prelude.Text)
startJobRunResponse_id = Lens.lens (\StartJobRunResponse' {id} -> id) (\s@StartJobRunResponse' {} a -> s {id = a} :: StartJobRunResponse)

-- | This output displays the name of the started job run.
startJobRunResponse_name :: Lens.Lens' StartJobRunResponse (Prelude.Maybe Prelude.Text)
startJobRunResponse_name = Lens.lens (\StartJobRunResponse' {name} -> name) (\s@StartJobRunResponse' {} a -> s {name = a} :: StartJobRunResponse)

-- | This output displays the virtual cluster ID for which the job run was
-- submitted.
startJobRunResponse_virtualClusterId :: Lens.Lens' StartJobRunResponse (Prelude.Maybe Prelude.Text)
startJobRunResponse_virtualClusterId = Lens.lens (\StartJobRunResponse' {virtualClusterId} -> virtualClusterId) (\s@StartJobRunResponse' {} a -> s {virtualClusterId = a} :: StartJobRunResponse)

-- | The response's http status code.
startJobRunResponse_httpStatus :: Lens.Lens' StartJobRunResponse Prelude.Int
startJobRunResponse_httpStatus = Lens.lens (\StartJobRunResponse' {httpStatus} -> httpStatus) (\s@StartJobRunResponse' {} a -> s {httpStatus = a} :: StartJobRunResponse)

instance Prelude.NFData StartJobRunResponse where
  rnf StartJobRunResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf virtualClusterId
      `Prelude.seq` Prelude.rnf httpStatus
