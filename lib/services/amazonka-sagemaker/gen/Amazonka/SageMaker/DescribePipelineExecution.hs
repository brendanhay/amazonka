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
-- Module      : Amazonka.SageMaker.DescribePipelineExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the details of a pipeline execution.
module Amazonka.SageMaker.DescribePipelineExecution
  ( -- * Creating a Request
    DescribePipelineExecution (..),
    newDescribePipelineExecution,

    -- * Request Lenses
    describePipelineExecution_pipelineExecutionArn,

    -- * Destructuring the Response
    DescribePipelineExecutionResponse (..),
    newDescribePipelineExecutionResponse,

    -- * Response Lenses
    describePipelineExecutionResponse_createdBy,
    describePipelineExecutionResponse_creationTime,
    describePipelineExecutionResponse_failureReason,
    describePipelineExecutionResponse_lastModifiedBy,
    describePipelineExecutionResponse_lastModifiedTime,
    describePipelineExecutionResponse_parallelismConfiguration,
    describePipelineExecutionResponse_pipelineArn,
    describePipelineExecutionResponse_pipelineExecutionArn,
    describePipelineExecutionResponse_pipelineExecutionDescription,
    describePipelineExecutionResponse_pipelineExecutionDisplayName,
    describePipelineExecutionResponse_pipelineExecutionStatus,
    describePipelineExecutionResponse_pipelineExperimentConfig,
    describePipelineExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribePipelineExecution' smart constructor.
data DescribePipelineExecution = DescribePipelineExecution'
  { -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePipelineExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionArn', 'describePipelineExecution_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
newDescribePipelineExecution ::
  -- | 'pipelineExecutionArn'
  Prelude.Text ->
  DescribePipelineExecution
newDescribePipelineExecution pPipelineExecutionArn_ =
  DescribePipelineExecution'
    { pipelineExecutionArn =
        pPipelineExecutionArn_
    }

-- | The Amazon Resource Name (ARN) of the pipeline execution.
describePipelineExecution_pipelineExecutionArn :: Lens.Lens' DescribePipelineExecution Prelude.Text
describePipelineExecution_pipelineExecutionArn = Lens.lens (\DescribePipelineExecution' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@DescribePipelineExecution' {} a -> s {pipelineExecutionArn = a} :: DescribePipelineExecution)

instance Core.AWSRequest DescribePipelineExecution where
  type
    AWSResponse DescribePipelineExecution =
      DescribePipelineExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePipelineExecutionResponse'
            Prelude.<$> (x Data..?> "CreatedBy")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "LastModifiedBy")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "ParallelismConfiguration")
            Prelude.<*> (x Data..?> "PipelineArn")
            Prelude.<*> (x Data..?> "PipelineExecutionArn")
            Prelude.<*> (x Data..?> "PipelineExecutionDescription")
            Prelude.<*> (x Data..?> "PipelineExecutionDisplayName")
            Prelude.<*> (x Data..?> "PipelineExecutionStatus")
            Prelude.<*> (x Data..?> "PipelineExperimentConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePipelineExecution where
  hashWithSalt _salt DescribePipelineExecution' {..} =
    _salt `Prelude.hashWithSalt` pipelineExecutionArn

instance Prelude.NFData DescribePipelineExecution where
  rnf DescribePipelineExecution' {..} =
    Prelude.rnf pipelineExecutionArn

instance Data.ToHeaders DescribePipelineExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribePipelineExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribePipelineExecution where
  toJSON DescribePipelineExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "PipelineExecutionArn"
                  Data..= pipelineExecutionArn
              )
          ]
      )

instance Data.ToPath DescribePipelineExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePipelineExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePipelineExecutionResponse' smart constructor.
data DescribePipelineExecutionResponse = DescribePipelineExecutionResponse'
  { createdBy :: Prelude.Maybe UserContext,
    -- | The time when the pipeline execution was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | If the execution failed, a message describing why.
    failureReason :: Prelude.Maybe Prelude.Text,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The time when the pipeline execution was modified last.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The parallelism configuration applied to the pipeline.
    parallelismConfiguration :: Prelude.Maybe ParallelismConfiguration,
    -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the pipeline execution.
    pipelineExecutionDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the pipeline execution.
    pipelineExecutionDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The status of the pipeline execution.
    pipelineExecutionStatus :: Prelude.Maybe PipelineExecutionStatus,
    pipelineExperimentConfig :: Prelude.Maybe PipelineExperimentConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePipelineExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdBy', 'describePipelineExecutionResponse_createdBy' - Undocumented member.
--
-- 'creationTime', 'describePipelineExecutionResponse_creationTime' - The time when the pipeline execution was created.
--
-- 'failureReason', 'describePipelineExecutionResponse_failureReason' - If the execution failed, a message describing why.
--
-- 'lastModifiedBy', 'describePipelineExecutionResponse_lastModifiedBy' - Undocumented member.
--
-- 'lastModifiedTime', 'describePipelineExecutionResponse_lastModifiedTime' - The time when the pipeline execution was modified last.
--
-- 'parallelismConfiguration', 'describePipelineExecutionResponse_parallelismConfiguration' - The parallelism configuration applied to the pipeline.
--
-- 'pipelineArn', 'describePipelineExecutionResponse_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
--
-- 'pipelineExecutionArn', 'describePipelineExecutionResponse_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
--
-- 'pipelineExecutionDescription', 'describePipelineExecutionResponse_pipelineExecutionDescription' - The description of the pipeline execution.
--
-- 'pipelineExecutionDisplayName', 'describePipelineExecutionResponse_pipelineExecutionDisplayName' - The display name of the pipeline execution.
--
-- 'pipelineExecutionStatus', 'describePipelineExecutionResponse_pipelineExecutionStatus' - The status of the pipeline execution.
--
-- 'pipelineExperimentConfig', 'describePipelineExecutionResponse_pipelineExperimentConfig' - Undocumented member.
--
-- 'httpStatus', 'describePipelineExecutionResponse_httpStatus' - The response's http status code.
newDescribePipelineExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePipelineExecutionResponse
newDescribePipelineExecutionResponse pHttpStatus_ =
  DescribePipelineExecutionResponse'
    { createdBy =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      parallelismConfiguration =
        Prelude.Nothing,
      pipelineArn = Prelude.Nothing,
      pipelineExecutionArn = Prelude.Nothing,
      pipelineExecutionDescription =
        Prelude.Nothing,
      pipelineExecutionDisplayName =
        Prelude.Nothing,
      pipelineExecutionStatus =
        Prelude.Nothing,
      pipelineExperimentConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describePipelineExecutionResponse_createdBy :: Lens.Lens' DescribePipelineExecutionResponse (Prelude.Maybe UserContext)
describePipelineExecutionResponse_createdBy = Lens.lens (\DescribePipelineExecutionResponse' {createdBy} -> createdBy) (\s@DescribePipelineExecutionResponse' {} a -> s {createdBy = a} :: DescribePipelineExecutionResponse)

-- | The time when the pipeline execution was created.
describePipelineExecutionResponse_creationTime :: Lens.Lens' DescribePipelineExecutionResponse (Prelude.Maybe Prelude.UTCTime)
describePipelineExecutionResponse_creationTime = Lens.lens (\DescribePipelineExecutionResponse' {creationTime} -> creationTime) (\s@DescribePipelineExecutionResponse' {} a -> s {creationTime = a} :: DescribePipelineExecutionResponse) Prelude.. Lens.mapping Data._Time

-- | If the execution failed, a message describing why.
describePipelineExecutionResponse_failureReason :: Lens.Lens' DescribePipelineExecutionResponse (Prelude.Maybe Prelude.Text)
describePipelineExecutionResponse_failureReason = Lens.lens (\DescribePipelineExecutionResponse' {failureReason} -> failureReason) (\s@DescribePipelineExecutionResponse' {} a -> s {failureReason = a} :: DescribePipelineExecutionResponse)

-- | Undocumented member.
describePipelineExecutionResponse_lastModifiedBy :: Lens.Lens' DescribePipelineExecutionResponse (Prelude.Maybe UserContext)
describePipelineExecutionResponse_lastModifiedBy = Lens.lens (\DescribePipelineExecutionResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribePipelineExecutionResponse' {} a -> s {lastModifiedBy = a} :: DescribePipelineExecutionResponse)

-- | The time when the pipeline execution was modified last.
describePipelineExecutionResponse_lastModifiedTime :: Lens.Lens' DescribePipelineExecutionResponse (Prelude.Maybe Prelude.UTCTime)
describePipelineExecutionResponse_lastModifiedTime = Lens.lens (\DescribePipelineExecutionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribePipelineExecutionResponse' {} a -> s {lastModifiedTime = a} :: DescribePipelineExecutionResponse) Prelude.. Lens.mapping Data._Time

-- | The parallelism configuration applied to the pipeline.
describePipelineExecutionResponse_parallelismConfiguration :: Lens.Lens' DescribePipelineExecutionResponse (Prelude.Maybe ParallelismConfiguration)
describePipelineExecutionResponse_parallelismConfiguration = Lens.lens (\DescribePipelineExecutionResponse' {parallelismConfiguration} -> parallelismConfiguration) (\s@DescribePipelineExecutionResponse' {} a -> s {parallelismConfiguration = a} :: DescribePipelineExecutionResponse)

-- | The Amazon Resource Name (ARN) of the pipeline.
describePipelineExecutionResponse_pipelineArn :: Lens.Lens' DescribePipelineExecutionResponse (Prelude.Maybe Prelude.Text)
describePipelineExecutionResponse_pipelineArn = Lens.lens (\DescribePipelineExecutionResponse' {pipelineArn} -> pipelineArn) (\s@DescribePipelineExecutionResponse' {} a -> s {pipelineArn = a} :: DescribePipelineExecutionResponse)

-- | The Amazon Resource Name (ARN) of the pipeline execution.
describePipelineExecutionResponse_pipelineExecutionArn :: Lens.Lens' DescribePipelineExecutionResponse (Prelude.Maybe Prelude.Text)
describePipelineExecutionResponse_pipelineExecutionArn = Lens.lens (\DescribePipelineExecutionResponse' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@DescribePipelineExecutionResponse' {} a -> s {pipelineExecutionArn = a} :: DescribePipelineExecutionResponse)

-- | The description of the pipeline execution.
describePipelineExecutionResponse_pipelineExecutionDescription :: Lens.Lens' DescribePipelineExecutionResponse (Prelude.Maybe Prelude.Text)
describePipelineExecutionResponse_pipelineExecutionDescription = Lens.lens (\DescribePipelineExecutionResponse' {pipelineExecutionDescription} -> pipelineExecutionDescription) (\s@DescribePipelineExecutionResponse' {} a -> s {pipelineExecutionDescription = a} :: DescribePipelineExecutionResponse)

-- | The display name of the pipeline execution.
describePipelineExecutionResponse_pipelineExecutionDisplayName :: Lens.Lens' DescribePipelineExecutionResponse (Prelude.Maybe Prelude.Text)
describePipelineExecutionResponse_pipelineExecutionDisplayName = Lens.lens (\DescribePipelineExecutionResponse' {pipelineExecutionDisplayName} -> pipelineExecutionDisplayName) (\s@DescribePipelineExecutionResponse' {} a -> s {pipelineExecutionDisplayName = a} :: DescribePipelineExecutionResponse)

-- | The status of the pipeline execution.
describePipelineExecutionResponse_pipelineExecutionStatus :: Lens.Lens' DescribePipelineExecutionResponse (Prelude.Maybe PipelineExecutionStatus)
describePipelineExecutionResponse_pipelineExecutionStatus = Lens.lens (\DescribePipelineExecutionResponse' {pipelineExecutionStatus} -> pipelineExecutionStatus) (\s@DescribePipelineExecutionResponse' {} a -> s {pipelineExecutionStatus = a} :: DescribePipelineExecutionResponse)

-- | Undocumented member.
describePipelineExecutionResponse_pipelineExperimentConfig :: Lens.Lens' DescribePipelineExecutionResponse (Prelude.Maybe PipelineExperimentConfig)
describePipelineExecutionResponse_pipelineExperimentConfig = Lens.lens (\DescribePipelineExecutionResponse' {pipelineExperimentConfig} -> pipelineExperimentConfig) (\s@DescribePipelineExecutionResponse' {} a -> s {pipelineExperimentConfig = a} :: DescribePipelineExecutionResponse)

-- | The response's http status code.
describePipelineExecutionResponse_httpStatus :: Lens.Lens' DescribePipelineExecutionResponse Prelude.Int
describePipelineExecutionResponse_httpStatus = Lens.lens (\DescribePipelineExecutionResponse' {httpStatus} -> httpStatus) (\s@DescribePipelineExecutionResponse' {} a -> s {httpStatus = a} :: DescribePipelineExecutionResponse)

instance
  Prelude.NFData
    DescribePipelineExecutionResponse
  where
  rnf DescribePipelineExecutionResponse' {..} =
    Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf parallelismConfiguration
      `Prelude.seq` Prelude.rnf pipelineArn
      `Prelude.seq` Prelude.rnf pipelineExecutionArn
      `Prelude.seq` Prelude.rnf pipelineExecutionDescription
      `Prelude.seq` Prelude.rnf pipelineExecutionDisplayName
      `Prelude.seq` Prelude.rnf pipelineExecutionStatus
      `Prelude.seq` Prelude.rnf pipelineExperimentConfig
      `Prelude.seq` Prelude.rnf httpStatus
