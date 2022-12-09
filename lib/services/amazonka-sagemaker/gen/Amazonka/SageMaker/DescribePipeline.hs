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
-- Module      : Amazonka.SageMaker.DescribePipeline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the details of a pipeline.
module Amazonka.SageMaker.DescribePipeline
  ( -- * Creating a Request
    DescribePipeline (..),
    newDescribePipeline,

    -- * Request Lenses
    describePipeline_pipelineName,

    -- * Destructuring the Response
    DescribePipelineResponse (..),
    newDescribePipelineResponse,

    -- * Response Lenses
    describePipelineResponse_createdBy,
    describePipelineResponse_creationTime,
    describePipelineResponse_lastModifiedBy,
    describePipelineResponse_lastModifiedTime,
    describePipelineResponse_lastRunTime,
    describePipelineResponse_parallelismConfiguration,
    describePipelineResponse_pipelineArn,
    describePipelineResponse_pipelineDefinition,
    describePipelineResponse_pipelineDescription,
    describePipelineResponse_pipelineDisplayName,
    describePipelineResponse_pipelineName,
    describePipelineResponse_pipelineStatus,
    describePipelineResponse_roleArn,
    describePipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribePipeline' smart constructor.
data DescribePipeline = DescribePipeline'
  { -- | The name of the pipeline to describe.
    pipelineName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'describePipeline_pipelineName' - The name of the pipeline to describe.
newDescribePipeline ::
  -- | 'pipelineName'
  Prelude.Text ->
  DescribePipeline
newDescribePipeline pPipelineName_ =
  DescribePipeline' {pipelineName = pPipelineName_}

-- | The name of the pipeline to describe.
describePipeline_pipelineName :: Lens.Lens' DescribePipeline Prelude.Text
describePipeline_pipelineName = Lens.lens (\DescribePipeline' {pipelineName} -> pipelineName) (\s@DescribePipeline' {} a -> s {pipelineName = a} :: DescribePipeline)

instance Core.AWSRequest DescribePipeline where
  type
    AWSResponse DescribePipeline =
      DescribePipelineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePipelineResponse'
            Prelude.<$> (x Data..?> "CreatedBy")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "LastModifiedBy")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "LastRunTime")
            Prelude.<*> (x Data..?> "ParallelismConfiguration")
            Prelude.<*> (x Data..?> "PipelineArn")
            Prelude.<*> (x Data..?> "PipelineDefinition")
            Prelude.<*> (x Data..?> "PipelineDescription")
            Prelude.<*> (x Data..?> "PipelineDisplayName")
            Prelude.<*> (x Data..?> "PipelineName")
            Prelude.<*> (x Data..?> "PipelineStatus")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePipeline where
  hashWithSalt _salt DescribePipeline' {..} =
    _salt `Prelude.hashWithSalt` pipelineName

instance Prelude.NFData DescribePipeline where
  rnf DescribePipeline' {..} = Prelude.rnf pipelineName

instance Data.ToHeaders DescribePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DescribePipeline" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribePipeline where
  toJSON DescribePipeline' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("PipelineName" Data..= pipelineName)]
      )

instance Data.ToPath DescribePipeline where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePipelineResponse' smart constructor.
data DescribePipelineResponse = DescribePipelineResponse'
  { createdBy :: Prelude.Maybe UserContext,
    -- | The time when the pipeline was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The time when the pipeline was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The time when the pipeline was last run.
    lastRunTime :: Prelude.Maybe Data.POSIX,
    -- | Lists the parallelism configuration applied to the pipeline.
    parallelismConfiguration :: Prelude.Maybe ParallelismConfiguration,
    -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The JSON pipeline definition.
    pipelineDefinition :: Prelude.Maybe Prelude.Text,
    -- | The description of the pipeline.
    pipelineDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the pipeline.
    pipelineDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the pipeline.
    pipelineName :: Prelude.Maybe Prelude.Text,
    -- | The status of the pipeline execution.
    pipelineStatus :: Prelude.Maybe PipelineStatus,
    -- | The Amazon Resource Name (ARN) that the pipeline uses to execute.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdBy', 'describePipelineResponse_createdBy' - Undocumented member.
--
-- 'creationTime', 'describePipelineResponse_creationTime' - The time when the pipeline was created.
--
-- 'lastModifiedBy', 'describePipelineResponse_lastModifiedBy' - Undocumented member.
--
-- 'lastModifiedTime', 'describePipelineResponse_lastModifiedTime' - The time when the pipeline was last modified.
--
-- 'lastRunTime', 'describePipelineResponse_lastRunTime' - The time when the pipeline was last run.
--
-- 'parallelismConfiguration', 'describePipelineResponse_parallelismConfiguration' - Lists the parallelism configuration applied to the pipeline.
--
-- 'pipelineArn', 'describePipelineResponse_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
--
-- 'pipelineDefinition', 'describePipelineResponse_pipelineDefinition' - The JSON pipeline definition.
--
-- 'pipelineDescription', 'describePipelineResponse_pipelineDescription' - The description of the pipeline.
--
-- 'pipelineDisplayName', 'describePipelineResponse_pipelineDisplayName' - The display name of the pipeline.
--
-- 'pipelineName', 'describePipelineResponse_pipelineName' - The name of the pipeline.
--
-- 'pipelineStatus', 'describePipelineResponse_pipelineStatus' - The status of the pipeline execution.
--
-- 'roleArn', 'describePipelineResponse_roleArn' - The Amazon Resource Name (ARN) that the pipeline uses to execute.
--
-- 'httpStatus', 'describePipelineResponse_httpStatus' - The response's http status code.
newDescribePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePipelineResponse
newDescribePipelineResponse pHttpStatus_ =
  DescribePipelineResponse'
    { createdBy =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lastRunTime = Prelude.Nothing,
      parallelismConfiguration = Prelude.Nothing,
      pipelineArn = Prelude.Nothing,
      pipelineDefinition = Prelude.Nothing,
      pipelineDescription = Prelude.Nothing,
      pipelineDisplayName = Prelude.Nothing,
      pipelineName = Prelude.Nothing,
      pipelineStatus = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describePipelineResponse_createdBy :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe UserContext)
describePipelineResponse_createdBy = Lens.lens (\DescribePipelineResponse' {createdBy} -> createdBy) (\s@DescribePipelineResponse' {} a -> s {createdBy = a} :: DescribePipelineResponse)

-- | The time when the pipeline was created.
describePipelineResponse_creationTime :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.UTCTime)
describePipelineResponse_creationTime = Lens.lens (\DescribePipelineResponse' {creationTime} -> creationTime) (\s@DescribePipelineResponse' {} a -> s {creationTime = a} :: DescribePipelineResponse) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
describePipelineResponse_lastModifiedBy :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe UserContext)
describePipelineResponse_lastModifiedBy = Lens.lens (\DescribePipelineResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribePipelineResponse' {} a -> s {lastModifiedBy = a} :: DescribePipelineResponse)

-- | The time when the pipeline was last modified.
describePipelineResponse_lastModifiedTime :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.UTCTime)
describePipelineResponse_lastModifiedTime = Lens.lens (\DescribePipelineResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribePipelineResponse' {} a -> s {lastModifiedTime = a} :: DescribePipelineResponse) Prelude.. Lens.mapping Data._Time

-- | The time when the pipeline was last run.
describePipelineResponse_lastRunTime :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.UTCTime)
describePipelineResponse_lastRunTime = Lens.lens (\DescribePipelineResponse' {lastRunTime} -> lastRunTime) (\s@DescribePipelineResponse' {} a -> s {lastRunTime = a} :: DescribePipelineResponse) Prelude.. Lens.mapping Data._Time

-- | Lists the parallelism configuration applied to the pipeline.
describePipelineResponse_parallelismConfiguration :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe ParallelismConfiguration)
describePipelineResponse_parallelismConfiguration = Lens.lens (\DescribePipelineResponse' {parallelismConfiguration} -> parallelismConfiguration) (\s@DescribePipelineResponse' {} a -> s {parallelismConfiguration = a} :: DescribePipelineResponse)

-- | The Amazon Resource Name (ARN) of the pipeline.
describePipelineResponse_pipelineArn :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.Text)
describePipelineResponse_pipelineArn = Lens.lens (\DescribePipelineResponse' {pipelineArn} -> pipelineArn) (\s@DescribePipelineResponse' {} a -> s {pipelineArn = a} :: DescribePipelineResponse)

-- | The JSON pipeline definition.
describePipelineResponse_pipelineDefinition :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.Text)
describePipelineResponse_pipelineDefinition = Lens.lens (\DescribePipelineResponse' {pipelineDefinition} -> pipelineDefinition) (\s@DescribePipelineResponse' {} a -> s {pipelineDefinition = a} :: DescribePipelineResponse)

-- | The description of the pipeline.
describePipelineResponse_pipelineDescription :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.Text)
describePipelineResponse_pipelineDescription = Lens.lens (\DescribePipelineResponse' {pipelineDescription} -> pipelineDescription) (\s@DescribePipelineResponse' {} a -> s {pipelineDescription = a} :: DescribePipelineResponse)

-- | The display name of the pipeline.
describePipelineResponse_pipelineDisplayName :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.Text)
describePipelineResponse_pipelineDisplayName = Lens.lens (\DescribePipelineResponse' {pipelineDisplayName} -> pipelineDisplayName) (\s@DescribePipelineResponse' {} a -> s {pipelineDisplayName = a} :: DescribePipelineResponse)

-- | The name of the pipeline.
describePipelineResponse_pipelineName :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.Text)
describePipelineResponse_pipelineName = Lens.lens (\DescribePipelineResponse' {pipelineName} -> pipelineName) (\s@DescribePipelineResponse' {} a -> s {pipelineName = a} :: DescribePipelineResponse)

-- | The status of the pipeline execution.
describePipelineResponse_pipelineStatus :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe PipelineStatus)
describePipelineResponse_pipelineStatus = Lens.lens (\DescribePipelineResponse' {pipelineStatus} -> pipelineStatus) (\s@DescribePipelineResponse' {} a -> s {pipelineStatus = a} :: DescribePipelineResponse)

-- | The Amazon Resource Name (ARN) that the pipeline uses to execute.
describePipelineResponse_roleArn :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.Text)
describePipelineResponse_roleArn = Lens.lens (\DescribePipelineResponse' {roleArn} -> roleArn) (\s@DescribePipelineResponse' {} a -> s {roleArn = a} :: DescribePipelineResponse)

-- | The response's http status code.
describePipelineResponse_httpStatus :: Lens.Lens' DescribePipelineResponse Prelude.Int
describePipelineResponse_httpStatus = Lens.lens (\DescribePipelineResponse' {httpStatus} -> httpStatus) (\s@DescribePipelineResponse' {} a -> s {httpStatus = a} :: DescribePipelineResponse)

instance Prelude.NFData DescribePipelineResponse where
  rnf DescribePipelineResponse' {..} =
    Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf lastRunTime
      `Prelude.seq` Prelude.rnf parallelismConfiguration
      `Prelude.seq` Prelude.rnf pipelineArn
      `Prelude.seq` Prelude.rnf pipelineDefinition
      `Prelude.seq` Prelude.rnf pipelineDescription
      `Prelude.seq` Prelude.rnf pipelineDisplayName
      `Prelude.seq` Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf pipelineStatus
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf httpStatus
