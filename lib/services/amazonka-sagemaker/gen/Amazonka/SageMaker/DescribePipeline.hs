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
    describePipelineResponse_roleArn,
    describePipelineResponse_pipelineArn,
    describePipelineResponse_pipelineDisplayName,
    describePipelineResponse_pipelineDefinition,
    describePipelineResponse_pipelineDescription,
    describePipelineResponse_lastModifiedTime,
    describePipelineResponse_parallelismConfiguration,
    describePipelineResponse_pipelineName,
    describePipelineResponse_lastRunTime,
    describePipelineResponse_creationTime,
    describePipelineResponse_lastModifiedBy,
    describePipelineResponse_createdBy,
    describePipelineResponse_pipelineStatus,
    describePipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
            Prelude.<$> (x Core..?> "RoleArn")
            Prelude.<*> (x Core..?> "PipelineArn")
            Prelude.<*> (x Core..?> "PipelineDisplayName")
            Prelude.<*> (x Core..?> "PipelineDefinition")
            Prelude.<*> (x Core..?> "PipelineDescription")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "ParallelismConfiguration")
            Prelude.<*> (x Core..?> "PipelineName")
            Prelude.<*> (x Core..?> "LastRunTime")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "LastModifiedBy")
            Prelude.<*> (x Core..?> "CreatedBy")
            Prelude.<*> (x Core..?> "PipelineStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePipeline where
  hashWithSalt _salt DescribePipeline' {..} =
    _salt `Prelude.hashWithSalt` pipelineName

instance Prelude.NFData DescribePipeline where
  rnf DescribePipeline' {..} = Prelude.rnf pipelineName

instance Core.ToHeaders DescribePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribePipeline" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribePipeline where
  toJSON DescribePipeline' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("PipelineName" Core..= pipelineName)]
      )

instance Core.ToPath DescribePipeline where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePipelineResponse' smart constructor.
data DescribePipelineResponse = DescribePipelineResponse'
  { -- | The Amazon Resource Name (ARN) that the pipeline uses to execute.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The display name of the pipeline.
    pipelineDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The JSON pipeline definition.
    pipelineDefinition :: Prelude.Maybe Prelude.Text,
    -- | The description of the pipeline.
    pipelineDescription :: Prelude.Maybe Prelude.Text,
    -- | The time when the pipeline was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | Lists the parallelism configuration applied to the pipeline.
    parallelismConfiguration :: Prelude.Maybe ParallelismConfiguration,
    -- | The name of the pipeline.
    pipelineName :: Prelude.Maybe Prelude.Text,
    -- | The time when the pipeline was last run.
    lastRunTime :: Prelude.Maybe Core.POSIX,
    -- | The time when the pipeline was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    lastModifiedBy :: Prelude.Maybe UserContext,
    createdBy :: Prelude.Maybe UserContext,
    -- | The status of the pipeline execution.
    pipelineStatus :: Prelude.Maybe PipelineStatus,
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
-- 'roleArn', 'describePipelineResponse_roleArn' - The Amazon Resource Name (ARN) that the pipeline uses to execute.
--
-- 'pipelineArn', 'describePipelineResponse_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
--
-- 'pipelineDisplayName', 'describePipelineResponse_pipelineDisplayName' - The display name of the pipeline.
--
-- 'pipelineDefinition', 'describePipelineResponse_pipelineDefinition' - The JSON pipeline definition.
--
-- 'pipelineDescription', 'describePipelineResponse_pipelineDescription' - The description of the pipeline.
--
-- 'lastModifiedTime', 'describePipelineResponse_lastModifiedTime' - The time when the pipeline was last modified.
--
-- 'parallelismConfiguration', 'describePipelineResponse_parallelismConfiguration' - Lists the parallelism configuration applied to the pipeline.
--
-- 'pipelineName', 'describePipelineResponse_pipelineName' - The name of the pipeline.
--
-- 'lastRunTime', 'describePipelineResponse_lastRunTime' - The time when the pipeline was last run.
--
-- 'creationTime', 'describePipelineResponse_creationTime' - The time when the pipeline was created.
--
-- 'lastModifiedBy', 'describePipelineResponse_lastModifiedBy' - Undocumented member.
--
-- 'createdBy', 'describePipelineResponse_createdBy' - Undocumented member.
--
-- 'pipelineStatus', 'describePipelineResponse_pipelineStatus' - The status of the pipeline execution.
--
-- 'httpStatus', 'describePipelineResponse_httpStatus' - The response's http status code.
newDescribePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePipelineResponse
newDescribePipelineResponse pHttpStatus_ =
  DescribePipelineResponse'
    { roleArn =
        Prelude.Nothing,
      pipelineArn = Prelude.Nothing,
      pipelineDisplayName = Prelude.Nothing,
      pipelineDefinition = Prelude.Nothing,
      pipelineDescription = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      parallelismConfiguration = Prelude.Nothing,
      pipelineName = Prelude.Nothing,
      lastRunTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      pipelineStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) that the pipeline uses to execute.
describePipelineResponse_roleArn :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.Text)
describePipelineResponse_roleArn = Lens.lens (\DescribePipelineResponse' {roleArn} -> roleArn) (\s@DescribePipelineResponse' {} a -> s {roleArn = a} :: DescribePipelineResponse)

-- | The Amazon Resource Name (ARN) of the pipeline.
describePipelineResponse_pipelineArn :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.Text)
describePipelineResponse_pipelineArn = Lens.lens (\DescribePipelineResponse' {pipelineArn} -> pipelineArn) (\s@DescribePipelineResponse' {} a -> s {pipelineArn = a} :: DescribePipelineResponse)

-- | The display name of the pipeline.
describePipelineResponse_pipelineDisplayName :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.Text)
describePipelineResponse_pipelineDisplayName = Lens.lens (\DescribePipelineResponse' {pipelineDisplayName} -> pipelineDisplayName) (\s@DescribePipelineResponse' {} a -> s {pipelineDisplayName = a} :: DescribePipelineResponse)

-- | The JSON pipeline definition.
describePipelineResponse_pipelineDefinition :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.Text)
describePipelineResponse_pipelineDefinition = Lens.lens (\DescribePipelineResponse' {pipelineDefinition} -> pipelineDefinition) (\s@DescribePipelineResponse' {} a -> s {pipelineDefinition = a} :: DescribePipelineResponse)

-- | The description of the pipeline.
describePipelineResponse_pipelineDescription :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.Text)
describePipelineResponse_pipelineDescription = Lens.lens (\DescribePipelineResponse' {pipelineDescription} -> pipelineDescription) (\s@DescribePipelineResponse' {} a -> s {pipelineDescription = a} :: DescribePipelineResponse)

-- | The time when the pipeline was last modified.
describePipelineResponse_lastModifiedTime :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.UTCTime)
describePipelineResponse_lastModifiedTime = Lens.lens (\DescribePipelineResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribePipelineResponse' {} a -> s {lastModifiedTime = a} :: DescribePipelineResponse) Prelude.. Lens.mapping Core._Time

-- | Lists the parallelism configuration applied to the pipeline.
describePipelineResponse_parallelismConfiguration :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe ParallelismConfiguration)
describePipelineResponse_parallelismConfiguration = Lens.lens (\DescribePipelineResponse' {parallelismConfiguration} -> parallelismConfiguration) (\s@DescribePipelineResponse' {} a -> s {parallelismConfiguration = a} :: DescribePipelineResponse)

-- | The name of the pipeline.
describePipelineResponse_pipelineName :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.Text)
describePipelineResponse_pipelineName = Lens.lens (\DescribePipelineResponse' {pipelineName} -> pipelineName) (\s@DescribePipelineResponse' {} a -> s {pipelineName = a} :: DescribePipelineResponse)

-- | The time when the pipeline was last run.
describePipelineResponse_lastRunTime :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.UTCTime)
describePipelineResponse_lastRunTime = Lens.lens (\DescribePipelineResponse' {lastRunTime} -> lastRunTime) (\s@DescribePipelineResponse' {} a -> s {lastRunTime = a} :: DescribePipelineResponse) Prelude.. Lens.mapping Core._Time

-- | The time when the pipeline was created.
describePipelineResponse_creationTime :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe Prelude.UTCTime)
describePipelineResponse_creationTime = Lens.lens (\DescribePipelineResponse' {creationTime} -> creationTime) (\s@DescribePipelineResponse' {} a -> s {creationTime = a} :: DescribePipelineResponse) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
describePipelineResponse_lastModifiedBy :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe UserContext)
describePipelineResponse_lastModifiedBy = Lens.lens (\DescribePipelineResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribePipelineResponse' {} a -> s {lastModifiedBy = a} :: DescribePipelineResponse)

-- | Undocumented member.
describePipelineResponse_createdBy :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe UserContext)
describePipelineResponse_createdBy = Lens.lens (\DescribePipelineResponse' {createdBy} -> createdBy) (\s@DescribePipelineResponse' {} a -> s {createdBy = a} :: DescribePipelineResponse)

-- | The status of the pipeline execution.
describePipelineResponse_pipelineStatus :: Lens.Lens' DescribePipelineResponse (Prelude.Maybe PipelineStatus)
describePipelineResponse_pipelineStatus = Lens.lens (\DescribePipelineResponse' {pipelineStatus} -> pipelineStatus) (\s@DescribePipelineResponse' {} a -> s {pipelineStatus = a} :: DescribePipelineResponse)

-- | The response's http status code.
describePipelineResponse_httpStatus :: Lens.Lens' DescribePipelineResponse Prelude.Int
describePipelineResponse_httpStatus = Lens.lens (\DescribePipelineResponse' {httpStatus} -> httpStatus) (\s@DescribePipelineResponse' {} a -> s {httpStatus = a} :: DescribePipelineResponse)

instance Prelude.NFData DescribePipelineResponse where
  rnf DescribePipelineResponse' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf pipelineArn
      `Prelude.seq` Prelude.rnf pipelineDisplayName
      `Prelude.seq` Prelude.rnf pipelineDefinition
      `Prelude.seq` Prelude.rnf pipelineDescription
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf parallelismConfiguration
      `Prelude.seq` Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf lastRunTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf pipelineStatus
      `Prelude.seq` Prelude.rnf httpStatus
