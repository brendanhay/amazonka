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
-- Module      : Network.AWS.SageMaker.DescribePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the details of a pipeline.
module Network.AWS.SageMaker.DescribePipeline
  ( -- * Creating a Request
    DescribePipeline (..),
    newDescribePipeline,

    -- * Request Lenses
    describePipeline_pipelineName,

    -- * Destructuring the Response
    DescribePipelineResponse (..),
    newDescribePipelineResponse,

    -- * Response Lenses
    describePipelineResponse_pipelineArn,
    describePipelineResponse_pipelineDescription,
    describePipelineResponse_creationTime,
    describePipelineResponse_roleArn,
    describePipelineResponse_lastRunTime,
    describePipelineResponse_pipelineDefinition,
    describePipelineResponse_pipelineDisplayName,
    describePipelineResponse_lastModifiedTime,
    describePipelineResponse_pipelineStatus,
    describePipelineResponse_createdBy,
    describePipelineResponse_lastModifiedBy,
    describePipelineResponse_pipelineName,
    describePipelineResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribePipeline' smart constructor.
data DescribePipeline = DescribePipeline'
  { -- | The name of the pipeline to describe.
    pipelineName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribePipeline
newDescribePipeline pPipelineName_ =
  DescribePipeline' {pipelineName = pPipelineName_}

-- | The name of the pipeline to describe.
describePipeline_pipelineName :: Lens.Lens' DescribePipeline Core.Text
describePipeline_pipelineName = Lens.lens (\DescribePipeline' {pipelineName} -> pipelineName) (\s@DescribePipeline' {} a -> s {pipelineName = a} :: DescribePipeline)

instance Core.AWSRequest DescribePipeline where
  type
    AWSResponse DescribePipeline =
      DescribePipelineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePipelineResponse'
            Core.<$> (x Core..?> "PipelineArn")
            Core.<*> (x Core..?> "PipelineDescription")
            Core.<*> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "RoleArn")
            Core.<*> (x Core..?> "LastRunTime")
            Core.<*> (x Core..?> "PipelineDefinition")
            Core.<*> (x Core..?> "PipelineDisplayName")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "PipelineStatus")
            Core.<*> (x Core..?> "CreatedBy")
            Core.<*> (x Core..?> "LastModifiedBy")
            Core.<*> (x Core..?> "PipelineName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribePipeline

instance Core.NFData DescribePipeline

instance Core.ToHeaders DescribePipeline where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribePipeline" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribePipeline where
  toJSON DescribePipeline' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("PipelineName" Core..= pipelineName)]
      )

instance Core.ToPath DescribePipeline where
  toPath = Core.const "/"

instance Core.ToQuery DescribePipeline where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribePipelineResponse' smart constructor.
data DescribePipelineResponse = DescribePipelineResponse'
  { -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Core.Maybe Core.Text,
    -- | The description of the pipeline.
    pipelineDescription :: Core.Maybe Core.Text,
    -- | The time when the pipeline was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) that the pipeline uses to execute.
    roleArn :: Core.Maybe Core.Text,
    -- | The time when the pipeline was last run.
    lastRunTime :: Core.Maybe Core.POSIX,
    -- | The JSON pipeline definition.
    pipelineDefinition :: Core.Maybe Core.Text,
    -- | The display name of the pipeline.
    pipelineDisplayName :: Core.Maybe Core.Text,
    -- | The time when the pipeline was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The status of the pipeline execution.
    pipelineStatus :: Core.Maybe PipelineStatus,
    createdBy :: Core.Maybe UserContext,
    lastModifiedBy :: Core.Maybe UserContext,
    -- | The name of the pipeline.
    pipelineName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineArn', 'describePipelineResponse_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
--
-- 'pipelineDescription', 'describePipelineResponse_pipelineDescription' - The description of the pipeline.
--
-- 'creationTime', 'describePipelineResponse_creationTime' - The time when the pipeline was created.
--
-- 'roleArn', 'describePipelineResponse_roleArn' - The Amazon Resource Name (ARN) that the pipeline uses to execute.
--
-- 'lastRunTime', 'describePipelineResponse_lastRunTime' - The time when the pipeline was last run.
--
-- 'pipelineDefinition', 'describePipelineResponse_pipelineDefinition' - The JSON pipeline definition.
--
-- 'pipelineDisplayName', 'describePipelineResponse_pipelineDisplayName' - The display name of the pipeline.
--
-- 'lastModifiedTime', 'describePipelineResponse_lastModifiedTime' - The time when the pipeline was last modified.
--
-- 'pipelineStatus', 'describePipelineResponse_pipelineStatus' - The status of the pipeline execution.
--
-- 'createdBy', 'describePipelineResponse_createdBy' - Undocumented member.
--
-- 'lastModifiedBy', 'describePipelineResponse_lastModifiedBy' - Undocumented member.
--
-- 'pipelineName', 'describePipelineResponse_pipelineName' - The name of the pipeline.
--
-- 'httpStatus', 'describePipelineResponse_httpStatus' - The response's http status code.
newDescribePipelineResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribePipelineResponse
newDescribePipelineResponse pHttpStatus_ =
  DescribePipelineResponse'
    { pipelineArn =
        Core.Nothing,
      pipelineDescription = Core.Nothing,
      creationTime = Core.Nothing,
      roleArn = Core.Nothing,
      lastRunTime = Core.Nothing,
      pipelineDefinition = Core.Nothing,
      pipelineDisplayName = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      pipelineStatus = Core.Nothing,
      createdBy = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      pipelineName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the pipeline.
describePipelineResponse_pipelineArn :: Lens.Lens' DescribePipelineResponse (Core.Maybe Core.Text)
describePipelineResponse_pipelineArn = Lens.lens (\DescribePipelineResponse' {pipelineArn} -> pipelineArn) (\s@DescribePipelineResponse' {} a -> s {pipelineArn = a} :: DescribePipelineResponse)

-- | The description of the pipeline.
describePipelineResponse_pipelineDescription :: Lens.Lens' DescribePipelineResponse (Core.Maybe Core.Text)
describePipelineResponse_pipelineDescription = Lens.lens (\DescribePipelineResponse' {pipelineDescription} -> pipelineDescription) (\s@DescribePipelineResponse' {} a -> s {pipelineDescription = a} :: DescribePipelineResponse)

-- | The time when the pipeline was created.
describePipelineResponse_creationTime :: Lens.Lens' DescribePipelineResponse (Core.Maybe Core.UTCTime)
describePipelineResponse_creationTime = Lens.lens (\DescribePipelineResponse' {creationTime} -> creationTime) (\s@DescribePipelineResponse' {} a -> s {creationTime = a} :: DescribePipelineResponse) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) that the pipeline uses to execute.
describePipelineResponse_roleArn :: Lens.Lens' DescribePipelineResponse (Core.Maybe Core.Text)
describePipelineResponse_roleArn = Lens.lens (\DescribePipelineResponse' {roleArn} -> roleArn) (\s@DescribePipelineResponse' {} a -> s {roleArn = a} :: DescribePipelineResponse)

-- | The time when the pipeline was last run.
describePipelineResponse_lastRunTime :: Lens.Lens' DescribePipelineResponse (Core.Maybe Core.UTCTime)
describePipelineResponse_lastRunTime = Lens.lens (\DescribePipelineResponse' {lastRunTime} -> lastRunTime) (\s@DescribePipelineResponse' {} a -> s {lastRunTime = a} :: DescribePipelineResponse) Core.. Lens.mapping Core._Time

-- | The JSON pipeline definition.
describePipelineResponse_pipelineDefinition :: Lens.Lens' DescribePipelineResponse (Core.Maybe Core.Text)
describePipelineResponse_pipelineDefinition = Lens.lens (\DescribePipelineResponse' {pipelineDefinition} -> pipelineDefinition) (\s@DescribePipelineResponse' {} a -> s {pipelineDefinition = a} :: DescribePipelineResponse)

-- | The display name of the pipeline.
describePipelineResponse_pipelineDisplayName :: Lens.Lens' DescribePipelineResponse (Core.Maybe Core.Text)
describePipelineResponse_pipelineDisplayName = Lens.lens (\DescribePipelineResponse' {pipelineDisplayName} -> pipelineDisplayName) (\s@DescribePipelineResponse' {} a -> s {pipelineDisplayName = a} :: DescribePipelineResponse)

-- | The time when the pipeline was last modified.
describePipelineResponse_lastModifiedTime :: Lens.Lens' DescribePipelineResponse (Core.Maybe Core.UTCTime)
describePipelineResponse_lastModifiedTime = Lens.lens (\DescribePipelineResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribePipelineResponse' {} a -> s {lastModifiedTime = a} :: DescribePipelineResponse) Core.. Lens.mapping Core._Time

-- | The status of the pipeline execution.
describePipelineResponse_pipelineStatus :: Lens.Lens' DescribePipelineResponse (Core.Maybe PipelineStatus)
describePipelineResponse_pipelineStatus = Lens.lens (\DescribePipelineResponse' {pipelineStatus} -> pipelineStatus) (\s@DescribePipelineResponse' {} a -> s {pipelineStatus = a} :: DescribePipelineResponse)

-- | Undocumented member.
describePipelineResponse_createdBy :: Lens.Lens' DescribePipelineResponse (Core.Maybe UserContext)
describePipelineResponse_createdBy = Lens.lens (\DescribePipelineResponse' {createdBy} -> createdBy) (\s@DescribePipelineResponse' {} a -> s {createdBy = a} :: DescribePipelineResponse)

-- | Undocumented member.
describePipelineResponse_lastModifiedBy :: Lens.Lens' DescribePipelineResponse (Core.Maybe UserContext)
describePipelineResponse_lastModifiedBy = Lens.lens (\DescribePipelineResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribePipelineResponse' {} a -> s {lastModifiedBy = a} :: DescribePipelineResponse)

-- | The name of the pipeline.
describePipelineResponse_pipelineName :: Lens.Lens' DescribePipelineResponse (Core.Maybe Core.Text)
describePipelineResponse_pipelineName = Lens.lens (\DescribePipelineResponse' {pipelineName} -> pipelineName) (\s@DescribePipelineResponse' {} a -> s {pipelineName = a} :: DescribePipelineResponse)

-- | The response's http status code.
describePipelineResponse_httpStatus :: Lens.Lens' DescribePipelineResponse Core.Int
describePipelineResponse_httpStatus = Lens.lens (\DescribePipelineResponse' {httpStatus} -> httpStatus) (\s@DescribePipelineResponse' {} a -> s {httpStatus = a} :: DescribePipelineResponse)

instance Core.NFData DescribePipelineResponse
