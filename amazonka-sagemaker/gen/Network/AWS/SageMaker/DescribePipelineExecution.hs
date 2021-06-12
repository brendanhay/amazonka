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
-- Module      : Network.AWS.SageMaker.DescribePipelineExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the details of a pipeline execution.
module Network.AWS.SageMaker.DescribePipelineExecution
  ( -- * Creating a Request
    DescribePipelineExecution (..),
    newDescribePipelineExecution,

    -- * Request Lenses
    describePipelineExecution_pipelineExecutionArn,

    -- * Destructuring the Response
    DescribePipelineExecutionResponse (..),
    newDescribePipelineExecutionResponse,

    -- * Response Lenses
    describePipelineExecutionResponse_pipelineArn,
    describePipelineExecutionResponse_creationTime,
    describePipelineExecutionResponse_pipelineExecutionDescription,
    describePipelineExecutionResponse_pipelineExecutionDisplayName,
    describePipelineExecutionResponse_pipelineExecutionStatus,
    describePipelineExecutionResponse_lastModifiedTime,
    describePipelineExecutionResponse_createdBy,
    describePipelineExecutionResponse_lastModifiedBy,
    describePipelineExecutionResponse_pipelineExecutionArn,
    describePipelineExecutionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribePipelineExecution' smart constructor.
data DescribePipelineExecution = DescribePipelineExecution'
  { -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribePipelineExecution
newDescribePipelineExecution pPipelineExecutionArn_ =
  DescribePipelineExecution'
    { pipelineExecutionArn =
        pPipelineExecutionArn_
    }

-- | The Amazon Resource Name (ARN) of the pipeline execution.
describePipelineExecution_pipelineExecutionArn :: Lens.Lens' DescribePipelineExecution Core.Text
describePipelineExecution_pipelineExecutionArn = Lens.lens (\DescribePipelineExecution' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@DescribePipelineExecution' {} a -> s {pipelineExecutionArn = a} :: DescribePipelineExecution)

instance Core.AWSRequest DescribePipelineExecution where
  type
    AWSResponse DescribePipelineExecution =
      DescribePipelineExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePipelineExecutionResponse'
            Core.<$> (x Core..?> "PipelineArn")
            Core.<*> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "PipelineExecutionDescription")
            Core.<*> (x Core..?> "PipelineExecutionDisplayName")
            Core.<*> (x Core..?> "PipelineExecutionStatus")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "CreatedBy")
            Core.<*> (x Core..?> "LastModifiedBy")
            Core.<*> (x Core..?> "PipelineExecutionArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribePipelineExecution

instance Core.NFData DescribePipelineExecution

instance Core.ToHeaders DescribePipelineExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribePipelineExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribePipelineExecution where
  toJSON DescribePipelineExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "PipelineExecutionArn"
                  Core..= pipelineExecutionArn
              )
          ]
      )

instance Core.ToPath DescribePipelineExecution where
  toPath = Core.const "/"

instance Core.ToQuery DescribePipelineExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribePipelineExecutionResponse' smart constructor.
data DescribePipelineExecutionResponse = DescribePipelineExecutionResponse'
  { -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Core.Maybe Core.Text,
    -- | The time when the pipeline execution was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The description of the pipeline execution.
    pipelineExecutionDescription :: Core.Maybe Core.Text,
    -- | The display name of the pipeline execution.
    pipelineExecutionDisplayName :: Core.Maybe Core.Text,
    -- | The status of the pipeline execution.
    pipelineExecutionStatus :: Core.Maybe PipelineExecutionStatus,
    -- | The time when the pipeline execution was modified last.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    createdBy :: Core.Maybe UserContext,
    lastModifiedBy :: Core.Maybe UserContext,
    -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePipelineExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineArn', 'describePipelineExecutionResponse_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
--
-- 'creationTime', 'describePipelineExecutionResponse_creationTime' - The time when the pipeline execution was created.
--
-- 'pipelineExecutionDescription', 'describePipelineExecutionResponse_pipelineExecutionDescription' - The description of the pipeline execution.
--
-- 'pipelineExecutionDisplayName', 'describePipelineExecutionResponse_pipelineExecutionDisplayName' - The display name of the pipeline execution.
--
-- 'pipelineExecutionStatus', 'describePipelineExecutionResponse_pipelineExecutionStatus' - The status of the pipeline execution.
--
-- 'lastModifiedTime', 'describePipelineExecutionResponse_lastModifiedTime' - The time when the pipeline execution was modified last.
--
-- 'createdBy', 'describePipelineExecutionResponse_createdBy' - Undocumented member.
--
-- 'lastModifiedBy', 'describePipelineExecutionResponse_lastModifiedBy' - Undocumented member.
--
-- 'pipelineExecutionArn', 'describePipelineExecutionResponse_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
--
-- 'httpStatus', 'describePipelineExecutionResponse_httpStatus' - The response's http status code.
newDescribePipelineExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribePipelineExecutionResponse
newDescribePipelineExecutionResponse pHttpStatus_ =
  DescribePipelineExecutionResponse'
    { pipelineArn =
        Core.Nothing,
      creationTime = Core.Nothing,
      pipelineExecutionDescription =
        Core.Nothing,
      pipelineExecutionDisplayName =
        Core.Nothing,
      pipelineExecutionStatus = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      createdBy = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      pipelineExecutionArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the pipeline.
describePipelineExecutionResponse_pipelineArn :: Lens.Lens' DescribePipelineExecutionResponse (Core.Maybe Core.Text)
describePipelineExecutionResponse_pipelineArn = Lens.lens (\DescribePipelineExecutionResponse' {pipelineArn} -> pipelineArn) (\s@DescribePipelineExecutionResponse' {} a -> s {pipelineArn = a} :: DescribePipelineExecutionResponse)

-- | The time when the pipeline execution was created.
describePipelineExecutionResponse_creationTime :: Lens.Lens' DescribePipelineExecutionResponse (Core.Maybe Core.UTCTime)
describePipelineExecutionResponse_creationTime = Lens.lens (\DescribePipelineExecutionResponse' {creationTime} -> creationTime) (\s@DescribePipelineExecutionResponse' {} a -> s {creationTime = a} :: DescribePipelineExecutionResponse) Core.. Lens.mapping Core._Time

-- | The description of the pipeline execution.
describePipelineExecutionResponse_pipelineExecutionDescription :: Lens.Lens' DescribePipelineExecutionResponse (Core.Maybe Core.Text)
describePipelineExecutionResponse_pipelineExecutionDescription = Lens.lens (\DescribePipelineExecutionResponse' {pipelineExecutionDescription} -> pipelineExecutionDescription) (\s@DescribePipelineExecutionResponse' {} a -> s {pipelineExecutionDescription = a} :: DescribePipelineExecutionResponse)

-- | The display name of the pipeline execution.
describePipelineExecutionResponse_pipelineExecutionDisplayName :: Lens.Lens' DescribePipelineExecutionResponse (Core.Maybe Core.Text)
describePipelineExecutionResponse_pipelineExecutionDisplayName = Lens.lens (\DescribePipelineExecutionResponse' {pipelineExecutionDisplayName} -> pipelineExecutionDisplayName) (\s@DescribePipelineExecutionResponse' {} a -> s {pipelineExecutionDisplayName = a} :: DescribePipelineExecutionResponse)

-- | The status of the pipeline execution.
describePipelineExecutionResponse_pipelineExecutionStatus :: Lens.Lens' DescribePipelineExecutionResponse (Core.Maybe PipelineExecutionStatus)
describePipelineExecutionResponse_pipelineExecutionStatus = Lens.lens (\DescribePipelineExecutionResponse' {pipelineExecutionStatus} -> pipelineExecutionStatus) (\s@DescribePipelineExecutionResponse' {} a -> s {pipelineExecutionStatus = a} :: DescribePipelineExecutionResponse)

-- | The time when the pipeline execution was modified last.
describePipelineExecutionResponse_lastModifiedTime :: Lens.Lens' DescribePipelineExecutionResponse (Core.Maybe Core.UTCTime)
describePipelineExecutionResponse_lastModifiedTime = Lens.lens (\DescribePipelineExecutionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribePipelineExecutionResponse' {} a -> s {lastModifiedTime = a} :: DescribePipelineExecutionResponse) Core.. Lens.mapping Core._Time

-- | Undocumented member.
describePipelineExecutionResponse_createdBy :: Lens.Lens' DescribePipelineExecutionResponse (Core.Maybe UserContext)
describePipelineExecutionResponse_createdBy = Lens.lens (\DescribePipelineExecutionResponse' {createdBy} -> createdBy) (\s@DescribePipelineExecutionResponse' {} a -> s {createdBy = a} :: DescribePipelineExecutionResponse)

-- | Undocumented member.
describePipelineExecutionResponse_lastModifiedBy :: Lens.Lens' DescribePipelineExecutionResponse (Core.Maybe UserContext)
describePipelineExecutionResponse_lastModifiedBy = Lens.lens (\DescribePipelineExecutionResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribePipelineExecutionResponse' {} a -> s {lastModifiedBy = a} :: DescribePipelineExecutionResponse)

-- | The Amazon Resource Name (ARN) of the pipeline execution.
describePipelineExecutionResponse_pipelineExecutionArn :: Lens.Lens' DescribePipelineExecutionResponse (Core.Maybe Core.Text)
describePipelineExecutionResponse_pipelineExecutionArn = Lens.lens (\DescribePipelineExecutionResponse' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@DescribePipelineExecutionResponse' {} a -> s {pipelineExecutionArn = a} :: DescribePipelineExecutionResponse)

-- | The response's http status code.
describePipelineExecutionResponse_httpStatus :: Lens.Lens' DescribePipelineExecutionResponse Core.Int
describePipelineExecutionResponse_httpStatus = Lens.lens (\DescribePipelineExecutionResponse' {httpStatus} -> httpStatus) (\s@DescribePipelineExecutionResponse' {} a -> s {httpStatus = a} :: DescribePipelineExecutionResponse)

instance
  Core.NFData
    DescribePipelineExecutionResponse
