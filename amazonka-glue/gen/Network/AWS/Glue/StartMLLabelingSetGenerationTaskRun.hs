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
-- Module      : Network.AWS.Glue.StartMLLabelingSetGenerationTaskRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the active learning workflow for your machine learning transform
-- to improve the transform\'s quality by generating label sets and adding
-- labels.
--
-- When the @StartMLLabelingSetGenerationTaskRun@ finishes, AWS Glue will
-- have generated a \"labeling set\" or a set of questions for humans to
-- answer.
--
-- In the case of the @FindMatches@ transform, these questions are of the
-- form, “What is the correct way to group these rows together into groups
-- composed entirely of matching records?”
--
-- After the labeling process is finished, you can upload your labels with
-- a call to @StartImportLabelsTaskRun@. After @StartImportLabelsTaskRun@
-- finishes, all future runs of the machine learning transform will use the
-- new and improved labels and perform a higher-quality transformation.
module Network.AWS.Glue.StartMLLabelingSetGenerationTaskRun
  ( -- * Creating a Request
    StartMLLabelingSetGenerationTaskRun (..),
    newStartMLLabelingSetGenerationTaskRun,

    -- * Request Lenses
    startMLLabelingSetGenerationTaskRun_transformId,
    startMLLabelingSetGenerationTaskRun_outputS3Path,

    -- * Destructuring the Response
    StartMLLabelingSetGenerationTaskRunResponse (..),
    newStartMLLabelingSetGenerationTaskRunResponse,

    -- * Response Lenses
    startMLLabelingSetGenerationTaskRunResponse_taskRunId,
    startMLLabelingSetGenerationTaskRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartMLLabelingSetGenerationTaskRun' smart constructor.
data StartMLLabelingSetGenerationTaskRun = StartMLLabelingSetGenerationTaskRun'
  { -- | The unique identifier of the machine learning transform.
    transformId :: Core.Text,
    -- | The Amazon Simple Storage Service (Amazon S3) path where you generate
    -- the labeling set.
    outputS3Path :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartMLLabelingSetGenerationTaskRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformId', 'startMLLabelingSetGenerationTaskRun_transformId' - The unique identifier of the machine learning transform.
--
-- 'outputS3Path', 'startMLLabelingSetGenerationTaskRun_outputS3Path' - The Amazon Simple Storage Service (Amazon S3) path where you generate
-- the labeling set.
newStartMLLabelingSetGenerationTaskRun ::
  -- | 'transformId'
  Core.Text ->
  -- | 'outputS3Path'
  Core.Text ->
  StartMLLabelingSetGenerationTaskRun
newStartMLLabelingSetGenerationTaskRun
  pTransformId_
  pOutputS3Path_ =
    StartMLLabelingSetGenerationTaskRun'
      { transformId =
          pTransformId_,
        outputS3Path = pOutputS3Path_
      }

-- | The unique identifier of the machine learning transform.
startMLLabelingSetGenerationTaskRun_transformId :: Lens.Lens' StartMLLabelingSetGenerationTaskRun Core.Text
startMLLabelingSetGenerationTaskRun_transformId = Lens.lens (\StartMLLabelingSetGenerationTaskRun' {transformId} -> transformId) (\s@StartMLLabelingSetGenerationTaskRun' {} a -> s {transformId = a} :: StartMLLabelingSetGenerationTaskRun)

-- | The Amazon Simple Storage Service (Amazon S3) path where you generate
-- the labeling set.
startMLLabelingSetGenerationTaskRun_outputS3Path :: Lens.Lens' StartMLLabelingSetGenerationTaskRun Core.Text
startMLLabelingSetGenerationTaskRun_outputS3Path = Lens.lens (\StartMLLabelingSetGenerationTaskRun' {outputS3Path} -> outputS3Path) (\s@StartMLLabelingSetGenerationTaskRun' {} a -> s {outputS3Path = a} :: StartMLLabelingSetGenerationTaskRun)

instance
  Core.AWSRequest
    StartMLLabelingSetGenerationTaskRun
  where
  type
    AWSResponse StartMLLabelingSetGenerationTaskRun =
      StartMLLabelingSetGenerationTaskRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMLLabelingSetGenerationTaskRunResponse'
            Core.<$> (x Core..?> "TaskRunId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    StartMLLabelingSetGenerationTaskRun

instance
  Core.NFData
    StartMLLabelingSetGenerationTaskRun

instance
  Core.ToHeaders
    StartMLLabelingSetGenerationTaskRun
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.StartMLLabelingSetGenerationTaskRun" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    StartMLLabelingSetGenerationTaskRun
  where
  toJSON StartMLLabelingSetGenerationTaskRun' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TransformId" Core..= transformId),
            Core.Just ("OutputS3Path" Core..= outputS3Path)
          ]
      )

instance
  Core.ToPath
    StartMLLabelingSetGenerationTaskRun
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    StartMLLabelingSetGenerationTaskRun
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartMLLabelingSetGenerationTaskRunResponse' smart constructor.
data StartMLLabelingSetGenerationTaskRunResponse = StartMLLabelingSetGenerationTaskRunResponse'
  { -- | The unique run identifier that is associated with this task run.
    taskRunId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartMLLabelingSetGenerationTaskRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskRunId', 'startMLLabelingSetGenerationTaskRunResponse_taskRunId' - The unique run identifier that is associated with this task run.
--
-- 'httpStatus', 'startMLLabelingSetGenerationTaskRunResponse_httpStatus' - The response's http status code.
newStartMLLabelingSetGenerationTaskRunResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartMLLabelingSetGenerationTaskRunResponse
newStartMLLabelingSetGenerationTaskRunResponse
  pHttpStatus_ =
    StartMLLabelingSetGenerationTaskRunResponse'
      { taskRunId =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The unique run identifier that is associated with this task run.
startMLLabelingSetGenerationTaskRunResponse_taskRunId :: Lens.Lens' StartMLLabelingSetGenerationTaskRunResponse (Core.Maybe Core.Text)
startMLLabelingSetGenerationTaskRunResponse_taskRunId = Lens.lens (\StartMLLabelingSetGenerationTaskRunResponse' {taskRunId} -> taskRunId) (\s@StartMLLabelingSetGenerationTaskRunResponse' {} a -> s {taskRunId = a} :: StartMLLabelingSetGenerationTaskRunResponse)

-- | The response's http status code.
startMLLabelingSetGenerationTaskRunResponse_httpStatus :: Lens.Lens' StartMLLabelingSetGenerationTaskRunResponse Core.Int
startMLLabelingSetGenerationTaskRunResponse_httpStatus = Lens.lens (\StartMLLabelingSetGenerationTaskRunResponse' {httpStatus} -> httpStatus) (\s@StartMLLabelingSetGenerationTaskRunResponse' {} a -> s {httpStatus = a} :: StartMLLabelingSetGenerationTaskRunResponse)

instance
  Core.NFData
    StartMLLabelingSetGenerationTaskRunResponse
