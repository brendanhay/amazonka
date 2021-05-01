{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.StartImportLabelsTaskRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables you to provide additional labels (examples of truth) to be used
-- to teach the machine learning transform and improve its quality. This
-- API operation is generally used as part of the active learning workflow
-- that starts with the @StartMLLabelingSetGenerationTaskRun@ call and that
-- ultimately results in improving the quality of your machine learning
-- transform.
--
-- After the @StartMLLabelingSetGenerationTaskRun@ finishes, AWS Glue
-- machine learning will have generated a series of questions for humans to
-- answer. (Answering these questions is often called \'labeling\' in the
-- machine learning workflows). In the case of the @FindMatches@ transform,
-- these questions are of the form, “What is the correct way to group these
-- rows together into groups composed entirely of matching records?” After
-- the labeling process is finished, users upload their answers\/labels
-- with a call to @StartImportLabelsTaskRun@. After
-- @StartImportLabelsTaskRun@ finishes, all future runs of the machine
-- learning transform use the new and improved labels and perform a
-- higher-quality transformation.
--
-- By default, @StartMLLabelingSetGenerationTaskRun@ continually learns
-- from and combines all labels that you upload unless you set @Replace@ to
-- true. If you set @Replace@ to true, @StartImportLabelsTaskRun@ deletes
-- and forgets all previously uploaded labels and learns only from the
-- exact set that you upload. Replacing labels can be helpful if you
-- realize that you previously uploaded incorrect labels, and you believe
-- that they are having a negative effect on your transform quality.
--
-- You can check on the status of your task run by calling the
-- @GetMLTaskRun@ operation.
module Network.AWS.Glue.StartImportLabelsTaskRun
  ( -- * Creating a Request
    StartImportLabelsTaskRun (..),
    newStartImportLabelsTaskRun,

    -- * Request Lenses
    startImportLabelsTaskRun_replaceAllLabels,
    startImportLabelsTaskRun_transformId,
    startImportLabelsTaskRun_inputS3Path,

    -- * Destructuring the Response
    StartImportLabelsTaskRunResponse (..),
    newStartImportLabelsTaskRunResponse,

    -- * Response Lenses
    startImportLabelsTaskRunResponse_taskRunId,
    startImportLabelsTaskRunResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartImportLabelsTaskRun' smart constructor.
data StartImportLabelsTaskRun = StartImportLabelsTaskRun'
  { -- | Indicates whether to overwrite your existing labels.
    replaceAllLabels :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier of the machine learning transform.
    transformId :: Prelude.Text,
    -- | The Amazon Simple Storage Service (Amazon S3) path from where you import
    -- the labels.
    inputS3Path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartImportLabelsTaskRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replaceAllLabels', 'startImportLabelsTaskRun_replaceAllLabels' - Indicates whether to overwrite your existing labels.
--
-- 'transformId', 'startImportLabelsTaskRun_transformId' - The unique identifier of the machine learning transform.
--
-- 'inputS3Path', 'startImportLabelsTaskRun_inputS3Path' - The Amazon Simple Storage Service (Amazon S3) path from where you import
-- the labels.
newStartImportLabelsTaskRun ::
  -- | 'transformId'
  Prelude.Text ->
  -- | 'inputS3Path'
  Prelude.Text ->
  StartImportLabelsTaskRun
newStartImportLabelsTaskRun
  pTransformId_
  pInputS3Path_ =
    StartImportLabelsTaskRun'
      { replaceAllLabels =
          Prelude.Nothing,
        transformId = pTransformId_,
        inputS3Path = pInputS3Path_
      }

-- | Indicates whether to overwrite your existing labels.
startImportLabelsTaskRun_replaceAllLabels :: Lens.Lens' StartImportLabelsTaskRun (Prelude.Maybe Prelude.Bool)
startImportLabelsTaskRun_replaceAllLabels = Lens.lens (\StartImportLabelsTaskRun' {replaceAllLabels} -> replaceAllLabels) (\s@StartImportLabelsTaskRun' {} a -> s {replaceAllLabels = a} :: StartImportLabelsTaskRun)

-- | The unique identifier of the machine learning transform.
startImportLabelsTaskRun_transformId :: Lens.Lens' StartImportLabelsTaskRun Prelude.Text
startImportLabelsTaskRun_transformId = Lens.lens (\StartImportLabelsTaskRun' {transformId} -> transformId) (\s@StartImportLabelsTaskRun' {} a -> s {transformId = a} :: StartImportLabelsTaskRun)

-- | The Amazon Simple Storage Service (Amazon S3) path from where you import
-- the labels.
startImportLabelsTaskRun_inputS3Path :: Lens.Lens' StartImportLabelsTaskRun Prelude.Text
startImportLabelsTaskRun_inputS3Path = Lens.lens (\StartImportLabelsTaskRun' {inputS3Path} -> inputS3Path) (\s@StartImportLabelsTaskRun' {} a -> s {inputS3Path = a} :: StartImportLabelsTaskRun)

instance Prelude.AWSRequest StartImportLabelsTaskRun where
  type
    Rs StartImportLabelsTaskRun =
      StartImportLabelsTaskRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImportLabelsTaskRunResponse'
            Prelude.<$> (x Prelude..?> "TaskRunId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartImportLabelsTaskRun

instance Prelude.NFData StartImportLabelsTaskRun

instance Prelude.ToHeaders StartImportLabelsTaskRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSGlue.StartImportLabelsTaskRun" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartImportLabelsTaskRun where
  toJSON StartImportLabelsTaskRun' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ReplaceAllLabels" Prelude..=)
              Prelude.<$> replaceAllLabels,
            Prelude.Just ("TransformId" Prelude..= transformId),
            Prelude.Just ("InputS3Path" Prelude..= inputS3Path)
          ]
      )

instance Prelude.ToPath StartImportLabelsTaskRun where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartImportLabelsTaskRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartImportLabelsTaskRunResponse' smart constructor.
data StartImportLabelsTaskRunResponse = StartImportLabelsTaskRunResponse'
  { -- | The unique identifier for the task run.
    taskRunId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartImportLabelsTaskRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskRunId', 'startImportLabelsTaskRunResponse_taskRunId' - The unique identifier for the task run.
--
-- 'httpStatus', 'startImportLabelsTaskRunResponse_httpStatus' - The response's http status code.
newStartImportLabelsTaskRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartImportLabelsTaskRunResponse
newStartImportLabelsTaskRunResponse pHttpStatus_ =
  StartImportLabelsTaskRunResponse'
    { taskRunId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the task run.
startImportLabelsTaskRunResponse_taskRunId :: Lens.Lens' StartImportLabelsTaskRunResponse (Prelude.Maybe Prelude.Text)
startImportLabelsTaskRunResponse_taskRunId = Lens.lens (\StartImportLabelsTaskRunResponse' {taskRunId} -> taskRunId) (\s@StartImportLabelsTaskRunResponse' {} a -> s {taskRunId = a} :: StartImportLabelsTaskRunResponse)

-- | The response's http status code.
startImportLabelsTaskRunResponse_httpStatus :: Lens.Lens' StartImportLabelsTaskRunResponse Prelude.Int
startImportLabelsTaskRunResponse_httpStatus = Lens.lens (\StartImportLabelsTaskRunResponse' {httpStatus} -> httpStatus) (\s@StartImportLabelsTaskRunResponse' {} a -> s {httpStatus = a} :: StartImportLabelsTaskRunResponse)

instance
  Prelude.NFData
    StartImportLabelsTaskRunResponse
