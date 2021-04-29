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
-- Module      : Network.AWS.Glue.StartExportLabelsTaskRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Begins an asynchronous task to export all labeled data for a particular
-- transform. This task is the only label-related API call that is not part
-- of the typical active learning workflow. You typically use
-- @StartExportLabelsTaskRun@ when you want to work with all of your
-- existing labels at the same time, such as when you want to remove or
-- change labels that were previously submitted as truth. This API
-- operation accepts the @TransformId@ whose labels you want to export and
-- an Amazon Simple Storage Service (Amazon S3) path to export the labels
-- to. The operation returns a @TaskRunId@. You can check on the status of
-- your task run by calling the @GetMLTaskRun@ API.
module Network.AWS.Glue.StartExportLabelsTaskRun
  ( -- * Creating a Request
    StartExportLabelsTaskRun (..),
    newStartExportLabelsTaskRun,

    -- * Request Lenses
    startExportLabelsTaskRun_transformId,
    startExportLabelsTaskRun_outputS3Path,

    -- * Destructuring the Response
    StartExportLabelsTaskRunResponse (..),
    newStartExportLabelsTaskRunResponse,

    -- * Response Lenses
    startExportLabelsTaskRunResponse_taskRunId,
    startExportLabelsTaskRunResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartExportLabelsTaskRun' smart constructor.
data StartExportLabelsTaskRun = StartExportLabelsTaskRun'
  { -- | The unique identifier of the machine learning transform.
    transformId :: Prelude.Text,
    -- | The Amazon S3 path where you export the labels.
    outputS3Path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartExportLabelsTaskRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformId', 'startExportLabelsTaskRun_transformId' - The unique identifier of the machine learning transform.
--
-- 'outputS3Path', 'startExportLabelsTaskRun_outputS3Path' - The Amazon S3 path where you export the labels.
newStartExportLabelsTaskRun ::
  -- | 'transformId'
  Prelude.Text ->
  -- | 'outputS3Path'
  Prelude.Text ->
  StartExportLabelsTaskRun
newStartExportLabelsTaskRun
  pTransformId_
  pOutputS3Path_ =
    StartExportLabelsTaskRun'
      { transformId =
          pTransformId_,
        outputS3Path = pOutputS3Path_
      }

-- | The unique identifier of the machine learning transform.
startExportLabelsTaskRun_transformId :: Lens.Lens' StartExportLabelsTaskRun Prelude.Text
startExportLabelsTaskRun_transformId = Lens.lens (\StartExportLabelsTaskRun' {transformId} -> transformId) (\s@StartExportLabelsTaskRun' {} a -> s {transformId = a} :: StartExportLabelsTaskRun)

-- | The Amazon S3 path where you export the labels.
startExportLabelsTaskRun_outputS3Path :: Lens.Lens' StartExportLabelsTaskRun Prelude.Text
startExportLabelsTaskRun_outputS3Path = Lens.lens (\StartExportLabelsTaskRun' {outputS3Path} -> outputS3Path) (\s@StartExportLabelsTaskRun' {} a -> s {outputS3Path = a} :: StartExportLabelsTaskRun)

instance Prelude.AWSRequest StartExportLabelsTaskRun where
  type
    Rs StartExportLabelsTaskRun =
      StartExportLabelsTaskRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartExportLabelsTaskRunResponse'
            Prelude.<$> (x Prelude..?> "TaskRunId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartExportLabelsTaskRun

instance Prelude.NFData StartExportLabelsTaskRun

instance Prelude.ToHeaders StartExportLabelsTaskRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSGlue.StartExportLabelsTaskRun" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartExportLabelsTaskRun where
  toJSON StartExportLabelsTaskRun' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TransformId" Prelude..= transformId),
            Prelude.Just
              ("OutputS3Path" Prelude..= outputS3Path)
          ]
      )

instance Prelude.ToPath StartExportLabelsTaskRun where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartExportLabelsTaskRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartExportLabelsTaskRunResponse' smart constructor.
data StartExportLabelsTaskRunResponse = StartExportLabelsTaskRunResponse'
  { -- | The unique identifier for the task run.
    taskRunId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartExportLabelsTaskRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskRunId', 'startExportLabelsTaskRunResponse_taskRunId' - The unique identifier for the task run.
--
-- 'httpStatus', 'startExportLabelsTaskRunResponse_httpStatus' - The response's http status code.
newStartExportLabelsTaskRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartExportLabelsTaskRunResponse
newStartExportLabelsTaskRunResponse pHttpStatus_ =
  StartExportLabelsTaskRunResponse'
    { taskRunId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the task run.
startExportLabelsTaskRunResponse_taskRunId :: Lens.Lens' StartExportLabelsTaskRunResponse (Prelude.Maybe Prelude.Text)
startExportLabelsTaskRunResponse_taskRunId = Lens.lens (\StartExportLabelsTaskRunResponse' {taskRunId} -> taskRunId) (\s@StartExportLabelsTaskRunResponse' {} a -> s {taskRunId = a} :: StartExportLabelsTaskRunResponse)

-- | The response's http status code.
startExportLabelsTaskRunResponse_httpStatus :: Lens.Lens' StartExportLabelsTaskRunResponse Prelude.Int
startExportLabelsTaskRunResponse_httpStatus = Lens.lens (\StartExportLabelsTaskRunResponse' {httpStatus} -> httpStatus) (\s@StartExportLabelsTaskRunResponse' {} a -> s {httpStatus = a} :: StartExportLabelsTaskRunResponse)

instance
  Prelude.NFData
    StartExportLabelsTaskRunResponse
