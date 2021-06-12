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
-- Module      : Network.AWS.IoT.StartThingRegistrationTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a bulk thing provisioning task.
module Network.AWS.IoT.StartThingRegistrationTask
  ( -- * Creating a Request
    StartThingRegistrationTask (..),
    newStartThingRegistrationTask,

    -- * Request Lenses
    startThingRegistrationTask_templateBody,
    startThingRegistrationTask_inputFileBucket,
    startThingRegistrationTask_inputFileKey,
    startThingRegistrationTask_roleArn,

    -- * Destructuring the Response
    StartThingRegistrationTaskResponse (..),
    newStartThingRegistrationTaskResponse,

    -- * Response Lenses
    startThingRegistrationTaskResponse_taskId,
    startThingRegistrationTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartThingRegistrationTask' smart constructor.
data StartThingRegistrationTask = StartThingRegistrationTask'
  { -- | The provisioning template.
    templateBody :: Core.Text,
    -- | The S3 bucket that contains the input file.
    inputFileBucket :: Core.Text,
    -- | The name of input file within the S3 bucket. This file contains a
    -- newline delimited JSON file. Each line contains the parameter values to
    -- provision one device (thing).
    inputFileKey :: Core.Text,
    -- | The IAM role ARN that grants permission the input file.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartThingRegistrationTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateBody', 'startThingRegistrationTask_templateBody' - The provisioning template.
--
-- 'inputFileBucket', 'startThingRegistrationTask_inputFileBucket' - The S3 bucket that contains the input file.
--
-- 'inputFileKey', 'startThingRegistrationTask_inputFileKey' - The name of input file within the S3 bucket. This file contains a
-- newline delimited JSON file. Each line contains the parameter values to
-- provision one device (thing).
--
-- 'roleArn', 'startThingRegistrationTask_roleArn' - The IAM role ARN that grants permission the input file.
newStartThingRegistrationTask ::
  -- | 'templateBody'
  Core.Text ->
  -- | 'inputFileBucket'
  Core.Text ->
  -- | 'inputFileKey'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  StartThingRegistrationTask
newStartThingRegistrationTask
  pTemplateBody_
  pInputFileBucket_
  pInputFileKey_
  pRoleArn_ =
    StartThingRegistrationTask'
      { templateBody =
          pTemplateBody_,
        inputFileBucket = pInputFileBucket_,
        inputFileKey = pInputFileKey_,
        roleArn = pRoleArn_
      }

-- | The provisioning template.
startThingRegistrationTask_templateBody :: Lens.Lens' StartThingRegistrationTask Core.Text
startThingRegistrationTask_templateBody = Lens.lens (\StartThingRegistrationTask' {templateBody} -> templateBody) (\s@StartThingRegistrationTask' {} a -> s {templateBody = a} :: StartThingRegistrationTask)

-- | The S3 bucket that contains the input file.
startThingRegistrationTask_inputFileBucket :: Lens.Lens' StartThingRegistrationTask Core.Text
startThingRegistrationTask_inputFileBucket = Lens.lens (\StartThingRegistrationTask' {inputFileBucket} -> inputFileBucket) (\s@StartThingRegistrationTask' {} a -> s {inputFileBucket = a} :: StartThingRegistrationTask)

-- | The name of input file within the S3 bucket. This file contains a
-- newline delimited JSON file. Each line contains the parameter values to
-- provision one device (thing).
startThingRegistrationTask_inputFileKey :: Lens.Lens' StartThingRegistrationTask Core.Text
startThingRegistrationTask_inputFileKey = Lens.lens (\StartThingRegistrationTask' {inputFileKey} -> inputFileKey) (\s@StartThingRegistrationTask' {} a -> s {inputFileKey = a} :: StartThingRegistrationTask)

-- | The IAM role ARN that grants permission the input file.
startThingRegistrationTask_roleArn :: Lens.Lens' StartThingRegistrationTask Core.Text
startThingRegistrationTask_roleArn = Lens.lens (\StartThingRegistrationTask' {roleArn} -> roleArn) (\s@StartThingRegistrationTask' {} a -> s {roleArn = a} :: StartThingRegistrationTask)

instance Core.AWSRequest StartThingRegistrationTask where
  type
    AWSResponse StartThingRegistrationTask =
      StartThingRegistrationTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartThingRegistrationTaskResponse'
            Core.<$> (x Core..?> "taskId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartThingRegistrationTask

instance Core.NFData StartThingRegistrationTask

instance Core.ToHeaders StartThingRegistrationTask where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON StartThingRegistrationTask where
  toJSON StartThingRegistrationTask' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("templateBody" Core..= templateBody),
            Core.Just
              ("inputFileBucket" Core..= inputFileBucket),
            Core.Just ("inputFileKey" Core..= inputFileKey),
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath StartThingRegistrationTask where
  toPath = Core.const "/thing-registration-tasks"

instance Core.ToQuery StartThingRegistrationTask where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartThingRegistrationTaskResponse' smart constructor.
data StartThingRegistrationTaskResponse = StartThingRegistrationTaskResponse'
  { -- | The bulk thing provisioning task ID.
    taskId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartThingRegistrationTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'startThingRegistrationTaskResponse_taskId' - The bulk thing provisioning task ID.
--
-- 'httpStatus', 'startThingRegistrationTaskResponse_httpStatus' - The response's http status code.
newStartThingRegistrationTaskResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartThingRegistrationTaskResponse
newStartThingRegistrationTaskResponse pHttpStatus_ =
  StartThingRegistrationTaskResponse'
    { taskId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The bulk thing provisioning task ID.
startThingRegistrationTaskResponse_taskId :: Lens.Lens' StartThingRegistrationTaskResponse (Core.Maybe Core.Text)
startThingRegistrationTaskResponse_taskId = Lens.lens (\StartThingRegistrationTaskResponse' {taskId} -> taskId) (\s@StartThingRegistrationTaskResponse' {} a -> s {taskId = a} :: StartThingRegistrationTaskResponse)

-- | The response's http status code.
startThingRegistrationTaskResponse_httpStatus :: Lens.Lens' StartThingRegistrationTaskResponse Core.Int
startThingRegistrationTaskResponse_httpStatus = Lens.lens (\StartThingRegistrationTaskResponse' {httpStatus} -> httpStatus) (\s@StartThingRegistrationTaskResponse' {} a -> s {httpStatus = a} :: StartThingRegistrationTaskResponse)

instance
  Core.NFData
    StartThingRegistrationTaskResponse
