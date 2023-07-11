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
-- Module      : Amazonka.IoT.StartThingRegistrationTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a bulk thing provisioning task.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions StartThingRegistrationTask>
-- action.
module Amazonka.IoT.StartThingRegistrationTask
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartThingRegistrationTask' smart constructor.
data StartThingRegistrationTask = StartThingRegistrationTask'
  { -- | The provisioning template.
    templateBody :: Prelude.Text,
    -- | The S3 bucket that contains the input file.
    inputFileBucket :: Prelude.Text,
    -- | The name of input file within the S3 bucket. This file contains a
    -- newline delimited JSON file. Each line contains the parameter values to
    -- provision one device (thing).
    inputFileKey :: Prelude.Text,
    -- | The IAM role ARN that grants permission the input file.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'inputFileBucket'
  Prelude.Text ->
  -- | 'inputFileKey'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
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
startThingRegistrationTask_templateBody :: Lens.Lens' StartThingRegistrationTask Prelude.Text
startThingRegistrationTask_templateBody = Lens.lens (\StartThingRegistrationTask' {templateBody} -> templateBody) (\s@StartThingRegistrationTask' {} a -> s {templateBody = a} :: StartThingRegistrationTask)

-- | The S3 bucket that contains the input file.
startThingRegistrationTask_inputFileBucket :: Lens.Lens' StartThingRegistrationTask Prelude.Text
startThingRegistrationTask_inputFileBucket = Lens.lens (\StartThingRegistrationTask' {inputFileBucket} -> inputFileBucket) (\s@StartThingRegistrationTask' {} a -> s {inputFileBucket = a} :: StartThingRegistrationTask)

-- | The name of input file within the S3 bucket. This file contains a
-- newline delimited JSON file. Each line contains the parameter values to
-- provision one device (thing).
startThingRegistrationTask_inputFileKey :: Lens.Lens' StartThingRegistrationTask Prelude.Text
startThingRegistrationTask_inputFileKey = Lens.lens (\StartThingRegistrationTask' {inputFileKey} -> inputFileKey) (\s@StartThingRegistrationTask' {} a -> s {inputFileKey = a} :: StartThingRegistrationTask)

-- | The IAM role ARN that grants permission the input file.
startThingRegistrationTask_roleArn :: Lens.Lens' StartThingRegistrationTask Prelude.Text
startThingRegistrationTask_roleArn = Lens.lens (\StartThingRegistrationTask' {roleArn} -> roleArn) (\s@StartThingRegistrationTask' {} a -> s {roleArn = a} :: StartThingRegistrationTask)

instance Core.AWSRequest StartThingRegistrationTask where
  type
    AWSResponse StartThingRegistrationTask =
      StartThingRegistrationTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartThingRegistrationTaskResponse'
            Prelude.<$> (x Data..?> "taskId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartThingRegistrationTask where
  hashWithSalt _salt StartThingRegistrationTask' {..} =
    _salt
      `Prelude.hashWithSalt` templateBody
      `Prelude.hashWithSalt` inputFileBucket
      `Prelude.hashWithSalt` inputFileKey
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData StartThingRegistrationTask where
  rnf StartThingRegistrationTask' {..} =
    Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf inputFileBucket
      `Prelude.seq` Prelude.rnf inputFileKey
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders StartThingRegistrationTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StartThingRegistrationTask where
  toJSON StartThingRegistrationTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("templateBody" Data..= templateBody),
            Prelude.Just
              ("inputFileBucket" Data..= inputFileBucket),
            Prelude.Just ("inputFileKey" Data..= inputFileKey),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath StartThingRegistrationTask where
  toPath = Prelude.const "/thing-registration-tasks"

instance Data.ToQuery StartThingRegistrationTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartThingRegistrationTaskResponse' smart constructor.
data StartThingRegistrationTaskResponse = StartThingRegistrationTaskResponse'
  { -- | The bulk thing provisioning task ID.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartThingRegistrationTaskResponse
newStartThingRegistrationTaskResponse pHttpStatus_ =
  StartThingRegistrationTaskResponse'
    { taskId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The bulk thing provisioning task ID.
startThingRegistrationTaskResponse_taskId :: Lens.Lens' StartThingRegistrationTaskResponse (Prelude.Maybe Prelude.Text)
startThingRegistrationTaskResponse_taskId = Lens.lens (\StartThingRegistrationTaskResponse' {taskId} -> taskId) (\s@StartThingRegistrationTaskResponse' {} a -> s {taskId = a} :: StartThingRegistrationTaskResponse)

-- | The response's http status code.
startThingRegistrationTaskResponse_httpStatus :: Lens.Lens' StartThingRegistrationTaskResponse Prelude.Int
startThingRegistrationTaskResponse_httpStatus = Lens.lens (\StartThingRegistrationTaskResponse' {httpStatus} -> httpStatus) (\s@StartThingRegistrationTaskResponse' {} a -> s {httpStatus = a} :: StartThingRegistrationTaskResponse)

instance
  Prelude.NFData
    StartThingRegistrationTaskResponse
  where
  rnf StartThingRegistrationTaskResponse' {..} =
    Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf httpStatus
