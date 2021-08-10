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
-- Module      : Network.AWS.Discovery.StartImportTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an import task, which allows you to import details of your
-- on-premises environment directly into AWS Migration Hub without having
-- to use the Application Discovery Service (ADS) tools such as the
-- Discovery Connector or Discovery Agent. This gives you the option to
-- perform migration assessment and planning directly from your imported
-- data, including the ability to group your devices as applications and
-- track their migration status.
--
-- To start an import request, do this:
--
-- 1.  Download the specially formatted comma separated value (CSV) import
--     template, which you can find here:
--     <https://s3-us-west-2.amazonaws.com/templates-7cffcf56-bd96-4b1c-b45b-a5b42f282e46/import_template.csv>.
--
-- 2.  Fill out the template with your server and application data.
--
-- 3.  Upload your import file to an Amazon S3 bucket, and make a note of
--     it\'s Object URL. Your import file must be in the CSV format.
--
-- 4.  Use the console or the @StartImportTask@ command with the AWS CLI or
--     one of the AWS SDKs to import the records from your file.
--
-- For more information, including step-by-step procedures, see
-- <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-import.html Migration Hub Import>
-- in the /AWS Application Discovery Service User Guide/.
--
-- There are limits to the number of import tasks you can create (and
-- delete) in an AWS account. For more information, see
-- <https://docs.aws.amazon.com/application-discovery/latest/userguide/ads_service_limits.html AWS Application Discovery Service Limits>
-- in the /AWS Application Discovery Service User Guide/.
module Network.AWS.Discovery.StartImportTask
  ( -- * Creating a Request
    StartImportTask (..),
    newStartImportTask,

    -- * Request Lenses
    startImportTask_clientRequestToken,
    startImportTask_name,
    startImportTask_importUrl,

    -- * Destructuring the Response
    StartImportTaskResponse (..),
    newStartImportTaskResponse,

    -- * Response Lenses
    startImportTaskResponse_task,
    startImportTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartImportTask' smart constructor.
data StartImportTask = StartImportTask'
  { -- | Optional. A unique token that you can provide to prevent the same import
    -- request from occurring more than once. If you don\'t provide a token, a
    -- token is automatically generated.
    --
    -- Sending more than one @StartImportTask@ request with the same client
    -- request token will return information about the original import task
    -- with that client request token.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | A descriptive name for this request. You can use this name to filter
    -- future requests related to this import task, such as identifying
    -- applications and servers that were included in this import task. We
    -- recommend that you use a meaningful name for each import task.
    name :: Prelude.Text,
    -- | The URL for your import file that you\'ve uploaded to Amazon S3.
    --
    -- If you\'re using the AWS CLI, this URL is structured as follows:
    -- @s3:\/\/BucketName\/ImportFileName.CSV@
    importUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startImportTask_clientRequestToken' - Optional. A unique token that you can provide to prevent the same import
-- request from occurring more than once. If you don\'t provide a token, a
-- token is automatically generated.
--
-- Sending more than one @StartImportTask@ request with the same client
-- request token will return information about the original import task
-- with that client request token.
--
-- 'name', 'startImportTask_name' - A descriptive name for this request. You can use this name to filter
-- future requests related to this import task, such as identifying
-- applications and servers that were included in this import task. We
-- recommend that you use a meaningful name for each import task.
--
-- 'importUrl', 'startImportTask_importUrl' - The URL for your import file that you\'ve uploaded to Amazon S3.
--
-- If you\'re using the AWS CLI, this URL is structured as follows:
-- @s3:\/\/BucketName\/ImportFileName.CSV@
newStartImportTask ::
  -- | 'name'
  Prelude.Text ->
  -- | 'importUrl'
  Prelude.Text ->
  StartImportTask
newStartImportTask pName_ pImportUrl_ =
  StartImportTask'
    { clientRequestToken =
        Prelude.Nothing,
      name = pName_,
      importUrl = pImportUrl_
    }

-- | Optional. A unique token that you can provide to prevent the same import
-- request from occurring more than once. If you don\'t provide a token, a
-- token is automatically generated.
--
-- Sending more than one @StartImportTask@ request with the same client
-- request token will return information about the original import task
-- with that client request token.
startImportTask_clientRequestToken :: Lens.Lens' StartImportTask (Prelude.Maybe Prelude.Text)
startImportTask_clientRequestToken = Lens.lens (\StartImportTask' {clientRequestToken} -> clientRequestToken) (\s@StartImportTask' {} a -> s {clientRequestToken = a} :: StartImportTask)

-- | A descriptive name for this request. You can use this name to filter
-- future requests related to this import task, such as identifying
-- applications and servers that were included in this import task. We
-- recommend that you use a meaningful name for each import task.
startImportTask_name :: Lens.Lens' StartImportTask Prelude.Text
startImportTask_name = Lens.lens (\StartImportTask' {name} -> name) (\s@StartImportTask' {} a -> s {name = a} :: StartImportTask)

-- | The URL for your import file that you\'ve uploaded to Amazon S3.
--
-- If you\'re using the AWS CLI, this URL is structured as follows:
-- @s3:\/\/BucketName\/ImportFileName.CSV@
startImportTask_importUrl :: Lens.Lens' StartImportTask Prelude.Text
startImportTask_importUrl = Lens.lens (\StartImportTask' {importUrl} -> importUrl) (\s@StartImportTask' {} a -> s {importUrl = a} :: StartImportTask)

instance Core.AWSRequest StartImportTask where
  type
    AWSResponse StartImportTask =
      StartImportTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImportTaskResponse'
            Prelude.<$> (x Core..?> "task")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartImportTask

instance Prelude.NFData StartImportTask

instance Core.ToHeaders StartImportTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.StartImportTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartImportTask where
  toJSON StartImportTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("importUrl" Core..= importUrl)
          ]
      )

instance Core.ToPath StartImportTask where
  toPath = Prelude.const "/"

instance Core.ToQuery StartImportTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartImportTaskResponse' smart constructor.
data StartImportTaskResponse = StartImportTaskResponse'
  { -- | An array of information related to the import task request including
    -- status information, times, IDs, the Amazon S3 Object URL for the import
    -- file, and more.
    task :: Prelude.Maybe ImportTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'task', 'startImportTaskResponse_task' - An array of information related to the import task request including
-- status information, times, IDs, the Amazon S3 Object URL for the import
-- file, and more.
--
-- 'httpStatus', 'startImportTaskResponse_httpStatus' - The response's http status code.
newStartImportTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartImportTaskResponse
newStartImportTaskResponse pHttpStatus_ =
  StartImportTaskResponse'
    { task = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of information related to the import task request including
-- status information, times, IDs, the Amazon S3 Object URL for the import
-- file, and more.
startImportTaskResponse_task :: Lens.Lens' StartImportTaskResponse (Prelude.Maybe ImportTask)
startImportTaskResponse_task = Lens.lens (\StartImportTaskResponse' {task} -> task) (\s@StartImportTaskResponse' {} a -> s {task = a} :: StartImportTaskResponse)

-- | The response's http status code.
startImportTaskResponse_httpStatus :: Lens.Lens' StartImportTaskResponse Prelude.Int
startImportTaskResponse_httpStatus = Lens.lens (\StartImportTaskResponse' {httpStatus} -> httpStatus) (\s@StartImportTaskResponse' {} a -> s {httpStatus = a} :: StartImportTaskResponse)

instance Prelude.NFData StartImportTaskResponse
