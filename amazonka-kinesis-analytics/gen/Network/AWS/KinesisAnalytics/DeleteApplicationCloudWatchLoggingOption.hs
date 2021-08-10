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
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplicationCloudWatchLoggingOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This documentation is for version 1 of the Amazon Kinesis Data Analytics
-- API, which only supports SQL applications. Version 2 of the API supports
-- SQL and Java applications. For more information about version 2, see
-- </kinesisanalytics/latest/apiv2/Welcome.html Amazon Kinesis Data Analytics API V2 Documentation>.
--
-- Deletes a CloudWatch log stream from an application. For more
-- information about using CloudWatch log streams with Amazon Kinesis
-- Analytics applications, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs>.
module Network.AWS.KinesisAnalytics.DeleteApplicationCloudWatchLoggingOption
  ( -- * Creating a Request
    DeleteApplicationCloudWatchLoggingOption (..),
    newDeleteApplicationCloudWatchLoggingOption,

    -- * Request Lenses
    deleteApplicationCloudWatchLoggingOption_applicationName,
    deleteApplicationCloudWatchLoggingOption_currentApplicationVersionId,
    deleteApplicationCloudWatchLoggingOption_cloudWatchLoggingOptionId,

    -- * Destructuring the Response
    DeleteApplicationCloudWatchLoggingOptionResponse (..),
    newDeleteApplicationCloudWatchLoggingOptionResponse,

    -- * Response Lenses
    deleteApplicationCloudWatchLoggingOptionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteApplicationCloudWatchLoggingOption' smart constructor.
data DeleteApplicationCloudWatchLoggingOption = DeleteApplicationCloudWatchLoggingOption'
  { -- | The Kinesis Analytics application name.
    applicationName :: Prelude.Text,
    -- | The version ID of the Kinesis Analytics application.
    currentApplicationVersionId :: Prelude.Natural,
    -- | The @CloudWatchLoggingOptionId@ of the CloudWatch logging option to
    -- delete. You can get the @CloudWatchLoggingOptionId@ by using the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
    -- operation.
    cloudWatchLoggingOptionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationCloudWatchLoggingOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'deleteApplicationCloudWatchLoggingOption_applicationName' - The Kinesis Analytics application name.
--
-- 'currentApplicationVersionId', 'deleteApplicationCloudWatchLoggingOption_currentApplicationVersionId' - The version ID of the Kinesis Analytics application.
--
-- 'cloudWatchLoggingOptionId', 'deleteApplicationCloudWatchLoggingOption_cloudWatchLoggingOptionId' - The @CloudWatchLoggingOptionId@ of the CloudWatch logging option to
-- delete. You can get the @CloudWatchLoggingOptionId@ by using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation.
newDeleteApplicationCloudWatchLoggingOption ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'currentApplicationVersionId'
  Prelude.Natural ->
  -- | 'cloudWatchLoggingOptionId'
  Prelude.Text ->
  DeleteApplicationCloudWatchLoggingOption
newDeleteApplicationCloudWatchLoggingOption
  pApplicationName_
  pCurrentApplicationVersionId_
  pCloudWatchLoggingOptionId_ =
    DeleteApplicationCloudWatchLoggingOption'
      { applicationName =
          pApplicationName_,
        currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        cloudWatchLoggingOptionId =
          pCloudWatchLoggingOptionId_
      }

-- | The Kinesis Analytics application name.
deleteApplicationCloudWatchLoggingOption_applicationName :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption Prelude.Text
deleteApplicationCloudWatchLoggingOption_applicationName = Lens.lens (\DeleteApplicationCloudWatchLoggingOption' {applicationName} -> applicationName) (\s@DeleteApplicationCloudWatchLoggingOption' {} a -> s {applicationName = a} :: DeleteApplicationCloudWatchLoggingOption)

-- | The version ID of the Kinesis Analytics application.
deleteApplicationCloudWatchLoggingOption_currentApplicationVersionId :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption Prelude.Natural
deleteApplicationCloudWatchLoggingOption_currentApplicationVersionId = Lens.lens (\DeleteApplicationCloudWatchLoggingOption' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@DeleteApplicationCloudWatchLoggingOption' {} a -> s {currentApplicationVersionId = a} :: DeleteApplicationCloudWatchLoggingOption)

-- | The @CloudWatchLoggingOptionId@ of the CloudWatch logging option to
-- delete. You can get the @CloudWatchLoggingOptionId@ by using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation.
deleteApplicationCloudWatchLoggingOption_cloudWatchLoggingOptionId :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption Prelude.Text
deleteApplicationCloudWatchLoggingOption_cloudWatchLoggingOptionId = Lens.lens (\DeleteApplicationCloudWatchLoggingOption' {cloudWatchLoggingOptionId} -> cloudWatchLoggingOptionId) (\s@DeleteApplicationCloudWatchLoggingOption' {} a -> s {cloudWatchLoggingOptionId = a} :: DeleteApplicationCloudWatchLoggingOption)

instance
  Core.AWSRequest
    DeleteApplicationCloudWatchLoggingOption
  where
  type
    AWSResponse
      DeleteApplicationCloudWatchLoggingOption =
      DeleteApplicationCloudWatchLoggingOptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApplicationCloudWatchLoggingOptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteApplicationCloudWatchLoggingOption

instance
  Prelude.NFData
    DeleteApplicationCloudWatchLoggingOption

instance
  Core.ToHeaders
    DeleteApplicationCloudWatchLoggingOption
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KinesisAnalytics_20150814.DeleteApplicationCloudWatchLoggingOption" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DeleteApplicationCloudWatchLoggingOption
  where
  toJSON DeleteApplicationCloudWatchLoggingOption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Core..= applicationName),
            Prelude.Just
              ( "CurrentApplicationVersionId"
                  Core..= currentApplicationVersionId
              ),
            Prelude.Just
              ( "CloudWatchLoggingOptionId"
                  Core..= cloudWatchLoggingOptionId
              )
          ]
      )

instance
  Core.ToPath
    DeleteApplicationCloudWatchLoggingOption
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeleteApplicationCloudWatchLoggingOption
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApplicationCloudWatchLoggingOptionResponse' smart constructor.
data DeleteApplicationCloudWatchLoggingOptionResponse = DeleteApplicationCloudWatchLoggingOptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationCloudWatchLoggingOptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApplicationCloudWatchLoggingOptionResponse_httpStatus' - The response's http status code.
newDeleteApplicationCloudWatchLoggingOptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApplicationCloudWatchLoggingOptionResponse
newDeleteApplicationCloudWatchLoggingOptionResponse
  pHttpStatus_ =
    DeleteApplicationCloudWatchLoggingOptionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteApplicationCloudWatchLoggingOptionResponse_httpStatus :: Lens.Lens' DeleteApplicationCloudWatchLoggingOptionResponse Prelude.Int
deleteApplicationCloudWatchLoggingOptionResponse_httpStatus = Lens.lens (\DeleteApplicationCloudWatchLoggingOptionResponse' {httpStatus} -> httpStatus) (\s@DeleteApplicationCloudWatchLoggingOptionResponse' {} a -> s {httpStatus = a} :: DeleteApplicationCloudWatchLoggingOptionResponse)

instance
  Prelude.NFData
    DeleteApplicationCloudWatchLoggingOptionResponse
