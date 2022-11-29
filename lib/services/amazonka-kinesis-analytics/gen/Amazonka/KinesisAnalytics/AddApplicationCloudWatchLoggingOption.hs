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
-- Module      : Amazonka.KinesisAnalytics.AddApplicationCloudWatchLoggingOption
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- Adds a CloudWatch log stream to monitor application configuration
-- errors. For more information about using CloudWatch log streams with
-- Amazon Kinesis Analytics applications, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs>.
module Amazonka.KinesisAnalytics.AddApplicationCloudWatchLoggingOption
  ( -- * Creating a Request
    AddApplicationCloudWatchLoggingOption (..),
    newAddApplicationCloudWatchLoggingOption,

    -- * Request Lenses
    addApplicationCloudWatchLoggingOption_applicationName,
    addApplicationCloudWatchLoggingOption_currentApplicationVersionId,
    addApplicationCloudWatchLoggingOption_cloudWatchLoggingOption,

    -- * Destructuring the Response
    AddApplicationCloudWatchLoggingOptionResponse (..),
    newAddApplicationCloudWatchLoggingOptionResponse,

    -- * Response Lenses
    addApplicationCloudWatchLoggingOptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddApplicationCloudWatchLoggingOption' smart constructor.
data AddApplicationCloudWatchLoggingOption = AddApplicationCloudWatchLoggingOption'
  { -- | The Kinesis Analytics application name.
    applicationName :: Prelude.Text,
    -- | The version ID of the Kinesis Analytics application.
    currentApplicationVersionId :: Prelude.Natural,
    -- | Provides the CloudWatch log stream Amazon Resource Name (ARN) and the
    -- IAM role ARN. Note: To write application messages to CloudWatch, the IAM
    -- role that is used must have the @PutLogEvents@ policy action enabled.
    cloudWatchLoggingOption :: CloudWatchLoggingOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddApplicationCloudWatchLoggingOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'addApplicationCloudWatchLoggingOption_applicationName' - The Kinesis Analytics application name.
--
-- 'currentApplicationVersionId', 'addApplicationCloudWatchLoggingOption_currentApplicationVersionId' - The version ID of the Kinesis Analytics application.
--
-- 'cloudWatchLoggingOption', 'addApplicationCloudWatchLoggingOption_cloudWatchLoggingOption' - Provides the CloudWatch log stream Amazon Resource Name (ARN) and the
-- IAM role ARN. Note: To write application messages to CloudWatch, the IAM
-- role that is used must have the @PutLogEvents@ policy action enabled.
newAddApplicationCloudWatchLoggingOption ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'currentApplicationVersionId'
  Prelude.Natural ->
  -- | 'cloudWatchLoggingOption'
  CloudWatchLoggingOption ->
  AddApplicationCloudWatchLoggingOption
newAddApplicationCloudWatchLoggingOption
  pApplicationName_
  pCurrentApplicationVersionId_
  pCloudWatchLoggingOption_ =
    AddApplicationCloudWatchLoggingOption'
      { applicationName =
          pApplicationName_,
        currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        cloudWatchLoggingOption =
          pCloudWatchLoggingOption_
      }

-- | The Kinesis Analytics application name.
addApplicationCloudWatchLoggingOption_applicationName :: Lens.Lens' AddApplicationCloudWatchLoggingOption Prelude.Text
addApplicationCloudWatchLoggingOption_applicationName = Lens.lens (\AddApplicationCloudWatchLoggingOption' {applicationName} -> applicationName) (\s@AddApplicationCloudWatchLoggingOption' {} a -> s {applicationName = a} :: AddApplicationCloudWatchLoggingOption)

-- | The version ID of the Kinesis Analytics application.
addApplicationCloudWatchLoggingOption_currentApplicationVersionId :: Lens.Lens' AddApplicationCloudWatchLoggingOption Prelude.Natural
addApplicationCloudWatchLoggingOption_currentApplicationVersionId = Lens.lens (\AddApplicationCloudWatchLoggingOption' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@AddApplicationCloudWatchLoggingOption' {} a -> s {currentApplicationVersionId = a} :: AddApplicationCloudWatchLoggingOption)

-- | Provides the CloudWatch log stream Amazon Resource Name (ARN) and the
-- IAM role ARN. Note: To write application messages to CloudWatch, the IAM
-- role that is used must have the @PutLogEvents@ policy action enabled.
addApplicationCloudWatchLoggingOption_cloudWatchLoggingOption :: Lens.Lens' AddApplicationCloudWatchLoggingOption CloudWatchLoggingOption
addApplicationCloudWatchLoggingOption_cloudWatchLoggingOption = Lens.lens (\AddApplicationCloudWatchLoggingOption' {cloudWatchLoggingOption} -> cloudWatchLoggingOption) (\s@AddApplicationCloudWatchLoggingOption' {} a -> s {cloudWatchLoggingOption = a} :: AddApplicationCloudWatchLoggingOption)

instance
  Core.AWSRequest
    AddApplicationCloudWatchLoggingOption
  where
  type
    AWSResponse
      AddApplicationCloudWatchLoggingOption =
      AddApplicationCloudWatchLoggingOptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddApplicationCloudWatchLoggingOptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AddApplicationCloudWatchLoggingOption
  where
  hashWithSalt
    _salt
    AddApplicationCloudWatchLoggingOption' {..} =
      _salt `Prelude.hashWithSalt` applicationName
        `Prelude.hashWithSalt` currentApplicationVersionId
        `Prelude.hashWithSalt` cloudWatchLoggingOption

instance
  Prelude.NFData
    AddApplicationCloudWatchLoggingOption
  where
  rnf AddApplicationCloudWatchLoggingOption' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf currentApplicationVersionId
      `Prelude.seq` Prelude.rnf cloudWatchLoggingOption

instance
  Core.ToHeaders
    AddApplicationCloudWatchLoggingOption
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KinesisAnalytics_20150814.AddApplicationCloudWatchLoggingOption" ::
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
    AddApplicationCloudWatchLoggingOption
  where
  toJSON AddApplicationCloudWatchLoggingOption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Core..= applicationName),
            Prelude.Just
              ( "CurrentApplicationVersionId"
                  Core..= currentApplicationVersionId
              ),
            Prelude.Just
              ( "CloudWatchLoggingOption"
                  Core..= cloudWatchLoggingOption
              )
          ]
      )

instance
  Core.ToPath
    AddApplicationCloudWatchLoggingOption
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    AddApplicationCloudWatchLoggingOption
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddApplicationCloudWatchLoggingOptionResponse' smart constructor.
data AddApplicationCloudWatchLoggingOptionResponse = AddApplicationCloudWatchLoggingOptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddApplicationCloudWatchLoggingOptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addApplicationCloudWatchLoggingOptionResponse_httpStatus' - The response's http status code.
newAddApplicationCloudWatchLoggingOptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddApplicationCloudWatchLoggingOptionResponse
newAddApplicationCloudWatchLoggingOptionResponse
  pHttpStatus_ =
    AddApplicationCloudWatchLoggingOptionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
addApplicationCloudWatchLoggingOptionResponse_httpStatus :: Lens.Lens' AddApplicationCloudWatchLoggingOptionResponse Prelude.Int
addApplicationCloudWatchLoggingOptionResponse_httpStatus = Lens.lens (\AddApplicationCloudWatchLoggingOptionResponse' {httpStatus} -> httpStatus) (\s@AddApplicationCloudWatchLoggingOptionResponse' {} a -> s {httpStatus = a} :: AddApplicationCloudWatchLoggingOptionResponse)

instance
  Prelude.NFData
    AddApplicationCloudWatchLoggingOptionResponse
  where
  rnf
    AddApplicationCloudWatchLoggingOptionResponse' {..} =
      Prelude.rnf httpStatus
