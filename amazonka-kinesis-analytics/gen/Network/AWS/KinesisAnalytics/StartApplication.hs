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
-- Module      : Network.AWS.KinesisAnalytics.StartApplication
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
-- Starts the specified Amazon Kinesis Analytics application. After
-- creating an application, you must exclusively call this operation to
-- start your application.
--
-- After the application starts, it begins consuming the input data,
-- processes it, and writes the output to the configured destination.
--
-- The application status must be @READY@ for you to start an application.
-- You can get the application status in the console or using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation.
--
-- After you start the application, you can stop the application from
-- processing the input by calling the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_StopApplication.html StopApplication>
-- operation.
--
-- This operation requires permissions to perform the
-- @kinesisanalytics:StartApplication@ action.
module Network.AWS.KinesisAnalytics.StartApplication
  ( -- * Creating a Request
    StartApplication (..),
    newStartApplication,

    -- * Request Lenses
    startApplication_applicationName,
    startApplication_inputConfigurations,

    -- * Destructuring the Response
    StartApplicationResponse (..),
    newStartApplicationResponse,

    -- * Response Lenses
    startApplicationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newStartApplication' smart constructor.
data StartApplication = StartApplication'
  { -- | Name of the application.
    applicationName :: Core.Text,
    -- | Identifies the specific input, by ID, that the application starts
    -- consuming. Amazon Kinesis Analytics starts reading the streaming source
    -- associated with the input. You can also specify where in the streaming
    -- source you want Amazon Kinesis Analytics to start reading.
    inputConfigurations :: [InputConfiguration]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'startApplication_applicationName' - Name of the application.
--
-- 'inputConfigurations', 'startApplication_inputConfigurations' - Identifies the specific input, by ID, that the application starts
-- consuming. Amazon Kinesis Analytics starts reading the streaming source
-- associated with the input. You can also specify where in the streaming
-- source you want Amazon Kinesis Analytics to start reading.
newStartApplication ::
  -- | 'applicationName'
  Core.Text ->
  StartApplication
newStartApplication pApplicationName_ =
  StartApplication'
    { applicationName =
        pApplicationName_,
      inputConfigurations = Core.mempty
    }

-- | Name of the application.
startApplication_applicationName :: Lens.Lens' StartApplication Core.Text
startApplication_applicationName = Lens.lens (\StartApplication' {applicationName} -> applicationName) (\s@StartApplication' {} a -> s {applicationName = a} :: StartApplication)

-- | Identifies the specific input, by ID, that the application starts
-- consuming. Amazon Kinesis Analytics starts reading the streaming source
-- associated with the input. You can also specify where in the streaming
-- source you want Amazon Kinesis Analytics to start reading.
startApplication_inputConfigurations :: Lens.Lens' StartApplication [InputConfiguration]
startApplication_inputConfigurations = Lens.lens (\StartApplication' {inputConfigurations} -> inputConfigurations) (\s@StartApplication' {} a -> s {inputConfigurations = a} :: StartApplication) Core.. Lens._Coerce

instance Core.AWSRequest StartApplication where
  type
    AWSResponse StartApplication =
      StartApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartApplicationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartApplication

instance Core.NFData StartApplication

instance Core.ToHeaders StartApplication where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KinesisAnalytics_20150814.StartApplication" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartApplication where
  toJSON StartApplication' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ApplicationName" Core..= applicationName),
            Core.Just
              ("InputConfigurations" Core..= inputConfigurations)
          ]
      )

instance Core.ToPath StartApplication where
  toPath = Core.const "/"

instance Core.ToQuery StartApplication where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newStartApplicationResponse' smart constructor.
data StartApplicationResponse = StartApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startApplicationResponse_httpStatus' - The response's http status code.
newStartApplicationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartApplicationResponse
newStartApplicationResponse pHttpStatus_ =
  StartApplicationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startApplicationResponse_httpStatus :: Lens.Lens' StartApplicationResponse Core.Int
startApplicationResponse_httpStatus = Lens.lens (\StartApplicationResponse' {httpStatus} -> httpStatus) (\s@StartApplicationResponse' {} a -> s {httpStatus = a} :: StartApplicationResponse)

instance Core.NFData StartApplicationResponse
