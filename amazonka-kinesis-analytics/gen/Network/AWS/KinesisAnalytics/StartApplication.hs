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

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newStartApplication' smart constructor.
data StartApplication = StartApplication'
  { -- | Name of the application.
    applicationName :: Prelude.Text,
    -- | Identifies the specific input, by ID, that the application starts
    -- consuming. Amazon Kinesis Analytics starts reading the streaming source
    -- associated with the input. You can also specify where in the streaming
    -- source you want Amazon Kinesis Analytics to start reading.
    inputConfigurations :: [InputConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  StartApplication
newStartApplication pApplicationName_ =
  StartApplication'
    { applicationName =
        pApplicationName_,
      inputConfigurations = Prelude.mempty
    }

-- | Name of the application.
startApplication_applicationName :: Lens.Lens' StartApplication Prelude.Text
startApplication_applicationName = Lens.lens (\StartApplication' {applicationName} -> applicationName) (\s@StartApplication' {} a -> s {applicationName = a} :: StartApplication)

-- | Identifies the specific input, by ID, that the application starts
-- consuming. Amazon Kinesis Analytics starts reading the streaming source
-- associated with the input. You can also specify where in the streaming
-- source you want Amazon Kinesis Analytics to start reading.
startApplication_inputConfigurations :: Lens.Lens' StartApplication [InputConfiguration]
startApplication_inputConfigurations = Lens.lens (\StartApplication' {inputConfigurations} -> inputConfigurations) (\s@StartApplication' {} a -> s {inputConfigurations = a} :: StartApplication) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest StartApplication where
  type Rs StartApplication = StartApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartApplication

instance Prelude.NFData StartApplication

instance Prelude.ToHeaders StartApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "KinesisAnalytics_20150814.StartApplication" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartApplication where
  toJSON StartApplication' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Prelude..= applicationName),
            Prelude.Just
              ( "InputConfigurations"
                  Prelude..= inputConfigurations
              )
          ]
      )

instance Prelude.ToPath StartApplication where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartApplication where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newStartApplicationResponse' smart constructor.
data StartApplicationResponse = StartApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  StartApplicationResponse
newStartApplicationResponse pHttpStatus_ =
  StartApplicationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startApplicationResponse_httpStatus :: Lens.Lens' StartApplicationResponse Prelude.Int
startApplicationResponse_httpStatus = Lens.lens (\StartApplicationResponse' {httpStatus} -> httpStatus) (\s@StartApplicationResponse' {} a -> s {httpStatus = a} :: StartApplicationResponse)

instance Prelude.NFData StartApplicationResponse
