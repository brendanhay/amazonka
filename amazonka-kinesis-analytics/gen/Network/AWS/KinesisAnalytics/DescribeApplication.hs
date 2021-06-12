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
-- Module      : Network.AWS.KinesisAnalytics.DescribeApplication
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
-- Returns information about a specific Amazon Kinesis Analytics
-- application.
--
-- If you want to retrieve a list of all applications in your account, use
-- the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_ListApplications.html ListApplications>
-- operation.
--
-- This operation requires permissions to perform the
-- @kinesisanalytics:DescribeApplication@ action. You can use
-- @DescribeApplication@ to get the current application versionId, which
-- you need to call other operations such as @Update@.
module Network.AWS.KinesisAnalytics.DescribeApplication
  ( -- * Creating a Request
    DescribeApplication (..),
    newDescribeApplication,

    -- * Request Lenses
    describeApplication_applicationName,

    -- * Destructuring the Response
    DescribeApplicationResponse (..),
    newDescribeApplicationResponse,

    -- * Response Lenses
    describeApplicationResponse_httpStatus,
    describeApplicationResponse_applicationDetail,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeApplication' smart constructor.
data DescribeApplication = DescribeApplication'
  { -- | Name of the application.
    applicationName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'describeApplication_applicationName' - Name of the application.
newDescribeApplication ::
  -- | 'applicationName'
  Core.Text ->
  DescribeApplication
newDescribeApplication pApplicationName_ =
  DescribeApplication'
    { applicationName =
        pApplicationName_
    }

-- | Name of the application.
describeApplication_applicationName :: Lens.Lens' DescribeApplication Core.Text
describeApplication_applicationName = Lens.lens (\DescribeApplication' {applicationName} -> applicationName) (\s@DescribeApplication' {} a -> s {applicationName = a} :: DescribeApplication)

instance Core.AWSRequest DescribeApplication where
  type
    AWSResponse DescribeApplication =
      DescribeApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeApplicationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "ApplicationDetail")
      )

instance Core.Hashable DescribeApplication

instance Core.NFData DescribeApplication

instance Core.ToHeaders DescribeApplication where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KinesisAnalytics_20150814.DescribeApplication" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeApplication where
  toJSON DescribeApplication' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ApplicationName" Core..= applicationName)
          ]
      )

instance Core.ToPath DescribeApplication where
  toPath = Core.const "/"

instance Core.ToQuery DescribeApplication where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribeApplicationResponse' smart constructor.
data DescribeApplicationResponse = DescribeApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Provides a description of the application, such as the application
    -- Amazon Resource Name (ARN), status, latest version, and input and output
    -- configuration details.
    applicationDetail :: ApplicationDetail
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeApplicationResponse_httpStatus' - The response's http status code.
--
-- 'applicationDetail', 'describeApplicationResponse_applicationDetail' - Provides a description of the application, such as the application
-- Amazon Resource Name (ARN), status, latest version, and input and output
-- configuration details.
newDescribeApplicationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'applicationDetail'
  ApplicationDetail ->
  DescribeApplicationResponse
newDescribeApplicationResponse
  pHttpStatus_
  pApplicationDetail_ =
    DescribeApplicationResponse'
      { httpStatus =
          pHttpStatus_,
        applicationDetail = pApplicationDetail_
      }

-- | The response's http status code.
describeApplicationResponse_httpStatus :: Lens.Lens' DescribeApplicationResponse Core.Int
describeApplicationResponse_httpStatus = Lens.lens (\DescribeApplicationResponse' {httpStatus} -> httpStatus) (\s@DescribeApplicationResponse' {} a -> s {httpStatus = a} :: DescribeApplicationResponse)

-- | Provides a description of the application, such as the application
-- Amazon Resource Name (ARN), status, latest version, and input and output
-- configuration details.
describeApplicationResponse_applicationDetail :: Lens.Lens' DescribeApplicationResponse ApplicationDetail
describeApplicationResponse_applicationDetail = Lens.lens (\DescribeApplicationResponse' {applicationDetail} -> applicationDetail) (\s@DescribeApplicationResponse' {} a -> s {applicationDetail = a} :: DescribeApplicationResponse)

instance Core.NFData DescribeApplicationResponse
