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
-- Module      : Amazonka.KinesisAnalytics.DescribeApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.KinesisAnalytics.DescribeApplication
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeApplication' smart constructor.
data DescribeApplication = DescribeApplication'
  { -- | Name of the application.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeApplication
newDescribeApplication pApplicationName_ =
  DescribeApplication'
    { applicationName =
        pApplicationName_
    }

-- | Name of the application.
describeApplication_applicationName :: Lens.Lens' DescribeApplication Prelude.Text
describeApplication_applicationName = Lens.lens (\DescribeApplication' {applicationName} -> applicationName) (\s@DescribeApplication' {} a -> s {applicationName = a} :: DescribeApplication)

instance Core.AWSRequest DescribeApplication where
  type
    AWSResponse DescribeApplication =
      DescribeApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ApplicationDetail")
      )

instance Prelude.Hashable DescribeApplication where
  hashWithSalt _salt DescribeApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationName

instance Prelude.NFData DescribeApplication where
  rnf DescribeApplication' {..} =
    Prelude.rnf applicationName

instance Data.ToHeaders DescribeApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20150814.DescribeApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeApplication where
  toJSON DescribeApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Data..= applicationName)
          ]
      )

instance Data.ToPath DescribeApplication where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeApplication where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeApplicationResponse' smart constructor.
data DescribeApplicationResponse = DescribeApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Provides a description of the application, such as the application
    -- Amazon Resource Name (ARN), status, latest version, and input and output
    -- configuration details.
    applicationDetail :: ApplicationDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
describeApplicationResponse_httpStatus :: Lens.Lens' DescribeApplicationResponse Prelude.Int
describeApplicationResponse_httpStatus = Lens.lens (\DescribeApplicationResponse' {httpStatus} -> httpStatus) (\s@DescribeApplicationResponse' {} a -> s {httpStatus = a} :: DescribeApplicationResponse)

-- | Provides a description of the application, such as the application
-- Amazon Resource Name (ARN), status, latest version, and input and output
-- configuration details.
describeApplicationResponse_applicationDetail :: Lens.Lens' DescribeApplicationResponse ApplicationDetail
describeApplicationResponse_applicationDetail = Lens.lens (\DescribeApplicationResponse' {applicationDetail} -> applicationDetail) (\s@DescribeApplicationResponse' {} a -> s {applicationDetail = a} :: DescribeApplicationResponse)

instance Prelude.NFData DescribeApplicationResponse where
  rnf DescribeApplicationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationDetail
