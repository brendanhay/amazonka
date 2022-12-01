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
-- Module      : Amazonka.KinesisAnalyticsV2.DescribeApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific Kinesis Data Analytics application.
--
-- If you want to retrieve a list of all applications in your account, use
-- the ListApplications operation.
module Amazonka.KinesisAnalyticsV2.DescribeApplication
  ( -- * Creating a Request
    DescribeApplication (..),
    newDescribeApplication,

    -- * Request Lenses
    describeApplication_includeAdditionalDetails,
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
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeApplication' smart constructor.
data DescribeApplication = DescribeApplication'
  { -- | Displays verbose information about a Kinesis Data Analytics application,
    -- including the application\'s job plan.
    includeAdditionalDetails :: Prelude.Maybe Prelude.Bool,
    -- | The name of the application.
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
-- 'includeAdditionalDetails', 'describeApplication_includeAdditionalDetails' - Displays verbose information about a Kinesis Data Analytics application,
-- including the application\'s job plan.
--
-- 'applicationName', 'describeApplication_applicationName' - The name of the application.
newDescribeApplication ::
  -- | 'applicationName'
  Prelude.Text ->
  DescribeApplication
newDescribeApplication pApplicationName_ =
  DescribeApplication'
    { includeAdditionalDetails =
        Prelude.Nothing,
      applicationName = pApplicationName_
    }

-- | Displays verbose information about a Kinesis Data Analytics application,
-- including the application\'s job plan.
describeApplication_includeAdditionalDetails :: Lens.Lens' DescribeApplication (Prelude.Maybe Prelude.Bool)
describeApplication_includeAdditionalDetails = Lens.lens (\DescribeApplication' {includeAdditionalDetails} -> includeAdditionalDetails) (\s@DescribeApplication' {} a -> s {includeAdditionalDetails = a} :: DescribeApplication)

-- | The name of the application.
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
            Prelude.<*> (x Core..:> "ApplicationDetail")
      )

instance Prelude.Hashable DescribeApplication where
  hashWithSalt _salt DescribeApplication' {..} =
    _salt
      `Prelude.hashWithSalt` includeAdditionalDetails
      `Prelude.hashWithSalt` applicationName

instance Prelude.NFData DescribeApplication where
  rnf DescribeApplication' {..} =
    Prelude.rnf includeAdditionalDetails
      `Prelude.seq` Prelude.rnf applicationName

instance Core.ToHeaders DescribeApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KinesisAnalytics_20180523.DescribeApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeApplication where
  toJSON DescribeApplication' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IncludeAdditionalDetails" Core..=)
              Prelude.<$> includeAdditionalDetails,
            Prelude.Just
              ("ApplicationName" Core..= applicationName)
          ]
      )

instance Core.ToPath DescribeApplication where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeApplicationResponse' smart constructor.
data DescribeApplicationResponse = DescribeApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Provides a description of the application, such as the application\'s
    -- Amazon Resource Name (ARN), status, and latest version.
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
-- 'applicationDetail', 'describeApplicationResponse_applicationDetail' - Provides a description of the application, such as the application\'s
-- Amazon Resource Name (ARN), status, and latest version.
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

-- | Provides a description of the application, such as the application\'s
-- Amazon Resource Name (ARN), status, and latest version.
describeApplicationResponse_applicationDetail :: Lens.Lens' DescribeApplicationResponse ApplicationDetail
describeApplicationResponse_applicationDetail = Lens.lens (\DescribeApplicationResponse' {applicationDetail} -> applicationDetail) (\s@DescribeApplicationResponse' {} a -> s {applicationDetail = a} :: DescribeApplicationResponse)

instance Prelude.NFData DescribeApplicationResponse where
  rnf DescribeApplicationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationDetail
