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
-- Module      : Amazonka.CloudWatch.GetDashboard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the details of the dashboard that you specify.
--
-- To copy an existing dashboard, use @GetDashboard@, and then use the data
-- returned within @DashboardBody@ as the template for the new dashboard
-- when you call @PutDashboard@ to create the copy.
module Amazonka.CloudWatch.GetDashboard
  ( -- * Creating a Request
    GetDashboard (..),
    newGetDashboard,

    -- * Request Lenses
    getDashboard_dashboardName,

    -- * Destructuring the Response
    GetDashboardResponse (..),
    newGetDashboardResponse,

    -- * Response Lenses
    getDashboardResponse_dashboardArn,
    getDashboardResponse_dashboardBody,
    getDashboardResponse_dashboardName,
    getDashboardResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDashboard' smart constructor.
data GetDashboard = GetDashboard'
  { -- | The name of the dashboard to be described.
    dashboardName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDashboard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardName', 'getDashboard_dashboardName' - The name of the dashboard to be described.
newGetDashboard ::
  -- | 'dashboardName'
  Prelude.Text ->
  GetDashboard
newGetDashboard pDashboardName_ =
  GetDashboard' {dashboardName = pDashboardName_}

-- | The name of the dashboard to be described.
getDashboard_dashboardName :: Lens.Lens' GetDashboard Prelude.Text
getDashboard_dashboardName = Lens.lens (\GetDashboard' {dashboardName} -> dashboardName) (\s@GetDashboard' {} a -> s {dashboardName = a} :: GetDashboard)

instance Core.AWSRequest GetDashboard where
  type AWSResponse GetDashboard = GetDashboardResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetDashboardResult"
      ( \s h x ->
          GetDashboardResponse'
            Prelude.<$> (x Data..@? "DashboardArn")
            Prelude.<*> (x Data..@? "DashboardBody")
            Prelude.<*> (x Data..@? "DashboardName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDashboard where
  hashWithSalt _salt GetDashboard' {..} =
    _salt `Prelude.hashWithSalt` dashboardName

instance Prelude.NFData GetDashboard where
  rnf GetDashboard' {..} = Prelude.rnf dashboardName

instance Data.ToHeaders GetDashboard where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetDashboard where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDashboard where
  toQuery GetDashboard' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetDashboard" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "DashboardName" Data.=: dashboardName
      ]

-- | /See:/ 'newGetDashboardResponse' smart constructor.
data GetDashboardResponse = GetDashboardResponse'
  { -- | The Amazon Resource Name (ARN) of the dashboard.
    dashboardArn :: Prelude.Maybe Prelude.Text,
    -- | The detailed information about the dashboard, including what widgets are
    -- included and their location on the dashboard. For more information about
    -- the @DashboardBody@ syntax, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax>.
    dashboardBody :: Prelude.Maybe Prelude.Text,
    -- | The name of the dashboard.
    dashboardName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDashboardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardArn', 'getDashboardResponse_dashboardArn' - The Amazon Resource Name (ARN) of the dashboard.
--
-- 'dashboardBody', 'getDashboardResponse_dashboardBody' - The detailed information about the dashboard, including what widgets are
-- included and their location on the dashboard. For more information about
-- the @DashboardBody@ syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax>.
--
-- 'dashboardName', 'getDashboardResponse_dashboardName' - The name of the dashboard.
--
-- 'httpStatus', 'getDashboardResponse_httpStatus' - The response's http status code.
newGetDashboardResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDashboardResponse
newGetDashboardResponse pHttpStatus_ =
  GetDashboardResponse'
    { dashboardArn =
        Prelude.Nothing,
      dashboardBody = Prelude.Nothing,
      dashboardName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the dashboard.
getDashboardResponse_dashboardArn :: Lens.Lens' GetDashboardResponse (Prelude.Maybe Prelude.Text)
getDashboardResponse_dashboardArn = Lens.lens (\GetDashboardResponse' {dashboardArn} -> dashboardArn) (\s@GetDashboardResponse' {} a -> s {dashboardArn = a} :: GetDashboardResponse)

-- | The detailed information about the dashboard, including what widgets are
-- included and their location on the dashboard. For more information about
-- the @DashboardBody@ syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax>.
getDashboardResponse_dashboardBody :: Lens.Lens' GetDashboardResponse (Prelude.Maybe Prelude.Text)
getDashboardResponse_dashboardBody = Lens.lens (\GetDashboardResponse' {dashboardBody} -> dashboardBody) (\s@GetDashboardResponse' {} a -> s {dashboardBody = a} :: GetDashboardResponse)

-- | The name of the dashboard.
getDashboardResponse_dashboardName :: Lens.Lens' GetDashboardResponse (Prelude.Maybe Prelude.Text)
getDashboardResponse_dashboardName = Lens.lens (\GetDashboardResponse' {dashboardName} -> dashboardName) (\s@GetDashboardResponse' {} a -> s {dashboardName = a} :: GetDashboardResponse)

-- | The response's http status code.
getDashboardResponse_httpStatus :: Lens.Lens' GetDashboardResponse Prelude.Int
getDashboardResponse_httpStatus = Lens.lens (\GetDashboardResponse' {httpStatus} -> httpStatus) (\s@GetDashboardResponse' {} a -> s {httpStatus = a} :: GetDashboardResponse)

instance Prelude.NFData GetDashboardResponse where
  rnf GetDashboardResponse' {..} =
    Prelude.rnf dashboardArn `Prelude.seq`
      Prelude.rnf dashboardBody `Prelude.seq`
        Prelude.rnf dashboardName `Prelude.seq`
          Prelude.rnf httpStatus
