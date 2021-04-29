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
-- Module      : Network.AWS.CloudWatch.GetDashboard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the details of the dashboard that you specify.
--
-- To copy an existing dashboard, use @GetDashboard@, and then use the data
-- returned within @DashboardBody@ as the template for the new dashboard
-- when you call @PutDashboard@ to create the copy.
module Network.AWS.CloudWatch.GetDashboard
  ( -- * Creating a Request
    GetDashboard (..),
    newGetDashboard,

    -- * Request Lenses
    getDashboard_dashboardName,

    -- * Destructuring the Response
    GetDashboardResponse (..),
    newGetDashboardResponse,

    -- * Response Lenses
    getDashboardResponse_dashboardBody,
    getDashboardResponse_dashboardArn,
    getDashboardResponse_dashboardName,
    getDashboardResponse_httpStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDashboard' smart constructor.
data GetDashboard = GetDashboard'
  { -- | The name of the dashboard to be described.
    dashboardName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetDashboard where
  type Rs GetDashboard = GetDashboardResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetDashboardResult"
      ( \s h x ->
          GetDashboardResponse'
            Prelude.<$> (x Prelude..@? "DashboardBody")
            Prelude.<*> (x Prelude..@? "DashboardArn")
            Prelude.<*> (x Prelude..@? "DashboardName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDashboard

instance Prelude.NFData GetDashboard

instance Prelude.ToHeaders GetDashboard where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetDashboard where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetDashboard where
  toQuery GetDashboard' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("GetDashboard" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-08-01" :: Prelude.ByteString),
        "DashboardName" Prelude.=: dashboardName
      ]

-- | /See:/ 'newGetDashboardResponse' smart constructor.
data GetDashboardResponse = GetDashboardResponse'
  { -- | The detailed information about the dashboard, including what widgets are
    -- included and their location on the dashboard. For more information about
    -- the @DashboardBody@ syntax, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax>.
    dashboardBody :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dashboard.
    dashboardArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the dashboard.
    dashboardName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetDashboardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardBody', 'getDashboardResponse_dashboardBody' - The detailed information about the dashboard, including what widgets are
-- included and their location on the dashboard. For more information about
-- the @DashboardBody@ syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax>.
--
-- 'dashboardArn', 'getDashboardResponse_dashboardArn' - The Amazon Resource Name (ARN) of the dashboard.
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
    { dashboardBody =
        Prelude.Nothing,
      dashboardArn = Prelude.Nothing,
      dashboardName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detailed information about the dashboard, including what widgets are
-- included and their location on the dashboard. For more information about
-- the @DashboardBody@ syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax>.
getDashboardResponse_dashboardBody :: Lens.Lens' GetDashboardResponse (Prelude.Maybe Prelude.Text)
getDashboardResponse_dashboardBody = Lens.lens (\GetDashboardResponse' {dashboardBody} -> dashboardBody) (\s@GetDashboardResponse' {} a -> s {dashboardBody = a} :: GetDashboardResponse)

-- | The Amazon Resource Name (ARN) of the dashboard.
getDashboardResponse_dashboardArn :: Lens.Lens' GetDashboardResponse (Prelude.Maybe Prelude.Text)
getDashboardResponse_dashboardArn = Lens.lens (\GetDashboardResponse' {dashboardArn} -> dashboardArn) (\s@GetDashboardResponse' {} a -> s {dashboardArn = a} :: GetDashboardResponse)

-- | The name of the dashboard.
getDashboardResponse_dashboardName :: Lens.Lens' GetDashboardResponse (Prelude.Maybe Prelude.Text)
getDashboardResponse_dashboardName = Lens.lens (\GetDashboardResponse' {dashboardName} -> dashboardName) (\s@GetDashboardResponse' {} a -> s {dashboardName = a} :: GetDashboardResponse)

-- | The response's http status code.
getDashboardResponse_httpStatus :: Lens.Lens' GetDashboardResponse Prelude.Int
getDashboardResponse_httpStatus = Lens.lens (\GetDashboardResponse' {httpStatus} -> httpStatus) (\s@GetDashboardResponse' {} a -> s {httpStatus = a} :: GetDashboardResponse)

instance Prelude.NFData GetDashboardResponse
