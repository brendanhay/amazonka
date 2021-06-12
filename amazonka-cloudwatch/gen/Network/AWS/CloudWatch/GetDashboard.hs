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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDashboard' smart constructor.
data GetDashboard = GetDashboard'
  { -- | The name of the dashboard to be described.
    dashboardName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetDashboard
newGetDashboard pDashboardName_ =
  GetDashboard' {dashboardName = pDashboardName_}

-- | The name of the dashboard to be described.
getDashboard_dashboardName :: Lens.Lens' GetDashboard Core.Text
getDashboard_dashboardName = Lens.lens (\GetDashboard' {dashboardName} -> dashboardName) (\s@GetDashboard' {} a -> s {dashboardName = a} :: GetDashboard)

instance Core.AWSRequest GetDashboard where
  type AWSResponse GetDashboard = GetDashboardResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetDashboardResult"
      ( \s h x ->
          GetDashboardResponse'
            Core.<$> (x Core..@? "DashboardBody")
            Core.<*> (x Core..@? "DashboardArn")
            Core.<*> (x Core..@? "DashboardName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDashboard

instance Core.NFData GetDashboard

instance Core.ToHeaders GetDashboard where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetDashboard where
  toPath = Core.const "/"

instance Core.ToQuery GetDashboard where
  toQuery GetDashboard' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetDashboard" :: Core.ByteString),
        "Version" Core.=: ("2010-08-01" :: Core.ByteString),
        "DashboardName" Core.=: dashboardName
      ]

-- | /See:/ 'newGetDashboardResponse' smart constructor.
data GetDashboardResponse = GetDashboardResponse'
  { -- | The detailed information about the dashboard, including what widgets are
    -- included and their location on the dashboard. For more information about
    -- the @DashboardBody@ syntax, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax>.
    dashboardBody :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the dashboard.
    dashboardArn :: Core.Maybe Core.Text,
    -- | The name of the dashboard.
    dashboardName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetDashboardResponse
newGetDashboardResponse pHttpStatus_ =
  GetDashboardResponse'
    { dashboardBody = Core.Nothing,
      dashboardArn = Core.Nothing,
      dashboardName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detailed information about the dashboard, including what widgets are
-- included and their location on the dashboard. For more information about
-- the @DashboardBody@ syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax>.
getDashboardResponse_dashboardBody :: Lens.Lens' GetDashboardResponse (Core.Maybe Core.Text)
getDashboardResponse_dashboardBody = Lens.lens (\GetDashboardResponse' {dashboardBody} -> dashboardBody) (\s@GetDashboardResponse' {} a -> s {dashboardBody = a} :: GetDashboardResponse)

-- | The Amazon Resource Name (ARN) of the dashboard.
getDashboardResponse_dashboardArn :: Lens.Lens' GetDashboardResponse (Core.Maybe Core.Text)
getDashboardResponse_dashboardArn = Lens.lens (\GetDashboardResponse' {dashboardArn} -> dashboardArn) (\s@GetDashboardResponse' {} a -> s {dashboardArn = a} :: GetDashboardResponse)

-- | The name of the dashboard.
getDashboardResponse_dashboardName :: Lens.Lens' GetDashboardResponse (Core.Maybe Core.Text)
getDashboardResponse_dashboardName = Lens.lens (\GetDashboardResponse' {dashboardName} -> dashboardName) (\s@GetDashboardResponse' {} a -> s {dashboardName = a} :: GetDashboardResponse)

-- | The response's http status code.
getDashboardResponse_httpStatus :: Lens.Lens' GetDashboardResponse Core.Int
getDashboardResponse_httpStatus = Lens.lens (\GetDashboardResponse' {httpStatus} -> httpStatus) (\s@GetDashboardResponse' {} a -> s {httpStatus = a} :: GetDashboardResponse)

instance Core.NFData GetDashboardResponse
