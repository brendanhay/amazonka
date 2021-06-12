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
-- Module      : Network.AWS.CloudWatch.PutDashboard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a dashboard if it does not already exist, or updates an existing
-- dashboard. If you update a dashboard, the entire contents are replaced
-- with what you specify here.
--
-- All dashboards in your account are global, not region-specific.
--
-- A simple way to create a dashboard using @PutDashboard@ is to copy an
-- existing dashboard. To copy an existing dashboard using the console, you
-- can load the dashboard and then use the View\/edit source command in the
-- Actions menu to display the JSON block for that dashboard. Another way
-- to copy a dashboard is to use @GetDashboard@, and then use the data
-- returned within @DashboardBody@ as the template for the new dashboard
-- when you call @PutDashboard@.
--
-- When you create a dashboard with @PutDashboard@, a good practice is to
-- add a text widget at the top of the dashboard with a message that the
-- dashboard was created by script and should not be changed in the
-- console. This message could also point console users to the location of
-- the @DashboardBody@ script or the CloudFormation template used to create
-- the dashboard.
module Network.AWS.CloudWatch.PutDashboard
  ( -- * Creating a Request
    PutDashboard (..),
    newPutDashboard,

    -- * Request Lenses
    putDashboard_dashboardName,
    putDashboard_dashboardBody,

    -- * Destructuring the Response
    PutDashboardResponse (..),
    newPutDashboardResponse,

    -- * Response Lenses
    putDashboardResponse_dashboardValidationMessages,
    putDashboardResponse_httpStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutDashboard' smart constructor.
data PutDashboard = PutDashboard'
  { -- | The name of the dashboard. If a dashboard with this name already exists,
    -- this call modifies that dashboard, replacing its current contents.
    -- Otherwise, a new dashboard is created. The maximum length is 255, and
    -- valid characters are A-Z, a-z, 0-9, \"-\", and \"_\". This parameter is
    -- required.
    dashboardName :: Core.Text,
    -- | The detailed information about the dashboard in JSON format, including
    -- the widgets to include and their location on the dashboard. This
    -- parameter is required.
    --
    -- For more information about the syntax, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax>.
    dashboardBody :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutDashboard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardName', 'putDashboard_dashboardName' - The name of the dashboard. If a dashboard with this name already exists,
-- this call modifies that dashboard, replacing its current contents.
-- Otherwise, a new dashboard is created. The maximum length is 255, and
-- valid characters are A-Z, a-z, 0-9, \"-\", and \"_\". This parameter is
-- required.
--
-- 'dashboardBody', 'putDashboard_dashboardBody' - The detailed information about the dashboard in JSON format, including
-- the widgets to include and their location on the dashboard. This
-- parameter is required.
--
-- For more information about the syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax>.
newPutDashboard ::
  -- | 'dashboardName'
  Core.Text ->
  -- | 'dashboardBody'
  Core.Text ->
  PutDashboard
newPutDashboard pDashboardName_ pDashboardBody_ =
  PutDashboard'
    { dashboardName = pDashboardName_,
      dashboardBody = pDashboardBody_
    }

-- | The name of the dashboard. If a dashboard with this name already exists,
-- this call modifies that dashboard, replacing its current contents.
-- Otherwise, a new dashboard is created. The maximum length is 255, and
-- valid characters are A-Z, a-z, 0-9, \"-\", and \"_\". This parameter is
-- required.
putDashboard_dashboardName :: Lens.Lens' PutDashboard Core.Text
putDashboard_dashboardName = Lens.lens (\PutDashboard' {dashboardName} -> dashboardName) (\s@PutDashboard' {} a -> s {dashboardName = a} :: PutDashboard)

-- | The detailed information about the dashboard in JSON format, including
-- the widgets to include and their location on the dashboard. This
-- parameter is required.
--
-- For more information about the syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax>.
putDashboard_dashboardBody :: Lens.Lens' PutDashboard Core.Text
putDashboard_dashboardBody = Lens.lens (\PutDashboard' {dashboardBody} -> dashboardBody) (\s@PutDashboard' {} a -> s {dashboardBody = a} :: PutDashboard)

instance Core.AWSRequest PutDashboard where
  type AWSResponse PutDashboard = PutDashboardResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "PutDashboardResult"
      ( \s h x ->
          PutDashboardResponse'
            Core.<$> ( x Core..@? "DashboardValidationMessages"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutDashboard

instance Core.NFData PutDashboard

instance Core.ToHeaders PutDashboard where
  toHeaders = Core.const Core.mempty

instance Core.ToPath PutDashboard where
  toPath = Core.const "/"

instance Core.ToQuery PutDashboard where
  toQuery PutDashboard' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("PutDashboard" :: Core.ByteString),
        "Version" Core.=: ("2010-08-01" :: Core.ByteString),
        "DashboardName" Core.=: dashboardName,
        "DashboardBody" Core.=: dashboardBody
      ]

-- | /See:/ 'newPutDashboardResponse' smart constructor.
data PutDashboardResponse = PutDashboardResponse'
  { -- | If the input for @PutDashboard@ was correct and the dashboard was
    -- successfully created or modified, this result is empty.
    --
    -- If this result includes only warning messages, then the input was valid
    -- enough for the dashboard to be created or modified, but some elements of
    -- the dashboard might not render.
    --
    -- If this result includes error messages, the input was not valid and the
    -- operation failed.
    dashboardValidationMessages :: Core.Maybe [DashboardValidationMessage],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutDashboardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardValidationMessages', 'putDashboardResponse_dashboardValidationMessages' - If the input for @PutDashboard@ was correct and the dashboard was
-- successfully created or modified, this result is empty.
--
-- If this result includes only warning messages, then the input was valid
-- enough for the dashboard to be created or modified, but some elements of
-- the dashboard might not render.
--
-- If this result includes error messages, the input was not valid and the
-- operation failed.
--
-- 'httpStatus', 'putDashboardResponse_httpStatus' - The response's http status code.
newPutDashboardResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutDashboardResponse
newPutDashboardResponse pHttpStatus_ =
  PutDashboardResponse'
    { dashboardValidationMessages =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the input for @PutDashboard@ was correct and the dashboard was
-- successfully created or modified, this result is empty.
--
-- If this result includes only warning messages, then the input was valid
-- enough for the dashboard to be created or modified, but some elements of
-- the dashboard might not render.
--
-- If this result includes error messages, the input was not valid and the
-- operation failed.
putDashboardResponse_dashboardValidationMessages :: Lens.Lens' PutDashboardResponse (Core.Maybe [DashboardValidationMessage])
putDashboardResponse_dashboardValidationMessages = Lens.lens (\PutDashboardResponse' {dashboardValidationMessages} -> dashboardValidationMessages) (\s@PutDashboardResponse' {} a -> s {dashboardValidationMessages = a} :: PutDashboardResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
putDashboardResponse_httpStatus :: Lens.Lens' PutDashboardResponse Core.Int
putDashboardResponse_httpStatus = Lens.lens (\PutDashboardResponse' {httpStatus} -> httpStatus) (\s@PutDashboardResponse' {} a -> s {httpStatus = a} :: PutDashboardResponse)

instance Core.NFData PutDashboardResponse
