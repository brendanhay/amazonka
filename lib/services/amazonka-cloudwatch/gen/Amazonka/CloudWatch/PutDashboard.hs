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
-- Module      : Amazonka.CloudWatch.PutDashboard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.CloudWatch.PutDashboard
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

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutDashboard' smart constructor.
data PutDashboard = PutDashboard'
  { -- | The name of the dashboard. If a dashboard with this name already exists,
    -- this call modifies that dashboard, replacing its current contents.
    -- Otherwise, a new dashboard is created. The maximum length is 255, and
    -- valid characters are A-Z, a-z, 0-9, \"-\", and \"_\". This parameter is
    -- required.
    dashboardName :: Prelude.Text,
    -- | The detailed information about the dashboard in JSON format, including
    -- the widgets to include and their location on the dashboard. This
    -- parameter is required.
    --
    -- For more information about the syntax, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax>.
    dashboardBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'dashboardBody'
  Prelude.Text ->
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
putDashboard_dashboardName :: Lens.Lens' PutDashboard Prelude.Text
putDashboard_dashboardName = Lens.lens (\PutDashboard' {dashboardName} -> dashboardName) (\s@PutDashboard' {} a -> s {dashboardName = a} :: PutDashboard)

-- | The detailed information about the dashboard in JSON format, including
-- the widgets to include and their location on the dashboard. This
-- parameter is required.
--
-- For more information about the syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax>.
putDashboard_dashboardBody :: Lens.Lens' PutDashboard Prelude.Text
putDashboard_dashboardBody = Lens.lens (\PutDashboard' {dashboardBody} -> dashboardBody) (\s@PutDashboard' {} a -> s {dashboardBody = a} :: PutDashboard)

instance Core.AWSRequest PutDashboard where
  type AWSResponse PutDashboard = PutDashboardResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "PutDashboardResult"
      ( \s h x ->
          PutDashboardResponse'
            Prelude.<$> ( x
                            Data..@? "DashboardValidationMessages"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutDashboard where
  hashWithSalt _salt PutDashboard' {..} =
    _salt
      `Prelude.hashWithSalt` dashboardName
      `Prelude.hashWithSalt` dashboardBody

instance Prelude.NFData PutDashboard where
  rnf PutDashboard' {..} =
    Prelude.rnf dashboardName `Prelude.seq`
      Prelude.rnf dashboardBody

instance Data.ToHeaders PutDashboard where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PutDashboard where
  toPath = Prelude.const "/"

instance Data.ToQuery PutDashboard where
  toQuery PutDashboard' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("PutDashboard" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "DashboardName" Data.=: dashboardName,
        "DashboardBody" Data.=: dashboardBody
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
    dashboardValidationMessages :: Prelude.Maybe [DashboardValidationMessage],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutDashboardResponse
newPutDashboardResponse pHttpStatus_ =
  PutDashboardResponse'
    { dashboardValidationMessages =
        Prelude.Nothing,
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
putDashboardResponse_dashboardValidationMessages :: Lens.Lens' PutDashboardResponse (Prelude.Maybe [DashboardValidationMessage])
putDashboardResponse_dashboardValidationMessages = Lens.lens (\PutDashboardResponse' {dashboardValidationMessages} -> dashboardValidationMessages) (\s@PutDashboardResponse' {} a -> s {dashboardValidationMessages = a} :: PutDashboardResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
putDashboardResponse_httpStatus :: Lens.Lens' PutDashboardResponse Prelude.Int
putDashboardResponse_httpStatus = Lens.lens (\PutDashboardResponse' {httpStatus} -> httpStatus) (\s@PutDashboardResponse' {} a -> s {httpStatus = a} :: PutDashboardResponse)

instance Prelude.NFData PutDashboardResponse where
  rnf PutDashboardResponse' {..} =
    Prelude.rnf dashboardValidationMessages `Prelude.seq`
      Prelude.rnf httpStatus
