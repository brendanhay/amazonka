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
-- Module      : Amazonka.IoTSiteWise.DescribeDashboard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a dashboard.
module Amazonka.IoTSiteWise.DescribeDashboard
  ( -- * Creating a Request
    DescribeDashboard (..),
    newDescribeDashboard,

    -- * Request Lenses
    describeDashboard_dashboardId,

    -- * Destructuring the Response
    DescribeDashboardResponse (..),
    newDescribeDashboardResponse,

    -- * Response Lenses
    describeDashboardResponse_dashboardDescription,
    describeDashboardResponse_httpStatus,
    describeDashboardResponse_dashboardId,
    describeDashboardResponse_dashboardArn,
    describeDashboardResponse_dashboardName,
    describeDashboardResponse_projectId,
    describeDashboardResponse_dashboardDefinition,
    describeDashboardResponse_dashboardCreationDate,
    describeDashboardResponse_dashboardLastUpdateDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDashboard' smart constructor.
data DescribeDashboard = DescribeDashboard'
  { -- | The ID of the dashboard.
    dashboardId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDashboard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardId', 'describeDashboard_dashboardId' - The ID of the dashboard.
newDescribeDashboard ::
  -- | 'dashboardId'
  Prelude.Text ->
  DescribeDashboard
newDescribeDashboard pDashboardId_ =
  DescribeDashboard' {dashboardId = pDashboardId_}

-- | The ID of the dashboard.
describeDashboard_dashboardId :: Lens.Lens' DescribeDashboard Prelude.Text
describeDashboard_dashboardId = Lens.lens (\DescribeDashboard' {dashboardId} -> dashboardId) (\s@DescribeDashboard' {} a -> s {dashboardId = a} :: DescribeDashboard)

instance Core.AWSRequest DescribeDashboard where
  type
    AWSResponse DescribeDashboard =
      DescribeDashboardResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDashboardResponse'
            Prelude.<$> (x Data..?> "dashboardDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "dashboardId")
            Prelude.<*> (x Data..:> "dashboardArn")
            Prelude.<*> (x Data..:> "dashboardName")
            Prelude.<*> (x Data..:> "projectId")
            Prelude.<*> (x Data..:> "dashboardDefinition")
            Prelude.<*> (x Data..:> "dashboardCreationDate")
            Prelude.<*> (x Data..:> "dashboardLastUpdateDate")
      )

instance Prelude.Hashable DescribeDashboard where
  hashWithSalt _salt DescribeDashboard' {..} =
    _salt `Prelude.hashWithSalt` dashboardId

instance Prelude.NFData DescribeDashboard where
  rnf DescribeDashboard' {..} = Prelude.rnf dashboardId

instance Data.ToHeaders DescribeDashboard where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeDashboard where
  toPath DescribeDashboard' {..} =
    Prelude.mconcat
      ["/dashboards/", Data.toBS dashboardId]

instance Data.ToQuery DescribeDashboard where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDashboardResponse' smart constructor.
data DescribeDashboardResponse = DescribeDashboardResponse'
  { -- | The dashboard\'s description.
    dashboardDescription :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the dashboard.
    dashboardId :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the dashboard, which has the following format.
    --
    -- @arn:${Partition}:iotsitewise:${Region}:${Account}:dashboard\/${DashboardId}@
    dashboardArn :: Prelude.Text,
    -- | The name of the dashboard.
    dashboardName :: Prelude.Text,
    -- | The ID of the project that the dashboard is in.
    projectId :: Prelude.Text,
    -- | The dashboard\'s definition JSON literal. For detailed information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/create-dashboards-using-aws-cli.html Creating dashboards (CLI)>
    -- in the /IoT SiteWise User Guide/.
    dashboardDefinition :: Prelude.Text,
    -- | The date the dashboard was created, in Unix epoch time.
    dashboardCreationDate :: Data.POSIX,
    -- | The date the dashboard was last updated, in Unix epoch time.
    dashboardLastUpdateDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDashboardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardDescription', 'describeDashboardResponse_dashboardDescription' - The dashboard\'s description.
--
-- 'httpStatus', 'describeDashboardResponse_httpStatus' - The response's http status code.
--
-- 'dashboardId', 'describeDashboardResponse_dashboardId' - The ID of the dashboard.
--
-- 'dashboardArn', 'describeDashboardResponse_dashboardArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the dashboard, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:dashboard\/${DashboardId}@
--
-- 'dashboardName', 'describeDashboardResponse_dashboardName' - The name of the dashboard.
--
-- 'projectId', 'describeDashboardResponse_projectId' - The ID of the project that the dashboard is in.
--
-- 'dashboardDefinition', 'describeDashboardResponse_dashboardDefinition' - The dashboard\'s definition JSON literal. For detailed information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/create-dashboards-using-aws-cli.html Creating dashboards (CLI)>
-- in the /IoT SiteWise User Guide/.
--
-- 'dashboardCreationDate', 'describeDashboardResponse_dashboardCreationDate' - The date the dashboard was created, in Unix epoch time.
--
-- 'dashboardLastUpdateDate', 'describeDashboardResponse_dashboardLastUpdateDate' - The date the dashboard was last updated, in Unix epoch time.
newDescribeDashboardResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'dashboardId'
  Prelude.Text ->
  -- | 'dashboardArn'
  Prelude.Text ->
  -- | 'dashboardName'
  Prelude.Text ->
  -- | 'projectId'
  Prelude.Text ->
  -- | 'dashboardDefinition'
  Prelude.Text ->
  -- | 'dashboardCreationDate'
  Prelude.UTCTime ->
  -- | 'dashboardLastUpdateDate'
  Prelude.UTCTime ->
  DescribeDashboardResponse
newDescribeDashboardResponse
  pHttpStatus_
  pDashboardId_
  pDashboardArn_
  pDashboardName_
  pProjectId_
  pDashboardDefinition_
  pDashboardCreationDate_
  pDashboardLastUpdateDate_ =
    DescribeDashboardResponse'
      { dashboardDescription =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        dashboardId = pDashboardId_,
        dashboardArn = pDashboardArn_,
        dashboardName = pDashboardName_,
        projectId = pProjectId_,
        dashboardDefinition = pDashboardDefinition_,
        dashboardCreationDate =
          Data._Time Lens.# pDashboardCreationDate_,
        dashboardLastUpdateDate =
          Data._Time Lens.# pDashboardLastUpdateDate_
      }

-- | The dashboard\'s description.
describeDashboardResponse_dashboardDescription :: Lens.Lens' DescribeDashboardResponse (Prelude.Maybe Prelude.Text)
describeDashboardResponse_dashboardDescription = Lens.lens (\DescribeDashboardResponse' {dashboardDescription} -> dashboardDescription) (\s@DescribeDashboardResponse' {} a -> s {dashboardDescription = a} :: DescribeDashboardResponse)

-- | The response's http status code.
describeDashboardResponse_httpStatus :: Lens.Lens' DescribeDashboardResponse Prelude.Int
describeDashboardResponse_httpStatus = Lens.lens (\DescribeDashboardResponse' {httpStatus} -> httpStatus) (\s@DescribeDashboardResponse' {} a -> s {httpStatus = a} :: DescribeDashboardResponse)

-- | The ID of the dashboard.
describeDashboardResponse_dashboardId :: Lens.Lens' DescribeDashboardResponse Prelude.Text
describeDashboardResponse_dashboardId = Lens.lens (\DescribeDashboardResponse' {dashboardId} -> dashboardId) (\s@DescribeDashboardResponse' {} a -> s {dashboardId = a} :: DescribeDashboardResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the dashboard, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:dashboard\/${DashboardId}@
describeDashboardResponse_dashboardArn :: Lens.Lens' DescribeDashboardResponse Prelude.Text
describeDashboardResponse_dashboardArn = Lens.lens (\DescribeDashboardResponse' {dashboardArn} -> dashboardArn) (\s@DescribeDashboardResponse' {} a -> s {dashboardArn = a} :: DescribeDashboardResponse)

-- | The name of the dashboard.
describeDashboardResponse_dashboardName :: Lens.Lens' DescribeDashboardResponse Prelude.Text
describeDashboardResponse_dashboardName = Lens.lens (\DescribeDashboardResponse' {dashboardName} -> dashboardName) (\s@DescribeDashboardResponse' {} a -> s {dashboardName = a} :: DescribeDashboardResponse)

-- | The ID of the project that the dashboard is in.
describeDashboardResponse_projectId :: Lens.Lens' DescribeDashboardResponse Prelude.Text
describeDashboardResponse_projectId = Lens.lens (\DescribeDashboardResponse' {projectId} -> projectId) (\s@DescribeDashboardResponse' {} a -> s {projectId = a} :: DescribeDashboardResponse)

-- | The dashboard\'s definition JSON literal. For detailed information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/create-dashboards-using-aws-cli.html Creating dashboards (CLI)>
-- in the /IoT SiteWise User Guide/.
describeDashboardResponse_dashboardDefinition :: Lens.Lens' DescribeDashboardResponse Prelude.Text
describeDashboardResponse_dashboardDefinition = Lens.lens (\DescribeDashboardResponse' {dashboardDefinition} -> dashboardDefinition) (\s@DescribeDashboardResponse' {} a -> s {dashboardDefinition = a} :: DescribeDashboardResponse)

-- | The date the dashboard was created, in Unix epoch time.
describeDashboardResponse_dashboardCreationDate :: Lens.Lens' DescribeDashboardResponse Prelude.UTCTime
describeDashboardResponse_dashboardCreationDate = Lens.lens (\DescribeDashboardResponse' {dashboardCreationDate} -> dashboardCreationDate) (\s@DescribeDashboardResponse' {} a -> s {dashboardCreationDate = a} :: DescribeDashboardResponse) Prelude.. Data._Time

-- | The date the dashboard was last updated, in Unix epoch time.
describeDashboardResponse_dashboardLastUpdateDate :: Lens.Lens' DescribeDashboardResponse Prelude.UTCTime
describeDashboardResponse_dashboardLastUpdateDate = Lens.lens (\DescribeDashboardResponse' {dashboardLastUpdateDate} -> dashboardLastUpdateDate) (\s@DescribeDashboardResponse' {} a -> s {dashboardLastUpdateDate = a} :: DescribeDashboardResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeDashboardResponse where
  rnf DescribeDashboardResponse' {..} =
    Prelude.rnf dashboardDescription `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf dashboardId `Prelude.seq`
          Prelude.rnf dashboardArn `Prelude.seq`
            Prelude.rnf dashboardName `Prelude.seq`
              Prelude.rnf projectId `Prelude.seq`
                Prelude.rnf dashboardDefinition `Prelude.seq`
                  Prelude.rnf dashboardCreationDate `Prelude.seq`
                    Prelude.rnf dashboardLastUpdateDate
