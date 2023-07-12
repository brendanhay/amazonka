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
-- Module      : Amazonka.Backup.DescribeReportPlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all report plans for an Amazon Web Services account
-- and Amazon Web Services Region.
module Amazonka.Backup.DescribeReportPlan
  ( -- * Creating a Request
    DescribeReportPlan (..),
    newDescribeReportPlan,

    -- * Request Lenses
    describeReportPlan_reportPlanName,

    -- * Destructuring the Response
    DescribeReportPlanResponse (..),
    newDescribeReportPlanResponse,

    -- * Response Lenses
    describeReportPlanResponse_reportPlan,
    describeReportPlanResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeReportPlan' smart constructor.
data DescribeReportPlan = DescribeReportPlan'
  { -- | The unique name of a report plan.
    reportPlanName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReportPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportPlanName', 'describeReportPlan_reportPlanName' - The unique name of a report plan.
newDescribeReportPlan ::
  -- | 'reportPlanName'
  Prelude.Text ->
  DescribeReportPlan
newDescribeReportPlan pReportPlanName_ =
  DescribeReportPlan'
    { reportPlanName =
        pReportPlanName_
    }

-- | The unique name of a report plan.
describeReportPlan_reportPlanName :: Lens.Lens' DescribeReportPlan Prelude.Text
describeReportPlan_reportPlanName = Lens.lens (\DescribeReportPlan' {reportPlanName} -> reportPlanName) (\s@DescribeReportPlan' {} a -> s {reportPlanName = a} :: DescribeReportPlan)

instance Core.AWSRequest DescribeReportPlan where
  type
    AWSResponse DescribeReportPlan =
      DescribeReportPlanResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReportPlanResponse'
            Prelude.<$> (x Data..?> "ReportPlan")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReportPlan where
  hashWithSalt _salt DescribeReportPlan' {..} =
    _salt `Prelude.hashWithSalt` reportPlanName

instance Prelude.NFData DescribeReportPlan where
  rnf DescribeReportPlan' {..} =
    Prelude.rnf reportPlanName

instance Data.ToHeaders DescribeReportPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeReportPlan where
  toPath DescribeReportPlan' {..} =
    Prelude.mconcat
      ["/audit/report-plans/", Data.toBS reportPlanName]

instance Data.ToQuery DescribeReportPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeReportPlanResponse' smart constructor.
data DescribeReportPlanResponse = DescribeReportPlanResponse'
  { -- | Returns details about the report plan that is specified by its name.
    -- These details include the report plan\'s Amazon Resource Name (ARN),
    -- description, settings, delivery channel, deployment status, creation
    -- time, and last attempted and successful run times.
    reportPlan :: Prelude.Maybe ReportPlan,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReportPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportPlan', 'describeReportPlanResponse_reportPlan' - Returns details about the report plan that is specified by its name.
-- These details include the report plan\'s Amazon Resource Name (ARN),
-- description, settings, delivery channel, deployment status, creation
-- time, and last attempted and successful run times.
--
-- 'httpStatus', 'describeReportPlanResponse_httpStatus' - The response's http status code.
newDescribeReportPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReportPlanResponse
newDescribeReportPlanResponse pHttpStatus_ =
  DescribeReportPlanResponse'
    { reportPlan =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns details about the report plan that is specified by its name.
-- These details include the report plan\'s Amazon Resource Name (ARN),
-- description, settings, delivery channel, deployment status, creation
-- time, and last attempted and successful run times.
describeReportPlanResponse_reportPlan :: Lens.Lens' DescribeReportPlanResponse (Prelude.Maybe ReportPlan)
describeReportPlanResponse_reportPlan = Lens.lens (\DescribeReportPlanResponse' {reportPlan} -> reportPlan) (\s@DescribeReportPlanResponse' {} a -> s {reportPlan = a} :: DescribeReportPlanResponse)

-- | The response's http status code.
describeReportPlanResponse_httpStatus :: Lens.Lens' DescribeReportPlanResponse Prelude.Int
describeReportPlanResponse_httpStatus = Lens.lens (\DescribeReportPlanResponse' {httpStatus} -> httpStatus) (\s@DescribeReportPlanResponse' {} a -> s {httpStatus = a} :: DescribeReportPlanResponse)

instance Prelude.NFData DescribeReportPlanResponse where
  rnf DescribeReportPlanResponse' {..} =
    Prelude.rnf reportPlan
      `Prelude.seq` Prelude.rnf httpStatus
