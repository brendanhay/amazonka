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
-- Module      : Amazonka.LexV2Models.DescribeTestSetDiscrepancyReport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets metadata information about the test set discrepancy report.
module Amazonka.LexV2Models.DescribeTestSetDiscrepancyReport
  ( -- * Creating a Request
    DescribeTestSetDiscrepancyReport (..),
    newDescribeTestSetDiscrepancyReport,

    -- * Request Lenses
    describeTestSetDiscrepancyReport_testSetDiscrepancyReportId,

    -- * Destructuring the Response
    DescribeTestSetDiscrepancyReportResponse (..),
    newDescribeTestSetDiscrepancyReportResponse,

    -- * Response Lenses
    describeTestSetDiscrepancyReportResponse_creationDateTime,
    describeTestSetDiscrepancyReportResponse_failureReasons,
    describeTestSetDiscrepancyReportResponse_lastUpdatedDataTime,
    describeTestSetDiscrepancyReportResponse_target,
    describeTestSetDiscrepancyReportResponse_testSetDiscrepancyRawOutputUrl,
    describeTestSetDiscrepancyReportResponse_testSetDiscrepancyReportId,
    describeTestSetDiscrepancyReportResponse_testSetDiscrepancyReportStatus,
    describeTestSetDiscrepancyReportResponse_testSetDiscrepancyTopErrors,
    describeTestSetDiscrepancyReportResponse_testSetId,
    describeTestSetDiscrepancyReportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTestSetDiscrepancyReport' smart constructor.
data DescribeTestSetDiscrepancyReport = DescribeTestSetDiscrepancyReport'
  { -- | The unique identifier of the test set discrepancy report.
    testSetDiscrepancyReportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTestSetDiscrepancyReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testSetDiscrepancyReportId', 'describeTestSetDiscrepancyReport_testSetDiscrepancyReportId' - The unique identifier of the test set discrepancy report.
newDescribeTestSetDiscrepancyReport ::
  -- | 'testSetDiscrepancyReportId'
  Prelude.Text ->
  DescribeTestSetDiscrepancyReport
newDescribeTestSetDiscrepancyReport
  pTestSetDiscrepancyReportId_ =
    DescribeTestSetDiscrepancyReport'
      { testSetDiscrepancyReportId =
          pTestSetDiscrepancyReportId_
      }

-- | The unique identifier of the test set discrepancy report.
describeTestSetDiscrepancyReport_testSetDiscrepancyReportId :: Lens.Lens' DescribeTestSetDiscrepancyReport Prelude.Text
describeTestSetDiscrepancyReport_testSetDiscrepancyReportId = Lens.lens (\DescribeTestSetDiscrepancyReport' {testSetDiscrepancyReportId} -> testSetDiscrepancyReportId) (\s@DescribeTestSetDiscrepancyReport' {} a -> s {testSetDiscrepancyReportId = a} :: DescribeTestSetDiscrepancyReport)

instance
  Core.AWSRequest
    DescribeTestSetDiscrepancyReport
  where
  type
    AWSResponse DescribeTestSetDiscrepancyReport =
      DescribeTestSetDiscrepancyReportResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTestSetDiscrepancyReportResponse'
            Prelude.<$> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "failureReasons" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "lastUpdatedDataTime")
            Prelude.<*> (x Data..?> "target")
            Prelude.<*> (x Data..?> "testSetDiscrepancyRawOutputUrl")
            Prelude.<*> (x Data..?> "testSetDiscrepancyReportId")
            Prelude.<*> (x Data..?> "testSetDiscrepancyReportStatus")
            Prelude.<*> (x Data..?> "testSetDiscrepancyTopErrors")
            Prelude.<*> (x Data..?> "testSetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTestSetDiscrepancyReport
  where
  hashWithSalt
    _salt
    DescribeTestSetDiscrepancyReport' {..} =
      _salt
        `Prelude.hashWithSalt` testSetDiscrepancyReportId

instance
  Prelude.NFData
    DescribeTestSetDiscrepancyReport
  where
  rnf DescribeTestSetDiscrepancyReport' {..} =
    Prelude.rnf testSetDiscrepancyReportId

instance
  Data.ToHeaders
    DescribeTestSetDiscrepancyReport
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTestSetDiscrepancyReport where
  toPath DescribeTestSetDiscrepancyReport' {..} =
    Prelude.mconcat
      [ "/testsetdiscrepancy/",
        Data.toBS testSetDiscrepancyReportId
      ]

instance
  Data.ToQuery
    DescribeTestSetDiscrepancyReport
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTestSetDiscrepancyReportResponse' smart constructor.
data DescribeTestSetDiscrepancyReportResponse = DescribeTestSetDiscrepancyReportResponse'
  { -- | The time and date of creation for the test set discrepancy report.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The failure report for the test set discrepancy report generation
    -- action.
    failureReasons :: Prelude.Maybe [Prelude.Text],
    -- | The date and time of the last update for the test set discrepancy
    -- report.
    lastUpdatedDataTime :: Prelude.Maybe Data.POSIX,
    -- | The target bot location for the test set discrepancy report.
    target :: Prelude.Maybe TestSetDiscrepancyReportResourceTarget,
    -- | Pre-signed Amazon S3 URL to download the test set discrepancy report.
    testSetDiscrepancyRawOutputUrl :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the test set discrepancy report to describe.
    testSetDiscrepancyReportId :: Prelude.Maybe Prelude.Text,
    -- | The status for the test set discrepancy report.
    testSetDiscrepancyReportStatus :: Prelude.Maybe TestSetDiscrepancyReportStatus,
    -- | The top 200 error results from the test set discrepancy report.
    testSetDiscrepancyTopErrors :: Prelude.Maybe TestSetDiscrepancyErrors,
    -- | The test set Id for the test set discrepancy report.
    testSetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTestSetDiscrepancyReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'describeTestSetDiscrepancyReportResponse_creationDateTime' - The time and date of creation for the test set discrepancy report.
--
-- 'failureReasons', 'describeTestSetDiscrepancyReportResponse_failureReasons' - The failure report for the test set discrepancy report generation
-- action.
--
-- 'lastUpdatedDataTime', 'describeTestSetDiscrepancyReportResponse_lastUpdatedDataTime' - The date and time of the last update for the test set discrepancy
-- report.
--
-- 'target', 'describeTestSetDiscrepancyReportResponse_target' - The target bot location for the test set discrepancy report.
--
-- 'testSetDiscrepancyRawOutputUrl', 'describeTestSetDiscrepancyReportResponse_testSetDiscrepancyRawOutputUrl' - Pre-signed Amazon S3 URL to download the test set discrepancy report.
--
-- 'testSetDiscrepancyReportId', 'describeTestSetDiscrepancyReportResponse_testSetDiscrepancyReportId' - The unique identifier of the test set discrepancy report to describe.
--
-- 'testSetDiscrepancyReportStatus', 'describeTestSetDiscrepancyReportResponse_testSetDiscrepancyReportStatus' - The status for the test set discrepancy report.
--
-- 'testSetDiscrepancyTopErrors', 'describeTestSetDiscrepancyReportResponse_testSetDiscrepancyTopErrors' - The top 200 error results from the test set discrepancy report.
--
-- 'testSetId', 'describeTestSetDiscrepancyReportResponse_testSetId' - The test set Id for the test set discrepancy report.
--
-- 'httpStatus', 'describeTestSetDiscrepancyReportResponse_httpStatus' - The response's http status code.
newDescribeTestSetDiscrepancyReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTestSetDiscrepancyReportResponse
newDescribeTestSetDiscrepancyReportResponse
  pHttpStatus_ =
    DescribeTestSetDiscrepancyReportResponse'
      { creationDateTime =
          Prelude.Nothing,
        failureReasons = Prelude.Nothing,
        lastUpdatedDataTime =
          Prelude.Nothing,
        target = Prelude.Nothing,
        testSetDiscrepancyRawOutputUrl =
          Prelude.Nothing,
        testSetDiscrepancyReportId =
          Prelude.Nothing,
        testSetDiscrepancyReportStatus =
          Prelude.Nothing,
        testSetDiscrepancyTopErrors =
          Prelude.Nothing,
        testSetId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The time and date of creation for the test set discrepancy report.
describeTestSetDiscrepancyReportResponse_creationDateTime :: Lens.Lens' DescribeTestSetDiscrepancyReportResponse (Prelude.Maybe Prelude.UTCTime)
describeTestSetDiscrepancyReportResponse_creationDateTime = Lens.lens (\DescribeTestSetDiscrepancyReportResponse' {creationDateTime} -> creationDateTime) (\s@DescribeTestSetDiscrepancyReportResponse' {} a -> s {creationDateTime = a} :: DescribeTestSetDiscrepancyReportResponse) Prelude.. Lens.mapping Data._Time

-- | The failure report for the test set discrepancy report generation
-- action.
describeTestSetDiscrepancyReportResponse_failureReasons :: Lens.Lens' DescribeTestSetDiscrepancyReportResponse (Prelude.Maybe [Prelude.Text])
describeTestSetDiscrepancyReportResponse_failureReasons = Lens.lens (\DescribeTestSetDiscrepancyReportResponse' {failureReasons} -> failureReasons) (\s@DescribeTestSetDiscrepancyReportResponse' {} a -> s {failureReasons = a} :: DescribeTestSetDiscrepancyReportResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time of the last update for the test set discrepancy
-- report.
describeTestSetDiscrepancyReportResponse_lastUpdatedDataTime :: Lens.Lens' DescribeTestSetDiscrepancyReportResponse (Prelude.Maybe Prelude.UTCTime)
describeTestSetDiscrepancyReportResponse_lastUpdatedDataTime = Lens.lens (\DescribeTestSetDiscrepancyReportResponse' {lastUpdatedDataTime} -> lastUpdatedDataTime) (\s@DescribeTestSetDiscrepancyReportResponse' {} a -> s {lastUpdatedDataTime = a} :: DescribeTestSetDiscrepancyReportResponse) Prelude.. Lens.mapping Data._Time

-- | The target bot location for the test set discrepancy report.
describeTestSetDiscrepancyReportResponse_target :: Lens.Lens' DescribeTestSetDiscrepancyReportResponse (Prelude.Maybe TestSetDiscrepancyReportResourceTarget)
describeTestSetDiscrepancyReportResponse_target = Lens.lens (\DescribeTestSetDiscrepancyReportResponse' {target} -> target) (\s@DescribeTestSetDiscrepancyReportResponse' {} a -> s {target = a} :: DescribeTestSetDiscrepancyReportResponse)

-- | Pre-signed Amazon S3 URL to download the test set discrepancy report.
describeTestSetDiscrepancyReportResponse_testSetDiscrepancyRawOutputUrl :: Lens.Lens' DescribeTestSetDiscrepancyReportResponse (Prelude.Maybe Prelude.Text)
describeTestSetDiscrepancyReportResponse_testSetDiscrepancyRawOutputUrl = Lens.lens (\DescribeTestSetDiscrepancyReportResponse' {testSetDiscrepancyRawOutputUrl} -> testSetDiscrepancyRawOutputUrl) (\s@DescribeTestSetDiscrepancyReportResponse' {} a -> s {testSetDiscrepancyRawOutputUrl = a} :: DescribeTestSetDiscrepancyReportResponse)

-- | The unique identifier of the test set discrepancy report to describe.
describeTestSetDiscrepancyReportResponse_testSetDiscrepancyReportId :: Lens.Lens' DescribeTestSetDiscrepancyReportResponse (Prelude.Maybe Prelude.Text)
describeTestSetDiscrepancyReportResponse_testSetDiscrepancyReportId = Lens.lens (\DescribeTestSetDiscrepancyReportResponse' {testSetDiscrepancyReportId} -> testSetDiscrepancyReportId) (\s@DescribeTestSetDiscrepancyReportResponse' {} a -> s {testSetDiscrepancyReportId = a} :: DescribeTestSetDiscrepancyReportResponse)

-- | The status for the test set discrepancy report.
describeTestSetDiscrepancyReportResponse_testSetDiscrepancyReportStatus :: Lens.Lens' DescribeTestSetDiscrepancyReportResponse (Prelude.Maybe TestSetDiscrepancyReportStatus)
describeTestSetDiscrepancyReportResponse_testSetDiscrepancyReportStatus = Lens.lens (\DescribeTestSetDiscrepancyReportResponse' {testSetDiscrepancyReportStatus} -> testSetDiscrepancyReportStatus) (\s@DescribeTestSetDiscrepancyReportResponse' {} a -> s {testSetDiscrepancyReportStatus = a} :: DescribeTestSetDiscrepancyReportResponse)

-- | The top 200 error results from the test set discrepancy report.
describeTestSetDiscrepancyReportResponse_testSetDiscrepancyTopErrors :: Lens.Lens' DescribeTestSetDiscrepancyReportResponse (Prelude.Maybe TestSetDiscrepancyErrors)
describeTestSetDiscrepancyReportResponse_testSetDiscrepancyTopErrors = Lens.lens (\DescribeTestSetDiscrepancyReportResponse' {testSetDiscrepancyTopErrors} -> testSetDiscrepancyTopErrors) (\s@DescribeTestSetDiscrepancyReportResponse' {} a -> s {testSetDiscrepancyTopErrors = a} :: DescribeTestSetDiscrepancyReportResponse)

-- | The test set Id for the test set discrepancy report.
describeTestSetDiscrepancyReportResponse_testSetId :: Lens.Lens' DescribeTestSetDiscrepancyReportResponse (Prelude.Maybe Prelude.Text)
describeTestSetDiscrepancyReportResponse_testSetId = Lens.lens (\DescribeTestSetDiscrepancyReportResponse' {testSetId} -> testSetId) (\s@DescribeTestSetDiscrepancyReportResponse' {} a -> s {testSetId = a} :: DescribeTestSetDiscrepancyReportResponse)

-- | The response's http status code.
describeTestSetDiscrepancyReportResponse_httpStatus :: Lens.Lens' DescribeTestSetDiscrepancyReportResponse Prelude.Int
describeTestSetDiscrepancyReportResponse_httpStatus = Lens.lens (\DescribeTestSetDiscrepancyReportResponse' {httpStatus} -> httpStatus) (\s@DescribeTestSetDiscrepancyReportResponse' {} a -> s {httpStatus = a} :: DescribeTestSetDiscrepancyReportResponse)

instance
  Prelude.NFData
    DescribeTestSetDiscrepancyReportResponse
  where
  rnf DescribeTestSetDiscrepancyReportResponse' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf failureReasons
      `Prelude.seq` Prelude.rnf lastUpdatedDataTime
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf testSetDiscrepancyRawOutputUrl
      `Prelude.seq` Prelude.rnf testSetDiscrepancyReportId
      `Prelude.seq` Prelude.rnf testSetDiscrepancyReportStatus
      `Prelude.seq` Prelude.rnf testSetDiscrepancyTopErrors
      `Prelude.seq` Prelude.rnf testSetId
      `Prelude.seq` Prelude.rnf httpStatus
