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
-- Module      : Amazonka.Backup.ListReportJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about your report jobs.
module Amazonka.Backup.ListReportJobs
  ( -- * Creating a Request
    ListReportJobs (..),
    newListReportJobs,

    -- * Request Lenses
    listReportJobs_byCreationAfter,
    listReportJobs_byCreationBefore,
    listReportJobs_byReportPlanName,
    listReportJobs_byStatus,
    listReportJobs_maxResults,
    listReportJobs_nextToken,

    -- * Destructuring the Response
    ListReportJobsResponse (..),
    newListReportJobsResponse,

    -- * Response Lenses
    listReportJobsResponse_nextToken,
    listReportJobsResponse_reportJobs,
    listReportJobsResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReportJobs' smart constructor.
data ListReportJobs = ListReportJobs'
  { -- | Returns only report jobs that were created after the date and time
    -- specified in Unix format and Coordinated Universal Time (UTC). For
    -- example, the value 1516925490 represents Friday, January 26, 2018
    -- 12:11:30 AM.
    byCreationAfter :: Prelude.Maybe Data.POSIX,
    -- | Returns only report jobs that were created before the date and time
    -- specified in Unix format and Coordinated Universal Time (UTC). For
    -- example, the value 1516925490 represents Friday, January 26, 2018
    -- 12:11:30 AM.
    byCreationBefore :: Prelude.Maybe Data.POSIX,
    -- | Returns only report jobs with the specified report plan name.
    byReportPlanName :: Prelude.Maybe Prelude.Text,
    -- | Returns only report jobs that are in the specified status. The statuses
    -- are:
    --
    -- @CREATED | RUNNING | COMPLETED | FAILED@
    byStatus :: Prelude.Maybe Prelude.Text,
    -- | The number of desired results from 1 to 1000. Optional. If unspecified,
    -- the query will return 1 MB of data.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byCreationAfter', 'listReportJobs_byCreationAfter' - Returns only report jobs that were created after the date and time
-- specified in Unix format and Coordinated Universal Time (UTC). For
-- example, the value 1516925490 represents Friday, January 26, 2018
-- 12:11:30 AM.
--
-- 'byCreationBefore', 'listReportJobs_byCreationBefore' - Returns only report jobs that were created before the date and time
-- specified in Unix format and Coordinated Universal Time (UTC). For
-- example, the value 1516925490 represents Friday, January 26, 2018
-- 12:11:30 AM.
--
-- 'byReportPlanName', 'listReportJobs_byReportPlanName' - Returns only report jobs with the specified report plan name.
--
-- 'byStatus', 'listReportJobs_byStatus' - Returns only report jobs that are in the specified status. The statuses
-- are:
--
-- @CREATED | RUNNING | COMPLETED | FAILED@
--
-- 'maxResults', 'listReportJobs_maxResults' - The number of desired results from 1 to 1000. Optional. If unspecified,
-- the query will return 1 MB of data.
--
-- 'nextToken', 'listReportJobs_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
newListReportJobs ::
  ListReportJobs
newListReportJobs =
  ListReportJobs'
    { byCreationAfter = Prelude.Nothing,
      byCreationBefore = Prelude.Nothing,
      byReportPlanName = Prelude.Nothing,
      byStatus = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Returns only report jobs that were created after the date and time
-- specified in Unix format and Coordinated Universal Time (UTC). For
-- example, the value 1516925490 represents Friday, January 26, 2018
-- 12:11:30 AM.
listReportJobs_byCreationAfter :: Lens.Lens' ListReportJobs (Prelude.Maybe Prelude.UTCTime)
listReportJobs_byCreationAfter = Lens.lens (\ListReportJobs' {byCreationAfter} -> byCreationAfter) (\s@ListReportJobs' {} a -> s {byCreationAfter = a} :: ListReportJobs) Prelude.. Lens.mapping Data._Time

-- | Returns only report jobs that were created before the date and time
-- specified in Unix format and Coordinated Universal Time (UTC). For
-- example, the value 1516925490 represents Friday, January 26, 2018
-- 12:11:30 AM.
listReportJobs_byCreationBefore :: Lens.Lens' ListReportJobs (Prelude.Maybe Prelude.UTCTime)
listReportJobs_byCreationBefore = Lens.lens (\ListReportJobs' {byCreationBefore} -> byCreationBefore) (\s@ListReportJobs' {} a -> s {byCreationBefore = a} :: ListReportJobs) Prelude.. Lens.mapping Data._Time

-- | Returns only report jobs with the specified report plan name.
listReportJobs_byReportPlanName :: Lens.Lens' ListReportJobs (Prelude.Maybe Prelude.Text)
listReportJobs_byReportPlanName = Lens.lens (\ListReportJobs' {byReportPlanName} -> byReportPlanName) (\s@ListReportJobs' {} a -> s {byReportPlanName = a} :: ListReportJobs)

-- | Returns only report jobs that are in the specified status. The statuses
-- are:
--
-- @CREATED | RUNNING | COMPLETED | FAILED@
listReportJobs_byStatus :: Lens.Lens' ListReportJobs (Prelude.Maybe Prelude.Text)
listReportJobs_byStatus = Lens.lens (\ListReportJobs' {byStatus} -> byStatus) (\s@ListReportJobs' {} a -> s {byStatus = a} :: ListReportJobs)

-- | The number of desired results from 1 to 1000. Optional. If unspecified,
-- the query will return 1 MB of data.
listReportJobs_maxResults :: Lens.Lens' ListReportJobs (Prelude.Maybe Prelude.Natural)
listReportJobs_maxResults = Lens.lens (\ListReportJobs' {maxResults} -> maxResults) (\s@ListReportJobs' {} a -> s {maxResults = a} :: ListReportJobs)

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listReportJobs_nextToken :: Lens.Lens' ListReportJobs (Prelude.Maybe Prelude.Text)
listReportJobs_nextToken = Lens.lens (\ListReportJobs' {nextToken} -> nextToken) (\s@ListReportJobs' {} a -> s {nextToken = a} :: ListReportJobs)

instance Core.AWSRequest ListReportJobs where
  type
    AWSResponse ListReportJobs =
      ListReportJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReportJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ReportJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReportJobs where
  hashWithSalt _salt ListReportJobs' {..} =
    _salt
      `Prelude.hashWithSalt` byCreationAfter
      `Prelude.hashWithSalt` byCreationBefore
      `Prelude.hashWithSalt` byReportPlanName
      `Prelude.hashWithSalt` byStatus
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListReportJobs where
  rnf ListReportJobs' {..} =
    Prelude.rnf byCreationAfter
      `Prelude.seq` Prelude.rnf byCreationBefore
      `Prelude.seq` Prelude.rnf byReportPlanName
      `Prelude.seq` Prelude.rnf byStatus
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListReportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListReportJobs where
  toPath = Prelude.const "/audit/report-jobs"

instance Data.ToQuery ListReportJobs where
  toQuery ListReportJobs' {..} =
    Prelude.mconcat
      [ "CreationAfter" Data.=: byCreationAfter,
        "CreationBefore" Data.=: byCreationBefore,
        "ReportPlanName" Data.=: byReportPlanName,
        "Status" Data.=: byStatus,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListReportJobsResponse' smart constructor.
data ListReportJobsResponse = ListReportJobsResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Details about your report jobs in JSON format.
    reportJobs :: Prelude.Maybe [ReportJob],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReportJobsResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'reportJobs', 'listReportJobsResponse_reportJobs' - Details about your report jobs in JSON format.
--
-- 'httpStatus', 'listReportJobsResponse_httpStatus' - The response's http status code.
newListReportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReportJobsResponse
newListReportJobsResponse pHttpStatus_ =
  ListReportJobsResponse'
    { nextToken =
        Prelude.Nothing,
      reportJobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listReportJobsResponse_nextToken :: Lens.Lens' ListReportJobsResponse (Prelude.Maybe Prelude.Text)
listReportJobsResponse_nextToken = Lens.lens (\ListReportJobsResponse' {nextToken} -> nextToken) (\s@ListReportJobsResponse' {} a -> s {nextToken = a} :: ListReportJobsResponse)

-- | Details about your report jobs in JSON format.
listReportJobsResponse_reportJobs :: Lens.Lens' ListReportJobsResponse (Prelude.Maybe [ReportJob])
listReportJobsResponse_reportJobs = Lens.lens (\ListReportJobsResponse' {reportJobs} -> reportJobs) (\s@ListReportJobsResponse' {} a -> s {reportJobs = a} :: ListReportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listReportJobsResponse_httpStatus :: Lens.Lens' ListReportJobsResponse Prelude.Int
listReportJobsResponse_httpStatus = Lens.lens (\ListReportJobsResponse' {httpStatus} -> httpStatus) (\s@ListReportJobsResponse' {} a -> s {httpStatus = a} :: ListReportJobsResponse)

instance Prelude.NFData ListReportJobsResponse where
  rnf ListReportJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reportJobs
      `Prelude.seq` Prelude.rnf httpStatus
