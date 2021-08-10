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
-- Module      : Network.AWS.IoT.ListThingRegistrationTaskReports
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about the thing registration tasks.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingRegistrationTaskReports
  ( -- * Creating a Request
    ListThingRegistrationTaskReports (..),
    newListThingRegistrationTaskReports,

    -- * Request Lenses
    listThingRegistrationTaskReports_nextToken,
    listThingRegistrationTaskReports_maxResults,
    listThingRegistrationTaskReports_taskId,
    listThingRegistrationTaskReports_reportType,

    -- * Destructuring the Response
    ListThingRegistrationTaskReportsResponse (..),
    newListThingRegistrationTaskReportsResponse,

    -- * Response Lenses
    listThingRegistrationTaskReportsResponse_nextToken,
    listThingRegistrationTaskReportsResponse_reportType,
    listThingRegistrationTaskReportsResponse_resourceLinks,
    listThingRegistrationTaskReportsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListThingRegistrationTaskReports' smart constructor.
data ListThingRegistrationTaskReports = ListThingRegistrationTaskReports'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The id of the task.
    taskId :: Prelude.Text,
    -- | The type of task report.
    reportType :: ReportType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThingRegistrationTaskReports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingRegistrationTaskReports_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listThingRegistrationTaskReports_maxResults' - The maximum number of results to return per request.
--
-- 'taskId', 'listThingRegistrationTaskReports_taskId' - The id of the task.
--
-- 'reportType', 'listThingRegistrationTaskReports_reportType' - The type of task report.
newListThingRegistrationTaskReports ::
  -- | 'taskId'
  Prelude.Text ->
  -- | 'reportType'
  ReportType ->
  ListThingRegistrationTaskReports
newListThingRegistrationTaskReports
  pTaskId_
  pReportType_ =
    ListThingRegistrationTaskReports'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        taskId = pTaskId_,
        reportType = pReportType_
      }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listThingRegistrationTaskReports_nextToken :: Lens.Lens' ListThingRegistrationTaskReports (Prelude.Maybe Prelude.Text)
listThingRegistrationTaskReports_nextToken = Lens.lens (\ListThingRegistrationTaskReports' {nextToken} -> nextToken) (\s@ListThingRegistrationTaskReports' {} a -> s {nextToken = a} :: ListThingRegistrationTaskReports)

-- | The maximum number of results to return per request.
listThingRegistrationTaskReports_maxResults :: Lens.Lens' ListThingRegistrationTaskReports (Prelude.Maybe Prelude.Natural)
listThingRegistrationTaskReports_maxResults = Lens.lens (\ListThingRegistrationTaskReports' {maxResults} -> maxResults) (\s@ListThingRegistrationTaskReports' {} a -> s {maxResults = a} :: ListThingRegistrationTaskReports)

-- | The id of the task.
listThingRegistrationTaskReports_taskId :: Lens.Lens' ListThingRegistrationTaskReports Prelude.Text
listThingRegistrationTaskReports_taskId = Lens.lens (\ListThingRegistrationTaskReports' {taskId} -> taskId) (\s@ListThingRegistrationTaskReports' {} a -> s {taskId = a} :: ListThingRegistrationTaskReports)

-- | The type of task report.
listThingRegistrationTaskReports_reportType :: Lens.Lens' ListThingRegistrationTaskReports ReportType
listThingRegistrationTaskReports_reportType = Lens.lens (\ListThingRegistrationTaskReports' {reportType} -> reportType) (\s@ListThingRegistrationTaskReports' {} a -> s {reportType = a} :: ListThingRegistrationTaskReports)

instance
  Core.AWSPager
    ListThingRegistrationTaskReports
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThingRegistrationTaskReportsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listThingRegistrationTaskReportsResponse_resourceLinks
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listThingRegistrationTaskReports_nextToken
          Lens..~ rs
          Lens.^? listThingRegistrationTaskReportsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListThingRegistrationTaskReports
  where
  type
    AWSResponse ListThingRegistrationTaskReports =
      ListThingRegistrationTaskReportsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingRegistrationTaskReportsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "reportType")
            Prelude.<*> (x Core..?> "resourceLinks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListThingRegistrationTaskReports

instance
  Prelude.NFData
    ListThingRegistrationTaskReports

instance
  Core.ToHeaders
    ListThingRegistrationTaskReports
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListThingRegistrationTaskReports where
  toPath ListThingRegistrationTaskReports' {..} =
    Prelude.mconcat
      [ "/thing-registration-tasks/",
        Core.toBS taskId,
        "/reports"
      ]

instance
  Core.ToQuery
    ListThingRegistrationTaskReports
  where
  toQuery ListThingRegistrationTaskReports' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "reportType" Core.=: reportType
      ]

-- | /See:/ 'newListThingRegistrationTaskReportsResponse' smart constructor.
data ListThingRegistrationTaskReportsResponse = ListThingRegistrationTaskReportsResponse'
  { -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of task report.
    reportType :: Prelude.Maybe ReportType,
    -- | Links to the task resources.
    resourceLinks :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThingRegistrationTaskReportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingRegistrationTaskReportsResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'reportType', 'listThingRegistrationTaskReportsResponse_reportType' - The type of task report.
--
-- 'resourceLinks', 'listThingRegistrationTaskReportsResponse_resourceLinks' - Links to the task resources.
--
-- 'httpStatus', 'listThingRegistrationTaskReportsResponse_httpStatus' - The response's http status code.
newListThingRegistrationTaskReportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListThingRegistrationTaskReportsResponse
newListThingRegistrationTaskReportsResponse
  pHttpStatus_ =
    ListThingRegistrationTaskReportsResponse'
      { nextToken =
          Prelude.Nothing,
        reportType = Prelude.Nothing,
        resourceLinks = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listThingRegistrationTaskReportsResponse_nextToken :: Lens.Lens' ListThingRegistrationTaskReportsResponse (Prelude.Maybe Prelude.Text)
listThingRegistrationTaskReportsResponse_nextToken = Lens.lens (\ListThingRegistrationTaskReportsResponse' {nextToken} -> nextToken) (\s@ListThingRegistrationTaskReportsResponse' {} a -> s {nextToken = a} :: ListThingRegistrationTaskReportsResponse)

-- | The type of task report.
listThingRegistrationTaskReportsResponse_reportType :: Lens.Lens' ListThingRegistrationTaskReportsResponse (Prelude.Maybe ReportType)
listThingRegistrationTaskReportsResponse_reportType = Lens.lens (\ListThingRegistrationTaskReportsResponse' {reportType} -> reportType) (\s@ListThingRegistrationTaskReportsResponse' {} a -> s {reportType = a} :: ListThingRegistrationTaskReportsResponse)

-- | Links to the task resources.
listThingRegistrationTaskReportsResponse_resourceLinks :: Lens.Lens' ListThingRegistrationTaskReportsResponse (Prelude.Maybe [Prelude.Text])
listThingRegistrationTaskReportsResponse_resourceLinks = Lens.lens (\ListThingRegistrationTaskReportsResponse' {resourceLinks} -> resourceLinks) (\s@ListThingRegistrationTaskReportsResponse' {} a -> s {resourceLinks = a} :: ListThingRegistrationTaskReportsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listThingRegistrationTaskReportsResponse_httpStatus :: Lens.Lens' ListThingRegistrationTaskReportsResponse Prelude.Int
listThingRegistrationTaskReportsResponse_httpStatus = Lens.lens (\ListThingRegistrationTaskReportsResponse' {httpStatus} -> httpStatus) (\s@ListThingRegistrationTaskReportsResponse' {} a -> s {httpStatus = a} :: ListThingRegistrationTaskReportsResponse)

instance
  Prelude.NFData
    ListThingRegistrationTaskReportsResponse
