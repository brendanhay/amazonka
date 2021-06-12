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
-- Module      : Network.AWS.IoT.ListAuditFindings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the findings (results) of a Device Defender audit or of the audits
-- performed during a specified time period. (Findings are retained for 90
-- days.)
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditFindings
  ( -- * Creating a Request
    ListAuditFindings (..),
    newListAuditFindings,

    -- * Request Lenses
    listAuditFindings_nextToken,
    listAuditFindings_maxResults,
    listAuditFindings_taskId,
    listAuditFindings_startTime,
    listAuditFindings_endTime,
    listAuditFindings_listSuppressedFindings,
    listAuditFindings_resourceIdentifier,
    listAuditFindings_checkName,

    -- * Destructuring the Response
    ListAuditFindingsResponse (..),
    newListAuditFindingsResponse,

    -- * Response Lenses
    listAuditFindingsResponse_nextToken,
    listAuditFindingsResponse_findings,
    listAuditFindingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAuditFindings' smart constructor.
data ListAuditFindings = ListAuditFindings'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter to limit results to the audit with the specified ID. You must
    -- specify either the taskId or the startTime and endTime, but not both.
    taskId :: Core.Maybe Core.Text,
    -- | A filter to limit results to those found after the specified time. You
    -- must specify either the startTime and endTime or the taskId, but not
    -- both.
    startTime :: Core.Maybe Core.POSIX,
    -- | A filter to limit results to those found before the specified time. You
    -- must specify either the startTime and endTime or the taskId, but not
    -- both.
    endTime :: Core.Maybe Core.POSIX,
    -- | Boolean flag indicating whether only the suppressed findings or the
    -- unsuppressed findings should be listed. If this parameter isn\'t
    -- provided, the response will list both suppressed and unsuppressed
    -- findings.
    listSuppressedFindings :: Core.Maybe Core.Bool,
    -- | Information identifying the noncompliant resource.
    resourceIdentifier :: Core.Maybe ResourceIdentifier,
    -- | A filter to limit results to the findings for the specified audit check.
    checkName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAuditFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAuditFindings_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listAuditFindings_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- 'taskId', 'listAuditFindings_taskId' - A filter to limit results to the audit with the specified ID. You must
-- specify either the taskId or the startTime and endTime, but not both.
--
-- 'startTime', 'listAuditFindings_startTime' - A filter to limit results to those found after the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
--
-- 'endTime', 'listAuditFindings_endTime' - A filter to limit results to those found before the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
--
-- 'listSuppressedFindings', 'listAuditFindings_listSuppressedFindings' - Boolean flag indicating whether only the suppressed findings or the
-- unsuppressed findings should be listed. If this parameter isn\'t
-- provided, the response will list both suppressed and unsuppressed
-- findings.
--
-- 'resourceIdentifier', 'listAuditFindings_resourceIdentifier' - Information identifying the noncompliant resource.
--
-- 'checkName', 'listAuditFindings_checkName' - A filter to limit results to the findings for the specified audit check.
newListAuditFindings ::
  ListAuditFindings
newListAuditFindings =
  ListAuditFindings'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      taskId = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      listSuppressedFindings = Core.Nothing,
      resourceIdentifier = Core.Nothing,
      checkName = Core.Nothing
    }

-- | The token for the next set of results.
listAuditFindings_nextToken :: Lens.Lens' ListAuditFindings (Core.Maybe Core.Text)
listAuditFindings_nextToken = Lens.lens (\ListAuditFindings' {nextToken} -> nextToken) (\s@ListAuditFindings' {} a -> s {nextToken = a} :: ListAuditFindings)

-- | The maximum number of results to return at one time. The default is 25.
listAuditFindings_maxResults :: Lens.Lens' ListAuditFindings (Core.Maybe Core.Natural)
listAuditFindings_maxResults = Lens.lens (\ListAuditFindings' {maxResults} -> maxResults) (\s@ListAuditFindings' {} a -> s {maxResults = a} :: ListAuditFindings)

-- | A filter to limit results to the audit with the specified ID. You must
-- specify either the taskId or the startTime and endTime, but not both.
listAuditFindings_taskId :: Lens.Lens' ListAuditFindings (Core.Maybe Core.Text)
listAuditFindings_taskId = Lens.lens (\ListAuditFindings' {taskId} -> taskId) (\s@ListAuditFindings' {} a -> s {taskId = a} :: ListAuditFindings)

-- | A filter to limit results to those found after the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
listAuditFindings_startTime :: Lens.Lens' ListAuditFindings (Core.Maybe Core.UTCTime)
listAuditFindings_startTime = Lens.lens (\ListAuditFindings' {startTime} -> startTime) (\s@ListAuditFindings' {} a -> s {startTime = a} :: ListAuditFindings) Core.. Lens.mapping Core._Time

-- | A filter to limit results to those found before the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
listAuditFindings_endTime :: Lens.Lens' ListAuditFindings (Core.Maybe Core.UTCTime)
listAuditFindings_endTime = Lens.lens (\ListAuditFindings' {endTime} -> endTime) (\s@ListAuditFindings' {} a -> s {endTime = a} :: ListAuditFindings) Core.. Lens.mapping Core._Time

-- | Boolean flag indicating whether only the suppressed findings or the
-- unsuppressed findings should be listed. If this parameter isn\'t
-- provided, the response will list both suppressed and unsuppressed
-- findings.
listAuditFindings_listSuppressedFindings :: Lens.Lens' ListAuditFindings (Core.Maybe Core.Bool)
listAuditFindings_listSuppressedFindings = Lens.lens (\ListAuditFindings' {listSuppressedFindings} -> listSuppressedFindings) (\s@ListAuditFindings' {} a -> s {listSuppressedFindings = a} :: ListAuditFindings)

-- | Information identifying the noncompliant resource.
listAuditFindings_resourceIdentifier :: Lens.Lens' ListAuditFindings (Core.Maybe ResourceIdentifier)
listAuditFindings_resourceIdentifier = Lens.lens (\ListAuditFindings' {resourceIdentifier} -> resourceIdentifier) (\s@ListAuditFindings' {} a -> s {resourceIdentifier = a} :: ListAuditFindings)

-- | A filter to limit results to the findings for the specified audit check.
listAuditFindings_checkName :: Lens.Lens' ListAuditFindings (Core.Maybe Core.Text)
listAuditFindings_checkName = Lens.lens (\ListAuditFindings' {checkName} -> checkName) (\s@ListAuditFindings' {} a -> s {checkName = a} :: ListAuditFindings)

instance Core.AWSPager ListAuditFindings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAuditFindingsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAuditFindingsResponse_findings Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAuditFindings_nextToken
          Lens..~ rs
          Lens.^? listAuditFindingsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListAuditFindings where
  type
    AWSResponse ListAuditFindings =
      ListAuditFindingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAuditFindingsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "findings" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAuditFindings

instance Core.NFData ListAuditFindings

instance Core.ToHeaders ListAuditFindings where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON ListAuditFindings where
  toJSON ListAuditFindings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("taskId" Core..=) Core.<$> taskId,
            ("startTime" Core..=) Core.<$> startTime,
            ("endTime" Core..=) Core.<$> endTime,
            ("listSuppressedFindings" Core..=)
              Core.<$> listSuppressedFindings,
            ("resourceIdentifier" Core..=)
              Core.<$> resourceIdentifier,
            ("checkName" Core..=) Core.<$> checkName
          ]
      )

instance Core.ToPath ListAuditFindings where
  toPath = Core.const "/audit/findings"

instance Core.ToQuery ListAuditFindings where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAuditFindingsResponse' smart constructor.
data ListAuditFindingsResponse = ListAuditFindingsResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The findings (results) of the audit.
    findings :: Core.Maybe [AuditFinding],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAuditFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAuditFindingsResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'findings', 'listAuditFindingsResponse_findings' - The findings (results) of the audit.
--
-- 'httpStatus', 'listAuditFindingsResponse_httpStatus' - The response's http status code.
newListAuditFindingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAuditFindingsResponse
newListAuditFindingsResponse pHttpStatus_ =
  ListAuditFindingsResponse'
    { nextToken =
        Core.Nothing,
      findings = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listAuditFindingsResponse_nextToken :: Lens.Lens' ListAuditFindingsResponse (Core.Maybe Core.Text)
listAuditFindingsResponse_nextToken = Lens.lens (\ListAuditFindingsResponse' {nextToken} -> nextToken) (\s@ListAuditFindingsResponse' {} a -> s {nextToken = a} :: ListAuditFindingsResponse)

-- | The findings (results) of the audit.
listAuditFindingsResponse_findings :: Lens.Lens' ListAuditFindingsResponse (Core.Maybe [AuditFinding])
listAuditFindingsResponse_findings = Lens.lens (\ListAuditFindingsResponse' {findings} -> findings) (\s@ListAuditFindingsResponse' {} a -> s {findings = a} :: ListAuditFindingsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAuditFindingsResponse_httpStatus :: Lens.Lens' ListAuditFindingsResponse Core.Int
listAuditFindingsResponse_httpStatus = Lens.lens (\ListAuditFindingsResponse' {httpStatus} -> httpStatus) (\s@ListAuditFindingsResponse' {} a -> s {httpStatus = a} :: ListAuditFindingsResponse)

instance Core.NFData ListAuditFindingsResponse
