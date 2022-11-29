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
-- Module      : Amazonka.IoT.ListAuditFindings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the findings (results) of a Device Defender audit or of the audits
-- performed during a specified time period. (Findings are retained for 90
-- days.)
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListAuditFindings>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListAuditFindings
  ( -- * Creating a Request
    ListAuditFindings (..),
    newListAuditFindings,

    -- * Request Lenses
    listAuditFindings_nextToken,
    listAuditFindings_taskId,
    listAuditFindings_checkName,
    listAuditFindings_endTime,
    listAuditFindings_resourceIdentifier,
    listAuditFindings_maxResults,
    listAuditFindings_listSuppressedFindings,
    listAuditFindings_startTime,

    -- * Destructuring the Response
    ListAuditFindingsResponse (..),
    newListAuditFindingsResponse,

    -- * Response Lenses
    listAuditFindingsResponse_findings,
    listAuditFindingsResponse_nextToken,
    listAuditFindingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAuditFindings' smart constructor.
data ListAuditFindings = ListAuditFindings'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter to limit results to the audit with the specified ID. You must
    -- specify either the taskId or the startTime and endTime, but not both.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | A filter to limit results to the findings for the specified audit check.
    checkName :: Prelude.Maybe Prelude.Text,
    -- | A filter to limit results to those found before the specified time. You
    -- must specify either the startTime and endTime or the taskId, but not
    -- both.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | Information identifying the noncompliant resource.
    resourceIdentifier :: Prelude.Maybe ResourceIdentifier,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Boolean flag indicating whether only the suppressed findings or the
    -- unsuppressed findings should be listed. If this parameter isn\'t
    -- provided, the response will list both suppressed and unsuppressed
    -- findings.
    listSuppressedFindings :: Prelude.Maybe Prelude.Bool,
    -- | A filter to limit results to those found after the specified time. You
    -- must specify either the startTime and endTime or the taskId, but not
    -- both.
    startTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'taskId', 'listAuditFindings_taskId' - A filter to limit results to the audit with the specified ID. You must
-- specify either the taskId or the startTime and endTime, but not both.
--
-- 'checkName', 'listAuditFindings_checkName' - A filter to limit results to the findings for the specified audit check.
--
-- 'endTime', 'listAuditFindings_endTime' - A filter to limit results to those found before the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
--
-- 'resourceIdentifier', 'listAuditFindings_resourceIdentifier' - Information identifying the noncompliant resource.
--
-- 'maxResults', 'listAuditFindings_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- 'listSuppressedFindings', 'listAuditFindings_listSuppressedFindings' - Boolean flag indicating whether only the suppressed findings or the
-- unsuppressed findings should be listed. If this parameter isn\'t
-- provided, the response will list both suppressed and unsuppressed
-- findings.
--
-- 'startTime', 'listAuditFindings_startTime' - A filter to limit results to those found after the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
newListAuditFindings ::
  ListAuditFindings
newListAuditFindings =
  ListAuditFindings'
    { nextToken = Prelude.Nothing,
      taskId = Prelude.Nothing,
      checkName = Prelude.Nothing,
      endTime = Prelude.Nothing,
      resourceIdentifier = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      listSuppressedFindings = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The token for the next set of results.
listAuditFindings_nextToken :: Lens.Lens' ListAuditFindings (Prelude.Maybe Prelude.Text)
listAuditFindings_nextToken = Lens.lens (\ListAuditFindings' {nextToken} -> nextToken) (\s@ListAuditFindings' {} a -> s {nextToken = a} :: ListAuditFindings)

-- | A filter to limit results to the audit with the specified ID. You must
-- specify either the taskId or the startTime and endTime, but not both.
listAuditFindings_taskId :: Lens.Lens' ListAuditFindings (Prelude.Maybe Prelude.Text)
listAuditFindings_taskId = Lens.lens (\ListAuditFindings' {taskId} -> taskId) (\s@ListAuditFindings' {} a -> s {taskId = a} :: ListAuditFindings)

-- | A filter to limit results to the findings for the specified audit check.
listAuditFindings_checkName :: Lens.Lens' ListAuditFindings (Prelude.Maybe Prelude.Text)
listAuditFindings_checkName = Lens.lens (\ListAuditFindings' {checkName} -> checkName) (\s@ListAuditFindings' {} a -> s {checkName = a} :: ListAuditFindings)

-- | A filter to limit results to those found before the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
listAuditFindings_endTime :: Lens.Lens' ListAuditFindings (Prelude.Maybe Prelude.UTCTime)
listAuditFindings_endTime = Lens.lens (\ListAuditFindings' {endTime} -> endTime) (\s@ListAuditFindings' {} a -> s {endTime = a} :: ListAuditFindings) Prelude.. Lens.mapping Core._Time

-- | Information identifying the noncompliant resource.
listAuditFindings_resourceIdentifier :: Lens.Lens' ListAuditFindings (Prelude.Maybe ResourceIdentifier)
listAuditFindings_resourceIdentifier = Lens.lens (\ListAuditFindings' {resourceIdentifier} -> resourceIdentifier) (\s@ListAuditFindings' {} a -> s {resourceIdentifier = a} :: ListAuditFindings)

-- | The maximum number of results to return at one time. The default is 25.
listAuditFindings_maxResults :: Lens.Lens' ListAuditFindings (Prelude.Maybe Prelude.Natural)
listAuditFindings_maxResults = Lens.lens (\ListAuditFindings' {maxResults} -> maxResults) (\s@ListAuditFindings' {} a -> s {maxResults = a} :: ListAuditFindings)

-- | Boolean flag indicating whether only the suppressed findings or the
-- unsuppressed findings should be listed. If this parameter isn\'t
-- provided, the response will list both suppressed and unsuppressed
-- findings.
listAuditFindings_listSuppressedFindings :: Lens.Lens' ListAuditFindings (Prelude.Maybe Prelude.Bool)
listAuditFindings_listSuppressedFindings = Lens.lens (\ListAuditFindings' {listSuppressedFindings} -> listSuppressedFindings) (\s@ListAuditFindings' {} a -> s {listSuppressedFindings = a} :: ListAuditFindings)

-- | A filter to limit results to those found after the specified time. You
-- must specify either the startTime and endTime or the taskId, but not
-- both.
listAuditFindings_startTime :: Lens.Lens' ListAuditFindings (Prelude.Maybe Prelude.UTCTime)
listAuditFindings_startTime = Lens.lens (\ListAuditFindings' {startTime} -> startTime) (\s@ListAuditFindings' {} a -> s {startTime = a} :: ListAuditFindings) Prelude.. Lens.mapping Core._Time

instance Core.AWSPager ListAuditFindings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAuditFindingsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAuditFindingsResponse_findings
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAuditFindings_nextToken
          Lens..~ rs
          Lens.^? listAuditFindingsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAuditFindings where
  type
    AWSResponse ListAuditFindings =
      ListAuditFindingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAuditFindingsResponse'
            Prelude.<$> (x Core..?> "findings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAuditFindings where
  hashWithSalt _salt ListAuditFindings' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` taskId
      `Prelude.hashWithSalt` checkName
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` listSuppressedFindings
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData ListAuditFindings where
  rnf ListAuditFindings' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf checkName
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf listSuppressedFindings
      `Prelude.seq` Prelude.rnf startTime

instance Core.ToHeaders ListAuditFindings where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON ListAuditFindings where
  toJSON ListAuditFindings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("taskId" Core..=) Prelude.<$> taskId,
            ("checkName" Core..=) Prelude.<$> checkName,
            ("endTime" Core..=) Prelude.<$> endTime,
            ("resourceIdentifier" Core..=)
              Prelude.<$> resourceIdentifier,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("listSuppressedFindings" Core..=)
              Prelude.<$> listSuppressedFindings,
            ("startTime" Core..=) Prelude.<$> startTime
          ]
      )

instance Core.ToPath ListAuditFindings where
  toPath = Prelude.const "/audit/findings"

instance Core.ToQuery ListAuditFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAuditFindingsResponse' smart constructor.
data ListAuditFindingsResponse = ListAuditFindingsResponse'
  { -- | The findings (results) of the audit.
    findings :: Prelude.Maybe [AuditFinding],
    -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAuditFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findings', 'listAuditFindingsResponse_findings' - The findings (results) of the audit.
--
-- 'nextToken', 'listAuditFindingsResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'httpStatus', 'listAuditFindingsResponse_httpStatus' - The response's http status code.
newListAuditFindingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAuditFindingsResponse
newListAuditFindingsResponse pHttpStatus_ =
  ListAuditFindingsResponse'
    { findings =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The findings (results) of the audit.
listAuditFindingsResponse_findings :: Lens.Lens' ListAuditFindingsResponse (Prelude.Maybe [AuditFinding])
listAuditFindingsResponse_findings = Lens.lens (\ListAuditFindingsResponse' {findings} -> findings) (\s@ListAuditFindingsResponse' {} a -> s {findings = a} :: ListAuditFindingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listAuditFindingsResponse_nextToken :: Lens.Lens' ListAuditFindingsResponse (Prelude.Maybe Prelude.Text)
listAuditFindingsResponse_nextToken = Lens.lens (\ListAuditFindingsResponse' {nextToken} -> nextToken) (\s@ListAuditFindingsResponse' {} a -> s {nextToken = a} :: ListAuditFindingsResponse)

-- | The response's http status code.
listAuditFindingsResponse_httpStatus :: Lens.Lens' ListAuditFindingsResponse Prelude.Int
listAuditFindingsResponse_httpStatus = Lens.lens (\ListAuditFindingsResponse' {httpStatus} -> httpStatus) (\s@ListAuditFindingsResponse' {} a -> s {httpStatus = a} :: ListAuditFindingsResponse)

instance Prelude.NFData ListAuditFindingsResponse where
  rnf ListAuditFindingsResponse' {..} =
    Prelude.rnf findings
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
