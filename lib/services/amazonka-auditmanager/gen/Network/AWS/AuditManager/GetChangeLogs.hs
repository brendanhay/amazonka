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
-- Module      : Amazonka.AuditManager.GetChangeLogs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of changelogs from Audit Manager.
module Amazonka.AuditManager.GetChangeLogs
  ( -- * Creating a Request
    GetChangeLogs (..),
    newGetChangeLogs,

    -- * Request Lenses
    getChangeLogs_controlSetId,
    getChangeLogs_nextToken,
    getChangeLogs_controlId,
    getChangeLogs_maxResults,
    getChangeLogs_assessmentId,

    -- * Destructuring the Response
    GetChangeLogsResponse (..),
    newGetChangeLogsResponse,

    -- * Response Lenses
    getChangeLogsResponse_changeLogs,
    getChangeLogsResponse_nextToken,
    getChangeLogsResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetChangeLogs' smart constructor.
data GetChangeLogs = GetChangeLogs'
  { -- | The identifier for the specified control set.
    controlSetId :: Prelude.Maybe Prelude.Text,
    -- | The pagination token used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the specified control.
    controlId :: Prelude.Maybe Prelude.Text,
    -- | Represents the maximum number of results per page, or per API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier for the specified assessment.
    assessmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChangeLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlSetId', 'getChangeLogs_controlSetId' - The identifier for the specified control set.
--
-- 'nextToken', 'getChangeLogs_nextToken' - The pagination token used to fetch the next set of results.
--
-- 'controlId', 'getChangeLogs_controlId' - The identifier for the specified control.
--
-- 'maxResults', 'getChangeLogs_maxResults' - Represents the maximum number of results per page, or per API request
-- call.
--
-- 'assessmentId', 'getChangeLogs_assessmentId' - The identifier for the specified assessment.
newGetChangeLogs ::
  -- | 'assessmentId'
  Prelude.Text ->
  GetChangeLogs
newGetChangeLogs pAssessmentId_ =
  GetChangeLogs'
    { controlSetId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      controlId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      assessmentId = pAssessmentId_
    }

-- | The identifier for the specified control set.
getChangeLogs_controlSetId :: Lens.Lens' GetChangeLogs (Prelude.Maybe Prelude.Text)
getChangeLogs_controlSetId = Lens.lens (\GetChangeLogs' {controlSetId} -> controlSetId) (\s@GetChangeLogs' {} a -> s {controlSetId = a} :: GetChangeLogs)

-- | The pagination token used to fetch the next set of results.
getChangeLogs_nextToken :: Lens.Lens' GetChangeLogs (Prelude.Maybe Prelude.Text)
getChangeLogs_nextToken = Lens.lens (\GetChangeLogs' {nextToken} -> nextToken) (\s@GetChangeLogs' {} a -> s {nextToken = a} :: GetChangeLogs)

-- | The identifier for the specified control.
getChangeLogs_controlId :: Lens.Lens' GetChangeLogs (Prelude.Maybe Prelude.Text)
getChangeLogs_controlId = Lens.lens (\GetChangeLogs' {controlId} -> controlId) (\s@GetChangeLogs' {} a -> s {controlId = a} :: GetChangeLogs)

-- | Represents the maximum number of results per page, or per API request
-- call.
getChangeLogs_maxResults :: Lens.Lens' GetChangeLogs (Prelude.Maybe Prelude.Natural)
getChangeLogs_maxResults = Lens.lens (\GetChangeLogs' {maxResults} -> maxResults) (\s@GetChangeLogs' {} a -> s {maxResults = a} :: GetChangeLogs)

-- | The identifier for the specified assessment.
getChangeLogs_assessmentId :: Lens.Lens' GetChangeLogs Prelude.Text
getChangeLogs_assessmentId = Lens.lens (\GetChangeLogs' {assessmentId} -> assessmentId) (\s@GetChangeLogs' {} a -> s {assessmentId = a} :: GetChangeLogs)

instance Core.AWSRequest GetChangeLogs where
  type
    AWSResponse GetChangeLogs =
      GetChangeLogsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChangeLogsResponse'
            Prelude.<$> (x Core..?> "changeLogs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetChangeLogs

instance Prelude.NFData GetChangeLogs

instance Core.ToHeaders GetChangeLogs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetChangeLogs where
  toPath GetChangeLogs' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Core.toBS assessmentId,
        "/changelogs"
      ]

instance Core.ToQuery GetChangeLogs where
  toQuery GetChangeLogs' {..} =
    Prelude.mconcat
      [ "controlSetId" Core.=: controlSetId,
        "nextToken" Core.=: nextToken,
        "controlId" Core.=: controlId,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetChangeLogsResponse' smart constructor.
data GetChangeLogsResponse = GetChangeLogsResponse'
  { -- | The list of user activity for the control.
    changeLogs :: Prelude.Maybe [ChangeLog],
    -- | The pagination token used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChangeLogsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeLogs', 'getChangeLogsResponse_changeLogs' - The list of user activity for the control.
--
-- 'nextToken', 'getChangeLogsResponse_nextToken' - The pagination token used to fetch the next set of results.
--
-- 'httpStatus', 'getChangeLogsResponse_httpStatus' - The response's http status code.
newGetChangeLogsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetChangeLogsResponse
newGetChangeLogsResponse pHttpStatus_ =
  GetChangeLogsResponse'
    { changeLogs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of user activity for the control.
getChangeLogsResponse_changeLogs :: Lens.Lens' GetChangeLogsResponse (Prelude.Maybe [ChangeLog])
getChangeLogsResponse_changeLogs = Lens.lens (\GetChangeLogsResponse' {changeLogs} -> changeLogs) (\s@GetChangeLogsResponse' {} a -> s {changeLogs = a} :: GetChangeLogsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token used to fetch the next set of results.
getChangeLogsResponse_nextToken :: Lens.Lens' GetChangeLogsResponse (Prelude.Maybe Prelude.Text)
getChangeLogsResponse_nextToken = Lens.lens (\GetChangeLogsResponse' {nextToken} -> nextToken) (\s@GetChangeLogsResponse' {} a -> s {nextToken = a} :: GetChangeLogsResponse)

-- | The response's http status code.
getChangeLogsResponse_httpStatus :: Lens.Lens' GetChangeLogsResponse Prelude.Int
getChangeLogsResponse_httpStatus = Lens.lens (\GetChangeLogsResponse' {httpStatus} -> httpStatus) (\s@GetChangeLogsResponse' {} a -> s {httpStatus = a} :: GetChangeLogsResponse)

instance Prelude.NFData GetChangeLogsResponse
