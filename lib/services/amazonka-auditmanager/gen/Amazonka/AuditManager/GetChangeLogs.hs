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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    getChangeLogs_controlId,
    getChangeLogs_nextToken,
    getChangeLogs_maxResults,
    getChangeLogs_controlSetId,
    getChangeLogs_assessmentId,

    -- * Destructuring the Response
    GetChangeLogsResponse (..),
    newGetChangeLogsResponse,

    -- * Response Lenses
    getChangeLogsResponse_nextToken,
    getChangeLogsResponse_changeLogs,
    getChangeLogsResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetChangeLogs' smart constructor.
data GetChangeLogs = GetChangeLogs'
  { -- | The unique identifier for the control.
    controlId :: Prelude.Maybe Prelude.Text,
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Represents the maximum number of results on a page or for an API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique identifier for the control set.
    controlSetId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the assessment.
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
-- 'controlId', 'getChangeLogs_controlId' - The unique identifier for the control.
--
-- 'nextToken', 'getChangeLogs_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'maxResults', 'getChangeLogs_maxResults' - Represents the maximum number of results on a page or for an API request
-- call.
--
-- 'controlSetId', 'getChangeLogs_controlSetId' - The unique identifier for the control set.
--
-- 'assessmentId', 'getChangeLogs_assessmentId' - The unique identifier for the assessment.
newGetChangeLogs ::
  -- | 'assessmentId'
  Prelude.Text ->
  GetChangeLogs
newGetChangeLogs pAssessmentId_ =
  GetChangeLogs'
    { controlId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      controlSetId = Prelude.Nothing,
      assessmentId = pAssessmentId_
    }

-- | The unique identifier for the control.
getChangeLogs_controlId :: Lens.Lens' GetChangeLogs (Prelude.Maybe Prelude.Text)
getChangeLogs_controlId = Lens.lens (\GetChangeLogs' {controlId} -> controlId) (\s@GetChangeLogs' {} a -> s {controlId = a} :: GetChangeLogs)

-- | The pagination token that\'s used to fetch the next set of results.
getChangeLogs_nextToken :: Lens.Lens' GetChangeLogs (Prelude.Maybe Prelude.Text)
getChangeLogs_nextToken = Lens.lens (\GetChangeLogs' {nextToken} -> nextToken) (\s@GetChangeLogs' {} a -> s {nextToken = a} :: GetChangeLogs)

-- | Represents the maximum number of results on a page or for an API request
-- call.
getChangeLogs_maxResults :: Lens.Lens' GetChangeLogs (Prelude.Maybe Prelude.Natural)
getChangeLogs_maxResults = Lens.lens (\GetChangeLogs' {maxResults} -> maxResults) (\s@GetChangeLogs' {} a -> s {maxResults = a} :: GetChangeLogs)

-- | The unique identifier for the control set.
getChangeLogs_controlSetId :: Lens.Lens' GetChangeLogs (Prelude.Maybe Prelude.Text)
getChangeLogs_controlSetId = Lens.lens (\GetChangeLogs' {controlSetId} -> controlSetId) (\s@GetChangeLogs' {} a -> s {controlSetId = a} :: GetChangeLogs)

-- | The unique identifier for the assessment.
getChangeLogs_assessmentId :: Lens.Lens' GetChangeLogs Prelude.Text
getChangeLogs_assessmentId = Lens.lens (\GetChangeLogs' {assessmentId} -> assessmentId) (\s@GetChangeLogs' {} a -> s {assessmentId = a} :: GetChangeLogs)

instance Core.AWSRequest GetChangeLogs where
  type
    AWSResponse GetChangeLogs =
      GetChangeLogsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChangeLogsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "changeLogs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetChangeLogs where
  hashWithSalt _salt GetChangeLogs' {..} =
    _salt `Prelude.hashWithSalt` controlId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` controlSetId
      `Prelude.hashWithSalt` assessmentId

instance Prelude.NFData GetChangeLogs where
  rnf GetChangeLogs' {..} =
    Prelude.rnf controlId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf controlSetId
      `Prelude.seq` Prelude.rnf assessmentId

instance Data.ToHeaders GetChangeLogs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetChangeLogs where
  toPath GetChangeLogs' {..} =
    Prelude.mconcat
      [ "/assessments/",
        Data.toBS assessmentId,
        "/changelogs"
      ]

instance Data.ToQuery GetChangeLogs where
  toQuery GetChangeLogs' {..} =
    Prelude.mconcat
      [ "controlId" Data.=: controlId,
        "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults,
        "controlSetId" Data.=: controlSetId
      ]

-- | /See:/ 'newGetChangeLogsResponse' smart constructor.
data GetChangeLogsResponse = GetChangeLogsResponse'
  { -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of user activity for the control.
    changeLogs :: Prelude.Maybe [ChangeLog],
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
-- 'nextToken', 'getChangeLogsResponse_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'changeLogs', 'getChangeLogsResponse_changeLogs' - The list of user activity for the control.
--
-- 'httpStatus', 'getChangeLogsResponse_httpStatus' - The response's http status code.
newGetChangeLogsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetChangeLogsResponse
newGetChangeLogsResponse pHttpStatus_ =
  GetChangeLogsResponse'
    { nextToken = Prelude.Nothing,
      changeLogs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that\'s used to fetch the next set of results.
getChangeLogsResponse_nextToken :: Lens.Lens' GetChangeLogsResponse (Prelude.Maybe Prelude.Text)
getChangeLogsResponse_nextToken = Lens.lens (\GetChangeLogsResponse' {nextToken} -> nextToken) (\s@GetChangeLogsResponse' {} a -> s {nextToken = a} :: GetChangeLogsResponse)

-- | The list of user activity for the control.
getChangeLogsResponse_changeLogs :: Lens.Lens' GetChangeLogsResponse (Prelude.Maybe [ChangeLog])
getChangeLogsResponse_changeLogs = Lens.lens (\GetChangeLogsResponse' {changeLogs} -> changeLogs) (\s@GetChangeLogsResponse' {} a -> s {changeLogs = a} :: GetChangeLogsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getChangeLogsResponse_httpStatus :: Lens.Lens' GetChangeLogsResponse Prelude.Int
getChangeLogsResponse_httpStatus = Lens.lens (\GetChangeLogsResponse' {httpStatus} -> httpStatus) (\s@GetChangeLogsResponse' {} a -> s {httpStatus = a} :: GetChangeLogsResponse)

instance Prelude.NFData GetChangeLogsResponse where
  rnf GetChangeLogsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf changeLogs
      `Prelude.seq` Prelude.rnf httpStatus
