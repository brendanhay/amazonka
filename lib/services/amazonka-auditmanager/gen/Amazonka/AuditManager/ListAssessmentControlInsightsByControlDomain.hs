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
-- Module      : Amazonka.AuditManager.ListAssessmentControlInsightsByControlDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the latest analytics data for controls within a specific control
-- domain and a specific active assessment.
--
-- Control insights are listed only if the control belongs to the control
-- domain and assessment that was specified. Moreover, the control must
-- have collected evidence on the @lastUpdated@ date of
-- @controlInsightsByAssessment@. If neither of these conditions are met,
-- no data is listed for that control.
module Amazonka.AuditManager.ListAssessmentControlInsightsByControlDomain
  ( -- * Creating a Request
    ListAssessmentControlInsightsByControlDomain (..),
    newListAssessmentControlInsightsByControlDomain,

    -- * Request Lenses
    listAssessmentControlInsightsByControlDomain_maxResults,
    listAssessmentControlInsightsByControlDomain_nextToken,
    listAssessmentControlInsightsByControlDomain_controlDomainId,
    listAssessmentControlInsightsByControlDomain_assessmentId,

    -- * Destructuring the Response
    ListAssessmentControlInsightsByControlDomainResponse (..),
    newListAssessmentControlInsightsByControlDomainResponse,

    -- * Response Lenses
    listAssessmentControlInsightsByControlDomainResponse_controlInsightsByAssessment,
    listAssessmentControlInsightsByControlDomainResponse_nextToken,
    listAssessmentControlInsightsByControlDomainResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssessmentControlInsightsByControlDomain' smart constructor.
data ListAssessmentControlInsightsByControlDomain = ListAssessmentControlInsightsByControlDomain'
  { -- | Represents the maximum number of results on a page or for an API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the control domain.
    controlDomainId :: Prelude.Text,
    -- | The unique identifier for the active assessment.
    assessmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentControlInsightsByControlDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAssessmentControlInsightsByControlDomain_maxResults' - Represents the maximum number of results on a page or for an API request
-- call.
--
-- 'nextToken', 'listAssessmentControlInsightsByControlDomain_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'controlDomainId', 'listAssessmentControlInsightsByControlDomain_controlDomainId' - The unique identifier for the control domain.
--
-- 'assessmentId', 'listAssessmentControlInsightsByControlDomain_assessmentId' - The unique identifier for the active assessment.
newListAssessmentControlInsightsByControlDomain ::
  -- | 'controlDomainId'
  Prelude.Text ->
  -- | 'assessmentId'
  Prelude.Text ->
  ListAssessmentControlInsightsByControlDomain
newListAssessmentControlInsightsByControlDomain
  pControlDomainId_
  pAssessmentId_ =
    ListAssessmentControlInsightsByControlDomain'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        controlDomainId =
          pControlDomainId_,
        assessmentId = pAssessmentId_
      }

-- | Represents the maximum number of results on a page or for an API request
-- call.
listAssessmentControlInsightsByControlDomain_maxResults :: Lens.Lens' ListAssessmentControlInsightsByControlDomain (Prelude.Maybe Prelude.Natural)
listAssessmentControlInsightsByControlDomain_maxResults = Lens.lens (\ListAssessmentControlInsightsByControlDomain' {maxResults} -> maxResults) (\s@ListAssessmentControlInsightsByControlDomain' {} a -> s {maxResults = a} :: ListAssessmentControlInsightsByControlDomain)

-- | The pagination token that\'s used to fetch the next set of results.
listAssessmentControlInsightsByControlDomain_nextToken :: Lens.Lens' ListAssessmentControlInsightsByControlDomain (Prelude.Maybe Prelude.Text)
listAssessmentControlInsightsByControlDomain_nextToken = Lens.lens (\ListAssessmentControlInsightsByControlDomain' {nextToken} -> nextToken) (\s@ListAssessmentControlInsightsByControlDomain' {} a -> s {nextToken = a} :: ListAssessmentControlInsightsByControlDomain)

-- | The unique identifier for the control domain.
listAssessmentControlInsightsByControlDomain_controlDomainId :: Lens.Lens' ListAssessmentControlInsightsByControlDomain Prelude.Text
listAssessmentControlInsightsByControlDomain_controlDomainId = Lens.lens (\ListAssessmentControlInsightsByControlDomain' {controlDomainId} -> controlDomainId) (\s@ListAssessmentControlInsightsByControlDomain' {} a -> s {controlDomainId = a} :: ListAssessmentControlInsightsByControlDomain)

-- | The unique identifier for the active assessment.
listAssessmentControlInsightsByControlDomain_assessmentId :: Lens.Lens' ListAssessmentControlInsightsByControlDomain Prelude.Text
listAssessmentControlInsightsByControlDomain_assessmentId = Lens.lens (\ListAssessmentControlInsightsByControlDomain' {assessmentId} -> assessmentId) (\s@ListAssessmentControlInsightsByControlDomain' {} a -> s {assessmentId = a} :: ListAssessmentControlInsightsByControlDomain)

instance
  Core.AWSRequest
    ListAssessmentControlInsightsByControlDomain
  where
  type
    AWSResponse
      ListAssessmentControlInsightsByControlDomain =
      ListAssessmentControlInsightsByControlDomainResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssessmentControlInsightsByControlDomainResponse'
            Prelude.<$> ( x
                            Data..?> "controlInsightsByAssessment"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAssessmentControlInsightsByControlDomain
  where
  hashWithSalt
    _salt
    ListAssessmentControlInsightsByControlDomain' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` controlDomainId
        `Prelude.hashWithSalt` assessmentId

instance
  Prelude.NFData
    ListAssessmentControlInsightsByControlDomain
  where
  rnf ListAssessmentControlInsightsByControlDomain' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf controlDomainId `Prelude.seq`
          Prelude.rnf assessmentId

instance
  Data.ToHeaders
    ListAssessmentControlInsightsByControlDomain
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

instance
  Data.ToPath
    ListAssessmentControlInsightsByControlDomain
  where
  toPath =
    Prelude.const "/insights/controls-by-assessment"

instance
  Data.ToQuery
    ListAssessmentControlInsightsByControlDomain
  where
  toQuery
    ListAssessmentControlInsightsByControlDomain' {..} =
      Prelude.mconcat
        [ "maxResults" Data.=: maxResults,
          "nextToken" Data.=: nextToken,
          "controlDomainId" Data.=: controlDomainId,
          "assessmentId" Data.=: assessmentId
        ]

-- | /See:/ 'newListAssessmentControlInsightsByControlDomainResponse' smart constructor.
data ListAssessmentControlInsightsByControlDomainResponse = ListAssessmentControlInsightsByControlDomainResponse'
  { -- | The assessment control analytics data that the
    -- @ListAssessmentControlInsightsByControlDomain@ API returned.
    controlInsightsByAssessment :: Prelude.Maybe [ControlInsightsMetadataByAssessmentItem],
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentControlInsightsByControlDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlInsightsByAssessment', 'listAssessmentControlInsightsByControlDomainResponse_controlInsightsByAssessment' - The assessment control analytics data that the
-- @ListAssessmentControlInsightsByControlDomain@ API returned.
--
-- 'nextToken', 'listAssessmentControlInsightsByControlDomainResponse_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'httpStatus', 'listAssessmentControlInsightsByControlDomainResponse_httpStatus' - The response's http status code.
newListAssessmentControlInsightsByControlDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssessmentControlInsightsByControlDomainResponse
newListAssessmentControlInsightsByControlDomainResponse
  pHttpStatus_ =
    ListAssessmentControlInsightsByControlDomainResponse'
      { controlInsightsByAssessment =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The assessment control analytics data that the
-- @ListAssessmentControlInsightsByControlDomain@ API returned.
listAssessmentControlInsightsByControlDomainResponse_controlInsightsByAssessment :: Lens.Lens' ListAssessmentControlInsightsByControlDomainResponse (Prelude.Maybe [ControlInsightsMetadataByAssessmentItem])
listAssessmentControlInsightsByControlDomainResponse_controlInsightsByAssessment = Lens.lens (\ListAssessmentControlInsightsByControlDomainResponse' {controlInsightsByAssessment} -> controlInsightsByAssessment) (\s@ListAssessmentControlInsightsByControlDomainResponse' {} a -> s {controlInsightsByAssessment = a} :: ListAssessmentControlInsightsByControlDomainResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s used to fetch the next set of results.
listAssessmentControlInsightsByControlDomainResponse_nextToken :: Lens.Lens' ListAssessmentControlInsightsByControlDomainResponse (Prelude.Maybe Prelude.Text)
listAssessmentControlInsightsByControlDomainResponse_nextToken = Lens.lens (\ListAssessmentControlInsightsByControlDomainResponse' {nextToken} -> nextToken) (\s@ListAssessmentControlInsightsByControlDomainResponse' {} a -> s {nextToken = a} :: ListAssessmentControlInsightsByControlDomainResponse)

-- | The response's http status code.
listAssessmentControlInsightsByControlDomainResponse_httpStatus :: Lens.Lens' ListAssessmentControlInsightsByControlDomainResponse Prelude.Int
listAssessmentControlInsightsByControlDomainResponse_httpStatus = Lens.lens (\ListAssessmentControlInsightsByControlDomainResponse' {httpStatus} -> httpStatus) (\s@ListAssessmentControlInsightsByControlDomainResponse' {} a -> s {httpStatus = a} :: ListAssessmentControlInsightsByControlDomainResponse)

instance
  Prelude.NFData
    ListAssessmentControlInsightsByControlDomainResponse
  where
  rnf
    ListAssessmentControlInsightsByControlDomainResponse' {..} =
      Prelude.rnf controlInsightsByAssessment `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf httpStatus
