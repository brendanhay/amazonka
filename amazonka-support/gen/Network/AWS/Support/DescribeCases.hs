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
-- Module      : Network.AWS.Support.DescribeCases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cases that you specify by passing one or more case
-- IDs. You can use the @afterTime@ and @beforeTime@ parameters to filter
-- the cases by date. You can set values for the @includeResolvedCases@ and
-- @includeCommunications@ parameters to specify how much information to
-- return.
--
-- The response returns the following in JSON format:
--
-- -   One or more
--     <https://docs.aws.amazon.com/awssupport/latest/APIReference/API_CaseDetails.html CaseDetails>
--     data types.
--
-- -   One or more @nextToken@ values, which specify where to paginate the
--     returned records represented by the @CaseDetails@ objects.
--
-- Case data is available for 12 months after creation. If a case was
-- created more than 12 months ago, a request might return an error.
--
-- -   You must have a Business or Enterprise support plan to use the AWS
--     Support API.
--
-- -   If you call the AWS Support API from an account that does not have a
--     Business or Enterprise support plan, the
--     @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ AWS Support>.
--
-- This operation returns paginated results.
module Network.AWS.Support.DescribeCases
  ( -- * Creating a Request
    DescribeCases (..),
    newDescribeCases,

    -- * Request Lenses
    describeCases_displayId,
    describeCases_includeCommunications,
    describeCases_nextToken,
    describeCases_maxResults,
    describeCases_caseIdList,
    describeCases_includeResolvedCases,
    describeCases_beforeTime,
    describeCases_afterTime,
    describeCases_language,

    -- * Destructuring the Response
    DescribeCasesResponse (..),
    newDescribeCasesResponse,

    -- * Response Lenses
    describeCasesResponse_nextToken,
    describeCasesResponse_cases,
    describeCasesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Support.Types

-- | /See:/ 'newDescribeCases' smart constructor.
data DescribeCases = DescribeCases'
  { -- | The ID displayed for a case in the AWS Support Center user interface.
    displayId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to include communications in the @DescribeCases@
    -- response. By default, communications are incuded.
    includeCommunications :: Prelude.Maybe Prelude.Bool,
    -- | A resumption point for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return before paginating.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A list of ID numbers of the support cases you want returned. The maximum
    -- number of cases is 100.
    caseIdList :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether to include resolved support cases in the
    -- @DescribeCases@ response. By default, resolved cases aren\'t included.
    includeResolvedCases :: Prelude.Maybe Prelude.Bool,
    -- | The end date for a filtered date search on support case communications.
    -- Case communications are available for 12 months after creation.
    beforeTime :: Prelude.Maybe Prelude.Text,
    -- | The start date for a filtered date search on support case
    -- communications. Case communications are available for 12 months after
    -- creation.
    afterTime :: Prelude.Maybe Prelude.Text,
    -- | The ISO 639-1 code for the language in which AWS provides support. AWS
    -- Support currently supports English (\"en\") and Japanese (\"ja\").
    -- Language parameters must be passed explicitly for operations that take
    -- them.
    language :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayId', 'describeCases_displayId' - The ID displayed for a case in the AWS Support Center user interface.
--
-- 'includeCommunications', 'describeCases_includeCommunications' - Specifies whether to include communications in the @DescribeCases@
-- response. By default, communications are incuded.
--
-- 'nextToken', 'describeCases_nextToken' - A resumption point for pagination.
--
-- 'maxResults', 'describeCases_maxResults' - The maximum number of results to return before paginating.
--
-- 'caseIdList', 'describeCases_caseIdList' - A list of ID numbers of the support cases you want returned. The maximum
-- number of cases is 100.
--
-- 'includeResolvedCases', 'describeCases_includeResolvedCases' - Specifies whether to include resolved support cases in the
-- @DescribeCases@ response. By default, resolved cases aren\'t included.
--
-- 'beforeTime', 'describeCases_beforeTime' - The end date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
--
-- 'afterTime', 'describeCases_afterTime' - The start date for a filtered date search on support case
-- communications. Case communications are available for 12 months after
-- creation.
--
-- 'language', 'describeCases_language' - The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
newDescribeCases ::
  DescribeCases
newDescribeCases =
  DescribeCases'
    { displayId = Prelude.Nothing,
      includeCommunications = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      caseIdList = Prelude.Nothing,
      includeResolvedCases = Prelude.Nothing,
      beforeTime = Prelude.Nothing,
      afterTime = Prelude.Nothing,
      language = Prelude.Nothing
    }

-- | The ID displayed for a case in the AWS Support Center user interface.
describeCases_displayId :: Lens.Lens' DescribeCases (Prelude.Maybe Prelude.Text)
describeCases_displayId = Lens.lens (\DescribeCases' {displayId} -> displayId) (\s@DescribeCases' {} a -> s {displayId = a} :: DescribeCases)

-- | Specifies whether to include communications in the @DescribeCases@
-- response. By default, communications are incuded.
describeCases_includeCommunications :: Lens.Lens' DescribeCases (Prelude.Maybe Prelude.Bool)
describeCases_includeCommunications = Lens.lens (\DescribeCases' {includeCommunications} -> includeCommunications) (\s@DescribeCases' {} a -> s {includeCommunications = a} :: DescribeCases)

-- | A resumption point for pagination.
describeCases_nextToken :: Lens.Lens' DescribeCases (Prelude.Maybe Prelude.Text)
describeCases_nextToken = Lens.lens (\DescribeCases' {nextToken} -> nextToken) (\s@DescribeCases' {} a -> s {nextToken = a} :: DescribeCases)

-- | The maximum number of results to return before paginating.
describeCases_maxResults :: Lens.Lens' DescribeCases (Prelude.Maybe Prelude.Natural)
describeCases_maxResults = Lens.lens (\DescribeCases' {maxResults} -> maxResults) (\s@DescribeCases' {} a -> s {maxResults = a} :: DescribeCases)

-- | A list of ID numbers of the support cases you want returned. The maximum
-- number of cases is 100.
describeCases_caseIdList :: Lens.Lens' DescribeCases (Prelude.Maybe [Prelude.Text])
describeCases_caseIdList = Lens.lens (\DescribeCases' {caseIdList} -> caseIdList) (\s@DescribeCases' {} a -> s {caseIdList = a} :: DescribeCases) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies whether to include resolved support cases in the
-- @DescribeCases@ response. By default, resolved cases aren\'t included.
describeCases_includeResolvedCases :: Lens.Lens' DescribeCases (Prelude.Maybe Prelude.Bool)
describeCases_includeResolvedCases = Lens.lens (\DescribeCases' {includeResolvedCases} -> includeResolvedCases) (\s@DescribeCases' {} a -> s {includeResolvedCases = a} :: DescribeCases)

-- | The end date for a filtered date search on support case communications.
-- Case communications are available for 12 months after creation.
describeCases_beforeTime :: Lens.Lens' DescribeCases (Prelude.Maybe Prelude.Text)
describeCases_beforeTime = Lens.lens (\DescribeCases' {beforeTime} -> beforeTime) (\s@DescribeCases' {} a -> s {beforeTime = a} :: DescribeCases)

-- | The start date for a filtered date search on support case
-- communications. Case communications are available for 12 months after
-- creation.
describeCases_afterTime :: Lens.Lens' DescribeCases (Prelude.Maybe Prelude.Text)
describeCases_afterTime = Lens.lens (\DescribeCases' {afterTime} -> afterTime) (\s@DescribeCases' {} a -> s {afterTime = a} :: DescribeCases)

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
describeCases_language :: Lens.Lens' DescribeCases (Prelude.Maybe Prelude.Text)
describeCases_language = Lens.lens (\DescribeCases' {language} -> language) (\s@DescribeCases' {} a -> s {language = a} :: DescribeCases)

instance Core.AWSPager DescribeCases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCasesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCasesResponse_cases Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeCases_nextToken
          Lens..~ rs
          Lens.^? describeCasesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest DescribeCases where
  type
    AWSResponse DescribeCases =
      DescribeCasesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCasesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "cases" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCases

instance Prelude.NFData DescribeCases

instance Core.ToHeaders DescribeCases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSupport_20130415.DescribeCases" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeCases where
  toJSON DescribeCases' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("displayId" Core..=) Prelude.<$> displayId,
            ("includeCommunications" Core..=)
              Prelude.<$> includeCommunications,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("caseIdList" Core..=) Prelude.<$> caseIdList,
            ("includeResolvedCases" Core..=)
              Prelude.<$> includeResolvedCases,
            ("beforeTime" Core..=) Prelude.<$> beforeTime,
            ("afterTime" Core..=) Prelude.<$> afterTime,
            ("language" Core..=) Prelude.<$> language
          ]
      )

instance Core.ToPath DescribeCases where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCases where
  toQuery = Prelude.const Prelude.mempty

-- | Returns an array of
-- <https://docs.aws.amazon.com/awssupport/latest/APIReference/API_CaseDetails.html CaseDetails>
-- objects and a @nextToken@ that defines a point for pagination in the
-- result set.
--
-- /See:/ 'newDescribeCasesResponse' smart constructor.
data DescribeCasesResponse = DescribeCasesResponse'
  { -- | A resumption point for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The details for the cases that match the request.
    cases :: Prelude.Maybe [CaseDetails],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeCasesResponse_nextToken' - A resumption point for pagination.
--
-- 'cases', 'describeCasesResponse_cases' - The details for the cases that match the request.
--
-- 'httpStatus', 'describeCasesResponse_httpStatus' - The response's http status code.
newDescribeCasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCasesResponse
newDescribeCasesResponse pHttpStatus_ =
  DescribeCasesResponse'
    { nextToken = Prelude.Nothing,
      cases = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A resumption point for pagination.
describeCasesResponse_nextToken :: Lens.Lens' DescribeCasesResponse (Prelude.Maybe Prelude.Text)
describeCasesResponse_nextToken = Lens.lens (\DescribeCasesResponse' {nextToken} -> nextToken) (\s@DescribeCasesResponse' {} a -> s {nextToken = a} :: DescribeCasesResponse)

-- | The details for the cases that match the request.
describeCasesResponse_cases :: Lens.Lens' DescribeCasesResponse (Prelude.Maybe [CaseDetails])
describeCasesResponse_cases = Lens.lens (\DescribeCasesResponse' {cases} -> cases) (\s@DescribeCasesResponse' {} a -> s {cases = a} :: DescribeCasesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeCasesResponse_httpStatus :: Lens.Lens' DescribeCasesResponse Prelude.Int
describeCasesResponse_httpStatus = Lens.lens (\DescribeCasesResponse' {httpStatus} -> httpStatus) (\s@DescribeCasesResponse' {} a -> s {httpStatus = a} :: DescribeCasesResponse)

instance Prelude.NFData DescribeCasesResponse
