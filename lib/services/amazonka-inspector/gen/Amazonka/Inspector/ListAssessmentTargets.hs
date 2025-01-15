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
-- Module      : Amazonka.Inspector.ListAssessmentTargets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the ARNs of the assessment targets within this AWS account. For
-- more information about assessment targets, see
-- <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_applications.html Amazon Inspector Assessment Targets>.
--
-- This operation returns paginated results.
module Amazonka.Inspector.ListAssessmentTargets
  ( -- * Creating a Request
    ListAssessmentTargets (..),
    newListAssessmentTargets,

    -- * Request Lenses
    listAssessmentTargets_filter,
    listAssessmentTargets_maxResults,
    listAssessmentTargets_nextToken,

    -- * Destructuring the Response
    ListAssessmentTargetsResponse (..),
    newListAssessmentTargetsResponse,

    -- * Response Lenses
    listAssessmentTargetsResponse_nextToken,
    listAssessmentTargetsResponse_httpStatus,
    listAssessmentTargetsResponse_assessmentTargetArns,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssessmentTargets' smart constructor.
data ListAssessmentTargets = ListAssessmentTargets'
  { -- | You can use this parameter to specify a subset of data to be included in
    -- the action\'s response.
    --
    -- For a record to match a filter, all specified filter attributes must
    -- match. When multiple values are specified for a filter attribute, any of
    -- the values can match.
    filter' :: Prelude.Maybe AssessmentTargetFilter,
    -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 10. The maximum value is 500.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the
    -- __ListAssessmentTargets__ action. Subsequent calls to the action fill
    -- __nextToken__ in the request with the value of __NextToken__ from the
    -- previous response to continue listing data.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listAssessmentTargets_filter' - You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
--
-- 'maxResults', 'listAssessmentTargets_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
--
-- 'nextToken', 'listAssessmentTargets_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentTargets__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
newListAssessmentTargets ::
  ListAssessmentTargets
newListAssessmentTargets =
  ListAssessmentTargets'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
listAssessmentTargets_filter :: Lens.Lens' ListAssessmentTargets (Prelude.Maybe AssessmentTargetFilter)
listAssessmentTargets_filter = Lens.lens (\ListAssessmentTargets' {filter'} -> filter') (\s@ListAssessmentTargets' {} a -> s {filter' = a} :: ListAssessmentTargets)

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
listAssessmentTargets_maxResults :: Lens.Lens' ListAssessmentTargets (Prelude.Maybe Prelude.Int)
listAssessmentTargets_maxResults = Lens.lens (\ListAssessmentTargets' {maxResults} -> maxResults) (\s@ListAssessmentTargets' {} a -> s {maxResults = a} :: ListAssessmentTargets)

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentTargets__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
listAssessmentTargets_nextToken :: Lens.Lens' ListAssessmentTargets (Prelude.Maybe Prelude.Text)
listAssessmentTargets_nextToken = Lens.lens (\ListAssessmentTargets' {nextToken} -> nextToken) (\s@ListAssessmentTargets' {} a -> s {nextToken = a} :: ListAssessmentTargets)

instance Core.AWSPager ListAssessmentTargets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssessmentTargetsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listAssessmentTargetsResponse_assessmentTargetArns
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listAssessmentTargets_nextToken
              Lens..~ rs
              Lens.^? listAssessmentTargetsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListAssessmentTargets where
  type
    AWSResponse ListAssessmentTargets =
      ListAssessmentTargetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssessmentTargetsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "assessmentTargetArns"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAssessmentTargets where
  hashWithSalt _salt ListAssessmentTargets' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListAssessmentTargets where
  rnf ListAssessmentTargets' {..} =
    Prelude.rnf filter' `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders ListAssessmentTargets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.ListAssessmentTargets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAssessmentTargets where
  toJSON ListAssessmentTargets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filter" Data..=) Prelude.<$> filter',
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListAssessmentTargets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAssessmentTargets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAssessmentTargetsResponse' smart constructor.
data ListAssessmentTargetsResponse = ListAssessmentTargetsResponse'
  { -- | When a response is generated, if there is more data to be listed, this
    -- parameter is present in the response and contains the value to use for
    -- the __nextToken__ parameter in a subsequent pagination request. If there
    -- is no more data to be listed, this parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of ARNs that specifies the assessment targets that are returned
    -- by the action.
    assessmentTargetArns :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentTargetsResponse_nextToken' - When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
--
-- 'httpStatus', 'listAssessmentTargetsResponse_httpStatus' - The response's http status code.
--
-- 'assessmentTargetArns', 'listAssessmentTargetsResponse_assessmentTargetArns' - A list of ARNs that specifies the assessment targets that are returned
-- by the action.
newListAssessmentTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssessmentTargetsResponse
newListAssessmentTargetsResponse pHttpStatus_ =
  ListAssessmentTargetsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      assessmentTargetArns = Prelude.mempty
    }

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
listAssessmentTargetsResponse_nextToken :: Lens.Lens' ListAssessmentTargetsResponse (Prelude.Maybe Prelude.Text)
listAssessmentTargetsResponse_nextToken = Lens.lens (\ListAssessmentTargetsResponse' {nextToken} -> nextToken) (\s@ListAssessmentTargetsResponse' {} a -> s {nextToken = a} :: ListAssessmentTargetsResponse)

-- | The response's http status code.
listAssessmentTargetsResponse_httpStatus :: Lens.Lens' ListAssessmentTargetsResponse Prelude.Int
listAssessmentTargetsResponse_httpStatus = Lens.lens (\ListAssessmentTargetsResponse' {httpStatus} -> httpStatus) (\s@ListAssessmentTargetsResponse' {} a -> s {httpStatus = a} :: ListAssessmentTargetsResponse)

-- | A list of ARNs that specifies the assessment targets that are returned
-- by the action.
listAssessmentTargetsResponse_assessmentTargetArns :: Lens.Lens' ListAssessmentTargetsResponse [Prelude.Text]
listAssessmentTargetsResponse_assessmentTargetArns = Lens.lens (\ListAssessmentTargetsResponse' {assessmentTargetArns} -> assessmentTargetArns) (\s@ListAssessmentTargetsResponse' {} a -> s {assessmentTargetArns = a} :: ListAssessmentTargetsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAssessmentTargetsResponse where
  rnf ListAssessmentTargetsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf assessmentTargetArns
