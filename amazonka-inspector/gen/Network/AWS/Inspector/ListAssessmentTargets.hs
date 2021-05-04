{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Inspector.ListAssessmentTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the ARNs of the assessment targets within this AWS account. For
-- more information about assessment targets, see
-- <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_applications.html Amazon Inspector Assessment Targets>.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListAssessmentTargets
  ( -- * Creating a Request
    ListAssessmentTargets (..),
    newListAssessmentTargets,

    -- * Request Lenses
    listAssessmentTargets_nextToken,
    listAssessmentTargets_maxResults,
    listAssessmentTargets_filter,

    -- * Destructuring the Response
    ListAssessmentTargetsResponse (..),
    newListAssessmentTargetsResponse,

    -- * Response Lenses
    listAssessmentTargetsResponse_nextToken,
    listAssessmentTargetsResponse_httpStatus,
    listAssessmentTargetsResponse_assessmentTargetArns,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAssessmentTargets' smart constructor.
data ListAssessmentTargets = ListAssessmentTargets'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the
    -- __ListAssessmentTargets__ action. Subsequent calls to the action fill
    -- __nextToken__ in the request with the value of __NextToken__ from the
    -- previous response to continue listing data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 10. The maximum value is 500.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | You can use this parameter to specify a subset of data to be included in
    -- the action\'s response.
    --
    -- For a record to match a filter, all specified filter attributes must
    -- match. When multiple values are specified for a filter attribute, any of
    -- the values can match.
    filter' :: Prelude.Maybe AssessmentTargetFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentTargets_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentTargets__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
--
-- 'maxResults', 'listAssessmentTargets_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
--
-- 'filter'', 'listAssessmentTargets_filter' - You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
newListAssessmentTargets ::
  ListAssessmentTargets
newListAssessmentTargets =
  ListAssessmentTargets'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentTargets__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
listAssessmentTargets_nextToken :: Lens.Lens' ListAssessmentTargets (Prelude.Maybe Prelude.Text)
listAssessmentTargets_nextToken = Lens.lens (\ListAssessmentTargets' {nextToken} -> nextToken) (\s@ListAssessmentTargets' {} a -> s {nextToken = a} :: ListAssessmentTargets)

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
listAssessmentTargets_maxResults :: Lens.Lens' ListAssessmentTargets (Prelude.Maybe Prelude.Int)
listAssessmentTargets_maxResults = Lens.lens (\ListAssessmentTargets' {maxResults} -> maxResults) (\s@ListAssessmentTargets' {} a -> s {maxResults = a} :: ListAssessmentTargets)

-- | You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
listAssessmentTargets_filter :: Lens.Lens' ListAssessmentTargets (Prelude.Maybe AssessmentTargetFilter)
listAssessmentTargets_filter = Lens.lens (\ListAssessmentTargets' {filter'} -> filter') (\s@ListAssessmentTargets' {} a -> s {filter' = a} :: ListAssessmentTargets)

instance Pager.AWSPager ListAssessmentTargets where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listAssessmentTargetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^. listAssessmentTargetsResponse_assessmentTargetArns
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listAssessmentTargets_nextToken
          Lens..~ rs
          Lens.^? listAssessmentTargetsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListAssessmentTargets where
  type
    Rs ListAssessmentTargets =
      ListAssessmentTargetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssessmentTargetsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..?> "assessmentTargetArns"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAssessmentTargets

instance Prelude.NFData ListAssessmentTargets

instance Prelude.ToHeaders ListAssessmentTargets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "InspectorService.ListAssessmentTargets" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListAssessmentTargets where
  toJSON ListAssessmentTargets' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("maxResults" Prelude..=) Prelude.<$> maxResults,
            ("filter" Prelude..=) Prelude.<$> filter'
          ]
      )

instance Prelude.ToPath ListAssessmentTargets where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListAssessmentTargets where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listAssessmentTargetsResponse_assessmentTargetArns = Lens.lens (\ListAssessmentTargetsResponse' {assessmentTargetArns} -> assessmentTargetArns) (\s@ListAssessmentTargetsResponse' {} a -> s {assessmentTargetArns = a} :: ListAssessmentTargetsResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListAssessmentTargetsResponse
