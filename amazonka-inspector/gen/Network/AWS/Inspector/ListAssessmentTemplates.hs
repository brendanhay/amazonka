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
-- Module      : Network.AWS.Inspector.ListAssessmentTemplates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the assessment templates that correspond to the assessment targets
-- that are specified by the ARNs of the assessment targets.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListAssessmentTemplates
  ( -- * Creating a Request
    ListAssessmentTemplates (..),
    newListAssessmentTemplates,

    -- * Request Lenses
    listAssessmentTemplates_nextToken,
    listAssessmentTemplates_maxResults,
    listAssessmentTemplates_assessmentTargetArns,
    listAssessmentTemplates_filter,

    -- * Destructuring the Response
    ListAssessmentTemplatesResponse (..),
    newListAssessmentTemplatesResponse,

    -- * Response Lenses
    listAssessmentTemplatesResponse_nextToken,
    listAssessmentTemplatesResponse_httpStatus,
    listAssessmentTemplatesResponse_assessmentTemplateArns,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAssessmentTemplates' smart constructor.
data ListAssessmentTemplates = ListAssessmentTemplates'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the
    -- __ListAssessmentTemplates__ action. Subsequent calls to the action fill
    -- __nextToken__ in the request with the value of __NextToken__ from the
    -- previous response to continue listing data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 10. The maximum value is 500.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A list of ARNs that specifies the assessment targets whose assessment
    -- templates you want to list.
    assessmentTargetArns :: Prelude.Maybe [Prelude.Text],
    -- | You can use this parameter to specify a subset of data to be included in
    -- the action\'s response.
    --
    -- For a record to match a filter, all specified filter attributes must
    -- match. When multiple values are specified for a filter attribute, any of
    -- the values can match.
    filter' :: Prelude.Maybe AssessmentTemplateFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentTemplates_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentTemplates__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
--
-- 'maxResults', 'listAssessmentTemplates_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
--
-- 'assessmentTargetArns', 'listAssessmentTemplates_assessmentTargetArns' - A list of ARNs that specifies the assessment targets whose assessment
-- templates you want to list.
--
-- 'filter'', 'listAssessmentTemplates_filter' - You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
newListAssessmentTemplates ::
  ListAssessmentTemplates
newListAssessmentTemplates =
  ListAssessmentTemplates'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      assessmentTargetArns = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentTemplates__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
listAssessmentTemplates_nextToken :: Lens.Lens' ListAssessmentTemplates (Prelude.Maybe Prelude.Text)
listAssessmentTemplates_nextToken = Lens.lens (\ListAssessmentTemplates' {nextToken} -> nextToken) (\s@ListAssessmentTemplates' {} a -> s {nextToken = a} :: ListAssessmentTemplates)

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
listAssessmentTemplates_maxResults :: Lens.Lens' ListAssessmentTemplates (Prelude.Maybe Prelude.Int)
listAssessmentTemplates_maxResults = Lens.lens (\ListAssessmentTemplates' {maxResults} -> maxResults) (\s@ListAssessmentTemplates' {} a -> s {maxResults = a} :: ListAssessmentTemplates)

-- | A list of ARNs that specifies the assessment targets whose assessment
-- templates you want to list.
listAssessmentTemplates_assessmentTargetArns :: Lens.Lens' ListAssessmentTemplates (Prelude.Maybe [Prelude.Text])
listAssessmentTemplates_assessmentTargetArns = Lens.lens (\ListAssessmentTemplates' {assessmentTargetArns} -> assessmentTargetArns) (\s@ListAssessmentTemplates' {} a -> s {assessmentTargetArns = a} :: ListAssessmentTemplates) Prelude.. Lens.mapping Prelude._Coerce

-- | You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
listAssessmentTemplates_filter :: Lens.Lens' ListAssessmentTemplates (Prelude.Maybe AssessmentTemplateFilter)
listAssessmentTemplates_filter = Lens.lens (\ListAssessmentTemplates' {filter'} -> filter') (\s@ListAssessmentTemplates' {} a -> s {filter' = a} :: ListAssessmentTemplates)

instance Pager.AWSPager ListAssessmentTemplates where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listAssessmentTemplatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^. listAssessmentTemplatesResponse_assessmentTemplateArns
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listAssessmentTemplates_nextToken
          Lens..~ rs
          Lens.^? listAssessmentTemplatesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListAssessmentTemplates where
  type
    Rs ListAssessmentTemplates =
      ListAssessmentTemplatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssessmentTemplatesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..?> "assessmentTemplateArns"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAssessmentTemplates

instance Prelude.NFData ListAssessmentTemplates

instance Prelude.ToHeaders ListAssessmentTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "InspectorService.ListAssessmentTemplates" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListAssessmentTemplates where
  toJSON ListAssessmentTemplates' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("maxResults" Prelude..=) Prelude.<$> maxResults,
            ("assessmentTargetArns" Prelude..=)
              Prelude.<$> assessmentTargetArns,
            ("filter" Prelude..=) Prelude.<$> filter'
          ]
      )

instance Prelude.ToPath ListAssessmentTemplates where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListAssessmentTemplates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAssessmentTemplatesResponse' smart constructor.
data ListAssessmentTemplatesResponse = ListAssessmentTemplatesResponse'
  { -- | When a response is generated, if there is more data to be listed, this
    -- parameter is present in the response and contains the value to use for
    -- the __nextToken__ parameter in a subsequent pagination request. If there
    -- is no more data to be listed, this parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of ARNs that specifies the assessment templates returned by the
    -- action.
    assessmentTemplateArns :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentTemplatesResponse_nextToken' - When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
--
-- 'httpStatus', 'listAssessmentTemplatesResponse_httpStatus' - The response's http status code.
--
-- 'assessmentTemplateArns', 'listAssessmentTemplatesResponse_assessmentTemplateArns' - A list of ARNs that specifies the assessment templates returned by the
-- action.
newListAssessmentTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssessmentTemplatesResponse
newListAssessmentTemplatesResponse pHttpStatus_ =
  ListAssessmentTemplatesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      assessmentTemplateArns = Prelude.mempty
    }

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
listAssessmentTemplatesResponse_nextToken :: Lens.Lens' ListAssessmentTemplatesResponse (Prelude.Maybe Prelude.Text)
listAssessmentTemplatesResponse_nextToken = Lens.lens (\ListAssessmentTemplatesResponse' {nextToken} -> nextToken) (\s@ListAssessmentTemplatesResponse' {} a -> s {nextToken = a} :: ListAssessmentTemplatesResponse)

-- | The response's http status code.
listAssessmentTemplatesResponse_httpStatus :: Lens.Lens' ListAssessmentTemplatesResponse Prelude.Int
listAssessmentTemplatesResponse_httpStatus = Lens.lens (\ListAssessmentTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListAssessmentTemplatesResponse' {} a -> s {httpStatus = a} :: ListAssessmentTemplatesResponse)

-- | A list of ARNs that specifies the assessment templates returned by the
-- action.
listAssessmentTemplatesResponse_assessmentTemplateArns :: Lens.Lens' ListAssessmentTemplatesResponse [Prelude.Text]
listAssessmentTemplatesResponse_assessmentTemplateArns = Lens.lens (\ListAssessmentTemplatesResponse' {assessmentTemplateArns} -> assessmentTemplateArns) (\s@ListAssessmentTemplatesResponse' {} a -> s {assessmentTemplateArns = a} :: ListAssessmentTemplatesResponse) Prelude.. Prelude._Coerce

instance
  Prelude.NFData
    ListAssessmentTemplatesResponse
