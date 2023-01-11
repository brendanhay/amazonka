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
-- Module      : Amazonka.Inspector.ListAssessmentTemplates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the assessment templates that correspond to the assessment targets
-- that are specified by the ARNs of the assessment targets.
--
-- This operation returns paginated results.
module Amazonka.Inspector.ListAssessmentTemplates
  ( -- * Creating a Request
    ListAssessmentTemplates (..),
    newListAssessmentTemplates,

    -- * Request Lenses
    listAssessmentTemplates_assessmentTargetArns,
    listAssessmentTemplates_filter,
    listAssessmentTemplates_maxResults,
    listAssessmentTemplates_nextToken,

    -- * Destructuring the Response
    ListAssessmentTemplatesResponse (..),
    newListAssessmentTemplatesResponse,

    -- * Response Lenses
    listAssessmentTemplatesResponse_nextToken,
    listAssessmentTemplatesResponse_httpStatus,
    listAssessmentTemplatesResponse_assessmentTemplateArns,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssessmentTemplates' smart constructor.
data ListAssessmentTemplates = ListAssessmentTemplates'
  { -- | A list of ARNs that specifies the assessment targets whose assessment
    -- templates you want to list.
    assessmentTargetArns :: Prelude.Maybe [Prelude.Text],
    -- | You can use this parameter to specify a subset of data to be included in
    -- the action\'s response.
    --
    -- For a record to match a filter, all specified filter attributes must
    -- match. When multiple values are specified for a filter attribute, any of
    -- the values can match.
    filter' :: Prelude.Maybe AssessmentTemplateFilter,
    -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 10. The maximum value is 500.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the
    -- __ListAssessmentTemplates__ action. Subsequent calls to the action fill
    -- __nextToken__ in the request with the value of __NextToken__ from the
    -- previous response to continue listing data.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'maxResults', 'listAssessmentTemplates_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
--
-- 'nextToken', 'listAssessmentTemplates_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentTemplates__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
newListAssessmentTemplates ::
  ListAssessmentTemplates
newListAssessmentTemplates =
  ListAssessmentTemplates'
    { assessmentTargetArns =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A list of ARNs that specifies the assessment targets whose assessment
-- templates you want to list.
listAssessmentTemplates_assessmentTargetArns :: Lens.Lens' ListAssessmentTemplates (Prelude.Maybe [Prelude.Text])
listAssessmentTemplates_assessmentTargetArns = Lens.lens (\ListAssessmentTemplates' {assessmentTargetArns} -> assessmentTargetArns) (\s@ListAssessmentTemplates' {} a -> s {assessmentTargetArns = a} :: ListAssessmentTemplates) Prelude.. Lens.mapping Lens.coerced

-- | You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
listAssessmentTemplates_filter :: Lens.Lens' ListAssessmentTemplates (Prelude.Maybe AssessmentTemplateFilter)
listAssessmentTemplates_filter = Lens.lens (\ListAssessmentTemplates' {filter'} -> filter') (\s@ListAssessmentTemplates' {} a -> s {filter' = a} :: ListAssessmentTemplates)

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
listAssessmentTemplates_maxResults :: Lens.Lens' ListAssessmentTemplates (Prelude.Maybe Prelude.Int)
listAssessmentTemplates_maxResults = Lens.lens (\ListAssessmentTemplates' {maxResults} -> maxResults) (\s@ListAssessmentTemplates' {} a -> s {maxResults = a} :: ListAssessmentTemplates)

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentTemplates__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
listAssessmentTemplates_nextToken :: Lens.Lens' ListAssessmentTemplates (Prelude.Maybe Prelude.Text)
listAssessmentTemplates_nextToken = Lens.lens (\ListAssessmentTemplates' {nextToken} -> nextToken) (\s@ListAssessmentTemplates' {} a -> s {nextToken = a} :: ListAssessmentTemplates)

instance Core.AWSPager ListAssessmentTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssessmentTemplatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listAssessmentTemplatesResponse_assessmentTemplateArns
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAssessmentTemplates_nextToken
          Lens..~ rs
          Lens.^? listAssessmentTemplatesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAssessmentTemplates where
  type
    AWSResponse ListAssessmentTemplates =
      ListAssessmentTemplatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssessmentTemplatesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "assessmentTemplateArns"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAssessmentTemplates where
  hashWithSalt _salt ListAssessmentTemplates' {..} =
    _salt `Prelude.hashWithSalt` assessmentTargetArns
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListAssessmentTemplates where
  rnf ListAssessmentTemplates' {..} =
    Prelude.rnf assessmentTargetArns
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListAssessmentTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.ListAssessmentTemplates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAssessmentTemplates where
  toJSON ListAssessmentTemplates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assessmentTargetArns" Data..=)
              Prelude.<$> assessmentTargetArns,
            ("filter" Data..=) Prelude.<$> filter',
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListAssessmentTemplates where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAssessmentTemplates where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
listAssessmentTemplatesResponse_assessmentTemplateArns = Lens.lens (\ListAssessmentTemplatesResponse' {assessmentTemplateArns} -> assessmentTemplateArns) (\s@ListAssessmentTemplatesResponse' {} a -> s {assessmentTemplateArns = a} :: ListAssessmentTemplatesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListAssessmentTemplatesResponse
  where
  rnf ListAssessmentTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assessmentTemplateArns
