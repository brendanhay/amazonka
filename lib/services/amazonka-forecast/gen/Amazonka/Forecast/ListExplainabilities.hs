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
-- Module      : Amazonka.Forecast.ListExplainabilities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Explainability resources created using the
-- CreateExplainability operation. This operation returns a summary for
-- each Explainability. You can filter the list using an array of Filter
-- objects.
--
-- To retrieve the complete set of properties for a particular
-- Explainability resource, use the ARN with the DescribeExplainability
-- operation.
--
-- This operation returns paginated results.
module Amazonka.Forecast.ListExplainabilities
  ( -- * Creating a Request
    ListExplainabilities (..),
    newListExplainabilities,

    -- * Request Lenses
    listExplainabilities_filters,
    listExplainabilities_maxResults,
    listExplainabilities_nextToken,

    -- * Destructuring the Response
    ListExplainabilitiesResponse (..),
    newListExplainabilitiesResponse,

    -- * Response Lenses
    listExplainabilitiesResponse_explainabilities,
    listExplainabilitiesResponse_nextToken,
    listExplainabilitiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListExplainabilities' smart constructor.
data ListExplainabilities = ListExplainabilities'
  { -- | An array of filters. For each filter, provide a condition and a match
    -- statement. The condition is either @IS@ or @IS_NOT@, which specifies
    -- whether to include or exclude the resources that match the statement
    -- from the list. The match statement consists of a key and a value.
    --
    -- __Filter properties__
    --
    -- -   @Condition@ - The condition to apply. Valid values are @IS@ and
    --     @IS_NOT@.
    --
    -- -   @Key@ - The name of the parameter to filter on. Valid values are
    --     @ResourceArn@ and @Status@.
    --
    -- -   @Value@ - The value to match.
    filters :: Prelude.Maybe [Filter],
    -- | The number of items returned in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the result of the previous request was truncated, the response
    -- includes a NextToken. To retrieve the next set of results, use the token
    -- in the next request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExplainabilities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listExplainabilities_filters' - An array of filters. For each filter, provide a condition and a match
-- statement. The condition is either @IS@ or @IS_NOT@, which specifies
-- whether to include or exclude the resources that match the statement
-- from the list. The match statement consists of a key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @ResourceArn@ and @Status@.
--
-- -   @Value@ - The value to match.
--
-- 'maxResults', 'listExplainabilities_maxResults' - The number of items returned in the response.
--
-- 'nextToken', 'listExplainabilities_nextToken' - If the result of the previous request was truncated, the response
-- includes a NextToken. To retrieve the next set of results, use the token
-- in the next request. Tokens expire after 24 hours.
newListExplainabilities ::
  ListExplainabilities
newListExplainabilities =
  ListExplainabilities'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An array of filters. For each filter, provide a condition and a match
-- statement. The condition is either @IS@ or @IS_NOT@, which specifies
-- whether to include or exclude the resources that match the statement
-- from the list. The match statement consists of a key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @ResourceArn@ and @Status@.
--
-- -   @Value@ - The value to match.
listExplainabilities_filters :: Lens.Lens' ListExplainabilities (Prelude.Maybe [Filter])
listExplainabilities_filters = Lens.lens (\ListExplainabilities' {filters} -> filters) (\s@ListExplainabilities' {} a -> s {filters = a} :: ListExplainabilities) Prelude.. Lens.mapping Lens.coerced

-- | The number of items returned in the response.
listExplainabilities_maxResults :: Lens.Lens' ListExplainabilities (Prelude.Maybe Prelude.Natural)
listExplainabilities_maxResults = Lens.lens (\ListExplainabilities' {maxResults} -> maxResults) (\s@ListExplainabilities' {} a -> s {maxResults = a} :: ListExplainabilities)

-- | If the result of the previous request was truncated, the response
-- includes a NextToken. To retrieve the next set of results, use the token
-- in the next request. Tokens expire after 24 hours.
listExplainabilities_nextToken :: Lens.Lens' ListExplainabilities (Prelude.Maybe Prelude.Text)
listExplainabilities_nextToken = Lens.lens (\ListExplainabilities' {nextToken} -> nextToken) (\s@ListExplainabilities' {} a -> s {nextToken = a} :: ListExplainabilities)

instance Core.AWSPager ListExplainabilities where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listExplainabilitiesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listExplainabilitiesResponse_explainabilities
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listExplainabilities_nextToken
          Lens..~ rs
          Lens.^? listExplainabilitiesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListExplainabilities where
  type
    AWSResponse ListExplainabilities =
      ListExplainabilitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExplainabilitiesResponse'
            Prelude.<$> ( x
                            Data..?> "Explainabilities"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExplainabilities where
  hashWithSalt _salt ListExplainabilities' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListExplainabilities where
  rnf ListExplainabilities' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListExplainabilities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.ListExplainabilities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListExplainabilities where
  toJSON ListExplainabilities' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListExplainabilities where
  toPath = Prelude.const "/"

instance Data.ToQuery ListExplainabilities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListExplainabilitiesResponse' smart constructor.
data ListExplainabilitiesResponse = ListExplainabilitiesResponse'
  { -- | An array of objects that summarize the properties of each Explainability
    -- resource.
    explainabilities :: Prelude.Maybe [ExplainabilitySummary],
    -- | Returns this token if the response is truncated. To retrieve the next
    -- set of results, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExplainabilitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'explainabilities', 'listExplainabilitiesResponse_explainabilities' - An array of objects that summarize the properties of each Explainability
-- resource.
--
-- 'nextToken', 'listExplainabilitiesResponse_nextToken' - Returns this token if the response is truncated. To retrieve the next
-- set of results, use the token in the next request.
--
-- 'httpStatus', 'listExplainabilitiesResponse_httpStatus' - The response's http status code.
newListExplainabilitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExplainabilitiesResponse
newListExplainabilitiesResponse pHttpStatus_ =
  ListExplainabilitiesResponse'
    { explainabilities =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that summarize the properties of each Explainability
-- resource.
listExplainabilitiesResponse_explainabilities :: Lens.Lens' ListExplainabilitiesResponse (Prelude.Maybe [ExplainabilitySummary])
listExplainabilitiesResponse_explainabilities = Lens.lens (\ListExplainabilitiesResponse' {explainabilities} -> explainabilities) (\s@ListExplainabilitiesResponse' {} a -> s {explainabilities = a} :: ListExplainabilitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Returns this token if the response is truncated. To retrieve the next
-- set of results, use the token in the next request.
listExplainabilitiesResponse_nextToken :: Lens.Lens' ListExplainabilitiesResponse (Prelude.Maybe Prelude.Text)
listExplainabilitiesResponse_nextToken = Lens.lens (\ListExplainabilitiesResponse' {nextToken} -> nextToken) (\s@ListExplainabilitiesResponse' {} a -> s {nextToken = a} :: ListExplainabilitiesResponse)

-- | The response's http status code.
listExplainabilitiesResponse_httpStatus :: Lens.Lens' ListExplainabilitiesResponse Prelude.Int
listExplainabilitiesResponse_httpStatus = Lens.lens (\ListExplainabilitiesResponse' {httpStatus} -> httpStatus) (\s@ListExplainabilitiesResponse' {} a -> s {httpStatus = a} :: ListExplainabilitiesResponse)

instance Prelude.NFData ListExplainabilitiesResponse where
  rnf ListExplainabilitiesResponse' {..} =
    Prelude.rnf explainabilities
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
