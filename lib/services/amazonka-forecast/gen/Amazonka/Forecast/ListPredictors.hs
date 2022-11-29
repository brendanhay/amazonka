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
-- Module      : Amazonka.Forecast.ListPredictors
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of predictors created using the CreateAutoPredictor or
-- CreatePredictor operations. For each predictor, this operation returns a
-- summary of its properties, including its Amazon Resource Name (ARN).
--
-- You can retrieve the complete set of properties by using the ARN with
-- the DescribeAutoPredictor and DescribePredictor operations. You can
-- filter the list using an array of Filter objects.
--
-- This operation returns paginated results.
module Amazonka.Forecast.ListPredictors
  ( -- * Creating a Request
    ListPredictors (..),
    newListPredictors,

    -- * Request Lenses
    listPredictors_nextToken,
    listPredictors_filters,
    listPredictors_maxResults,

    -- * Destructuring the Response
    ListPredictorsResponse (..),
    newListPredictorsResponse,

    -- * Response Lenses
    listPredictorsResponse_nextToken,
    listPredictorsResponse_predictors,
    listPredictorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPredictors' smart constructor.
data ListPredictors = ListPredictors'
  { -- | If the result of the previous request was truncated, the response
    -- includes a @NextToken@. To retrieve the next set of results, use the
    -- token in the next request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of filters. For each filter, you provide a condition and a
    -- match statement. The condition is either @IS@ or @IS_NOT@, which
    -- specifies whether to include or exclude the predictors that match the
    -- statement from the list, respectively. The match statement consists of a
    -- key and a value.
    --
    -- __Filter properties__
    --
    -- -   @Condition@ - The condition to apply. Valid values are @IS@ and
    --     @IS_NOT@. To include the predictors that match the statement,
    --     specify @IS@. To exclude matching predictors, specify @IS_NOT@.
    --
    -- -   @Key@ - The name of the parameter to filter on. Valid values are
    --     @DatasetGroupArn@ and @Status@.
    --
    -- -   @Value@ - The value to match.
    --
    -- For example, to list all predictors whose status is ACTIVE, you would
    -- specify:
    --
    -- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"Status\", \"Value\": \"ACTIVE\" } ]@
    filters :: Prelude.Maybe [Filter],
    -- | The number of items to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPredictors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPredictors_nextToken' - If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
--
-- 'filters', 'listPredictors_filters' - An array of filters. For each filter, you provide a condition and a
-- match statement. The condition is either @IS@ or @IS_NOT@, which
-- specifies whether to include or exclude the predictors that match the
-- statement from the list, respectively. The match statement consists of a
-- key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@. To include the predictors that match the statement,
--     specify @IS@. To exclude matching predictors, specify @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @DatasetGroupArn@ and @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all predictors whose status is ACTIVE, you would
-- specify:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"Status\", \"Value\": \"ACTIVE\" } ]@
--
-- 'maxResults', 'listPredictors_maxResults' - The number of items to return in the response.
newListPredictors ::
  ListPredictors
newListPredictors =
  ListPredictors'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
listPredictors_nextToken :: Lens.Lens' ListPredictors (Prelude.Maybe Prelude.Text)
listPredictors_nextToken = Lens.lens (\ListPredictors' {nextToken} -> nextToken) (\s@ListPredictors' {} a -> s {nextToken = a} :: ListPredictors)

-- | An array of filters. For each filter, you provide a condition and a
-- match statement. The condition is either @IS@ or @IS_NOT@, which
-- specifies whether to include or exclude the predictors that match the
-- statement from the list, respectively. The match statement consists of a
-- key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@. To include the predictors that match the statement,
--     specify @IS@. To exclude matching predictors, specify @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @DatasetGroupArn@ and @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all predictors whose status is ACTIVE, you would
-- specify:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"Status\", \"Value\": \"ACTIVE\" } ]@
listPredictors_filters :: Lens.Lens' ListPredictors (Prelude.Maybe [Filter])
listPredictors_filters = Lens.lens (\ListPredictors' {filters} -> filters) (\s@ListPredictors' {} a -> s {filters = a} :: ListPredictors) Prelude.. Lens.mapping Lens.coerced

-- | The number of items to return in the response.
listPredictors_maxResults :: Lens.Lens' ListPredictors (Prelude.Maybe Prelude.Natural)
listPredictors_maxResults = Lens.lens (\ListPredictors' {maxResults} -> maxResults) (\s@ListPredictors' {} a -> s {maxResults = a} :: ListPredictors)

instance Core.AWSPager ListPredictors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPredictorsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPredictorsResponse_predictors
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPredictors_nextToken
          Lens..~ rs
          Lens.^? listPredictorsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListPredictors where
  type
    AWSResponse ListPredictors =
      ListPredictorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPredictorsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Predictors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPredictors where
  hashWithSalt _salt ListPredictors' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListPredictors where
  rnf ListPredictors' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListPredictors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonForecast.ListPredictors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPredictors where
  toJSON ListPredictors' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListPredictors where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPredictors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPredictorsResponse' smart constructor.
data ListPredictorsResponse = ListPredictorsResponse'
  { -- | If the response is truncated, Amazon Forecast returns this token. To
    -- retrieve the next set of results, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that summarize each predictor\'s properties.
    predictors :: Prelude.Maybe [PredictorSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPredictorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPredictorsResponse_nextToken' - If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
--
-- 'predictors', 'listPredictorsResponse_predictors' - An array of objects that summarize each predictor\'s properties.
--
-- 'httpStatus', 'listPredictorsResponse_httpStatus' - The response's http status code.
newListPredictorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPredictorsResponse
newListPredictorsResponse pHttpStatus_ =
  ListPredictorsResponse'
    { nextToken =
        Prelude.Nothing,
      predictors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
listPredictorsResponse_nextToken :: Lens.Lens' ListPredictorsResponse (Prelude.Maybe Prelude.Text)
listPredictorsResponse_nextToken = Lens.lens (\ListPredictorsResponse' {nextToken} -> nextToken) (\s@ListPredictorsResponse' {} a -> s {nextToken = a} :: ListPredictorsResponse)

-- | An array of objects that summarize each predictor\'s properties.
listPredictorsResponse_predictors :: Lens.Lens' ListPredictorsResponse (Prelude.Maybe [PredictorSummary])
listPredictorsResponse_predictors = Lens.lens (\ListPredictorsResponse' {predictors} -> predictors) (\s@ListPredictorsResponse' {} a -> s {predictors = a} :: ListPredictorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPredictorsResponse_httpStatus :: Lens.Lens' ListPredictorsResponse Prelude.Int
listPredictorsResponse_httpStatus = Lens.lens (\ListPredictorsResponse' {httpStatus} -> httpStatus) (\s@ListPredictorsResponse' {} a -> s {httpStatus = a} :: ListPredictorsResponse)

instance Prelude.NFData ListPredictorsResponse where
  rnf ListPredictorsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf predictors
      `Prelude.seq` Prelude.rnf httpStatus
