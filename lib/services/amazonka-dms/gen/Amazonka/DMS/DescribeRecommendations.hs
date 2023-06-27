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
-- Module      : Amazonka.DMS.DescribeRecommendations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of target engine recommendations for your
-- source databases.
module Amazonka.DMS.DescribeRecommendations
  ( -- * Creating a Request
    DescribeRecommendations (..),
    newDescribeRecommendations,

    -- * Request Lenses
    describeRecommendations_filters,
    describeRecommendations_maxRecords,
    describeRecommendations_nextToken,

    -- * Destructuring the Response
    DescribeRecommendationsResponse (..),
    newDescribeRecommendationsResponse,

    -- * Response Lenses
    describeRecommendationsResponse_nextToken,
    describeRecommendationsResponse_recommendations,
    describeRecommendationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRecommendations' smart constructor.
data DescribeRecommendations = DescribeRecommendations'
  { -- | Filters applied to the target engine recommendations described in the
    -- form of key-value pairs.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, Fleet Advisor
    -- includes a pagination token in the response so that you can retrieve the
    -- remaining results.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | Specifies the unique pagination token that makes it possible to display
    -- the next page of results. If this parameter is specified, the response
    -- includes only records beyond the marker, up to the value specified by
    -- @MaxRecords@.
    --
    -- If @NextToken@ is returned by a previous response, there are more
    -- results available. The value of @NextToken@ is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeRecommendations_filters' - Filters applied to the target engine recommendations described in the
-- form of key-value pairs.
--
-- 'maxRecords', 'describeRecommendations_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, Fleet Advisor
-- includes a pagination token in the response so that you can retrieve the
-- remaining results.
--
-- 'nextToken', 'describeRecommendations_nextToken' - Specifies the unique pagination token that makes it possible to display
-- the next page of results. If this parameter is specified, the response
-- includes only records beyond the marker, up to the value specified by
-- @MaxRecords@.
--
-- If @NextToken@ is returned by a previous response, there are more
-- results available. The value of @NextToken@ is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
newDescribeRecommendations ::
  DescribeRecommendations
newDescribeRecommendations =
  DescribeRecommendations'
    { filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters applied to the target engine recommendations described in the
-- form of key-value pairs.
describeRecommendations_filters :: Lens.Lens' DescribeRecommendations (Prelude.Maybe [Filter])
describeRecommendations_filters = Lens.lens (\DescribeRecommendations' {filters} -> filters) (\s@DescribeRecommendations' {} a -> s {filters = a} :: DescribeRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, Fleet Advisor
-- includes a pagination token in the response so that you can retrieve the
-- remaining results.
describeRecommendations_maxRecords :: Lens.Lens' DescribeRecommendations (Prelude.Maybe Prelude.Int)
describeRecommendations_maxRecords = Lens.lens (\DescribeRecommendations' {maxRecords} -> maxRecords) (\s@DescribeRecommendations' {} a -> s {maxRecords = a} :: DescribeRecommendations)

-- | Specifies the unique pagination token that makes it possible to display
-- the next page of results. If this parameter is specified, the response
-- includes only records beyond the marker, up to the value specified by
-- @MaxRecords@.
--
-- If @NextToken@ is returned by a previous response, there are more
-- results available. The value of @NextToken@ is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeRecommendations_nextToken :: Lens.Lens' DescribeRecommendations (Prelude.Maybe Prelude.Text)
describeRecommendations_nextToken = Lens.lens (\DescribeRecommendations' {nextToken} -> nextToken) (\s@DescribeRecommendations' {} a -> s {nextToken = a} :: DescribeRecommendations)

instance Core.AWSRequest DescribeRecommendations where
  type
    AWSResponse DescribeRecommendations =
      DescribeRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRecommendationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "Recommendations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRecommendations where
  hashWithSalt _salt DescribeRecommendations' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeRecommendations where
  rnf DescribeRecommendations' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeRecommendations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeRecommendations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRecommendations where
  toJSON DescribeRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxRecords" Data..=) Prelude.<$> maxRecords,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeRecommendations where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRecommendationsResponse' smart constructor.
data DescribeRecommendationsResponse = DescribeRecommendationsResponse'
  { -- | The unique pagination token returned for you to pass to a subsequent
    -- request. Fleet Advisor returns this token when the number of records in
    -- the response is greater than the @MaxRecords@ value. To retrieve the
    -- next page, make the call again using the returned token and keeping all
    -- other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of recommendations of target engines that Fleet Advisor created
    -- for the source database.
    recommendations :: Prelude.Maybe [Recommendation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRecommendationsResponse_nextToken' - The unique pagination token returned for you to pass to a subsequent
-- request. Fleet Advisor returns this token when the number of records in
-- the response is greater than the @MaxRecords@ value. To retrieve the
-- next page, make the call again using the returned token and keeping all
-- other arguments unchanged.
--
-- 'recommendations', 'describeRecommendationsResponse_recommendations' - The list of recommendations of target engines that Fleet Advisor created
-- for the source database.
--
-- 'httpStatus', 'describeRecommendationsResponse_httpStatus' - The response's http status code.
newDescribeRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRecommendationsResponse
newDescribeRecommendationsResponse pHttpStatus_ =
  DescribeRecommendationsResponse'
    { nextToken =
        Prelude.Nothing,
      recommendations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique pagination token returned for you to pass to a subsequent
-- request. Fleet Advisor returns this token when the number of records in
-- the response is greater than the @MaxRecords@ value. To retrieve the
-- next page, make the call again using the returned token and keeping all
-- other arguments unchanged.
describeRecommendationsResponse_nextToken :: Lens.Lens' DescribeRecommendationsResponse (Prelude.Maybe Prelude.Text)
describeRecommendationsResponse_nextToken = Lens.lens (\DescribeRecommendationsResponse' {nextToken} -> nextToken) (\s@DescribeRecommendationsResponse' {} a -> s {nextToken = a} :: DescribeRecommendationsResponse)

-- | The list of recommendations of target engines that Fleet Advisor created
-- for the source database.
describeRecommendationsResponse_recommendations :: Lens.Lens' DescribeRecommendationsResponse (Prelude.Maybe [Recommendation])
describeRecommendationsResponse_recommendations = Lens.lens (\DescribeRecommendationsResponse' {recommendations} -> recommendations) (\s@DescribeRecommendationsResponse' {} a -> s {recommendations = a} :: DescribeRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRecommendationsResponse_httpStatus :: Lens.Lens' DescribeRecommendationsResponse Prelude.Int
describeRecommendationsResponse_httpStatus = Lens.lens (\DescribeRecommendationsResponse' {httpStatus} -> httpStatus) (\s@DescribeRecommendationsResponse' {} a -> s {httpStatus = a} :: DescribeRecommendationsResponse)

instance
  Prelude.NFData
    DescribeRecommendationsResponse
  where
  rnf DescribeRecommendationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recommendations
      `Prelude.seq` Prelude.rnf httpStatus
