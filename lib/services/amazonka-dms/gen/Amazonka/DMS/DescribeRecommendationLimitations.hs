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
-- Module      : Amazonka.DMS.DescribeRecommendationLimitations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of limitations for recommendations of target
-- Amazon Web Services engines.
module Amazonka.DMS.DescribeRecommendationLimitations
  ( -- * Creating a Request
    DescribeRecommendationLimitations (..),
    newDescribeRecommendationLimitations,

    -- * Request Lenses
    describeRecommendationLimitations_filters,
    describeRecommendationLimitations_maxRecords,
    describeRecommendationLimitations_nextToken,

    -- * Destructuring the Response
    DescribeRecommendationLimitationsResponse (..),
    newDescribeRecommendationLimitationsResponse,

    -- * Response Lenses
    describeRecommendationLimitationsResponse_limitations,
    describeRecommendationLimitationsResponse_nextToken,
    describeRecommendationLimitationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRecommendationLimitations' smart constructor.
data DescribeRecommendationLimitations = DescribeRecommendationLimitations'
  { -- | Filters applied to the limitations described in the form of key-value
    -- pairs.
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
-- Create a value of 'DescribeRecommendationLimitations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeRecommendationLimitations_filters' - Filters applied to the limitations described in the form of key-value
-- pairs.
--
-- 'maxRecords', 'describeRecommendationLimitations_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, Fleet Advisor
-- includes a pagination token in the response so that you can retrieve the
-- remaining results.
--
-- 'nextToken', 'describeRecommendationLimitations_nextToken' - Specifies the unique pagination token that makes it possible to display
-- the next page of results. If this parameter is specified, the response
-- includes only records beyond the marker, up to the value specified by
-- @MaxRecords@.
--
-- If @NextToken@ is returned by a previous response, there are more
-- results available. The value of @NextToken@ is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
newDescribeRecommendationLimitations ::
  DescribeRecommendationLimitations
newDescribeRecommendationLimitations =
  DescribeRecommendationLimitations'
    { filters =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters applied to the limitations described in the form of key-value
-- pairs.
describeRecommendationLimitations_filters :: Lens.Lens' DescribeRecommendationLimitations (Prelude.Maybe [Filter])
describeRecommendationLimitations_filters = Lens.lens (\DescribeRecommendationLimitations' {filters} -> filters) (\s@DescribeRecommendationLimitations' {} a -> s {filters = a} :: DescribeRecommendationLimitations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, Fleet Advisor
-- includes a pagination token in the response so that you can retrieve the
-- remaining results.
describeRecommendationLimitations_maxRecords :: Lens.Lens' DescribeRecommendationLimitations (Prelude.Maybe Prelude.Int)
describeRecommendationLimitations_maxRecords = Lens.lens (\DescribeRecommendationLimitations' {maxRecords} -> maxRecords) (\s@DescribeRecommendationLimitations' {} a -> s {maxRecords = a} :: DescribeRecommendationLimitations)

-- | Specifies the unique pagination token that makes it possible to display
-- the next page of results. If this parameter is specified, the response
-- includes only records beyond the marker, up to the value specified by
-- @MaxRecords@.
--
-- If @NextToken@ is returned by a previous response, there are more
-- results available. The value of @NextToken@ is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeRecommendationLimitations_nextToken :: Lens.Lens' DescribeRecommendationLimitations (Prelude.Maybe Prelude.Text)
describeRecommendationLimitations_nextToken = Lens.lens (\DescribeRecommendationLimitations' {nextToken} -> nextToken) (\s@DescribeRecommendationLimitations' {} a -> s {nextToken = a} :: DescribeRecommendationLimitations)

instance
  Core.AWSRequest
    DescribeRecommendationLimitations
  where
  type
    AWSResponse DescribeRecommendationLimitations =
      DescribeRecommendationLimitationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRecommendationLimitationsResponse'
            Prelude.<$> (x Data..?> "Limitations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeRecommendationLimitations
  where
  hashWithSalt
    _salt
    DescribeRecommendationLimitations' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeRecommendationLimitations
  where
  rnf DescribeRecommendationLimitations' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeRecommendationLimitations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeRecommendationLimitations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeRecommendationLimitations
  where
  toJSON DescribeRecommendationLimitations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxRecords" Data..=) Prelude.<$> maxRecords,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance
  Data.ToPath
    DescribeRecommendationLimitations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeRecommendationLimitations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRecommendationLimitationsResponse' smart constructor.
data DescribeRecommendationLimitationsResponse = DescribeRecommendationLimitationsResponse'
  { -- | The list of limitations for recommendations of target Amazon Web
    -- Services engines.
    limitations :: Prelude.Maybe [Limitation],
    -- | The unique pagination token returned for you to pass to a subsequent
    -- request. Fleet Advisor returns this token when the number of records in
    -- the response is greater than the @MaxRecords@ value. To retrieve the
    -- next page, make the call again using the returned token and keeping all
    -- other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecommendationLimitationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limitations', 'describeRecommendationLimitationsResponse_limitations' - The list of limitations for recommendations of target Amazon Web
-- Services engines.
--
-- 'nextToken', 'describeRecommendationLimitationsResponse_nextToken' - The unique pagination token returned for you to pass to a subsequent
-- request. Fleet Advisor returns this token when the number of records in
-- the response is greater than the @MaxRecords@ value. To retrieve the
-- next page, make the call again using the returned token and keeping all
-- other arguments unchanged.
--
-- 'httpStatus', 'describeRecommendationLimitationsResponse_httpStatus' - The response's http status code.
newDescribeRecommendationLimitationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRecommendationLimitationsResponse
newDescribeRecommendationLimitationsResponse
  pHttpStatus_ =
    DescribeRecommendationLimitationsResponse'
      { limitations =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of limitations for recommendations of target Amazon Web
-- Services engines.
describeRecommendationLimitationsResponse_limitations :: Lens.Lens' DescribeRecommendationLimitationsResponse (Prelude.Maybe [Limitation])
describeRecommendationLimitationsResponse_limitations = Lens.lens (\DescribeRecommendationLimitationsResponse' {limitations} -> limitations) (\s@DescribeRecommendationLimitationsResponse' {} a -> s {limitations = a} :: DescribeRecommendationLimitationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique pagination token returned for you to pass to a subsequent
-- request. Fleet Advisor returns this token when the number of records in
-- the response is greater than the @MaxRecords@ value. To retrieve the
-- next page, make the call again using the returned token and keeping all
-- other arguments unchanged.
describeRecommendationLimitationsResponse_nextToken :: Lens.Lens' DescribeRecommendationLimitationsResponse (Prelude.Maybe Prelude.Text)
describeRecommendationLimitationsResponse_nextToken = Lens.lens (\DescribeRecommendationLimitationsResponse' {nextToken} -> nextToken) (\s@DescribeRecommendationLimitationsResponse' {} a -> s {nextToken = a} :: DescribeRecommendationLimitationsResponse)

-- | The response's http status code.
describeRecommendationLimitationsResponse_httpStatus :: Lens.Lens' DescribeRecommendationLimitationsResponse Prelude.Int
describeRecommendationLimitationsResponse_httpStatus = Lens.lens (\DescribeRecommendationLimitationsResponse' {httpStatus} -> httpStatus) (\s@DescribeRecommendationLimitationsResponse' {} a -> s {httpStatus = a} :: DescribeRecommendationLimitationsResponse)

instance
  Prelude.NFData
    DescribeRecommendationLimitationsResponse
  where
  rnf DescribeRecommendationLimitationsResponse' {..} =
    Prelude.rnf limitations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
