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
-- Module      : Amazonka.Config.DescribeConformancePacks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of one or more conformance packs.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribeConformancePacks
  ( -- * Creating a Request
    DescribeConformancePacks (..),
    newDescribeConformancePacks,

    -- * Request Lenses
    describeConformancePacks_conformancePackNames,
    describeConformancePacks_limit,
    describeConformancePacks_nextToken,

    -- * Destructuring the Response
    DescribeConformancePacksResponse (..),
    newDescribeConformancePacksResponse,

    -- * Response Lenses
    describeConformancePacksResponse_conformancePackDetails,
    describeConformancePacksResponse_nextToken,
    describeConformancePacksResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeConformancePacks' smart constructor.
data DescribeConformancePacks = DescribeConformancePacks'
  { -- | Comma-separated list of conformance pack names for which you want
    -- details. If you do not specify any names, Config returns details for all
    -- your conformance packs.
    conformancePackNames :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of conformance packs returned on each page.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConformancePacks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conformancePackNames', 'describeConformancePacks_conformancePackNames' - Comma-separated list of conformance pack names for which you want
-- details. If you do not specify any names, Config returns details for all
-- your conformance packs.
--
-- 'limit', 'describeConformancePacks_limit' - The maximum number of conformance packs returned on each page.
--
-- 'nextToken', 'describeConformancePacks_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
newDescribeConformancePacks ::
  DescribeConformancePacks
newDescribeConformancePacks =
  DescribeConformancePacks'
    { conformancePackNames =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Comma-separated list of conformance pack names for which you want
-- details. If you do not specify any names, Config returns details for all
-- your conformance packs.
describeConformancePacks_conformancePackNames :: Lens.Lens' DescribeConformancePacks (Prelude.Maybe [Prelude.Text])
describeConformancePacks_conformancePackNames = Lens.lens (\DescribeConformancePacks' {conformancePackNames} -> conformancePackNames) (\s@DescribeConformancePacks' {} a -> s {conformancePackNames = a} :: DescribeConformancePacks) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of conformance packs returned on each page.
describeConformancePacks_limit :: Lens.Lens' DescribeConformancePacks (Prelude.Maybe Prelude.Natural)
describeConformancePacks_limit = Lens.lens (\DescribeConformancePacks' {limit} -> limit) (\s@DescribeConformancePacks' {} a -> s {limit = a} :: DescribeConformancePacks)

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeConformancePacks_nextToken :: Lens.Lens' DescribeConformancePacks (Prelude.Maybe Prelude.Text)
describeConformancePacks_nextToken = Lens.lens (\DescribeConformancePacks' {nextToken} -> nextToken) (\s@DescribeConformancePacks' {} a -> s {nextToken = a} :: DescribeConformancePacks)

instance Core.AWSPager DescribeConformancePacks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeConformancePacksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeConformancePacksResponse_conformancePackDetails
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeConformancePacks_nextToken
          Lens..~ rs
          Lens.^? describeConformancePacksResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeConformancePacks where
  type
    AWSResponse DescribeConformancePacks =
      DescribeConformancePacksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConformancePacksResponse'
            Prelude.<$> ( x
                            Data..?> "ConformancePackDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConformancePacks where
  hashWithSalt _salt DescribeConformancePacks' {..} =
    _salt
      `Prelude.hashWithSalt` conformancePackNames
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeConformancePacks where
  rnf DescribeConformancePacks' {..} =
    Prelude.rnf conformancePackNames
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeConformancePacks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeConformancePacks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeConformancePacks where
  toJSON DescribeConformancePacks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConformancePackNames" Data..=)
              Prelude.<$> conformancePackNames,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeConformancePacks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeConformancePacks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConformancePacksResponse' smart constructor.
data DescribeConformancePacksResponse = DescribeConformancePacksResponse'
  { -- | Returns a list of @ConformancePackDetail@ objects.
    conformancePackDetails :: Prelude.Maybe [ConformancePackDetail],
    -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConformancePacksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conformancePackDetails', 'describeConformancePacksResponse_conformancePackDetails' - Returns a list of @ConformancePackDetail@ objects.
--
-- 'nextToken', 'describeConformancePacksResponse_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'httpStatus', 'describeConformancePacksResponse_httpStatus' - The response's http status code.
newDescribeConformancePacksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConformancePacksResponse
newDescribeConformancePacksResponse pHttpStatus_ =
  DescribeConformancePacksResponse'
    { conformancePackDetails =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of @ConformancePackDetail@ objects.
describeConformancePacksResponse_conformancePackDetails :: Lens.Lens' DescribeConformancePacksResponse (Prelude.Maybe [ConformancePackDetail])
describeConformancePacksResponse_conformancePackDetails = Lens.lens (\DescribeConformancePacksResponse' {conformancePackDetails} -> conformancePackDetails) (\s@DescribeConformancePacksResponse' {} a -> s {conformancePackDetails = a} :: DescribeConformancePacksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeConformancePacksResponse_nextToken :: Lens.Lens' DescribeConformancePacksResponse (Prelude.Maybe Prelude.Text)
describeConformancePacksResponse_nextToken = Lens.lens (\DescribeConformancePacksResponse' {nextToken} -> nextToken) (\s@DescribeConformancePacksResponse' {} a -> s {nextToken = a} :: DescribeConformancePacksResponse)

-- | The response's http status code.
describeConformancePacksResponse_httpStatus :: Lens.Lens' DescribeConformancePacksResponse Prelude.Int
describeConformancePacksResponse_httpStatus = Lens.lens (\DescribeConformancePacksResponse' {httpStatus} -> httpStatus) (\s@DescribeConformancePacksResponse' {} a -> s {httpStatus = a} :: DescribeConformancePacksResponse)

instance
  Prelude.NFData
    DescribeConformancePacksResponse
  where
  rnf DescribeConformancePacksResponse' {..} =
    Prelude.rnf conformancePackDetails
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
