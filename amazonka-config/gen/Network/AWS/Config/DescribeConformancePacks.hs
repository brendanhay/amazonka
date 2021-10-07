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
-- Module      : Network.AWS.Config.DescribeConformancePacks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of one or more conformance packs.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeConformancePacks
  ( -- * Creating a Request
    DescribeConformancePacks (..),
    newDescribeConformancePacks,

    -- * Request Lenses
    describeConformancePacks_nextToken,
    describeConformancePacks_conformancePackNames,
    describeConformancePacks_limit,

    -- * Destructuring the Response
    DescribeConformancePacksResponse (..),
    newDescribeConformancePacksResponse,

    -- * Response Lenses
    describeConformancePacksResponse_nextToken,
    describeConformancePacksResponse_conformancePackDetails,
    describeConformancePacksResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeConformancePacks' smart constructor.
data DescribeConformancePacks = DescribeConformancePacks'
  { -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Comma-separated list of conformance pack names for which you want
    -- details. If you do not specify any names, Config returns details for all
    -- your conformance packs.
    conformancePackNames :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of conformance packs returned on each page.
    limit :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'describeConformancePacks_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'conformancePackNames', 'describeConformancePacks_conformancePackNames' - Comma-separated list of conformance pack names for which you want
-- details. If you do not specify any names, Config returns details for all
-- your conformance packs.
--
-- 'limit', 'describeConformancePacks_limit' - The maximum number of conformance packs returned on each page.
newDescribeConformancePacks ::
  DescribeConformancePacks
newDescribeConformancePacks =
  DescribeConformancePacks'
    { nextToken =
        Prelude.Nothing,
      conformancePackNames = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeConformancePacks_nextToken :: Lens.Lens' DescribeConformancePacks (Prelude.Maybe Prelude.Text)
describeConformancePacks_nextToken = Lens.lens (\DescribeConformancePacks' {nextToken} -> nextToken) (\s@DescribeConformancePacks' {} a -> s {nextToken = a} :: DescribeConformancePacks)

-- | Comma-separated list of conformance pack names for which you want
-- details. If you do not specify any names, Config returns details for all
-- your conformance packs.
describeConformancePacks_conformancePackNames :: Lens.Lens' DescribeConformancePacks (Prelude.Maybe [Prelude.Text])
describeConformancePacks_conformancePackNames = Lens.lens (\DescribeConformancePacks' {conformancePackNames} -> conformancePackNames) (\s@DescribeConformancePacks' {} a -> s {conformancePackNames = a} :: DescribeConformancePacks) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of conformance packs returned on each page.
describeConformancePacks_limit :: Lens.Lens' DescribeConformancePacks (Prelude.Maybe Prelude.Natural)
describeConformancePacks_limit = Lens.lens (\DescribeConformancePacks' {limit} -> limit) (\s@DescribeConformancePacks' {} a -> s {limit = a} :: DescribeConformancePacks)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& describeConformancePacks_nextToken
          Lens..~ rs
          Lens.^? describeConformancePacksResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeConformancePacks where
  type
    AWSResponse DescribeConformancePacks =
      DescribeConformancePacksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConformancePacksResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ConformancePackDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConformancePacks

instance Prelude.NFData DescribeConformancePacks

instance Core.ToHeaders DescribeConformancePacks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeConformancePacks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeConformancePacks where
  toJSON DescribeConformancePacks' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("ConformancePackNames" Core..=)
              Prelude.<$> conformancePackNames,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath DescribeConformancePacks where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeConformancePacks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConformancePacksResponse' smart constructor.
data DescribeConformancePacksResponse = DescribeConformancePacksResponse'
  { -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of @ConformancePackDetail@ objects.
    conformancePackDetails :: Prelude.Maybe [ConformancePackDetail],
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
-- 'nextToken', 'describeConformancePacksResponse_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'conformancePackDetails', 'describeConformancePacksResponse_conformancePackDetails' - Returns a list of @ConformancePackDetail@ objects.
--
-- 'httpStatus', 'describeConformancePacksResponse_httpStatus' - The response's http status code.
newDescribeConformancePacksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConformancePacksResponse
newDescribeConformancePacksResponse pHttpStatus_ =
  DescribeConformancePacksResponse'
    { nextToken =
        Prelude.Nothing,
      conformancePackDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeConformancePacksResponse_nextToken :: Lens.Lens' DescribeConformancePacksResponse (Prelude.Maybe Prelude.Text)
describeConformancePacksResponse_nextToken = Lens.lens (\DescribeConformancePacksResponse' {nextToken} -> nextToken) (\s@DescribeConformancePacksResponse' {} a -> s {nextToken = a} :: DescribeConformancePacksResponse)

-- | Returns a list of @ConformancePackDetail@ objects.
describeConformancePacksResponse_conformancePackDetails :: Lens.Lens' DescribeConformancePacksResponse (Prelude.Maybe [ConformancePackDetail])
describeConformancePacksResponse_conformancePackDetails = Lens.lens (\DescribeConformancePacksResponse' {conformancePackDetails} -> conformancePackDetails) (\s@DescribeConformancePacksResponse' {} a -> s {conformancePackDetails = a} :: DescribeConformancePacksResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeConformancePacksResponse_httpStatus :: Lens.Lens' DescribeConformancePacksResponse Prelude.Int
describeConformancePacksResponse_httpStatus = Lens.lens (\DescribeConformancePacksResponse' {httpStatus} -> httpStatus) (\s@DescribeConformancePacksResponse' {} a -> s {httpStatus = a} :: DescribeConformancePacksResponse)

instance
  Prelude.NFData
    DescribeConformancePacksResponse
