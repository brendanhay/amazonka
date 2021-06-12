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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeConformancePacks' smart constructor.
data DescribeConformancePacks = DescribeConformancePacks'
  { -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Comma-separated list of conformance pack names for which you want
    -- details. If you do not specify any names, AWS Config returns details for
    -- all your conformance packs.
    conformancePackNames :: Core.Maybe [Core.Text],
    -- | The maximum number of conformance packs returned on each page.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- details. If you do not specify any names, AWS Config returns details for
-- all your conformance packs.
--
-- 'limit', 'describeConformancePacks_limit' - The maximum number of conformance packs returned on each page.
newDescribeConformancePacks ::
  DescribeConformancePacks
newDescribeConformancePacks =
  DescribeConformancePacks'
    { nextToken = Core.Nothing,
      conformancePackNames = Core.Nothing,
      limit = Core.Nothing
    }

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeConformancePacks_nextToken :: Lens.Lens' DescribeConformancePacks (Core.Maybe Core.Text)
describeConformancePacks_nextToken = Lens.lens (\DescribeConformancePacks' {nextToken} -> nextToken) (\s@DescribeConformancePacks' {} a -> s {nextToken = a} :: DescribeConformancePacks)

-- | Comma-separated list of conformance pack names for which you want
-- details. If you do not specify any names, AWS Config returns details for
-- all your conformance packs.
describeConformancePacks_conformancePackNames :: Lens.Lens' DescribeConformancePacks (Core.Maybe [Core.Text])
describeConformancePacks_conformancePackNames = Lens.lens (\DescribeConformancePacks' {conformancePackNames} -> conformancePackNames) (\s@DescribeConformancePacks' {} a -> s {conformancePackNames = a} :: DescribeConformancePacks) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of conformance packs returned on each page.
describeConformancePacks_limit :: Lens.Lens' DescribeConformancePacks (Core.Maybe Core.Natural)
describeConformancePacks_limit = Lens.lens (\DescribeConformancePacks' {limit} -> limit) (\s@DescribeConformancePacks' {} a -> s {limit = a} :: DescribeConformancePacks)

instance Core.AWSRequest DescribeConformancePacks where
  type
    AWSResponse DescribeConformancePacks =
      DescribeConformancePacksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConformancePacksResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ConformancePackDetails"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeConformancePacks

instance Core.NFData DescribeConformancePacks

instance Core.ToHeaders DescribeConformancePacks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeConformancePacks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeConformancePacks where
  toJSON DescribeConformancePacks' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("ConformancePackNames" Core..=)
              Core.<$> conformancePackNames,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath DescribeConformancePacks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeConformancePacks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeConformancePacksResponse' smart constructor.
data DescribeConformancePacksResponse = DescribeConformancePacksResponse'
  { -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns a list of @ConformancePackDetail@ objects.
    conformancePackDetails :: Core.Maybe [ConformancePackDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeConformancePacksResponse
newDescribeConformancePacksResponse pHttpStatus_ =
  DescribeConformancePacksResponse'
    { nextToken =
        Core.Nothing,
      conformancePackDetails = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeConformancePacksResponse_nextToken :: Lens.Lens' DescribeConformancePacksResponse (Core.Maybe Core.Text)
describeConformancePacksResponse_nextToken = Lens.lens (\DescribeConformancePacksResponse' {nextToken} -> nextToken) (\s@DescribeConformancePacksResponse' {} a -> s {nextToken = a} :: DescribeConformancePacksResponse)

-- | Returns a list of @ConformancePackDetail@ objects.
describeConformancePacksResponse_conformancePackDetails :: Lens.Lens' DescribeConformancePacksResponse (Core.Maybe [ConformancePackDetail])
describeConformancePacksResponse_conformancePackDetails = Lens.lens (\DescribeConformancePacksResponse' {conformancePackDetails} -> conformancePackDetails) (\s@DescribeConformancePacksResponse' {} a -> s {conformancePackDetails = a} :: DescribeConformancePacksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeConformancePacksResponse_httpStatus :: Lens.Lens' DescribeConformancePacksResponse Core.Int
describeConformancePacksResponse_httpStatus = Lens.lens (\DescribeConformancePacksResponse' {httpStatus} -> httpStatus) (\s@DescribeConformancePacksResponse' {} a -> s {httpStatus = a} :: DescribeConformancePacksResponse)

instance Core.NFData DescribeConformancePacksResponse
