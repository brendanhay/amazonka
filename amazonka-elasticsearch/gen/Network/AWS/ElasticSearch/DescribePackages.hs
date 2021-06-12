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
-- Module      : Network.AWS.ElasticSearch.DescribePackages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all packages available to Amazon ES. Includes options for
-- filtering, limiting the number of results, and pagination.
module Network.AWS.ElasticSearch.DescribePackages
  ( -- * Creating a Request
    DescribePackages (..),
    newDescribePackages,

    -- * Request Lenses
    describePackages_nextToken,
    describePackages_maxResults,
    describePackages_filters,

    -- * Destructuring the Response
    DescribePackagesResponse (..),
    newDescribePackagesResponse,

    -- * Response Lenses
    describePackagesResponse_nextToken,
    describePackagesResponse_packageDetailsList,
    describePackagesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @ DescribePackage @ operation.
--
-- /See:/ 'newDescribePackages' smart constructor.
data DescribePackages = DescribePackages'
  { -- | Used for pagination. Only necessary if a previous API call includes a
    -- non-null NextToken value. If provided, returns results for the next
    -- page.
    nextToken :: Core.Maybe Core.Text,
    -- | Limits results to a maximum number of packages.
    maxResults :: Core.Maybe Core.Int,
    -- | Only returns packages that match the @DescribePackagesFilterList@
    -- values.
    filters :: Core.Maybe [DescribePackagesFilter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePackages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePackages_nextToken' - Used for pagination. Only necessary if a previous API call includes a
-- non-null NextToken value. If provided, returns results for the next
-- page.
--
-- 'maxResults', 'describePackages_maxResults' - Limits results to a maximum number of packages.
--
-- 'filters', 'describePackages_filters' - Only returns packages that match the @DescribePackagesFilterList@
-- values.
newDescribePackages ::
  DescribePackages
newDescribePackages =
  DescribePackages'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | Used for pagination. Only necessary if a previous API call includes a
-- non-null NextToken value. If provided, returns results for the next
-- page.
describePackages_nextToken :: Lens.Lens' DescribePackages (Core.Maybe Core.Text)
describePackages_nextToken = Lens.lens (\DescribePackages' {nextToken} -> nextToken) (\s@DescribePackages' {} a -> s {nextToken = a} :: DescribePackages)

-- | Limits results to a maximum number of packages.
describePackages_maxResults :: Lens.Lens' DescribePackages (Core.Maybe Core.Int)
describePackages_maxResults = Lens.lens (\DescribePackages' {maxResults} -> maxResults) (\s@DescribePackages' {} a -> s {maxResults = a} :: DescribePackages)

-- | Only returns packages that match the @DescribePackagesFilterList@
-- values.
describePackages_filters :: Lens.Lens' DescribePackages (Core.Maybe [DescribePackagesFilter])
describePackages_filters = Lens.lens (\DescribePackages' {filters} -> filters) (\s@DescribePackages' {} a -> s {filters = a} :: DescribePackages) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribePackages where
  type
    AWSResponse DescribePackages =
      DescribePackagesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePackagesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "PackageDetailsList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribePackages

instance Core.NFData DescribePackages

instance Core.ToHeaders DescribePackages where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON DescribePackages where
  toJSON DescribePackages' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath DescribePackages where
  toPath = Core.const "/2015-01-01/packages/describe"

instance Core.ToQuery DescribePackages where
  toQuery = Core.const Core.mempty

-- | Container for response returned by @ DescribePackages @ operation.
--
-- /See:/ 'newDescribePackagesResponse' smart constructor.
data DescribePackagesResponse = DescribePackagesResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | List of @PackageDetails@ objects.
    packageDetailsList :: Core.Maybe [PackageDetails],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePackagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePackagesResponse_nextToken' - Undocumented member.
--
-- 'packageDetailsList', 'describePackagesResponse_packageDetailsList' - List of @PackageDetails@ objects.
--
-- 'httpStatus', 'describePackagesResponse_httpStatus' - The response's http status code.
newDescribePackagesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribePackagesResponse
newDescribePackagesResponse pHttpStatus_ =
  DescribePackagesResponse'
    { nextToken = Core.Nothing,
      packageDetailsList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describePackagesResponse_nextToken :: Lens.Lens' DescribePackagesResponse (Core.Maybe Core.Text)
describePackagesResponse_nextToken = Lens.lens (\DescribePackagesResponse' {nextToken} -> nextToken) (\s@DescribePackagesResponse' {} a -> s {nextToken = a} :: DescribePackagesResponse)

-- | List of @PackageDetails@ objects.
describePackagesResponse_packageDetailsList :: Lens.Lens' DescribePackagesResponse (Core.Maybe [PackageDetails])
describePackagesResponse_packageDetailsList = Lens.lens (\DescribePackagesResponse' {packageDetailsList} -> packageDetailsList) (\s@DescribePackagesResponse' {} a -> s {packageDetailsList = a} :: DescribePackagesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describePackagesResponse_httpStatus :: Lens.Lens' DescribePackagesResponse Core.Int
describePackagesResponse_httpStatus = Lens.lens (\DescribePackagesResponse' {httpStatus} -> httpStatus) (\s@DescribePackagesResponse' {} a -> s {httpStatus = a} :: DescribePackagesResponse)

instance Core.NFData DescribePackagesResponse
