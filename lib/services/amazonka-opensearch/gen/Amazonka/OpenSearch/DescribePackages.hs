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
-- Module      : Amazonka.OpenSearch.DescribePackages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all packages available to OpenSearch Service. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/custom-packages.html Custom packages for Amazon OpenSearch Service>.
module Amazonka.OpenSearch.DescribePackages
  ( -- * Creating a Request
    DescribePackages (..),
    newDescribePackages,

    -- * Request Lenses
    describePackages_filters,
    describePackages_maxResults,
    describePackages_nextToken,

    -- * Destructuring the Response
    DescribePackagesResponse (..),
    newDescribePackagesResponse,

    -- * Response Lenses
    describePackagesResponse_nextToken,
    describePackagesResponse_packageDetailsList,
    describePackagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to the @DescribePackage@ operation.
--
-- /See:/ 'newDescribePackages' smart constructor.
data DescribePackages = DescribePackages'
  { -- | Only returns packages that match the @DescribePackagesFilterList@
    -- values.
    filters :: Prelude.Maybe [DescribePackagesFilter],
    -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | If your initial @DescribePackageFilters@ operation returns a
    -- @nextToken@, you can include the returned @nextToken@ in subsequent
    -- @DescribePackageFilters@ operations, which returns results in the next
    -- page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePackages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describePackages_filters' - Only returns packages that match the @DescribePackagesFilterList@
-- values.
--
-- 'maxResults', 'describePackages_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
--
-- 'nextToken', 'describePackages_nextToken' - If your initial @DescribePackageFilters@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @DescribePackageFilters@ operations, which returns results in the next
-- page.
newDescribePackages ::
  DescribePackages
newDescribePackages =
  DescribePackages'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Only returns packages that match the @DescribePackagesFilterList@
-- values.
describePackages_filters :: Lens.Lens' DescribePackages (Prelude.Maybe [DescribePackagesFilter])
describePackages_filters = Lens.lens (\DescribePackages' {filters} -> filters) (\s@DescribePackages' {} a -> s {filters = a} :: DescribePackages) Prelude.. Lens.mapping Lens.coerced

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
describePackages_maxResults :: Lens.Lens' DescribePackages (Prelude.Maybe Prelude.Int)
describePackages_maxResults = Lens.lens (\DescribePackages' {maxResults} -> maxResults) (\s@DescribePackages' {} a -> s {maxResults = a} :: DescribePackages)

-- | If your initial @DescribePackageFilters@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @DescribePackageFilters@ operations, which returns results in the next
-- page.
describePackages_nextToken :: Lens.Lens' DescribePackages (Prelude.Maybe Prelude.Text)
describePackages_nextToken = Lens.lens (\DescribePackages' {nextToken} -> nextToken) (\s@DescribePackages' {} a -> s {nextToken = a} :: DescribePackages)

instance Core.AWSRequest DescribePackages where
  type
    AWSResponse DescribePackages =
      DescribePackagesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePackagesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "PackageDetailsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePackages where
  hashWithSalt _salt DescribePackages' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribePackages where
  rnf DescribePackages' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribePackages where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON DescribePackages where
  toJSON DescribePackages' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribePackages where
  toPath =
    Prelude.const "/2021-01-01/packages/describe"

instance Data.ToQuery DescribePackages where
  toQuery = Prelude.const Prelude.mempty

-- | Container for the response returned by the @DescribePackages@ operation.
--
-- /See:/ 'newDescribePackagesResponse' smart constructor.
data DescribePackagesResponse = DescribePackagesResponse'
  { -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Basic information about a package.
    packageDetailsList :: Prelude.Maybe [PackageDetails],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePackagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePackagesResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'packageDetailsList', 'describePackagesResponse_packageDetailsList' - Basic information about a package.
--
-- 'httpStatus', 'describePackagesResponse_httpStatus' - The response's http status code.
newDescribePackagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePackagesResponse
newDescribePackagesResponse pHttpStatus_ =
  DescribePackagesResponse'
    { nextToken =
        Prelude.Nothing,
      packageDetailsList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
describePackagesResponse_nextToken :: Lens.Lens' DescribePackagesResponse (Prelude.Maybe Prelude.Text)
describePackagesResponse_nextToken = Lens.lens (\DescribePackagesResponse' {nextToken} -> nextToken) (\s@DescribePackagesResponse' {} a -> s {nextToken = a} :: DescribePackagesResponse)

-- | Basic information about a package.
describePackagesResponse_packageDetailsList :: Lens.Lens' DescribePackagesResponse (Prelude.Maybe [PackageDetails])
describePackagesResponse_packageDetailsList = Lens.lens (\DescribePackagesResponse' {packageDetailsList} -> packageDetailsList) (\s@DescribePackagesResponse' {} a -> s {packageDetailsList = a} :: DescribePackagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePackagesResponse_httpStatus :: Lens.Lens' DescribePackagesResponse Prelude.Int
describePackagesResponse_httpStatus = Lens.lens (\DescribePackagesResponse' {httpStatus} -> httpStatus) (\s@DescribePackagesResponse' {} a -> s {httpStatus = a} :: DescribePackagesResponse)

instance Prelude.NFData DescribePackagesResponse where
  rnf DescribePackagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf packageDetailsList
      `Prelude.seq` Prelude.rnf httpStatus
