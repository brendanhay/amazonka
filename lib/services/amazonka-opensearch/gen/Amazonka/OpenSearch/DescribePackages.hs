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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all packages available to Amazon OpenSearch Service domains.
-- Includes options for filtering, limiting the number of results, and
-- pagination.
module Amazonka.OpenSearch.DescribePackages
  ( -- * Creating a Request
    DescribePackages (..),
    newDescribePackages,

    -- * Request Lenses
    describePackages_filters,
    describePackages_nextToken,
    describePackages_maxResults,

    -- * Destructuring the Response
    DescribePackagesResponse (..),
    newDescribePackagesResponse,

    -- * Response Lenses
    describePackagesResponse_packageDetailsList,
    describePackagesResponse_nextToken,
    describePackagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to the @ DescribePackage @
-- operation.
--
-- /See:/ 'newDescribePackages' smart constructor.
data DescribePackages = DescribePackages'
  { -- | Only returns packages that match the @DescribePackagesFilterList@
    -- values.
    filters :: Prelude.Maybe [DescribePackagesFilter],
    -- | Used for pagination. Only necessary if a previous API call includes a
    -- non-null NextToken value. If provided, returns results for the next
    -- page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Limits results to a maximum number of packages.
    maxResults :: Prelude.Maybe Prelude.Int
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
-- 'nextToken', 'describePackages_nextToken' - Used for pagination. Only necessary if a previous API call includes a
-- non-null NextToken value. If provided, returns results for the next
-- page.
--
-- 'maxResults', 'describePackages_maxResults' - Limits results to a maximum number of packages.
newDescribePackages ::
  DescribePackages
newDescribePackages =
  DescribePackages'
    { filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Only returns packages that match the @DescribePackagesFilterList@
-- values.
describePackages_filters :: Lens.Lens' DescribePackages (Prelude.Maybe [DescribePackagesFilter])
describePackages_filters = Lens.lens (\DescribePackages' {filters} -> filters) (\s@DescribePackages' {} a -> s {filters = a} :: DescribePackages) Prelude.. Lens.mapping Lens.coerced

-- | Used for pagination. Only necessary if a previous API call includes a
-- non-null NextToken value. If provided, returns results for the next
-- page.
describePackages_nextToken :: Lens.Lens' DescribePackages (Prelude.Maybe Prelude.Text)
describePackages_nextToken = Lens.lens (\DescribePackages' {nextToken} -> nextToken) (\s@DescribePackages' {} a -> s {nextToken = a} :: DescribePackages)

-- | Limits results to a maximum number of packages.
describePackages_maxResults :: Lens.Lens' DescribePackages (Prelude.Maybe Prelude.Int)
describePackages_maxResults = Lens.lens (\DescribePackages' {maxResults} -> maxResults) (\s@DescribePackages' {} a -> s {maxResults = a} :: DescribePackages)

instance Core.AWSRequest DescribePackages where
  type
    AWSResponse DescribePackages =
      DescribePackagesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePackagesResponse'
            Prelude.<$> ( x Core..?> "PackageDetailsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePackages where
  hashWithSalt salt' DescribePackages' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters

instance Prelude.NFData DescribePackages where
  rnf DescribePackages' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders DescribePackages where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON DescribePackages where
  toJSON DescribePackages' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Filters" Core..=) Prelude.<$> filters,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribePackages where
  toPath =
    Prelude.const "/2021-01-01/packages/describe"

instance Core.ToQuery DescribePackages where
  toQuery = Prelude.const Prelude.mempty

-- | Container for the response returned by the @ DescribePackages @
-- operation.
--
-- /See:/ 'newDescribePackagesResponse' smart constructor.
data DescribePackagesResponse = DescribePackagesResponse'
  { -- | List of @PackageDetails@ objects.
    packageDetailsList :: Prelude.Maybe [PackageDetails],
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'packageDetailsList', 'describePackagesResponse_packageDetailsList' - List of @PackageDetails@ objects.
--
-- 'nextToken', 'describePackagesResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'describePackagesResponse_httpStatus' - The response's http status code.
newDescribePackagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePackagesResponse
newDescribePackagesResponse pHttpStatus_ =
  DescribePackagesResponse'
    { packageDetailsList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of @PackageDetails@ objects.
describePackagesResponse_packageDetailsList :: Lens.Lens' DescribePackagesResponse (Prelude.Maybe [PackageDetails])
describePackagesResponse_packageDetailsList = Lens.lens (\DescribePackagesResponse' {packageDetailsList} -> packageDetailsList) (\s@DescribePackagesResponse' {} a -> s {packageDetailsList = a} :: DescribePackagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describePackagesResponse_nextToken :: Lens.Lens' DescribePackagesResponse (Prelude.Maybe Prelude.Text)
describePackagesResponse_nextToken = Lens.lens (\DescribePackagesResponse' {nextToken} -> nextToken) (\s@DescribePackagesResponse' {} a -> s {nextToken = a} :: DescribePackagesResponse)

-- | The response's http status code.
describePackagesResponse_httpStatus :: Lens.Lens' DescribePackagesResponse Prelude.Int
describePackagesResponse_httpStatus = Lens.lens (\DescribePackagesResponse' {httpStatus} -> httpStatus) (\s@DescribePackagesResponse' {} a -> s {httpStatus = a} :: DescribePackagesResponse)

instance Prelude.NFData DescribePackagesResponse where
  rnf DescribePackagesResponse' {..} =
    Prelude.rnf packageDetailsList
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
