{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @ DescribePackage @ operation.
--
-- /See:/ 'newDescribePackages' smart constructor.
data DescribePackages = DescribePackages'
  { -- | Used for pagination. Only necessary if a previous API call includes a
    -- non-null NextToken value. If provided, returns results for the next
    -- page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Limits results to a maximum number of packages.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Only returns packages that match the @DescribePackagesFilterList@
    -- values.
    filters :: Prelude.Maybe [DescribePackagesFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | Used for pagination. Only necessary if a previous API call includes a
-- non-null NextToken value. If provided, returns results for the next
-- page.
describePackages_nextToken :: Lens.Lens' DescribePackages (Prelude.Maybe Prelude.Text)
describePackages_nextToken = Lens.lens (\DescribePackages' {nextToken} -> nextToken) (\s@DescribePackages' {} a -> s {nextToken = a} :: DescribePackages)

-- | Limits results to a maximum number of packages.
describePackages_maxResults :: Lens.Lens' DescribePackages (Prelude.Maybe Prelude.Int)
describePackages_maxResults = Lens.lens (\DescribePackages' {maxResults} -> maxResults) (\s@DescribePackages' {} a -> s {maxResults = a} :: DescribePackages)

-- | Only returns packages that match the @DescribePackagesFilterList@
-- values.
describePackages_filters :: Lens.Lens' DescribePackages (Prelude.Maybe [DescribePackagesFilter])
describePackages_filters = Lens.lens (\DescribePackages' {filters} -> filters) (\s@DescribePackages' {} a -> s {filters = a} :: DescribePackages) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.AWSRequest DescribePackages where
  type Rs DescribePackages = DescribePackagesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePackagesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "PackageDetailsList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePackages

instance Prelude.NFData DescribePackages

instance Prelude.ToHeaders DescribePackages where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON DescribePackages where
  toJSON DescribePackages' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("Filters" Prelude..=) Prelude.<$> filters
          ]
      )

instance Prelude.ToPath DescribePackages where
  toPath =
    Prelude.const "/2015-01-01/packages/describe"

instance Prelude.ToQuery DescribePackages where
  toQuery = Prelude.const Prelude.mempty

-- | Container for response returned by @ DescribePackages @ operation.
--
-- /See:/ 'newDescribePackagesResponse' smart constructor.
data DescribePackagesResponse = DescribePackagesResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of @PackageDetails@ objects.
    packageDetailsList :: Prelude.Maybe [PackageDetails],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribePackagesResponse
newDescribePackagesResponse pHttpStatus_ =
  DescribePackagesResponse'
    { nextToken =
        Prelude.Nothing,
      packageDetailsList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describePackagesResponse_nextToken :: Lens.Lens' DescribePackagesResponse (Prelude.Maybe Prelude.Text)
describePackagesResponse_nextToken = Lens.lens (\DescribePackagesResponse' {nextToken} -> nextToken) (\s@DescribePackagesResponse' {} a -> s {nextToken = a} :: DescribePackagesResponse)

-- | List of @PackageDetails@ objects.
describePackagesResponse_packageDetailsList :: Lens.Lens' DescribePackagesResponse (Prelude.Maybe [PackageDetails])
describePackagesResponse_packageDetailsList = Lens.lens (\DescribePackagesResponse' {packageDetailsList} -> packageDetailsList) (\s@DescribePackagesResponse' {} a -> s {packageDetailsList = a} :: DescribePackagesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describePackagesResponse_httpStatus :: Lens.Lens' DescribePackagesResponse Prelude.Int
describePackagesResponse_httpStatus = Lens.lens (\DescribePackagesResponse' {httpStatus} -> httpStatus) (\s@DescribePackagesResponse' {} a -> s {httpStatus = a} :: DescribePackagesResponse)

instance Prelude.NFData DescribePackagesResponse
