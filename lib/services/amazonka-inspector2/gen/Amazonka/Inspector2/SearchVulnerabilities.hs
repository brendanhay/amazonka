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
-- Module      : Amazonka.Inspector2.SearchVulnerabilities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists Amazon Inspector coverage details for a specific vulnerability.
--
-- This operation returns paginated results.
module Amazonka.Inspector2.SearchVulnerabilities
  ( -- * Creating a Request
    SearchVulnerabilities (..),
    newSearchVulnerabilities,

    -- * Request Lenses
    searchVulnerabilities_nextToken,
    searchVulnerabilities_filterCriteria,

    -- * Destructuring the Response
    SearchVulnerabilitiesResponse (..),
    newSearchVulnerabilitiesResponse,

    -- * Response Lenses
    searchVulnerabilitiesResponse_nextToken,
    searchVulnerabilitiesResponse_httpStatus,
    searchVulnerabilitiesResponse_vulnerabilities,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchVulnerabilities' smart constructor.
data SearchVulnerabilities = SearchVulnerabilities'
  { -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The criteria used to filter the results of a vulnerability search.
    filterCriteria :: SearchVulnerabilitiesFilterCriteria
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchVulnerabilities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchVulnerabilities_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
--
-- 'filterCriteria', 'searchVulnerabilities_filterCriteria' - The criteria used to filter the results of a vulnerability search.
newSearchVulnerabilities ::
  -- | 'filterCriteria'
  SearchVulnerabilitiesFilterCriteria ->
  SearchVulnerabilities
newSearchVulnerabilities pFilterCriteria_ =
  SearchVulnerabilities'
    { nextToken = Prelude.Nothing,
      filterCriteria = pFilterCriteria_
    }

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
searchVulnerabilities_nextToken :: Lens.Lens' SearchVulnerabilities (Prelude.Maybe Prelude.Text)
searchVulnerabilities_nextToken = Lens.lens (\SearchVulnerabilities' {nextToken} -> nextToken) (\s@SearchVulnerabilities' {} a -> s {nextToken = a} :: SearchVulnerabilities)

-- | The criteria used to filter the results of a vulnerability search.
searchVulnerabilities_filterCriteria :: Lens.Lens' SearchVulnerabilities SearchVulnerabilitiesFilterCriteria
searchVulnerabilities_filterCriteria = Lens.lens (\SearchVulnerabilities' {filterCriteria} -> filterCriteria) (\s@SearchVulnerabilities' {} a -> s {filterCriteria = a} :: SearchVulnerabilities)

instance Core.AWSPager SearchVulnerabilities where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchVulnerabilitiesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. searchVulnerabilitiesResponse_vulnerabilities
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchVulnerabilities_nextToken
          Lens..~ rs
          Lens.^? searchVulnerabilitiesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchVulnerabilities where
  type
    AWSResponse SearchVulnerabilities =
      SearchVulnerabilitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchVulnerabilitiesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "vulnerabilities"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable SearchVulnerabilities where
  hashWithSalt _salt SearchVulnerabilities' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filterCriteria

instance Prelude.NFData SearchVulnerabilities where
  rnf SearchVulnerabilities' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filterCriteria

instance Data.ToHeaders SearchVulnerabilities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchVulnerabilities where
  toJSON SearchVulnerabilities' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("filterCriteria" Data..= filterCriteria)
          ]
      )

instance Data.ToPath SearchVulnerabilities where
  toPath = Prelude.const "/vulnerabilities/search"

instance Data.ToQuery SearchVulnerabilities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchVulnerabilitiesResponse' smart constructor.
data SearchVulnerabilitiesResponse = SearchVulnerabilitiesResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Details about the listed vulnerability.
    vulnerabilities :: [Vulnerability]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchVulnerabilitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchVulnerabilitiesResponse_nextToken' - The pagination parameter to be used on the next list operation to
-- retrieve more items.
--
-- 'httpStatus', 'searchVulnerabilitiesResponse_httpStatus' - The response's http status code.
--
-- 'vulnerabilities', 'searchVulnerabilitiesResponse_vulnerabilities' - Details about the listed vulnerability.
newSearchVulnerabilitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchVulnerabilitiesResponse
newSearchVulnerabilitiesResponse pHttpStatus_ =
  SearchVulnerabilitiesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      vulnerabilities = Prelude.mempty
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
searchVulnerabilitiesResponse_nextToken :: Lens.Lens' SearchVulnerabilitiesResponse (Prelude.Maybe Prelude.Text)
searchVulnerabilitiesResponse_nextToken = Lens.lens (\SearchVulnerabilitiesResponse' {nextToken} -> nextToken) (\s@SearchVulnerabilitiesResponse' {} a -> s {nextToken = a} :: SearchVulnerabilitiesResponse)

-- | The response's http status code.
searchVulnerabilitiesResponse_httpStatus :: Lens.Lens' SearchVulnerabilitiesResponse Prelude.Int
searchVulnerabilitiesResponse_httpStatus = Lens.lens (\SearchVulnerabilitiesResponse' {httpStatus} -> httpStatus) (\s@SearchVulnerabilitiesResponse' {} a -> s {httpStatus = a} :: SearchVulnerabilitiesResponse)

-- | Details about the listed vulnerability.
searchVulnerabilitiesResponse_vulnerabilities :: Lens.Lens' SearchVulnerabilitiesResponse [Vulnerability]
searchVulnerabilitiesResponse_vulnerabilities = Lens.lens (\SearchVulnerabilitiesResponse' {vulnerabilities} -> vulnerabilities) (\s@SearchVulnerabilitiesResponse' {} a -> s {vulnerabilities = a} :: SearchVulnerabilitiesResponse) Prelude.. Lens.coerced

instance Prelude.NFData SearchVulnerabilitiesResponse where
  rnf SearchVulnerabilitiesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vulnerabilities
