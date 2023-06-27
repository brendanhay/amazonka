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
-- Module      : Amazonka.TNB.ListSolFunctionPackages
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about function packages.
--
-- A function package is a .zip file in CSAR (Cloud Service Archive) format
-- that contains a network function (an ETSI standard telecommunication
-- application) and function package descriptor that uses the TOSCA
-- standard to describe how the network functions should run on your
-- network.
--
-- This operation returns paginated results.
module Amazonka.TNB.ListSolFunctionPackages
  ( -- * Creating a Request
    ListSolFunctionPackages (..),
    newListSolFunctionPackages,

    -- * Request Lenses
    listSolFunctionPackages_maxResults,
    listSolFunctionPackages_nextToken,

    -- * Destructuring the Response
    ListSolFunctionPackagesResponse (..),
    newListSolFunctionPackagesResponse,

    -- * Response Lenses
    listSolFunctionPackagesResponse_nextToken,
    listSolFunctionPackagesResponse_httpStatus,
    listSolFunctionPackagesResponse_functionPackages,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newListSolFunctionPackages' smart constructor.
data ListSolFunctionPackages = ListSolFunctionPackages'
  { -- | The maximum number of results to include in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolFunctionPackages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSolFunctionPackages_maxResults' - The maximum number of results to include in the response.
--
-- 'nextToken', 'listSolFunctionPackages_nextToken' - The token for the next page of results.
newListSolFunctionPackages ::
  ListSolFunctionPackages
newListSolFunctionPackages =
  ListSolFunctionPackages'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to include in the response.
listSolFunctionPackages_maxResults :: Lens.Lens' ListSolFunctionPackages (Prelude.Maybe Prelude.Natural)
listSolFunctionPackages_maxResults = Lens.lens (\ListSolFunctionPackages' {maxResults} -> maxResults) (\s@ListSolFunctionPackages' {} a -> s {maxResults = a} :: ListSolFunctionPackages)

-- | The token for the next page of results.
listSolFunctionPackages_nextToken :: Lens.Lens' ListSolFunctionPackages (Prelude.Maybe Prelude.Text)
listSolFunctionPackages_nextToken = Lens.lens (\ListSolFunctionPackages' {nextToken} -> nextToken) (\s@ListSolFunctionPackages' {} a -> s {nextToken = a} :: ListSolFunctionPackages)

instance Core.AWSPager ListSolFunctionPackages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSolFunctionPackagesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listSolFunctionPackagesResponse_functionPackages
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSolFunctionPackages_nextToken
          Lens..~ rs
          Lens.^? listSolFunctionPackagesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSolFunctionPackages where
  type
    AWSResponse ListSolFunctionPackages =
      ListSolFunctionPackagesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSolFunctionPackagesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "functionPackages"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListSolFunctionPackages where
  hashWithSalt _salt ListSolFunctionPackages' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSolFunctionPackages where
  rnf ListSolFunctionPackages' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSolFunctionPackages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSolFunctionPackages where
  toPath = Prelude.const "/sol/vnfpkgm/v1/vnf_packages"

instance Data.ToQuery ListSolFunctionPackages where
  toQuery ListSolFunctionPackages' {..} =
    Prelude.mconcat
      [ "max_results" Data.=: maxResults,
        "nextpage_opaque_marker" Data.=: nextToken
      ]

-- | /See:/ 'newListSolFunctionPackagesResponse' smart constructor.
data ListSolFunctionPackagesResponse = ListSolFunctionPackagesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Function packages. A function package is a .zip file in CSAR (Cloud
    -- Service Archive) format that contains a network function (an ETSI
    -- standard telecommunication application) and function package descriptor
    -- that uses the TOSCA standard to describe how the network functions
    -- should run on your network.
    functionPackages :: [ListSolFunctionPackageInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolFunctionPackagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSolFunctionPackagesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'listSolFunctionPackagesResponse_httpStatus' - The response's http status code.
--
-- 'functionPackages', 'listSolFunctionPackagesResponse_functionPackages' - Function packages. A function package is a .zip file in CSAR (Cloud
-- Service Archive) format that contains a network function (an ETSI
-- standard telecommunication application) and function package descriptor
-- that uses the TOSCA standard to describe how the network functions
-- should run on your network.
newListSolFunctionPackagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSolFunctionPackagesResponse
newListSolFunctionPackagesResponse pHttpStatus_ =
  ListSolFunctionPackagesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      functionPackages = Prelude.mempty
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listSolFunctionPackagesResponse_nextToken :: Lens.Lens' ListSolFunctionPackagesResponse (Prelude.Maybe Prelude.Text)
listSolFunctionPackagesResponse_nextToken = Lens.lens (\ListSolFunctionPackagesResponse' {nextToken} -> nextToken) (\s@ListSolFunctionPackagesResponse' {} a -> s {nextToken = a} :: ListSolFunctionPackagesResponse)

-- | The response's http status code.
listSolFunctionPackagesResponse_httpStatus :: Lens.Lens' ListSolFunctionPackagesResponse Prelude.Int
listSolFunctionPackagesResponse_httpStatus = Lens.lens (\ListSolFunctionPackagesResponse' {httpStatus} -> httpStatus) (\s@ListSolFunctionPackagesResponse' {} a -> s {httpStatus = a} :: ListSolFunctionPackagesResponse)

-- | Function packages. A function package is a .zip file in CSAR (Cloud
-- Service Archive) format that contains a network function (an ETSI
-- standard telecommunication application) and function package descriptor
-- that uses the TOSCA standard to describe how the network functions
-- should run on your network.
listSolFunctionPackagesResponse_functionPackages :: Lens.Lens' ListSolFunctionPackagesResponse [ListSolFunctionPackageInfo]
listSolFunctionPackagesResponse_functionPackages = Lens.lens (\ListSolFunctionPackagesResponse' {functionPackages} -> functionPackages) (\s@ListSolFunctionPackagesResponse' {} a -> s {functionPackages = a} :: ListSolFunctionPackagesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListSolFunctionPackagesResponse
  where
  rnf ListSolFunctionPackagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf functionPackages
