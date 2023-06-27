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
-- Module      : Amazonka.TNB.ListSolNetworkPackages
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists network packages.
--
-- A network package is a .zip file in CSAR (Cloud Service Archive) format
-- defines the function packages you want to deploy and the Amazon Web
-- Services infrastructure you want to deploy them on.
--
-- This operation returns paginated results.
module Amazonka.TNB.ListSolNetworkPackages
  ( -- * Creating a Request
    ListSolNetworkPackages (..),
    newListSolNetworkPackages,

    -- * Request Lenses
    listSolNetworkPackages_maxResults,
    listSolNetworkPackages_nextToken,

    -- * Destructuring the Response
    ListSolNetworkPackagesResponse (..),
    newListSolNetworkPackagesResponse,

    -- * Response Lenses
    listSolNetworkPackagesResponse_nextToken,
    listSolNetworkPackagesResponse_httpStatus,
    listSolNetworkPackagesResponse_networkPackages,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newListSolNetworkPackages' smart constructor.
data ListSolNetworkPackages = ListSolNetworkPackages'
  { -- | The maximum number of results to include in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolNetworkPackages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSolNetworkPackages_maxResults' - The maximum number of results to include in the response.
--
-- 'nextToken', 'listSolNetworkPackages_nextToken' - The token for the next page of results.
newListSolNetworkPackages ::
  ListSolNetworkPackages
newListSolNetworkPackages =
  ListSolNetworkPackages'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to include in the response.
listSolNetworkPackages_maxResults :: Lens.Lens' ListSolNetworkPackages (Prelude.Maybe Prelude.Natural)
listSolNetworkPackages_maxResults = Lens.lens (\ListSolNetworkPackages' {maxResults} -> maxResults) (\s@ListSolNetworkPackages' {} a -> s {maxResults = a} :: ListSolNetworkPackages)

-- | The token for the next page of results.
listSolNetworkPackages_nextToken :: Lens.Lens' ListSolNetworkPackages (Prelude.Maybe Prelude.Text)
listSolNetworkPackages_nextToken = Lens.lens (\ListSolNetworkPackages' {nextToken} -> nextToken) (\s@ListSolNetworkPackages' {} a -> s {nextToken = a} :: ListSolNetworkPackages)

instance Core.AWSPager ListSolNetworkPackages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSolNetworkPackagesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listSolNetworkPackagesResponse_networkPackages
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSolNetworkPackages_nextToken
          Lens..~ rs
          Lens.^? listSolNetworkPackagesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSolNetworkPackages where
  type
    AWSResponse ListSolNetworkPackages =
      ListSolNetworkPackagesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSolNetworkPackagesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "networkPackages"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListSolNetworkPackages where
  hashWithSalt _salt ListSolNetworkPackages' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSolNetworkPackages where
  rnf ListSolNetworkPackages' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSolNetworkPackages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSolNetworkPackages where
  toPath = Prelude.const "/sol/nsd/v1/ns_descriptors"

instance Data.ToQuery ListSolNetworkPackages where
  toQuery ListSolNetworkPackages' {..} =
    Prelude.mconcat
      [ "max_results" Data.=: maxResults,
        "nextpage_opaque_marker" Data.=: nextToken
      ]

-- | /See:/ 'newListSolNetworkPackagesResponse' smart constructor.
data ListSolNetworkPackagesResponse = ListSolNetworkPackagesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Network packages. A network package is a .zip file in CSAR (Cloud
    -- Service Archive) format defines the function packages you want to deploy
    -- and the Amazon Web Services infrastructure you want to deploy them on.
    networkPackages :: [ListSolNetworkPackageInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolNetworkPackagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSolNetworkPackagesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'listSolNetworkPackagesResponse_httpStatus' - The response's http status code.
--
-- 'networkPackages', 'listSolNetworkPackagesResponse_networkPackages' - Network packages. A network package is a .zip file in CSAR (Cloud
-- Service Archive) format defines the function packages you want to deploy
-- and the Amazon Web Services infrastructure you want to deploy them on.
newListSolNetworkPackagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSolNetworkPackagesResponse
newListSolNetworkPackagesResponse pHttpStatus_ =
  ListSolNetworkPackagesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      networkPackages = Prelude.mempty
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listSolNetworkPackagesResponse_nextToken :: Lens.Lens' ListSolNetworkPackagesResponse (Prelude.Maybe Prelude.Text)
listSolNetworkPackagesResponse_nextToken = Lens.lens (\ListSolNetworkPackagesResponse' {nextToken} -> nextToken) (\s@ListSolNetworkPackagesResponse' {} a -> s {nextToken = a} :: ListSolNetworkPackagesResponse)

-- | The response's http status code.
listSolNetworkPackagesResponse_httpStatus :: Lens.Lens' ListSolNetworkPackagesResponse Prelude.Int
listSolNetworkPackagesResponse_httpStatus = Lens.lens (\ListSolNetworkPackagesResponse' {httpStatus} -> httpStatus) (\s@ListSolNetworkPackagesResponse' {} a -> s {httpStatus = a} :: ListSolNetworkPackagesResponse)

-- | Network packages. A network package is a .zip file in CSAR (Cloud
-- Service Archive) format defines the function packages you want to deploy
-- and the Amazon Web Services infrastructure you want to deploy them on.
listSolNetworkPackagesResponse_networkPackages :: Lens.Lens' ListSolNetworkPackagesResponse [ListSolNetworkPackageInfo]
listSolNetworkPackagesResponse_networkPackages = Lens.lens (\ListSolNetworkPackagesResponse' {networkPackages} -> networkPackages) (\s@ListSolNetworkPackagesResponse' {} a -> s {networkPackages = a} :: ListSolNetworkPackagesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListSolNetworkPackagesResponse
  where
  rnf ListSolNetworkPackagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf networkPackages
