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
-- Module      : Amazonka.ManagedBlockChain.ListNetworks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the networks in which the current Amazon Web
-- Services account participates.
--
-- Applies to Hyperledger Fabric and Ethereum.
module Amazonka.ManagedBlockChain.ListNetworks
  ( -- * Creating a Request
    ListNetworks (..),
    newListNetworks,

    -- * Request Lenses
    listNetworks_framework,
    listNetworks_maxResults,
    listNetworks_name,
    listNetworks_nextToken,
    listNetworks_status,

    -- * Destructuring the Response
    ListNetworksResponse (..),
    newListNetworksResponse,

    -- * Response Lenses
    listNetworksResponse_networks,
    listNetworksResponse_nextToken,
    listNetworksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListNetworks' smart constructor.
data ListNetworks = ListNetworks'
  { -- | An optional framework specifier. If provided, only networks of this
    -- framework type are listed.
    framework :: Prelude.Maybe Framework,
    -- | The maximum number of networks to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the network.
    name :: Prelude.Maybe Prelude.Text,
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional status specifier. If provided, only networks currently in
    -- this status are listed.
    --
    -- Applies only to Hyperledger Fabric.
    status :: Prelude.Maybe NetworkStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNetworks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'framework', 'listNetworks_framework' - An optional framework specifier. If provided, only networks of this
-- framework type are listed.
--
-- 'maxResults', 'listNetworks_maxResults' - The maximum number of networks to list.
--
-- 'name', 'listNetworks_name' - The name of the network.
--
-- 'nextToken', 'listNetworks_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'status', 'listNetworks_status' - An optional status specifier. If provided, only networks currently in
-- this status are listed.
--
-- Applies only to Hyperledger Fabric.
newListNetworks ::
  ListNetworks
newListNetworks =
  ListNetworks'
    { framework = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | An optional framework specifier. If provided, only networks of this
-- framework type are listed.
listNetworks_framework :: Lens.Lens' ListNetworks (Prelude.Maybe Framework)
listNetworks_framework = Lens.lens (\ListNetworks' {framework} -> framework) (\s@ListNetworks' {} a -> s {framework = a} :: ListNetworks)

-- | The maximum number of networks to list.
listNetworks_maxResults :: Lens.Lens' ListNetworks (Prelude.Maybe Prelude.Natural)
listNetworks_maxResults = Lens.lens (\ListNetworks' {maxResults} -> maxResults) (\s@ListNetworks' {} a -> s {maxResults = a} :: ListNetworks)

-- | The name of the network.
listNetworks_name :: Lens.Lens' ListNetworks (Prelude.Maybe Prelude.Text)
listNetworks_name = Lens.lens (\ListNetworks' {name} -> name) (\s@ListNetworks' {} a -> s {name = a} :: ListNetworks)

-- | The pagination token that indicates the next set of results to retrieve.
listNetworks_nextToken :: Lens.Lens' ListNetworks (Prelude.Maybe Prelude.Text)
listNetworks_nextToken = Lens.lens (\ListNetworks' {nextToken} -> nextToken) (\s@ListNetworks' {} a -> s {nextToken = a} :: ListNetworks)

-- | An optional status specifier. If provided, only networks currently in
-- this status are listed.
--
-- Applies only to Hyperledger Fabric.
listNetworks_status :: Lens.Lens' ListNetworks (Prelude.Maybe NetworkStatus)
listNetworks_status = Lens.lens (\ListNetworks' {status} -> status) (\s@ListNetworks' {} a -> s {status = a} :: ListNetworks)

instance Core.AWSRequest ListNetworks where
  type AWSResponse ListNetworks = ListNetworksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNetworksResponse'
            Prelude.<$> (x Data..?> "Networks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNetworks where
  hashWithSalt _salt ListNetworks' {..} =
    _salt
      `Prelude.hashWithSalt` framework
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListNetworks where
  rnf ListNetworks' {..} =
    Prelude.rnf framework `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf status

instance Data.ToHeaders ListNetworks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListNetworks where
  toPath = Prelude.const "/networks"

instance Data.ToQuery ListNetworks where
  toQuery ListNetworks' {..} =
    Prelude.mconcat
      [ "framework" Data.=: framework,
        "maxResults" Data.=: maxResults,
        "name" Data.=: name,
        "nextToken" Data.=: nextToken,
        "status" Data.=: status
      ]

-- | /See:/ 'newListNetworksResponse' smart constructor.
data ListNetworksResponse = ListNetworksResponse'
  { -- | An array of @NetworkSummary@ objects that contain configuration
    -- properties for each network.
    networks :: Prelude.Maybe [NetworkSummary],
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNetworksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networks', 'listNetworksResponse_networks' - An array of @NetworkSummary@ objects that contain configuration
-- properties for each network.
--
-- 'nextToken', 'listNetworksResponse_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'httpStatus', 'listNetworksResponse_httpStatus' - The response's http status code.
newListNetworksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNetworksResponse
newListNetworksResponse pHttpStatus_ =
  ListNetworksResponse'
    { networks = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @NetworkSummary@ objects that contain configuration
-- properties for each network.
listNetworksResponse_networks :: Lens.Lens' ListNetworksResponse (Prelude.Maybe [NetworkSummary])
listNetworksResponse_networks = Lens.lens (\ListNetworksResponse' {networks} -> networks) (\s@ListNetworksResponse' {} a -> s {networks = a} :: ListNetworksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that indicates the next set of results to retrieve.
listNetworksResponse_nextToken :: Lens.Lens' ListNetworksResponse (Prelude.Maybe Prelude.Text)
listNetworksResponse_nextToken = Lens.lens (\ListNetworksResponse' {nextToken} -> nextToken) (\s@ListNetworksResponse' {} a -> s {nextToken = a} :: ListNetworksResponse)

-- | The response's http status code.
listNetworksResponse_httpStatus :: Lens.Lens' ListNetworksResponse Prelude.Int
listNetworksResponse_httpStatus = Lens.lens (\ListNetworksResponse' {httpStatus} -> httpStatus) (\s@ListNetworksResponse' {} a -> s {httpStatus = a} :: ListNetworksResponse)

instance Prelude.NFData ListNetworksResponse where
  rnf ListNetworksResponse' {..} =
    Prelude.rnf networks `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
