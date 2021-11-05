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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the networks in which the current AWS account
-- participates.
--
-- Applies to Hyperledger Fabric and Ethereum.
module Amazonka.ManagedBlockChain.ListNetworks
  ( -- * Creating a Request
    ListNetworks (..),
    newListNetworks,

    -- * Request Lenses
    listNetworks_status,
    listNetworks_framework,
    listNetworks_nextToken,
    listNetworks_name,
    listNetworks_maxResults,

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
import qualified Amazonka.Lens as Lens
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListNetworks' smart constructor.
data ListNetworks = ListNetworks'
  { -- | An optional status specifier. If provided, only networks currently in
    -- this status are listed.
    --
    -- Applies only to Hyperledger Fabric.
    status :: Prelude.Maybe NetworkStatus,
    -- | An optional framework specifier. If provided, only networks of this
    -- framework type are listed.
    framework :: Prelude.Maybe Framework,
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the network.
    name :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of networks to list.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'status', 'listNetworks_status' - An optional status specifier. If provided, only networks currently in
-- this status are listed.
--
-- Applies only to Hyperledger Fabric.
--
-- 'framework', 'listNetworks_framework' - An optional framework specifier. If provided, only networks of this
-- framework type are listed.
--
-- 'nextToken', 'listNetworks_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'name', 'listNetworks_name' - The name of the network.
--
-- 'maxResults', 'listNetworks_maxResults' - The maximum number of networks to list.
newListNetworks ::
  ListNetworks
newListNetworks =
  ListNetworks'
    { status = Prelude.Nothing,
      framework = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      name = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | An optional status specifier. If provided, only networks currently in
-- this status are listed.
--
-- Applies only to Hyperledger Fabric.
listNetworks_status :: Lens.Lens' ListNetworks (Prelude.Maybe NetworkStatus)
listNetworks_status = Lens.lens (\ListNetworks' {status} -> status) (\s@ListNetworks' {} a -> s {status = a} :: ListNetworks)

-- | An optional framework specifier. If provided, only networks of this
-- framework type are listed.
listNetworks_framework :: Lens.Lens' ListNetworks (Prelude.Maybe Framework)
listNetworks_framework = Lens.lens (\ListNetworks' {framework} -> framework) (\s@ListNetworks' {} a -> s {framework = a} :: ListNetworks)

-- | The pagination token that indicates the next set of results to retrieve.
listNetworks_nextToken :: Lens.Lens' ListNetworks (Prelude.Maybe Prelude.Text)
listNetworks_nextToken = Lens.lens (\ListNetworks' {nextToken} -> nextToken) (\s@ListNetworks' {} a -> s {nextToken = a} :: ListNetworks)

-- | The name of the network.
listNetworks_name :: Lens.Lens' ListNetworks (Prelude.Maybe Prelude.Text)
listNetworks_name = Lens.lens (\ListNetworks' {name} -> name) (\s@ListNetworks' {} a -> s {name = a} :: ListNetworks)

-- | The maximum number of networks to list.
listNetworks_maxResults :: Lens.Lens' ListNetworks (Prelude.Maybe Prelude.Natural)
listNetworks_maxResults = Lens.lens (\ListNetworks' {maxResults} -> maxResults) (\s@ListNetworks' {} a -> s {maxResults = a} :: ListNetworks)

instance Core.AWSRequest ListNetworks where
  type AWSResponse ListNetworks = ListNetworksResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNetworksResponse'
            Prelude.<$> (x Core..?> "Networks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNetworks

instance Prelude.NFData ListNetworks

instance Core.ToHeaders ListNetworks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListNetworks where
  toPath = Prelude.const "/networks"

instance Core.ToQuery ListNetworks where
  toQuery ListNetworks' {..} =
    Prelude.mconcat
      [ "status" Core.=: status,
        "framework" Core.=: framework,
        "nextToken" Core.=: nextToken,
        "name" Core.=: name,
        "maxResults" Core.=: maxResults
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

instance Prelude.NFData ListNetworksResponse
