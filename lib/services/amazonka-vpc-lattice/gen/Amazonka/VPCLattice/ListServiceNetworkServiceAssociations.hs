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
-- Module      : Amazonka.VPCLattice.ListServiceNetworkServiceAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the associations between the service network and the service. You
-- can filter the list either by service or service network. You must
-- provide either the service network identifier or the service identifier.
--
-- Every association in Amazon VPC Lattice is given a unique Amazon
-- Resource Name (ARN), such as when a service network is associated with a
-- VPC or when a service is associated with a service network. If the
-- association is for a resource that is shared with another account, the
-- association will include the local account ID as the prefix in the ARN
-- for each account the resource is shared with.
--
-- This operation returns paginated results.
module Amazonka.VPCLattice.ListServiceNetworkServiceAssociations
  ( -- * Creating a Request
    ListServiceNetworkServiceAssociations (..),
    newListServiceNetworkServiceAssociations,

    -- * Request Lenses
    listServiceNetworkServiceAssociations_maxResults,
    listServiceNetworkServiceAssociations_nextToken,
    listServiceNetworkServiceAssociations_serviceIdentifier,
    listServiceNetworkServiceAssociations_serviceNetworkIdentifier,

    -- * Destructuring the Response
    ListServiceNetworkServiceAssociationsResponse (..),
    newListServiceNetworkServiceAssociationsResponse,

    -- * Response Lenses
    listServiceNetworkServiceAssociationsResponse_nextToken,
    listServiceNetworkServiceAssociationsResponse_httpStatus,
    listServiceNetworkServiceAssociationsResponse_items,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newListServiceNetworkServiceAssociations' smart constructor.
data ListServiceNetworkServiceAssociations = ListServiceNetworkServiceAssociations'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service network.
    serviceNetworkIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceNetworkServiceAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listServiceNetworkServiceAssociations_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listServiceNetworkServiceAssociations_nextToken' - A pagination token for the next page of results.
--
-- 'serviceIdentifier', 'listServiceNetworkServiceAssociations_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
--
-- 'serviceNetworkIdentifier', 'listServiceNetworkServiceAssociations_serviceNetworkIdentifier' - The ID or Amazon Resource Name (ARN) of the service network.
newListServiceNetworkServiceAssociations ::
  ListServiceNetworkServiceAssociations
newListServiceNetworkServiceAssociations =
  ListServiceNetworkServiceAssociations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceIdentifier = Prelude.Nothing,
      serviceNetworkIdentifier =
        Prelude.Nothing
    }

-- | The maximum number of results to return.
listServiceNetworkServiceAssociations_maxResults :: Lens.Lens' ListServiceNetworkServiceAssociations (Prelude.Maybe Prelude.Natural)
listServiceNetworkServiceAssociations_maxResults = Lens.lens (\ListServiceNetworkServiceAssociations' {maxResults} -> maxResults) (\s@ListServiceNetworkServiceAssociations' {} a -> s {maxResults = a} :: ListServiceNetworkServiceAssociations)

-- | A pagination token for the next page of results.
listServiceNetworkServiceAssociations_nextToken :: Lens.Lens' ListServiceNetworkServiceAssociations (Prelude.Maybe Prelude.Text)
listServiceNetworkServiceAssociations_nextToken = Lens.lens (\ListServiceNetworkServiceAssociations' {nextToken} -> nextToken) (\s@ListServiceNetworkServiceAssociations' {} a -> s {nextToken = a} :: ListServiceNetworkServiceAssociations)

-- | The ID or Amazon Resource Name (ARN) of the service.
listServiceNetworkServiceAssociations_serviceIdentifier :: Lens.Lens' ListServiceNetworkServiceAssociations (Prelude.Maybe Prelude.Text)
listServiceNetworkServiceAssociations_serviceIdentifier = Lens.lens (\ListServiceNetworkServiceAssociations' {serviceIdentifier} -> serviceIdentifier) (\s@ListServiceNetworkServiceAssociations' {} a -> s {serviceIdentifier = a} :: ListServiceNetworkServiceAssociations)

-- | The ID or Amazon Resource Name (ARN) of the service network.
listServiceNetworkServiceAssociations_serviceNetworkIdentifier :: Lens.Lens' ListServiceNetworkServiceAssociations (Prelude.Maybe Prelude.Text)
listServiceNetworkServiceAssociations_serviceNetworkIdentifier = Lens.lens (\ListServiceNetworkServiceAssociations' {serviceNetworkIdentifier} -> serviceNetworkIdentifier) (\s@ListServiceNetworkServiceAssociations' {} a -> s {serviceNetworkIdentifier = a} :: ListServiceNetworkServiceAssociations)

instance
  Core.AWSPager
    ListServiceNetworkServiceAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServiceNetworkServiceAssociationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listServiceNetworkServiceAssociationsResponse_items
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listServiceNetworkServiceAssociations_nextToken
          Lens..~ rs
          Lens.^? listServiceNetworkServiceAssociationsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListServiceNetworkServiceAssociations
  where
  type
    AWSResponse
      ListServiceNetworkServiceAssociations =
      ListServiceNetworkServiceAssociationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServiceNetworkServiceAssociationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "items" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    ListServiceNetworkServiceAssociations
  where
  hashWithSalt
    _salt
    ListServiceNetworkServiceAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` serviceIdentifier
        `Prelude.hashWithSalt` serviceNetworkIdentifier

instance
  Prelude.NFData
    ListServiceNetworkServiceAssociations
  where
  rnf ListServiceNetworkServiceAssociations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceIdentifier
      `Prelude.seq` Prelude.rnf serviceNetworkIdentifier

instance
  Data.ToHeaders
    ListServiceNetworkServiceAssociations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    ListServiceNetworkServiceAssociations
  where
  toPath =
    Prelude.const "/servicenetworkserviceassociations"

instance
  Data.ToQuery
    ListServiceNetworkServiceAssociations
  where
  toQuery ListServiceNetworkServiceAssociations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "serviceIdentifier" Data.=: serviceIdentifier,
        "serviceNetworkIdentifier"
          Data.=: serviceNetworkIdentifier
      ]

-- | /See:/ 'newListServiceNetworkServiceAssociationsResponse' smart constructor.
data ListServiceNetworkServiceAssociationsResponse = ListServiceNetworkServiceAssociationsResponse'
  { -- | If there are additional results, a pagination token for the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the associations.
    items :: [ServiceNetworkServiceAssociationSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceNetworkServiceAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServiceNetworkServiceAssociationsResponse_nextToken' - If there are additional results, a pagination token for the next page of
-- results.
--
-- 'httpStatus', 'listServiceNetworkServiceAssociationsResponse_httpStatus' - The response's http status code.
--
-- 'items', 'listServiceNetworkServiceAssociationsResponse_items' - Information about the associations.
newListServiceNetworkServiceAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServiceNetworkServiceAssociationsResponse
newListServiceNetworkServiceAssociationsResponse
  pHttpStatus_ =
    ListServiceNetworkServiceAssociationsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        items = Prelude.mempty
      }

-- | If there are additional results, a pagination token for the next page of
-- results.
listServiceNetworkServiceAssociationsResponse_nextToken :: Lens.Lens' ListServiceNetworkServiceAssociationsResponse (Prelude.Maybe Prelude.Text)
listServiceNetworkServiceAssociationsResponse_nextToken = Lens.lens (\ListServiceNetworkServiceAssociationsResponse' {nextToken} -> nextToken) (\s@ListServiceNetworkServiceAssociationsResponse' {} a -> s {nextToken = a} :: ListServiceNetworkServiceAssociationsResponse)

-- | The response's http status code.
listServiceNetworkServiceAssociationsResponse_httpStatus :: Lens.Lens' ListServiceNetworkServiceAssociationsResponse Prelude.Int
listServiceNetworkServiceAssociationsResponse_httpStatus = Lens.lens (\ListServiceNetworkServiceAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListServiceNetworkServiceAssociationsResponse' {} a -> s {httpStatus = a} :: ListServiceNetworkServiceAssociationsResponse)

-- | Information about the associations.
listServiceNetworkServiceAssociationsResponse_items :: Lens.Lens' ListServiceNetworkServiceAssociationsResponse [ServiceNetworkServiceAssociationSummary]
listServiceNetworkServiceAssociationsResponse_items = Lens.lens (\ListServiceNetworkServiceAssociationsResponse' {items} -> items) (\s@ListServiceNetworkServiceAssociationsResponse' {} a -> s {items = a} :: ListServiceNetworkServiceAssociationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListServiceNetworkServiceAssociationsResponse
  where
  rnf
    ListServiceNetworkServiceAssociationsResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
        `Prelude.seq` Prelude.rnf items
