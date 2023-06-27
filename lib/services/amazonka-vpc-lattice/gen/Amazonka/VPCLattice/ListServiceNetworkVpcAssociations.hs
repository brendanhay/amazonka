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
-- Module      : Amazonka.VPCLattice.ListServiceNetworkVpcAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the service network and VPC associations. You can filter the list
-- either by VPC or service network. You must provide either the service
-- network identifier or the VPC identifier.
--
-- This operation returns paginated results.
module Amazonka.VPCLattice.ListServiceNetworkVpcAssociations
  ( -- * Creating a Request
    ListServiceNetworkVpcAssociations (..),
    newListServiceNetworkVpcAssociations,

    -- * Request Lenses
    listServiceNetworkVpcAssociations_maxResults,
    listServiceNetworkVpcAssociations_nextToken,
    listServiceNetworkVpcAssociations_serviceNetworkIdentifier,
    listServiceNetworkVpcAssociations_vpcIdentifier,

    -- * Destructuring the Response
    ListServiceNetworkVpcAssociationsResponse (..),
    newListServiceNetworkVpcAssociationsResponse,

    -- * Response Lenses
    listServiceNetworkVpcAssociationsResponse_nextToken,
    listServiceNetworkVpcAssociationsResponse_httpStatus,
    listServiceNetworkVpcAssociationsResponse_items,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newListServiceNetworkVpcAssociations' smart constructor.
data ListServiceNetworkVpcAssociations = ListServiceNetworkVpcAssociations'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service network.
    serviceNetworkIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the VPC.
    vpcIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceNetworkVpcAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listServiceNetworkVpcAssociations_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listServiceNetworkVpcAssociations_nextToken' - A pagination token for the next page of results.
--
-- 'serviceNetworkIdentifier', 'listServiceNetworkVpcAssociations_serviceNetworkIdentifier' - The ID or Amazon Resource Name (ARN) of the service network.
--
-- 'vpcIdentifier', 'listServiceNetworkVpcAssociations_vpcIdentifier' - The ID or Amazon Resource Name (ARN) of the VPC.
newListServiceNetworkVpcAssociations ::
  ListServiceNetworkVpcAssociations
newListServiceNetworkVpcAssociations =
  ListServiceNetworkVpcAssociations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceNetworkIdentifier =
        Prelude.Nothing,
      vpcIdentifier = Prelude.Nothing
    }

-- | The maximum number of results to return.
listServiceNetworkVpcAssociations_maxResults :: Lens.Lens' ListServiceNetworkVpcAssociations (Prelude.Maybe Prelude.Natural)
listServiceNetworkVpcAssociations_maxResults = Lens.lens (\ListServiceNetworkVpcAssociations' {maxResults} -> maxResults) (\s@ListServiceNetworkVpcAssociations' {} a -> s {maxResults = a} :: ListServiceNetworkVpcAssociations)

-- | A pagination token for the next page of results.
listServiceNetworkVpcAssociations_nextToken :: Lens.Lens' ListServiceNetworkVpcAssociations (Prelude.Maybe Prelude.Text)
listServiceNetworkVpcAssociations_nextToken = Lens.lens (\ListServiceNetworkVpcAssociations' {nextToken} -> nextToken) (\s@ListServiceNetworkVpcAssociations' {} a -> s {nextToken = a} :: ListServiceNetworkVpcAssociations)

-- | The ID or Amazon Resource Name (ARN) of the service network.
listServiceNetworkVpcAssociations_serviceNetworkIdentifier :: Lens.Lens' ListServiceNetworkVpcAssociations (Prelude.Maybe Prelude.Text)
listServiceNetworkVpcAssociations_serviceNetworkIdentifier = Lens.lens (\ListServiceNetworkVpcAssociations' {serviceNetworkIdentifier} -> serviceNetworkIdentifier) (\s@ListServiceNetworkVpcAssociations' {} a -> s {serviceNetworkIdentifier = a} :: ListServiceNetworkVpcAssociations)

-- | The ID or Amazon Resource Name (ARN) of the VPC.
listServiceNetworkVpcAssociations_vpcIdentifier :: Lens.Lens' ListServiceNetworkVpcAssociations (Prelude.Maybe Prelude.Text)
listServiceNetworkVpcAssociations_vpcIdentifier = Lens.lens (\ListServiceNetworkVpcAssociations' {vpcIdentifier} -> vpcIdentifier) (\s@ListServiceNetworkVpcAssociations' {} a -> s {vpcIdentifier = a} :: ListServiceNetworkVpcAssociations)

instance
  Core.AWSPager
    ListServiceNetworkVpcAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServiceNetworkVpcAssociationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listServiceNetworkVpcAssociationsResponse_items
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listServiceNetworkVpcAssociations_nextToken
          Lens..~ rs
          Lens.^? listServiceNetworkVpcAssociationsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListServiceNetworkVpcAssociations
  where
  type
    AWSResponse ListServiceNetworkVpcAssociations =
      ListServiceNetworkVpcAssociationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServiceNetworkVpcAssociationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "items" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    ListServiceNetworkVpcAssociations
  where
  hashWithSalt
    _salt
    ListServiceNetworkVpcAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` serviceNetworkIdentifier
        `Prelude.hashWithSalt` vpcIdentifier

instance
  Prelude.NFData
    ListServiceNetworkVpcAssociations
  where
  rnf ListServiceNetworkVpcAssociations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceNetworkIdentifier
      `Prelude.seq` Prelude.rnf vpcIdentifier

instance
  Data.ToHeaders
    ListServiceNetworkVpcAssociations
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
    ListServiceNetworkVpcAssociations
  where
  toPath =
    Prelude.const "/servicenetworkvpcassociations"

instance
  Data.ToQuery
    ListServiceNetworkVpcAssociations
  where
  toQuery ListServiceNetworkVpcAssociations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "serviceNetworkIdentifier"
          Data.=: serviceNetworkIdentifier,
        "vpcIdentifier" Data.=: vpcIdentifier
      ]

-- | /See:/ 'newListServiceNetworkVpcAssociationsResponse' smart constructor.
data ListServiceNetworkVpcAssociationsResponse = ListServiceNetworkVpcAssociationsResponse'
  { -- | If there are additional results, a pagination token for the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the associations.
    items :: [ServiceNetworkVpcAssociationSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceNetworkVpcAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServiceNetworkVpcAssociationsResponse_nextToken' - If there are additional results, a pagination token for the next page of
-- results.
--
-- 'httpStatus', 'listServiceNetworkVpcAssociationsResponse_httpStatus' - The response's http status code.
--
-- 'items', 'listServiceNetworkVpcAssociationsResponse_items' - Information about the associations.
newListServiceNetworkVpcAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServiceNetworkVpcAssociationsResponse
newListServiceNetworkVpcAssociationsResponse
  pHttpStatus_ =
    ListServiceNetworkVpcAssociationsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        items = Prelude.mempty
      }

-- | If there are additional results, a pagination token for the next page of
-- results.
listServiceNetworkVpcAssociationsResponse_nextToken :: Lens.Lens' ListServiceNetworkVpcAssociationsResponse (Prelude.Maybe Prelude.Text)
listServiceNetworkVpcAssociationsResponse_nextToken = Lens.lens (\ListServiceNetworkVpcAssociationsResponse' {nextToken} -> nextToken) (\s@ListServiceNetworkVpcAssociationsResponse' {} a -> s {nextToken = a} :: ListServiceNetworkVpcAssociationsResponse)

-- | The response's http status code.
listServiceNetworkVpcAssociationsResponse_httpStatus :: Lens.Lens' ListServiceNetworkVpcAssociationsResponse Prelude.Int
listServiceNetworkVpcAssociationsResponse_httpStatus = Lens.lens (\ListServiceNetworkVpcAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListServiceNetworkVpcAssociationsResponse' {} a -> s {httpStatus = a} :: ListServiceNetworkVpcAssociationsResponse)

-- | Information about the associations.
listServiceNetworkVpcAssociationsResponse_items :: Lens.Lens' ListServiceNetworkVpcAssociationsResponse [ServiceNetworkVpcAssociationSummary]
listServiceNetworkVpcAssociationsResponse_items = Lens.lens (\ListServiceNetworkVpcAssociationsResponse' {items} -> items) (\s@ListServiceNetworkVpcAssociationsResponse' {} a -> s {items = a} :: ListServiceNetworkVpcAssociationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListServiceNetworkVpcAssociationsResponse
  where
  rnf ListServiceNetworkVpcAssociationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf items
