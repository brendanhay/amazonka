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
-- Module      : Amazonka.EC2.DescribeTrunkInterfaceAssociations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API action is currently in __limited preview only__. If you are
-- interested in using this feature, contact your account manager.
--
-- Describes one or more network interface trunk associations.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeTrunkInterfaceAssociations
  ( -- * Creating a Request
    DescribeTrunkInterfaceAssociations (..),
    newDescribeTrunkInterfaceAssociations,

    -- * Request Lenses
    describeTrunkInterfaceAssociations_nextToken,
    describeTrunkInterfaceAssociations_filters,
    describeTrunkInterfaceAssociations_dryRun,
    describeTrunkInterfaceAssociations_maxResults,
    describeTrunkInterfaceAssociations_associationIds,

    -- * Destructuring the Response
    DescribeTrunkInterfaceAssociationsResponse (..),
    newDescribeTrunkInterfaceAssociationsResponse,

    -- * Response Lenses
    describeTrunkInterfaceAssociationsResponse_nextToken,
    describeTrunkInterfaceAssociationsResponse_interfaceAssociations,
    describeTrunkInterfaceAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTrunkInterfaceAssociations' smart constructor.
data DescribeTrunkInterfaceAssociations = DescribeTrunkInterfaceAssociations'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more filters.
    --
    -- -   @gre-key@ - The ID of a trunk interface association.
    --
    -- -   @interface-protocol@ - The interface protocol. Valid values are
    --     @VLAN@ and @GRE@.
    filters :: Prelude.Maybe [Filter],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The IDs of the associations.
    associationIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrunkInterfaceAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTrunkInterfaceAssociations_nextToken' - The token for the next page of results.
--
-- 'filters', 'describeTrunkInterfaceAssociations_filters' - One or more filters.
--
-- -   @gre-key@ - The ID of a trunk interface association.
--
-- -   @interface-protocol@ - The interface protocol. Valid values are
--     @VLAN@ and @GRE@.
--
-- 'dryRun', 'describeTrunkInterfaceAssociations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeTrunkInterfaceAssociations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'associationIds', 'describeTrunkInterfaceAssociations_associationIds' - The IDs of the associations.
newDescribeTrunkInterfaceAssociations ::
  DescribeTrunkInterfaceAssociations
newDescribeTrunkInterfaceAssociations =
  DescribeTrunkInterfaceAssociations'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      associationIds = Prelude.Nothing
    }

-- | The token for the next page of results.
describeTrunkInterfaceAssociations_nextToken :: Lens.Lens' DescribeTrunkInterfaceAssociations (Prelude.Maybe Prelude.Text)
describeTrunkInterfaceAssociations_nextToken = Lens.lens (\DescribeTrunkInterfaceAssociations' {nextToken} -> nextToken) (\s@DescribeTrunkInterfaceAssociations' {} a -> s {nextToken = a} :: DescribeTrunkInterfaceAssociations)

-- | One or more filters.
--
-- -   @gre-key@ - The ID of a trunk interface association.
--
-- -   @interface-protocol@ - The interface protocol. Valid values are
--     @VLAN@ and @GRE@.
describeTrunkInterfaceAssociations_filters :: Lens.Lens' DescribeTrunkInterfaceAssociations (Prelude.Maybe [Filter])
describeTrunkInterfaceAssociations_filters = Lens.lens (\DescribeTrunkInterfaceAssociations' {filters} -> filters) (\s@DescribeTrunkInterfaceAssociations' {} a -> s {filters = a} :: DescribeTrunkInterfaceAssociations) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTrunkInterfaceAssociations_dryRun :: Lens.Lens' DescribeTrunkInterfaceAssociations (Prelude.Maybe Prelude.Bool)
describeTrunkInterfaceAssociations_dryRun = Lens.lens (\DescribeTrunkInterfaceAssociations' {dryRun} -> dryRun) (\s@DescribeTrunkInterfaceAssociations' {} a -> s {dryRun = a} :: DescribeTrunkInterfaceAssociations)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTrunkInterfaceAssociations_maxResults :: Lens.Lens' DescribeTrunkInterfaceAssociations (Prelude.Maybe Prelude.Natural)
describeTrunkInterfaceAssociations_maxResults = Lens.lens (\DescribeTrunkInterfaceAssociations' {maxResults} -> maxResults) (\s@DescribeTrunkInterfaceAssociations' {} a -> s {maxResults = a} :: DescribeTrunkInterfaceAssociations)

-- | The IDs of the associations.
describeTrunkInterfaceAssociations_associationIds :: Lens.Lens' DescribeTrunkInterfaceAssociations (Prelude.Maybe [Prelude.Text])
describeTrunkInterfaceAssociations_associationIds = Lens.lens (\DescribeTrunkInterfaceAssociations' {associationIds} -> associationIds) (\s@DescribeTrunkInterfaceAssociations' {} a -> s {associationIds = a} :: DescribeTrunkInterfaceAssociations) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSPager
    DescribeTrunkInterfaceAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTrunkInterfaceAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTrunkInterfaceAssociationsResponse_interfaceAssociations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTrunkInterfaceAssociations_nextToken
          Lens..~ rs
          Lens.^? describeTrunkInterfaceAssociationsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeTrunkInterfaceAssociations
  where
  type
    AWSResponse DescribeTrunkInterfaceAssociations =
      DescribeTrunkInterfaceAssociationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTrunkInterfaceAssociationsResponse'
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "interfaceAssociationSet"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTrunkInterfaceAssociations
  where
  hashWithSalt
    _salt
    DescribeTrunkInterfaceAssociations' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` associationIds

instance
  Prelude.NFData
    DescribeTrunkInterfaceAssociations
  where
  rnf DescribeTrunkInterfaceAssociations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf associationIds

instance
  Core.ToHeaders
    DescribeTrunkInterfaceAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeTrunkInterfaceAssociations
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeTrunkInterfaceAssociations
  where
  toQuery DescribeTrunkInterfaceAssociations' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeTrunkInterfaceAssociations" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "AssociationId"
              Prelude.<$> associationIds
          )
      ]

-- | /See:/ 'newDescribeTrunkInterfaceAssociationsResponse' smart constructor.
data DescribeTrunkInterfaceAssociationsResponse = DescribeTrunkInterfaceAssociationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the trunk associations.
    interfaceAssociations :: Prelude.Maybe [TrunkInterfaceAssociation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrunkInterfaceAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTrunkInterfaceAssociationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'interfaceAssociations', 'describeTrunkInterfaceAssociationsResponse_interfaceAssociations' - Information about the trunk associations.
--
-- 'httpStatus', 'describeTrunkInterfaceAssociationsResponse_httpStatus' - The response's http status code.
newDescribeTrunkInterfaceAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTrunkInterfaceAssociationsResponse
newDescribeTrunkInterfaceAssociationsResponse
  pHttpStatus_ =
    DescribeTrunkInterfaceAssociationsResponse'
      { nextToken =
          Prelude.Nothing,
        interfaceAssociations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeTrunkInterfaceAssociationsResponse_nextToken :: Lens.Lens' DescribeTrunkInterfaceAssociationsResponse (Prelude.Maybe Prelude.Text)
describeTrunkInterfaceAssociationsResponse_nextToken = Lens.lens (\DescribeTrunkInterfaceAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeTrunkInterfaceAssociationsResponse' {} a -> s {nextToken = a} :: DescribeTrunkInterfaceAssociationsResponse)

-- | Information about the trunk associations.
describeTrunkInterfaceAssociationsResponse_interfaceAssociations :: Lens.Lens' DescribeTrunkInterfaceAssociationsResponse (Prelude.Maybe [TrunkInterfaceAssociation])
describeTrunkInterfaceAssociationsResponse_interfaceAssociations = Lens.lens (\DescribeTrunkInterfaceAssociationsResponse' {interfaceAssociations} -> interfaceAssociations) (\s@DescribeTrunkInterfaceAssociationsResponse' {} a -> s {interfaceAssociations = a} :: DescribeTrunkInterfaceAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTrunkInterfaceAssociationsResponse_httpStatus :: Lens.Lens' DescribeTrunkInterfaceAssociationsResponse Prelude.Int
describeTrunkInterfaceAssociationsResponse_httpStatus = Lens.lens (\DescribeTrunkInterfaceAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeTrunkInterfaceAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeTrunkInterfaceAssociationsResponse)

instance
  Prelude.NFData
    DescribeTrunkInterfaceAssociationsResponse
  where
  rnf DescribeTrunkInterfaceAssociationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf interfaceAssociations
      `Prelude.seq` Prelude.rnf httpStatus
