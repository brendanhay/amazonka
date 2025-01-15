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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    describeTrunkInterfaceAssociations_associationIds,
    describeTrunkInterfaceAssociations_dryRun,
    describeTrunkInterfaceAssociations_filters,
    describeTrunkInterfaceAssociations_maxResults,
    describeTrunkInterfaceAssociations_nextToken,

    -- * Destructuring the Response
    DescribeTrunkInterfaceAssociationsResponse (..),
    newDescribeTrunkInterfaceAssociationsResponse,

    -- * Response Lenses
    describeTrunkInterfaceAssociationsResponse_interfaceAssociations,
    describeTrunkInterfaceAssociationsResponse_nextToken,
    describeTrunkInterfaceAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTrunkInterfaceAssociations' smart constructor.
data DescribeTrunkInterfaceAssociations = DescribeTrunkInterfaceAssociations'
  { -- | The IDs of the associations.
    associationIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @gre-key@ - The ID of a trunk interface association.
    --
    -- -   @interface-protocol@ - The interface protocol. Valid values are
    --     @VLAN@ and @GRE@.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'associationIds', 'describeTrunkInterfaceAssociations_associationIds' - The IDs of the associations.
--
-- 'dryRun', 'describeTrunkInterfaceAssociations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeTrunkInterfaceAssociations_filters' - One or more filters.
--
-- -   @gre-key@ - The ID of a trunk interface association.
--
-- -   @interface-protocol@ - The interface protocol. Valid values are
--     @VLAN@ and @GRE@.
--
-- 'maxResults', 'describeTrunkInterfaceAssociations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeTrunkInterfaceAssociations_nextToken' - The token for the next page of results.
newDescribeTrunkInterfaceAssociations ::
  DescribeTrunkInterfaceAssociations
newDescribeTrunkInterfaceAssociations =
  DescribeTrunkInterfaceAssociations'
    { associationIds =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The IDs of the associations.
describeTrunkInterfaceAssociations_associationIds :: Lens.Lens' DescribeTrunkInterfaceAssociations (Prelude.Maybe [Prelude.Text])
describeTrunkInterfaceAssociations_associationIds = Lens.lens (\DescribeTrunkInterfaceAssociations' {associationIds} -> associationIds) (\s@DescribeTrunkInterfaceAssociations' {} a -> s {associationIds = a} :: DescribeTrunkInterfaceAssociations) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTrunkInterfaceAssociations_dryRun :: Lens.Lens' DescribeTrunkInterfaceAssociations (Prelude.Maybe Prelude.Bool)
describeTrunkInterfaceAssociations_dryRun = Lens.lens (\DescribeTrunkInterfaceAssociations' {dryRun} -> dryRun) (\s@DescribeTrunkInterfaceAssociations' {} a -> s {dryRun = a} :: DescribeTrunkInterfaceAssociations)

-- | One or more filters.
--
-- -   @gre-key@ - The ID of a trunk interface association.
--
-- -   @interface-protocol@ - The interface protocol. Valid values are
--     @VLAN@ and @GRE@.
describeTrunkInterfaceAssociations_filters :: Lens.Lens' DescribeTrunkInterfaceAssociations (Prelude.Maybe [Filter])
describeTrunkInterfaceAssociations_filters = Lens.lens (\DescribeTrunkInterfaceAssociations' {filters} -> filters) (\s@DescribeTrunkInterfaceAssociations' {} a -> s {filters = a} :: DescribeTrunkInterfaceAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTrunkInterfaceAssociations_maxResults :: Lens.Lens' DescribeTrunkInterfaceAssociations (Prelude.Maybe Prelude.Natural)
describeTrunkInterfaceAssociations_maxResults = Lens.lens (\DescribeTrunkInterfaceAssociations' {maxResults} -> maxResults) (\s@DescribeTrunkInterfaceAssociations' {} a -> s {maxResults = a} :: DescribeTrunkInterfaceAssociations)

-- | The token for the next page of results.
describeTrunkInterfaceAssociations_nextToken :: Lens.Lens' DescribeTrunkInterfaceAssociations (Prelude.Maybe Prelude.Text)
describeTrunkInterfaceAssociations_nextToken = Lens.lens (\DescribeTrunkInterfaceAssociations' {nextToken} -> nextToken) (\s@DescribeTrunkInterfaceAssociations' {} a -> s {nextToken = a} :: DescribeTrunkInterfaceAssociations)

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
            Prelude.<$> ( x
                            Data..@? "interfaceAssociationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTrunkInterfaceAssociations
  where
  hashWithSalt
    _salt
    DescribeTrunkInterfaceAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` associationIds
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeTrunkInterfaceAssociations
  where
  rnf DescribeTrunkInterfaceAssociations' {..} =
    Prelude.rnf associationIds `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf filters `Prelude.seq`
          Prelude.rnf maxResults `Prelude.seq`
            Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeTrunkInterfaceAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeTrunkInterfaceAssociations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeTrunkInterfaceAssociations
  where
  toQuery DescribeTrunkInterfaceAssociations' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeTrunkInterfaceAssociations" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "AssociationId"
              Prelude.<$> associationIds
          ),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeTrunkInterfaceAssociationsResponse' smart constructor.
data DescribeTrunkInterfaceAssociationsResponse = DescribeTrunkInterfaceAssociationsResponse'
  { -- | Information about the trunk associations.
    interfaceAssociations :: Prelude.Maybe [TrunkInterfaceAssociation],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'interfaceAssociations', 'describeTrunkInterfaceAssociationsResponse_interfaceAssociations' - Information about the trunk associations.
--
-- 'nextToken', 'describeTrunkInterfaceAssociationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeTrunkInterfaceAssociationsResponse_httpStatus' - The response's http status code.
newDescribeTrunkInterfaceAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTrunkInterfaceAssociationsResponse
newDescribeTrunkInterfaceAssociationsResponse
  pHttpStatus_ =
    DescribeTrunkInterfaceAssociationsResponse'
      { interfaceAssociations =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the trunk associations.
describeTrunkInterfaceAssociationsResponse_interfaceAssociations :: Lens.Lens' DescribeTrunkInterfaceAssociationsResponse (Prelude.Maybe [TrunkInterfaceAssociation])
describeTrunkInterfaceAssociationsResponse_interfaceAssociations = Lens.lens (\DescribeTrunkInterfaceAssociationsResponse' {interfaceAssociations} -> interfaceAssociations) (\s@DescribeTrunkInterfaceAssociationsResponse' {} a -> s {interfaceAssociations = a} :: DescribeTrunkInterfaceAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeTrunkInterfaceAssociationsResponse_nextToken :: Lens.Lens' DescribeTrunkInterfaceAssociationsResponse (Prelude.Maybe Prelude.Text)
describeTrunkInterfaceAssociationsResponse_nextToken = Lens.lens (\DescribeTrunkInterfaceAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeTrunkInterfaceAssociationsResponse' {} a -> s {nextToken = a} :: DescribeTrunkInterfaceAssociationsResponse)

-- | The response's http status code.
describeTrunkInterfaceAssociationsResponse_httpStatus :: Lens.Lens' DescribeTrunkInterfaceAssociationsResponse Prelude.Int
describeTrunkInterfaceAssociationsResponse_httpStatus = Lens.lens (\DescribeTrunkInterfaceAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeTrunkInterfaceAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeTrunkInterfaceAssociationsResponse)

instance
  Prelude.NFData
    DescribeTrunkInterfaceAssociationsResponse
  where
  rnf DescribeTrunkInterfaceAssociationsResponse' {..} =
    Prelude.rnf interfaceAssociations `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
