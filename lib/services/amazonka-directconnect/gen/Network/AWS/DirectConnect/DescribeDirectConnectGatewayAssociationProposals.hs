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
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociationProposals
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more association proposals for connection between a
-- virtual private gateway or transit gateway and a Direct Connect gateway.
module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociationProposals
  ( -- * Creating a Request
    DescribeDirectConnectGatewayAssociationProposals (..),
    newDescribeDirectConnectGatewayAssociationProposals,

    -- * Request Lenses
    describeDirectConnectGatewayAssociationProposals_nextToken,
    describeDirectConnectGatewayAssociationProposals_proposalId,
    describeDirectConnectGatewayAssociationProposals_maxResults,
    describeDirectConnectGatewayAssociationProposals_associatedGatewayId,
    describeDirectConnectGatewayAssociationProposals_directConnectGatewayId,

    -- * Destructuring the Response
    DescribeDirectConnectGatewayAssociationProposalsResponse (..),
    newDescribeDirectConnectGatewayAssociationProposalsResponse,

    -- * Response Lenses
    describeDirectConnectGatewayAssociationProposalsResponse_nextToken,
    describeDirectConnectGatewayAssociationProposalsResponse_directConnectGatewayAssociationProposals,
    describeDirectConnectGatewayAssociationProposalsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDirectConnectGatewayAssociationProposals' smart constructor.
data DescribeDirectConnectGatewayAssociationProposals = DescribeDirectConnectGatewayAssociationProposals'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the proposal.
    proposalId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    --
    -- If @MaxResults@ is given a value larger than 100, only 100 results are
    -- returned.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The ID of the associated gateway.
    associatedGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDirectConnectGatewayAssociationProposals' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDirectConnectGatewayAssociationProposals_nextToken' - The token for the next page of results.
--
-- 'proposalId', 'describeDirectConnectGatewayAssociationProposals_proposalId' - The ID of the proposal.
--
-- 'maxResults', 'describeDirectConnectGatewayAssociationProposals_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
--
-- 'associatedGatewayId', 'describeDirectConnectGatewayAssociationProposals_associatedGatewayId' - The ID of the associated gateway.
--
-- 'directConnectGatewayId', 'describeDirectConnectGatewayAssociationProposals_directConnectGatewayId' - The ID of the Direct Connect gateway.
newDescribeDirectConnectGatewayAssociationProposals ::
  DescribeDirectConnectGatewayAssociationProposals
newDescribeDirectConnectGatewayAssociationProposals =
  DescribeDirectConnectGatewayAssociationProposals'
    { nextToken =
        Prelude.Nothing,
      proposalId =
        Prelude.Nothing,
      maxResults =
        Prelude.Nothing,
      associatedGatewayId =
        Prelude.Nothing,
      directConnectGatewayId =
        Prelude.Nothing
    }

-- | The token for the next page of results.
describeDirectConnectGatewayAssociationProposals_nextToken :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposals (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAssociationProposals_nextToken = Lens.lens (\DescribeDirectConnectGatewayAssociationProposals' {nextToken} -> nextToken) (\s@DescribeDirectConnectGatewayAssociationProposals' {} a -> s {nextToken = a} :: DescribeDirectConnectGatewayAssociationProposals)

-- | The ID of the proposal.
describeDirectConnectGatewayAssociationProposals_proposalId :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposals (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAssociationProposals_proposalId = Lens.lens (\DescribeDirectConnectGatewayAssociationProposals' {proposalId} -> proposalId) (\s@DescribeDirectConnectGatewayAssociationProposals' {} a -> s {proposalId = a} :: DescribeDirectConnectGatewayAssociationProposals)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
describeDirectConnectGatewayAssociationProposals_maxResults :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposals (Prelude.Maybe Prelude.Int)
describeDirectConnectGatewayAssociationProposals_maxResults = Lens.lens (\DescribeDirectConnectGatewayAssociationProposals' {maxResults} -> maxResults) (\s@DescribeDirectConnectGatewayAssociationProposals' {} a -> s {maxResults = a} :: DescribeDirectConnectGatewayAssociationProposals)

-- | The ID of the associated gateway.
describeDirectConnectGatewayAssociationProposals_associatedGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposals (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAssociationProposals_associatedGatewayId = Lens.lens (\DescribeDirectConnectGatewayAssociationProposals' {associatedGatewayId} -> associatedGatewayId) (\s@DescribeDirectConnectGatewayAssociationProposals' {} a -> s {associatedGatewayId = a} :: DescribeDirectConnectGatewayAssociationProposals)

-- | The ID of the Direct Connect gateway.
describeDirectConnectGatewayAssociationProposals_directConnectGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposals (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAssociationProposals_directConnectGatewayId = Lens.lens (\DescribeDirectConnectGatewayAssociationProposals' {directConnectGatewayId} -> directConnectGatewayId) (\s@DescribeDirectConnectGatewayAssociationProposals' {} a -> s {directConnectGatewayId = a} :: DescribeDirectConnectGatewayAssociationProposals)

instance
  Core.AWSRequest
    DescribeDirectConnectGatewayAssociationProposals
  where
  type
    AWSResponse
      DescribeDirectConnectGatewayAssociationProposals =
      DescribeDirectConnectGatewayAssociationProposalsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDirectConnectGatewayAssociationProposalsResponse'
            Prelude.<$> (x Core..?> "nextToken")
              Prelude.<*> ( x
                              Core..?> "directConnectGatewayAssociationProposals"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDirectConnectGatewayAssociationProposals

instance
  Prelude.NFData
    DescribeDirectConnectGatewayAssociationProposals

instance
  Core.ToHeaders
    DescribeDirectConnectGatewayAssociationProposals
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DescribeDirectConnectGatewayAssociationProposals" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeDirectConnectGatewayAssociationProposals
  where
  toJSON
    DescribeDirectConnectGatewayAssociationProposals' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("nextToken" Core..=) Prelude.<$> nextToken,
              ("proposalId" Core..=) Prelude.<$> proposalId,
              ("maxResults" Core..=) Prelude.<$> maxResults,
              ("associatedGatewayId" Core..=)
                Prelude.<$> associatedGatewayId,
              ("directConnectGatewayId" Core..=)
                Prelude.<$> directConnectGatewayId
            ]
        )

instance
  Core.ToPath
    DescribeDirectConnectGatewayAssociationProposals
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeDirectConnectGatewayAssociationProposals
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDirectConnectGatewayAssociationProposalsResponse' smart constructor.
data DescribeDirectConnectGatewayAssociationProposalsResponse = DescribeDirectConnectGatewayAssociationProposalsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Describes the Direct Connect gateway association proposals.
    directConnectGatewayAssociationProposals :: Prelude.Maybe [DirectConnectGatewayAssociationProposal],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDirectConnectGatewayAssociationProposalsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDirectConnectGatewayAssociationProposalsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'directConnectGatewayAssociationProposals', 'describeDirectConnectGatewayAssociationProposalsResponse_directConnectGatewayAssociationProposals' - Describes the Direct Connect gateway association proposals.
--
-- 'httpStatus', 'describeDirectConnectGatewayAssociationProposalsResponse_httpStatus' - The response's http status code.
newDescribeDirectConnectGatewayAssociationProposalsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDirectConnectGatewayAssociationProposalsResponse
newDescribeDirectConnectGatewayAssociationProposalsResponse
  pHttpStatus_ =
    DescribeDirectConnectGatewayAssociationProposalsResponse'
      { nextToken =
          Prelude.Nothing,
        directConnectGatewayAssociationProposals =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeDirectConnectGatewayAssociationProposalsResponse_nextToken :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposalsResponse (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAssociationProposalsResponse_nextToken = Lens.lens (\DescribeDirectConnectGatewayAssociationProposalsResponse' {nextToken} -> nextToken) (\s@DescribeDirectConnectGatewayAssociationProposalsResponse' {} a -> s {nextToken = a} :: DescribeDirectConnectGatewayAssociationProposalsResponse)

-- | Describes the Direct Connect gateway association proposals.
describeDirectConnectGatewayAssociationProposalsResponse_directConnectGatewayAssociationProposals :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposalsResponse (Prelude.Maybe [DirectConnectGatewayAssociationProposal])
describeDirectConnectGatewayAssociationProposalsResponse_directConnectGatewayAssociationProposals = Lens.lens (\DescribeDirectConnectGatewayAssociationProposalsResponse' {directConnectGatewayAssociationProposals} -> directConnectGatewayAssociationProposals) (\s@DescribeDirectConnectGatewayAssociationProposalsResponse' {} a -> s {directConnectGatewayAssociationProposals = a} :: DescribeDirectConnectGatewayAssociationProposalsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeDirectConnectGatewayAssociationProposalsResponse_httpStatus :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposalsResponse Prelude.Int
describeDirectConnectGatewayAssociationProposalsResponse_httpStatus = Lens.lens (\DescribeDirectConnectGatewayAssociationProposalsResponse' {httpStatus} -> httpStatus) (\s@DescribeDirectConnectGatewayAssociationProposalsResponse' {} a -> s {httpStatus = a} :: DescribeDirectConnectGatewayAssociationProposalsResponse)

instance
  Prelude.NFData
    DescribeDirectConnectGatewayAssociationProposalsResponse
