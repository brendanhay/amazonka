{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociationProposals
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more association proposals for connection between a virtual private gateway or transit gateway and a Direct Connect gateway.
module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociationProposals
  ( -- * Creating a request
    DescribeDirectConnectGatewayAssociationProposals (..),
    mkDescribeDirectConnectGatewayAssociationProposals,

    -- ** Request lenses
    ddcgapsAssociatedGatewayId,
    ddcgapsDirectConnectGatewayId,
    ddcgapsProposalId,
    ddcgapsNextToken,
    ddcgapsMaxResults,

    -- * Destructuring the response
    DescribeDirectConnectGatewayAssociationProposalsResponse (..),
    mkDescribeDirectConnectGatewayAssociationProposalsResponse,

    -- ** Response lenses
    ddcgapsrsDirectConnectGatewayAssociationProposals,
    ddcgapsrsNextToken,
    ddcgapsrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDirectConnectGatewayAssociationProposals' smart constructor.
data DescribeDirectConnectGatewayAssociationProposals = DescribeDirectConnectGatewayAssociationProposals'
  { -- | The ID of the associated gateway.
    associatedGatewayId :: Lude.Maybe Lude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Lude.Maybe Lude.Text,
    -- | The ID of the proposal.
    proposalId :: Lude.Maybe Lude.Text,
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    --
    -- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDirectConnectGatewayAssociationProposals' with the minimum fields required to make a request.
--
-- * 'associatedGatewayId' - The ID of the associated gateway.
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'proposalId' - The ID of the proposal.
-- * 'nextToken' - The token for the next page of results.
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
mkDescribeDirectConnectGatewayAssociationProposals ::
  DescribeDirectConnectGatewayAssociationProposals
mkDescribeDirectConnectGatewayAssociationProposals =
  DescribeDirectConnectGatewayAssociationProposals'
    { associatedGatewayId =
        Lude.Nothing,
      directConnectGatewayId = Lude.Nothing,
      proposalId = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the associated gateway.
--
-- /Note:/ Consider using 'associatedGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapsAssociatedGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposals (Lude.Maybe Lude.Text)
ddcgapsAssociatedGatewayId = Lens.lens (associatedGatewayId :: DescribeDirectConnectGatewayAssociationProposals -> Lude.Maybe Lude.Text) (\s a -> s {associatedGatewayId = a} :: DescribeDirectConnectGatewayAssociationProposals)
{-# DEPRECATED ddcgapsAssociatedGatewayId "Use generic-lens or generic-optics with 'associatedGatewayId' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapsDirectConnectGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposals (Lude.Maybe Lude.Text)
ddcgapsDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: DescribeDirectConnectGatewayAssociationProposals -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayId = a} :: DescribeDirectConnectGatewayAssociationProposals)
{-# DEPRECATED ddcgapsDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The ID of the proposal.
--
-- /Note:/ Consider using 'proposalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapsProposalId :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposals (Lude.Maybe Lude.Text)
ddcgapsProposalId = Lens.lens (proposalId :: DescribeDirectConnectGatewayAssociationProposals -> Lude.Maybe Lude.Text) (\s a -> s {proposalId = a} :: DescribeDirectConnectGatewayAssociationProposals)
{-# DEPRECATED ddcgapsProposalId "Use generic-lens or generic-optics with 'proposalId' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapsNextToken :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposals (Lude.Maybe Lude.Text)
ddcgapsNextToken = Lens.lens (nextToken :: DescribeDirectConnectGatewayAssociationProposals -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDirectConnectGatewayAssociationProposals)
{-# DEPRECATED ddcgapsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapsMaxResults :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposals (Lude.Maybe Lude.Int)
ddcgapsMaxResults = Lens.lens (maxResults :: DescribeDirectConnectGatewayAssociationProposals -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeDirectConnectGatewayAssociationProposals)
{-# DEPRECATED ddcgapsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance
  Lude.AWSRequest
    DescribeDirectConnectGatewayAssociationProposals
  where
  type
    Rs DescribeDirectConnectGatewayAssociationProposals =
      DescribeDirectConnectGatewayAssociationProposalsResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDirectConnectGatewayAssociationProposalsResponse'
            Lude.<$> ( x Lude..?> "directConnectGatewayAssociationProposals"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DescribeDirectConnectGatewayAssociationProposals
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.DescribeDirectConnectGatewayAssociationProposals" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance
  Lude.ToJSON
    DescribeDirectConnectGatewayAssociationProposals
  where
  toJSON DescribeDirectConnectGatewayAssociationProposals' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("associatedGatewayId" Lude..=) Lude.<$> associatedGatewayId,
            ("directConnectGatewayId" Lude..=) Lude.<$> directConnectGatewayId,
            ("proposalId" Lude..=) Lude.<$> proposalId,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance
  Lude.ToPath
    DescribeDirectConnectGatewayAssociationProposals
  where
  toPath = Lude.const "/"

instance
  Lude.ToQuery
    DescribeDirectConnectGatewayAssociationProposals
  where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDirectConnectGatewayAssociationProposalsResponse' smart constructor.
data DescribeDirectConnectGatewayAssociationProposalsResponse = DescribeDirectConnectGatewayAssociationProposalsResponse'
  { -- | Describes the Direct Connect gateway association proposals.
    directConnectGatewayAssociationProposals :: Lude.Maybe [DirectConnectGatewayAssociationProposal],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDirectConnectGatewayAssociationProposalsResponse' with the minimum fields required to make a request.
--
-- * 'directConnectGatewayAssociationProposals' - Describes the Direct Connect gateway association proposals.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeDirectConnectGatewayAssociationProposalsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDirectConnectGatewayAssociationProposalsResponse
mkDescribeDirectConnectGatewayAssociationProposalsResponse
  pResponseStatus_ =
    DescribeDirectConnectGatewayAssociationProposalsResponse'
      { directConnectGatewayAssociationProposals =
          Lude.Nothing,
        nextToken = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Describes the Direct Connect gateway association proposals.
--
-- /Note:/ Consider using 'directConnectGatewayAssociationProposals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapsrsDirectConnectGatewayAssociationProposals :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposalsResponse (Lude.Maybe [DirectConnectGatewayAssociationProposal])
ddcgapsrsDirectConnectGatewayAssociationProposals = Lens.lens (directConnectGatewayAssociationProposals :: DescribeDirectConnectGatewayAssociationProposalsResponse -> Lude.Maybe [DirectConnectGatewayAssociationProposal]) (\s a -> s {directConnectGatewayAssociationProposals = a} :: DescribeDirectConnectGatewayAssociationProposalsResponse)
{-# DEPRECATED ddcgapsrsDirectConnectGatewayAssociationProposals "Use generic-lens or generic-optics with 'directConnectGatewayAssociationProposals' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapsrsNextToken :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposalsResponse (Lude.Maybe Lude.Text)
ddcgapsrsNextToken = Lens.lens (nextToken :: DescribeDirectConnectGatewayAssociationProposalsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDirectConnectGatewayAssociationProposalsResponse)
{-# DEPRECATED ddcgapsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgapsrsResponseStatus :: Lens.Lens' DescribeDirectConnectGatewayAssociationProposalsResponse Lude.Int
ddcgapsrsResponseStatus = Lens.lens (responseStatus :: DescribeDirectConnectGatewayAssociationProposalsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDirectConnectGatewayAssociationProposalsResponse)
{-# DEPRECATED ddcgapsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
