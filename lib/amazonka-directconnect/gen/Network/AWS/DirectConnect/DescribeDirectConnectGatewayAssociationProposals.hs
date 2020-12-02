{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( -- * Creating a Request
    describeDirectConnectGatewayAssociationProposals,
    DescribeDirectConnectGatewayAssociationProposals,

    -- * Request Lenses
    ddcgapsAssociatedGatewayId,
    ddcgapsDirectConnectGatewayId,
    ddcgapsProposalId,
    ddcgapsNextToken,
    ddcgapsMaxResults,

    -- * Destructuring the Response
    describeDirectConnectGatewayAssociationProposalsResponse,
    DescribeDirectConnectGatewayAssociationProposalsResponse,

    -- * Response Lenses
    ddcgapsrsDirectConnectGatewayAssociationProposals,
    ddcgapsrsNextToken,
    ddcgapsrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDirectConnectGatewayAssociationProposals' smart constructor.
data DescribeDirectConnectGatewayAssociationProposals = DescribeDirectConnectGatewayAssociationProposals'
  { _ddcgapsAssociatedGatewayId ::
      !( Maybe
           Text
       ),
    _ddcgapsDirectConnectGatewayId ::
      !( Maybe
           Text
       ),
    _ddcgapsProposalId ::
      !( Maybe
           Text
       ),
    _ddcgapsNextToken ::
      !( Maybe
           Text
       ),
    _ddcgapsMaxResults ::
      !( Maybe
           Int
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeDirectConnectGatewayAssociationProposals' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgapsAssociatedGatewayId' - The ID of the associated gateway.
--
-- * 'ddcgapsDirectConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- * 'ddcgapsProposalId' - The ID of the proposal.
--
-- * 'ddcgapsNextToken' - The token for the next page of results.
--
-- * 'ddcgapsMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value. If @MaxResults@ is given a value larger than 100, only 100 results are returned.
describeDirectConnectGatewayAssociationProposals ::
  DescribeDirectConnectGatewayAssociationProposals
describeDirectConnectGatewayAssociationProposals =
  DescribeDirectConnectGatewayAssociationProposals'
    { _ddcgapsAssociatedGatewayId =
        Nothing,
      _ddcgapsDirectConnectGatewayId = Nothing,
      _ddcgapsProposalId = Nothing,
      _ddcgapsNextToken = Nothing,
      _ddcgapsMaxResults = Nothing
    }

-- | The ID of the associated gateway.
ddcgapsAssociatedGatewayId :: Lens' DescribeDirectConnectGatewayAssociationProposals (Maybe Text)
ddcgapsAssociatedGatewayId = lens _ddcgapsAssociatedGatewayId (\s a -> s {_ddcgapsAssociatedGatewayId = a})

-- | The ID of the Direct Connect gateway.
ddcgapsDirectConnectGatewayId :: Lens' DescribeDirectConnectGatewayAssociationProposals (Maybe Text)
ddcgapsDirectConnectGatewayId = lens _ddcgapsDirectConnectGatewayId (\s a -> s {_ddcgapsDirectConnectGatewayId = a})

-- | The ID of the proposal.
ddcgapsProposalId :: Lens' DescribeDirectConnectGatewayAssociationProposals (Maybe Text)
ddcgapsProposalId = lens _ddcgapsProposalId (\s a -> s {_ddcgapsProposalId = a})

-- | The token for the next page of results.
ddcgapsNextToken :: Lens' DescribeDirectConnectGatewayAssociationProposals (Maybe Text)
ddcgapsNextToken = lens _ddcgapsNextToken (\s a -> s {_ddcgapsNextToken = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value. If @MaxResults@ is given a value larger than 100, only 100 results are returned.
ddcgapsMaxResults :: Lens' DescribeDirectConnectGatewayAssociationProposals (Maybe Int)
ddcgapsMaxResults = lens _ddcgapsMaxResults (\s a -> s {_ddcgapsMaxResults = a})

instance
  AWSRequest
    DescribeDirectConnectGatewayAssociationProposals
  where
  type
    Rs DescribeDirectConnectGatewayAssociationProposals =
      DescribeDirectConnectGatewayAssociationProposalsResponse
  request = postJSON directConnect
  response =
    receiveJSON
      ( \s h x ->
          DescribeDirectConnectGatewayAssociationProposalsResponse'
            <$> (x .?> "directConnectGatewayAssociationProposals" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeDirectConnectGatewayAssociationProposals

instance NFData DescribeDirectConnectGatewayAssociationProposals

instance ToHeaders DescribeDirectConnectGatewayAssociationProposals where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "OvertureService.DescribeDirectConnectGatewayAssociationProposals" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeDirectConnectGatewayAssociationProposals where
  toJSON DescribeDirectConnectGatewayAssociationProposals' {..} =
    object
      ( catMaybes
          [ ("associatedGatewayId" .=) <$> _ddcgapsAssociatedGatewayId,
            ("directConnectGatewayId" .=) <$> _ddcgapsDirectConnectGatewayId,
            ("proposalId" .=) <$> _ddcgapsProposalId,
            ("nextToken" .=) <$> _ddcgapsNextToken,
            ("maxResults" .=) <$> _ddcgapsMaxResults
          ]
      )

instance ToPath DescribeDirectConnectGatewayAssociationProposals where
  toPath = const "/"

instance ToQuery DescribeDirectConnectGatewayAssociationProposals where
  toQuery = const mempty

-- | /See:/ 'describeDirectConnectGatewayAssociationProposalsResponse' smart constructor.
data DescribeDirectConnectGatewayAssociationProposalsResponse = DescribeDirectConnectGatewayAssociationProposalsResponse'
  { _ddcgapsrsDirectConnectGatewayAssociationProposals ::
      !( Maybe
           [DirectConnectGatewayAssociationProposal]
       ),
    _ddcgapsrsNextToken ::
      !( Maybe
           Text
       ),
    _ddcgapsrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeDirectConnectGatewayAssociationProposalsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgapsrsDirectConnectGatewayAssociationProposals' - Describes the Direct Connect gateway association proposals.
--
-- * 'ddcgapsrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'ddcgapsrsResponseStatus' - -- | The response status code.
describeDirectConnectGatewayAssociationProposalsResponse ::
  -- | 'ddcgapsrsResponseStatus'
  Int ->
  DescribeDirectConnectGatewayAssociationProposalsResponse
describeDirectConnectGatewayAssociationProposalsResponse
  pResponseStatus_ =
    DescribeDirectConnectGatewayAssociationProposalsResponse'
      { _ddcgapsrsDirectConnectGatewayAssociationProposals =
          Nothing,
        _ddcgapsrsNextToken = Nothing,
        _ddcgapsrsResponseStatus =
          pResponseStatus_
      }

-- | Describes the Direct Connect gateway association proposals.
ddcgapsrsDirectConnectGatewayAssociationProposals :: Lens' DescribeDirectConnectGatewayAssociationProposalsResponse [DirectConnectGatewayAssociationProposal]
ddcgapsrsDirectConnectGatewayAssociationProposals = lens _ddcgapsrsDirectConnectGatewayAssociationProposals (\s a -> s {_ddcgapsrsDirectConnectGatewayAssociationProposals = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
ddcgapsrsNextToken :: Lens' DescribeDirectConnectGatewayAssociationProposalsResponse (Maybe Text)
ddcgapsrsNextToken = lens _ddcgapsrsNextToken (\s a -> s {_ddcgapsrsNextToken = a})

-- | -- | The response status code.
ddcgapsrsResponseStatus :: Lens' DescribeDirectConnectGatewayAssociationProposalsResponse Int
ddcgapsrsResponseStatus = lens _ddcgapsrsResponseStatus (\s a -> s {_ddcgapsrsResponseStatus = a})

instance
  NFData
    DescribeDirectConnectGatewayAssociationProposalsResponse
