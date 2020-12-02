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
-- Module      : Network.AWS.DirectConnect.AcceptDirectConnectGatewayAssociationProposal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a proposal request to attach a virtual private gateway or transit gateway to a Direct Connect gateway.
module Network.AWS.DirectConnect.AcceptDirectConnectGatewayAssociationProposal
  ( -- * Creating a Request
    acceptDirectConnectGatewayAssociationProposal,
    AcceptDirectConnectGatewayAssociationProposal,

    -- * Request Lenses
    adcgapOverrideAllowedPrefixesToDirectConnectGateway,
    adcgapDirectConnectGatewayId,
    adcgapProposalId,
    adcgapAssociatedGatewayOwnerAccount,

    -- * Destructuring the Response
    acceptDirectConnectGatewayAssociationProposalResponse,
    AcceptDirectConnectGatewayAssociationProposalResponse,

    -- * Response Lenses
    adcgaprsDirectConnectGatewayAssociation,
    adcgaprsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'acceptDirectConnectGatewayAssociationProposal' smart constructor.
data AcceptDirectConnectGatewayAssociationProposal = AcceptDirectConnectGatewayAssociationProposal'
  { _adcgapOverrideAllowedPrefixesToDirectConnectGateway ::
      !( Maybe
           [RouteFilterPrefix]
       ),
    _adcgapDirectConnectGatewayId ::
      !Text,
    _adcgapProposalId ::
      !Text,
    _adcgapAssociatedGatewayOwnerAccount ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'AcceptDirectConnectGatewayAssociationProposal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adcgapOverrideAllowedPrefixesToDirectConnectGateway' - Overrides the Amazon VPC prefixes advertised to the Direct Connect gateway. For information about how to set the prefixes, see <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes> in the /AWS Direct Connect User Guide/ .
--
-- * 'adcgapDirectConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- * 'adcgapProposalId' - The ID of the request proposal.
--
-- * 'adcgapAssociatedGatewayOwnerAccount' - The ID of the AWS account that owns the virtual private gateway or transit gateway.
acceptDirectConnectGatewayAssociationProposal ::
  -- | 'adcgapDirectConnectGatewayId'
  Text ->
  -- | 'adcgapProposalId'
  Text ->
  -- | 'adcgapAssociatedGatewayOwnerAccount'
  Text ->
  AcceptDirectConnectGatewayAssociationProposal
acceptDirectConnectGatewayAssociationProposal
  pDirectConnectGatewayId_
  pProposalId_
  pAssociatedGatewayOwnerAccount_ =
    AcceptDirectConnectGatewayAssociationProposal'
      { _adcgapOverrideAllowedPrefixesToDirectConnectGateway =
          Nothing,
        _adcgapDirectConnectGatewayId =
          pDirectConnectGatewayId_,
        _adcgapProposalId = pProposalId_,
        _adcgapAssociatedGatewayOwnerAccount =
          pAssociatedGatewayOwnerAccount_
      }

-- | Overrides the Amazon VPC prefixes advertised to the Direct Connect gateway. For information about how to set the prefixes, see <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes> in the /AWS Direct Connect User Guide/ .
adcgapOverrideAllowedPrefixesToDirectConnectGateway :: Lens' AcceptDirectConnectGatewayAssociationProposal [RouteFilterPrefix]
adcgapOverrideAllowedPrefixesToDirectConnectGateway = lens _adcgapOverrideAllowedPrefixesToDirectConnectGateway (\s a -> s {_adcgapOverrideAllowedPrefixesToDirectConnectGateway = a}) . _Default . _Coerce

-- | The ID of the Direct Connect gateway.
adcgapDirectConnectGatewayId :: Lens' AcceptDirectConnectGatewayAssociationProposal Text
adcgapDirectConnectGatewayId = lens _adcgapDirectConnectGatewayId (\s a -> s {_adcgapDirectConnectGatewayId = a})

-- | The ID of the request proposal.
adcgapProposalId :: Lens' AcceptDirectConnectGatewayAssociationProposal Text
adcgapProposalId = lens _adcgapProposalId (\s a -> s {_adcgapProposalId = a})

-- | The ID of the AWS account that owns the virtual private gateway or transit gateway.
adcgapAssociatedGatewayOwnerAccount :: Lens' AcceptDirectConnectGatewayAssociationProposal Text
adcgapAssociatedGatewayOwnerAccount = lens _adcgapAssociatedGatewayOwnerAccount (\s a -> s {_adcgapAssociatedGatewayOwnerAccount = a})

instance AWSRequest AcceptDirectConnectGatewayAssociationProposal where
  type
    Rs AcceptDirectConnectGatewayAssociationProposal =
      AcceptDirectConnectGatewayAssociationProposalResponse
  request = postJSON directConnect
  response =
    receiveJSON
      ( \s h x ->
          AcceptDirectConnectGatewayAssociationProposalResponse'
            <$> (x .?> "directConnectGatewayAssociation") <*> (pure (fromEnum s))
      )

instance Hashable AcceptDirectConnectGatewayAssociationProposal

instance NFData AcceptDirectConnectGatewayAssociationProposal

instance ToHeaders AcceptDirectConnectGatewayAssociationProposal where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "OvertureService.AcceptDirectConnectGatewayAssociationProposal" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AcceptDirectConnectGatewayAssociationProposal where
  toJSON AcceptDirectConnectGatewayAssociationProposal' {..} =
    object
      ( catMaybes
          [ ("overrideAllowedPrefixesToDirectConnectGateway" .=)
              <$> _adcgapOverrideAllowedPrefixesToDirectConnectGateway,
            Just ("directConnectGatewayId" .= _adcgapDirectConnectGatewayId),
            Just ("proposalId" .= _adcgapProposalId),
            Just
              ( "associatedGatewayOwnerAccount"
                  .= _adcgapAssociatedGatewayOwnerAccount
              )
          ]
      )

instance ToPath AcceptDirectConnectGatewayAssociationProposal where
  toPath = const "/"

instance ToQuery AcceptDirectConnectGatewayAssociationProposal where
  toQuery = const mempty

-- | /See:/ 'acceptDirectConnectGatewayAssociationProposalResponse' smart constructor.
data AcceptDirectConnectGatewayAssociationProposalResponse = AcceptDirectConnectGatewayAssociationProposalResponse'
  { _adcgaprsDirectConnectGatewayAssociation ::
      !( Maybe
           DirectConnectGatewayAssociation
       ),
    _adcgaprsResponseStatus ::
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

-- | Creates a value of 'AcceptDirectConnectGatewayAssociationProposalResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adcgaprsDirectConnectGatewayAssociation' - Undocumented member.
--
-- * 'adcgaprsResponseStatus' - -- | The response status code.
acceptDirectConnectGatewayAssociationProposalResponse ::
  -- | 'adcgaprsResponseStatus'
  Int ->
  AcceptDirectConnectGatewayAssociationProposalResponse
acceptDirectConnectGatewayAssociationProposalResponse
  pResponseStatus_ =
    AcceptDirectConnectGatewayAssociationProposalResponse'
      { _adcgaprsDirectConnectGatewayAssociation =
          Nothing,
        _adcgaprsResponseStatus =
          pResponseStatus_
      }

-- | Undocumented member.
adcgaprsDirectConnectGatewayAssociation :: Lens' AcceptDirectConnectGatewayAssociationProposalResponse (Maybe DirectConnectGatewayAssociation)
adcgaprsDirectConnectGatewayAssociation = lens _adcgaprsDirectConnectGatewayAssociation (\s a -> s {_adcgaprsDirectConnectGatewayAssociation = a})

-- | -- | The response status code.
adcgaprsResponseStatus :: Lens' AcceptDirectConnectGatewayAssociationProposalResponse Int
adcgaprsResponseStatus = lens _adcgaprsResponseStatus (\s a -> s {_adcgaprsResponseStatus = a})

instance
  NFData
    AcceptDirectConnectGatewayAssociationProposalResponse
