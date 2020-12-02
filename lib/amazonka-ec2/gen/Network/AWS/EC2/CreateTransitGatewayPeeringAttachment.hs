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
-- Module      : Network.AWS.EC2.CreateTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a transit gateway peering attachment between the specified transit gateway (requester) and a peer transit gateway (accepter). The transit gateways must be in different Regions. The peer transit gateway can be in your account or a different AWS account.
--
--
-- After you create the peering attachment, the owner of the accepter transit gateway must accept the attachment request.
module Network.AWS.EC2.CreateTransitGatewayPeeringAttachment
  ( -- * Creating a Request
    createTransitGatewayPeeringAttachment,
    CreateTransitGatewayPeeringAttachment,

    -- * Request Lenses
    ctgpaTagSpecifications,
    ctgpaDryRun,
    ctgpaTransitGatewayId,
    ctgpaPeerTransitGatewayId,
    ctgpaPeerAccountId,
    ctgpaPeerRegion,

    -- * Destructuring the Response
    createTransitGatewayPeeringAttachmentResponse,
    CreateTransitGatewayPeeringAttachmentResponse,

    -- * Response Lenses
    ctgparsTransitGatewayPeeringAttachment,
    ctgparsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTransitGatewayPeeringAttachment' smart constructor.
data CreateTransitGatewayPeeringAttachment = CreateTransitGatewayPeeringAttachment'
  { _ctgpaTagSpecifications ::
      !( Maybe
           [TagSpecification]
       ),
    _ctgpaDryRun ::
      !(Maybe Bool),
    _ctgpaTransitGatewayId ::
      !Text,
    _ctgpaPeerTransitGatewayId ::
      !Text,
    _ctgpaPeerAccountId ::
      !Text,
    _ctgpaPeerRegion ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTransitGatewayPeeringAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgpaTagSpecifications' - The tags to apply to the transit gateway peering attachment.
--
-- * 'ctgpaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ctgpaTransitGatewayId' - The ID of the transit gateway.
--
-- * 'ctgpaPeerTransitGatewayId' - The ID of the peer transit gateway with which to create the peering attachment.
--
-- * 'ctgpaPeerAccountId' - The AWS account ID of the owner of the peer transit gateway.
--
-- * 'ctgpaPeerRegion' - The Region where the peer transit gateway is located.
createTransitGatewayPeeringAttachment ::
  -- | 'ctgpaTransitGatewayId'
  Text ->
  -- | 'ctgpaPeerTransitGatewayId'
  Text ->
  -- | 'ctgpaPeerAccountId'
  Text ->
  -- | 'ctgpaPeerRegion'
  Text ->
  CreateTransitGatewayPeeringAttachment
createTransitGatewayPeeringAttachment
  pTransitGatewayId_
  pPeerTransitGatewayId_
  pPeerAccountId_
  pPeerRegion_ =
    CreateTransitGatewayPeeringAttachment'
      { _ctgpaTagSpecifications =
          Nothing,
        _ctgpaDryRun = Nothing,
        _ctgpaTransitGatewayId = pTransitGatewayId_,
        _ctgpaPeerTransitGatewayId = pPeerTransitGatewayId_,
        _ctgpaPeerAccountId = pPeerAccountId_,
        _ctgpaPeerRegion = pPeerRegion_
      }

-- | The tags to apply to the transit gateway peering attachment.
ctgpaTagSpecifications :: Lens' CreateTransitGatewayPeeringAttachment [TagSpecification]
ctgpaTagSpecifications = lens _ctgpaTagSpecifications (\s a -> s {_ctgpaTagSpecifications = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ctgpaDryRun :: Lens' CreateTransitGatewayPeeringAttachment (Maybe Bool)
ctgpaDryRun = lens _ctgpaDryRun (\s a -> s {_ctgpaDryRun = a})

-- | The ID of the transit gateway.
ctgpaTransitGatewayId :: Lens' CreateTransitGatewayPeeringAttachment Text
ctgpaTransitGatewayId = lens _ctgpaTransitGatewayId (\s a -> s {_ctgpaTransitGatewayId = a})

-- | The ID of the peer transit gateway with which to create the peering attachment.
ctgpaPeerTransitGatewayId :: Lens' CreateTransitGatewayPeeringAttachment Text
ctgpaPeerTransitGatewayId = lens _ctgpaPeerTransitGatewayId (\s a -> s {_ctgpaPeerTransitGatewayId = a})

-- | The AWS account ID of the owner of the peer transit gateway.
ctgpaPeerAccountId :: Lens' CreateTransitGatewayPeeringAttachment Text
ctgpaPeerAccountId = lens _ctgpaPeerAccountId (\s a -> s {_ctgpaPeerAccountId = a})

-- | The Region where the peer transit gateway is located.
ctgpaPeerRegion :: Lens' CreateTransitGatewayPeeringAttachment Text
ctgpaPeerRegion = lens _ctgpaPeerRegion (\s a -> s {_ctgpaPeerRegion = a})

instance AWSRequest CreateTransitGatewayPeeringAttachment where
  type
    Rs CreateTransitGatewayPeeringAttachment =
      CreateTransitGatewayPeeringAttachmentResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateTransitGatewayPeeringAttachmentResponse'
            <$> (x .@? "transitGatewayPeeringAttachment") <*> (pure (fromEnum s))
      )

instance Hashable CreateTransitGatewayPeeringAttachment

instance NFData CreateTransitGatewayPeeringAttachment

instance ToHeaders CreateTransitGatewayPeeringAttachment where
  toHeaders = const mempty

instance ToPath CreateTransitGatewayPeeringAttachment where
  toPath = const "/"

instance ToQuery CreateTransitGatewayPeeringAttachment where
  toQuery CreateTransitGatewayPeeringAttachment' {..} =
    mconcat
      [ "Action"
          =: ("CreateTransitGatewayPeeringAttachment" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery
          (toQueryList "TagSpecification" <$> _ctgpaTagSpecifications),
        "DryRun" =: _ctgpaDryRun,
        "TransitGatewayId" =: _ctgpaTransitGatewayId,
        "PeerTransitGatewayId" =: _ctgpaPeerTransitGatewayId,
        "PeerAccountId" =: _ctgpaPeerAccountId,
        "PeerRegion" =: _ctgpaPeerRegion
      ]

-- | /See:/ 'createTransitGatewayPeeringAttachmentResponse' smart constructor.
data CreateTransitGatewayPeeringAttachmentResponse = CreateTransitGatewayPeeringAttachmentResponse'
  { _ctgparsTransitGatewayPeeringAttachment ::
      !( Maybe
           TransitGatewayPeeringAttachment
       ),
    _ctgparsResponseStatus ::
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

-- | Creates a value of 'CreateTransitGatewayPeeringAttachmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgparsTransitGatewayPeeringAttachment' - The transit gateway peering attachment.
--
-- * 'ctgparsResponseStatus' - -- | The response status code.
createTransitGatewayPeeringAttachmentResponse ::
  -- | 'ctgparsResponseStatus'
  Int ->
  CreateTransitGatewayPeeringAttachmentResponse
createTransitGatewayPeeringAttachmentResponse pResponseStatus_ =
  CreateTransitGatewayPeeringAttachmentResponse'
    { _ctgparsTransitGatewayPeeringAttachment =
        Nothing,
      _ctgparsResponseStatus = pResponseStatus_
    }

-- | The transit gateway peering attachment.
ctgparsTransitGatewayPeeringAttachment :: Lens' CreateTransitGatewayPeeringAttachmentResponse (Maybe TransitGatewayPeeringAttachment)
ctgparsTransitGatewayPeeringAttachment = lens _ctgparsTransitGatewayPeeringAttachment (\s a -> s {_ctgparsTransitGatewayPeeringAttachment = a})

-- | -- | The response status code.
ctgparsResponseStatus :: Lens' CreateTransitGatewayPeeringAttachmentResponse Int
ctgparsResponseStatus = lens _ctgparsResponseStatus (\s a -> s {_ctgparsResponseStatus = a})

instance NFData CreateTransitGatewayPeeringAttachmentResponse
