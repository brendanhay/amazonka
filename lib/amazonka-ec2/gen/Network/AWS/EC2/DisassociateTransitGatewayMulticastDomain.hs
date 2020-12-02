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
-- Module      : Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified subnets from the transit gateway multicast domain.
module Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain
  ( -- * Creating a Request
    disassociateTransitGatewayMulticastDomain,
    DisassociateTransitGatewayMulticastDomain,

    -- * Request Lenses
    dtgmdtSubnetIds,
    dtgmdtTransitGatewayMulticastDomainId,
    dtgmdtTransitGatewayAttachmentId,
    dtgmdtDryRun,

    -- * Destructuring the Response
    disassociateTransitGatewayMulticastDomainResponse,
    DisassociateTransitGatewayMulticastDomainResponse,

    -- * Response Lenses
    dtgmdrsAssociations,
    dtgmdrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateTransitGatewayMulticastDomain' smart constructor.
data DisassociateTransitGatewayMulticastDomain = DisassociateTransitGatewayMulticastDomain'
  { _dtgmdtSubnetIds ::
      !( Maybe
           [Text]
       ),
    _dtgmdtTransitGatewayMulticastDomainId ::
      !( Maybe
           Text
       ),
    _dtgmdtTransitGatewayAttachmentId ::
      !( Maybe
           Text
       ),
    _dtgmdtDryRun ::
      !( Maybe
           Bool
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

-- | Creates a value of 'DisassociateTransitGatewayMulticastDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgmdtSubnetIds' - The IDs of the subnets;
--
-- * 'dtgmdtTransitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- * 'dtgmdtTransitGatewayAttachmentId' - The ID of the attachment.
--
-- * 'dtgmdtDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
disassociateTransitGatewayMulticastDomain ::
  DisassociateTransitGatewayMulticastDomain
disassociateTransitGatewayMulticastDomain =
  DisassociateTransitGatewayMulticastDomain'
    { _dtgmdtSubnetIds =
        Nothing,
      _dtgmdtTransitGatewayMulticastDomainId = Nothing,
      _dtgmdtTransitGatewayAttachmentId = Nothing,
      _dtgmdtDryRun = Nothing
    }

-- | The IDs of the subnets;
dtgmdtSubnetIds :: Lens' DisassociateTransitGatewayMulticastDomain [Text]
dtgmdtSubnetIds = lens _dtgmdtSubnetIds (\s a -> s {_dtgmdtSubnetIds = a}) . _Default . _Coerce

-- | The ID of the transit gateway multicast domain.
dtgmdtTransitGatewayMulticastDomainId :: Lens' DisassociateTransitGatewayMulticastDomain (Maybe Text)
dtgmdtTransitGatewayMulticastDomainId = lens _dtgmdtTransitGatewayMulticastDomainId (\s a -> s {_dtgmdtTransitGatewayMulticastDomainId = a})

-- | The ID of the attachment.
dtgmdtTransitGatewayAttachmentId :: Lens' DisassociateTransitGatewayMulticastDomain (Maybe Text)
dtgmdtTransitGatewayAttachmentId = lens _dtgmdtTransitGatewayAttachmentId (\s a -> s {_dtgmdtTransitGatewayAttachmentId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgmdtDryRun :: Lens' DisassociateTransitGatewayMulticastDomain (Maybe Bool)
dtgmdtDryRun = lens _dtgmdtDryRun (\s a -> s {_dtgmdtDryRun = a})

instance AWSRequest DisassociateTransitGatewayMulticastDomain where
  type
    Rs DisassociateTransitGatewayMulticastDomain =
      DisassociateTransitGatewayMulticastDomainResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DisassociateTransitGatewayMulticastDomainResponse'
            <$> (x .@? "associations") <*> (pure (fromEnum s))
      )

instance Hashable DisassociateTransitGatewayMulticastDomain

instance NFData DisassociateTransitGatewayMulticastDomain

instance ToHeaders DisassociateTransitGatewayMulticastDomain where
  toHeaders = const mempty

instance ToPath DisassociateTransitGatewayMulticastDomain where
  toPath = const "/"

instance ToQuery DisassociateTransitGatewayMulticastDomain where
  toQuery DisassociateTransitGatewayMulticastDomain' {..} =
    mconcat
      [ "Action"
          =: ("DisassociateTransitGatewayMulticastDomain" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "SubnetIds" <$> _dtgmdtSubnetIds),
        "TransitGatewayMulticastDomainId"
          =: _dtgmdtTransitGatewayMulticastDomainId,
        "TransitGatewayAttachmentId" =: _dtgmdtTransitGatewayAttachmentId,
        "DryRun" =: _dtgmdtDryRun
      ]

-- | /See:/ 'disassociateTransitGatewayMulticastDomainResponse' smart constructor.
data DisassociateTransitGatewayMulticastDomainResponse = DisassociateTransitGatewayMulticastDomainResponse'
  { _dtgmdrsAssociations ::
      !( Maybe
           TransitGatewayMulticastDomainAssociations
       ),
    _dtgmdrsResponseStatus ::
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

-- | Creates a value of 'DisassociateTransitGatewayMulticastDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgmdrsAssociations' - Information about the association.
--
-- * 'dtgmdrsResponseStatus' - -- | The response status code.
disassociateTransitGatewayMulticastDomainResponse ::
  -- | 'dtgmdrsResponseStatus'
  Int ->
  DisassociateTransitGatewayMulticastDomainResponse
disassociateTransitGatewayMulticastDomainResponse pResponseStatus_ =
  DisassociateTransitGatewayMulticastDomainResponse'
    { _dtgmdrsAssociations =
        Nothing,
      _dtgmdrsResponseStatus = pResponseStatus_
    }

-- | Information about the association.
dtgmdrsAssociations :: Lens' DisassociateTransitGatewayMulticastDomainResponse (Maybe TransitGatewayMulticastDomainAssociations)
dtgmdrsAssociations = lens _dtgmdrsAssociations (\s a -> s {_dtgmdrsAssociations = a})

-- | -- | The response status code.
dtgmdrsResponseStatus :: Lens' DisassociateTransitGatewayMulticastDomainResponse Int
dtgmdrsResponseStatus = lens _dtgmdrsResponseStatus (\s a -> s {_dtgmdrsResponseStatus = a})

instance NFData DisassociateTransitGatewayMulticastDomainResponse
