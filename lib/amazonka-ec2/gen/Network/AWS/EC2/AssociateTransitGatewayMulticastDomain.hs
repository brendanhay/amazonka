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
-- Module      : Network.AWS.EC2.AssociateTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified subnets and transit gateway attachments with the specified transit gateway multicast domain.
--
--
-- The transit gateway attachment must be in the available state before you can add a resource. Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeTransitGatewayAttachments.html DescribeTransitGatewayAttachments> to see the state of the attachment.
module Network.AWS.EC2.AssociateTransitGatewayMulticastDomain
  ( -- * Creating a Request
    associateTransitGatewayMulticastDomain,
    AssociateTransitGatewayMulticastDomain,

    -- * Request Lenses
    atgmdSubnetIds,
    atgmdTransitGatewayMulticastDomainId,
    atgmdTransitGatewayAttachmentId,
    atgmdDryRun,

    -- * Destructuring the Response
    associateTransitGatewayMulticastDomainResponse,
    AssociateTransitGatewayMulticastDomainResponse,

    -- * Response Lenses
    atgmdrsAssociations,
    atgmdrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateTransitGatewayMulticastDomain' smart constructor.
data AssociateTransitGatewayMulticastDomain = AssociateTransitGatewayMulticastDomain'
  { _atgmdSubnetIds ::
      !( Maybe
           [Text]
       ),
    _atgmdTransitGatewayMulticastDomainId ::
      !(Maybe Text),
    _atgmdTransitGatewayAttachmentId ::
      !(Maybe Text),
    _atgmdDryRun ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateTransitGatewayMulticastDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atgmdSubnetIds' - The IDs of the subnets to associate with the transit gateway multicast domain.
--
-- * 'atgmdTransitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- * 'atgmdTransitGatewayAttachmentId' - The ID of the transit gateway attachment to associate with the transit gateway multicast domain.
--
-- * 'atgmdDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
associateTransitGatewayMulticastDomain ::
  AssociateTransitGatewayMulticastDomain
associateTransitGatewayMulticastDomain =
  AssociateTransitGatewayMulticastDomain'
    { _atgmdSubnetIds =
        Nothing,
      _atgmdTransitGatewayMulticastDomainId = Nothing,
      _atgmdTransitGatewayAttachmentId = Nothing,
      _atgmdDryRun = Nothing
    }

-- | The IDs of the subnets to associate with the transit gateway multicast domain.
atgmdSubnetIds :: Lens' AssociateTransitGatewayMulticastDomain [Text]
atgmdSubnetIds = lens _atgmdSubnetIds (\s a -> s {_atgmdSubnetIds = a}) . _Default . _Coerce

-- | The ID of the transit gateway multicast domain.
atgmdTransitGatewayMulticastDomainId :: Lens' AssociateTransitGatewayMulticastDomain (Maybe Text)
atgmdTransitGatewayMulticastDomainId = lens _atgmdTransitGatewayMulticastDomainId (\s a -> s {_atgmdTransitGatewayMulticastDomainId = a})

-- | The ID of the transit gateway attachment to associate with the transit gateway multicast domain.
atgmdTransitGatewayAttachmentId :: Lens' AssociateTransitGatewayMulticastDomain (Maybe Text)
atgmdTransitGatewayAttachmentId = lens _atgmdTransitGatewayAttachmentId (\s a -> s {_atgmdTransitGatewayAttachmentId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
atgmdDryRun :: Lens' AssociateTransitGatewayMulticastDomain (Maybe Bool)
atgmdDryRun = lens _atgmdDryRun (\s a -> s {_atgmdDryRun = a})

instance AWSRequest AssociateTransitGatewayMulticastDomain where
  type
    Rs AssociateTransitGatewayMulticastDomain =
      AssociateTransitGatewayMulticastDomainResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          AssociateTransitGatewayMulticastDomainResponse'
            <$> (x .@? "associations") <*> (pure (fromEnum s))
      )

instance Hashable AssociateTransitGatewayMulticastDomain

instance NFData AssociateTransitGatewayMulticastDomain

instance ToHeaders AssociateTransitGatewayMulticastDomain where
  toHeaders = const mempty

instance ToPath AssociateTransitGatewayMulticastDomain where
  toPath = const "/"

instance ToQuery AssociateTransitGatewayMulticastDomain where
  toQuery AssociateTransitGatewayMulticastDomain' {..} =
    mconcat
      [ "Action"
          =: ("AssociateTransitGatewayMulticastDomain" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "SubnetIds" <$> _atgmdSubnetIds),
        "TransitGatewayMulticastDomainId"
          =: _atgmdTransitGatewayMulticastDomainId,
        "TransitGatewayAttachmentId" =: _atgmdTransitGatewayAttachmentId,
        "DryRun" =: _atgmdDryRun
      ]

-- | /See:/ 'associateTransitGatewayMulticastDomainResponse' smart constructor.
data AssociateTransitGatewayMulticastDomainResponse = AssociateTransitGatewayMulticastDomainResponse'
  { _atgmdrsAssociations ::
      !( Maybe
           TransitGatewayMulticastDomainAssociations
       ),
    _atgmdrsResponseStatus ::
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

-- | Creates a value of 'AssociateTransitGatewayMulticastDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atgmdrsAssociations' - Information about the transit gateway multicast domain associations.
--
-- * 'atgmdrsResponseStatus' - -- | The response status code.
associateTransitGatewayMulticastDomainResponse ::
  -- | 'atgmdrsResponseStatus'
  Int ->
  AssociateTransitGatewayMulticastDomainResponse
associateTransitGatewayMulticastDomainResponse pResponseStatus_ =
  AssociateTransitGatewayMulticastDomainResponse'
    { _atgmdrsAssociations =
        Nothing,
      _atgmdrsResponseStatus = pResponseStatus_
    }

-- | Information about the transit gateway multicast domain associations.
atgmdrsAssociations :: Lens' AssociateTransitGatewayMulticastDomainResponse (Maybe TransitGatewayMulticastDomainAssociations)
atgmdrsAssociations = lens _atgmdrsAssociations (\s a -> s {_atgmdrsAssociations = a})

-- | -- | The response status code.
atgmdrsResponseStatus :: Lens' AssociateTransitGatewayMulticastDomainResponse Int
atgmdrsResponseStatus = lens _atgmdrsResponseStatus (\s a -> s {_atgmdrsResponseStatus = a})

instance NFData AssociateTransitGatewayMulticastDomainResponse
