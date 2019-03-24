{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified attachment with the specified transit gateway route table. You can associate only one route table with an attachment.
--
--
module Network.AWS.EC2.AssociateTransitGatewayRouteTable
    (
    -- * Creating a Request
      associateTransitGatewayRouteTable
    , AssociateTransitGatewayRouteTable
    -- * Request Lenses
    , atgrtDryRun
    , atgrtTransitGatewayRouteTableId
    , atgrtTransitGatewayAttachmentId

    -- * Destructuring the Response
    , associateTransitGatewayRouteTableResponse
    , AssociateTransitGatewayRouteTableResponse
    -- * Response Lenses
    , atgrtrsAssociation
    , atgrtrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateTransitGatewayRouteTable' smart constructor.
data AssociateTransitGatewayRouteTable = AssociateTransitGatewayRouteTable'
  { _atgrtDryRun                     :: !(Maybe Bool)
  , _atgrtTransitGatewayRouteTableId :: !Text
  , _atgrtTransitGatewayAttachmentId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateTransitGatewayRouteTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atgrtDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'atgrtTransitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- * 'atgrtTransitGatewayAttachmentId' - The ID of the attachment.
associateTransitGatewayRouteTable
    :: Text -- ^ 'atgrtTransitGatewayRouteTableId'
    -> Text -- ^ 'atgrtTransitGatewayAttachmentId'
    -> AssociateTransitGatewayRouteTable
associateTransitGatewayRouteTable pTransitGatewayRouteTableId_ pTransitGatewayAttachmentId_ =
  AssociateTransitGatewayRouteTable'
    { _atgrtDryRun = Nothing
    , _atgrtTransitGatewayRouteTableId = pTransitGatewayRouteTableId_
    , _atgrtTransitGatewayAttachmentId = pTransitGatewayAttachmentId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
atgrtDryRun :: Lens' AssociateTransitGatewayRouteTable (Maybe Bool)
atgrtDryRun = lens _atgrtDryRun (\ s a -> s{_atgrtDryRun = a})

-- | The ID of the transit gateway route table.
atgrtTransitGatewayRouteTableId :: Lens' AssociateTransitGatewayRouteTable Text
atgrtTransitGatewayRouteTableId = lens _atgrtTransitGatewayRouteTableId (\ s a -> s{_atgrtTransitGatewayRouteTableId = a})

-- | The ID of the attachment.
atgrtTransitGatewayAttachmentId :: Lens' AssociateTransitGatewayRouteTable Text
atgrtTransitGatewayAttachmentId = lens _atgrtTransitGatewayAttachmentId (\ s a -> s{_atgrtTransitGatewayAttachmentId = a})

instance AWSRequest AssociateTransitGatewayRouteTable
         where
        type Rs AssociateTransitGatewayRouteTable =
             AssociateTransitGatewayRouteTableResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AssociateTransitGatewayRouteTableResponse' <$>
                   (x .@? "association") <*> (pure (fromEnum s)))

instance Hashable AssociateTransitGatewayRouteTable
         where

instance NFData AssociateTransitGatewayRouteTable
         where

instance ToHeaders AssociateTransitGatewayRouteTable
         where
        toHeaders = const mempty

instance ToPath AssociateTransitGatewayRouteTable
         where
        toPath = const "/"

instance ToQuery AssociateTransitGatewayRouteTable
         where
        toQuery AssociateTransitGatewayRouteTable'{..}
          = mconcat
              ["Action" =:
                 ("AssociateTransitGatewayRouteTable" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _atgrtDryRun,
               "TransitGatewayRouteTableId" =:
                 _atgrtTransitGatewayRouteTableId,
               "TransitGatewayAttachmentId" =:
                 _atgrtTransitGatewayAttachmentId]

-- | /See:/ 'associateTransitGatewayRouteTableResponse' smart constructor.
data AssociateTransitGatewayRouteTableResponse = AssociateTransitGatewayRouteTableResponse'
  { _atgrtrsAssociation    :: !(Maybe TransitGatewayAssociation)
  , _atgrtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateTransitGatewayRouteTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atgrtrsAssociation' - The ID of the association.
--
-- * 'atgrtrsResponseStatus' - -- | The response status code.
associateTransitGatewayRouteTableResponse
    :: Int -- ^ 'atgrtrsResponseStatus'
    -> AssociateTransitGatewayRouteTableResponse
associateTransitGatewayRouteTableResponse pResponseStatus_ =
  AssociateTransitGatewayRouteTableResponse'
    {_atgrtrsAssociation = Nothing, _atgrtrsResponseStatus = pResponseStatus_}


-- | The ID of the association.
atgrtrsAssociation :: Lens' AssociateTransitGatewayRouteTableResponse (Maybe TransitGatewayAssociation)
atgrtrsAssociation = lens _atgrtrsAssociation (\ s a -> s{_atgrtrsAssociation = a})

-- | -- | The response status code.
atgrtrsResponseStatus :: Lens' AssociateTransitGatewayRouteTableResponse Int
atgrtrsResponseStatus = lens _atgrtrsResponseStatus (\ s a -> s{_atgrtrsResponseStatus = a})

instance NFData
           AssociateTransitGatewayRouteTableResponse
         where
