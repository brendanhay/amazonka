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
-- Module      : Network.AWS.EC2.DisassociateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a resource attachment from a transit gateway route table.
--
--
module Network.AWS.EC2.DisassociateTransitGatewayRouteTable
    (
    -- * Creating a Request
      disassociateTransitGatewayRouteTable
    , DisassociateTransitGatewayRouteTable
    -- * Request Lenses
    , dtgrttDryRun
    , dtgrttTransitGatewayRouteTableId
    , dtgrttTransitGatewayAttachmentId

    -- * Destructuring the Response
    , disassociateTransitGatewayRouteTableResponse
    , DisassociateTransitGatewayRouteTableResponse
    -- * Response Lenses
    , dtgrttrsAssociation
    , dtgrttrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateTransitGatewayRouteTable' smart constructor.
data DisassociateTransitGatewayRouteTable = DisassociateTransitGatewayRouteTable'
  { _dtgrttDryRun                     :: !(Maybe Bool)
  , _dtgrttTransitGatewayRouteTableId :: !Text
  , _dtgrttTransitGatewayAttachmentId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateTransitGatewayRouteTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgrttDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgrttTransitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- * 'dtgrttTransitGatewayAttachmentId' - The ID of the attachment.
disassociateTransitGatewayRouteTable
    :: Text -- ^ 'dtgrttTransitGatewayRouteTableId'
    -> Text -- ^ 'dtgrttTransitGatewayAttachmentId'
    -> DisassociateTransitGatewayRouteTable
disassociateTransitGatewayRouteTable pTransitGatewayRouteTableId_ pTransitGatewayAttachmentId_ =
  DisassociateTransitGatewayRouteTable'
    { _dtgrttDryRun = Nothing
    , _dtgrttTransitGatewayRouteTableId = pTransitGatewayRouteTableId_
    , _dtgrttTransitGatewayAttachmentId = pTransitGatewayAttachmentId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgrttDryRun :: Lens' DisassociateTransitGatewayRouteTable (Maybe Bool)
dtgrttDryRun = lens _dtgrttDryRun (\ s a -> s{_dtgrttDryRun = a})

-- | The ID of the transit gateway route table.
dtgrttTransitGatewayRouteTableId :: Lens' DisassociateTransitGatewayRouteTable Text
dtgrttTransitGatewayRouteTableId = lens _dtgrttTransitGatewayRouteTableId (\ s a -> s{_dtgrttTransitGatewayRouteTableId = a})

-- | The ID of the attachment.
dtgrttTransitGatewayAttachmentId :: Lens' DisassociateTransitGatewayRouteTable Text
dtgrttTransitGatewayAttachmentId = lens _dtgrttTransitGatewayAttachmentId (\ s a -> s{_dtgrttTransitGatewayAttachmentId = a})

instance AWSRequest
           DisassociateTransitGatewayRouteTable
         where
        type Rs DisassociateTransitGatewayRouteTable =
             DisassociateTransitGatewayRouteTableResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DisassociateTransitGatewayRouteTableResponse' <$>
                   (x .@? "association") <*> (pure (fromEnum s)))

instance Hashable
           DisassociateTransitGatewayRouteTable
         where

instance NFData DisassociateTransitGatewayRouteTable
         where

instance ToHeaders
           DisassociateTransitGatewayRouteTable
         where
        toHeaders = const mempty

instance ToPath DisassociateTransitGatewayRouteTable
         where
        toPath = const "/"

instance ToQuery DisassociateTransitGatewayRouteTable
         where
        toQuery DisassociateTransitGatewayRouteTable'{..}
          = mconcat
              ["Action" =:
                 ("DisassociateTransitGatewayRouteTable" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dtgrttDryRun,
               "TransitGatewayRouteTableId" =:
                 _dtgrttTransitGatewayRouteTableId,
               "TransitGatewayAttachmentId" =:
                 _dtgrttTransitGatewayAttachmentId]

-- | /See:/ 'disassociateTransitGatewayRouteTableResponse' smart constructor.
data DisassociateTransitGatewayRouteTableResponse = DisassociateTransitGatewayRouteTableResponse'
  { _dtgrttrsAssociation    :: !(Maybe TransitGatewayAssociation)
  , _dtgrttrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateTransitGatewayRouteTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgrttrsAssociation' - Information about the association.
--
-- * 'dtgrttrsResponseStatus' - -- | The response status code.
disassociateTransitGatewayRouteTableResponse
    :: Int -- ^ 'dtgrttrsResponseStatus'
    -> DisassociateTransitGatewayRouteTableResponse
disassociateTransitGatewayRouteTableResponse pResponseStatus_ =
  DisassociateTransitGatewayRouteTableResponse'
    {_dtgrttrsAssociation = Nothing, _dtgrttrsResponseStatus = pResponseStatus_}


-- | Information about the association.
dtgrttrsAssociation :: Lens' DisassociateTransitGatewayRouteTableResponse (Maybe TransitGatewayAssociation)
dtgrttrsAssociation = lens _dtgrttrsAssociation (\ s a -> s{_dtgrttrsAssociation = a})

-- | -- | The response status code.
dtgrttrsResponseStatus :: Lens' DisassociateTransitGatewayRouteTableResponse Int
dtgrttrsResponseStatus = lens _dtgrttrsResponseStatus (\ s a -> s{_dtgrttrsResponseStatus = a})

instance NFData
           DisassociateTransitGatewayRouteTableResponse
         where
