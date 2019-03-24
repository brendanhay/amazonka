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
-- Module      : Network.AWS.EC2.CreateTransitGatewayRoute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a static route for the specified transit gateway route table.
--
--
module Network.AWS.EC2.CreateTransitGatewayRoute
    (
    -- * Creating a Request
      createTransitGatewayRoute
    , CreateTransitGatewayRoute
    -- * Request Lenses
    , ctgrBlackhole
    , ctgrTransitGatewayAttachmentId
    , ctgrDryRun
    , ctgrDestinationCidrBlock
    , ctgrTransitGatewayRouteTableId

    -- * Destructuring the Response
    , createTransitGatewayRouteResponse
    , CreateTransitGatewayRouteResponse
    -- * Response Lenses
    , ctgrrsRoute
    , ctgrrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTransitGatewayRoute' smart constructor.
data CreateTransitGatewayRoute = CreateTransitGatewayRoute'
  { _ctgrBlackhole                  :: !(Maybe Bool)
  , _ctgrTransitGatewayAttachmentId :: !(Maybe Text)
  , _ctgrDryRun                     :: !(Maybe Bool)
  , _ctgrDestinationCidrBlock       :: !Text
  , _ctgrTransitGatewayRouteTableId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTransitGatewayRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgrBlackhole' - Indicates whether traffic matching this route is to be dropped.
--
-- * 'ctgrTransitGatewayAttachmentId' - The ID of the attachment.
--
-- * 'ctgrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ctgrDestinationCidrBlock' - The CIDR range used for destination matches. Routing decisions are based on the most specific match.
--
-- * 'ctgrTransitGatewayRouteTableId' - The ID of the transit gateway route table.
createTransitGatewayRoute
    :: Text -- ^ 'ctgrDestinationCidrBlock'
    -> Text -- ^ 'ctgrTransitGatewayRouteTableId'
    -> CreateTransitGatewayRoute
createTransitGatewayRoute pDestinationCidrBlock_ pTransitGatewayRouteTableId_ =
  CreateTransitGatewayRoute'
    { _ctgrBlackhole = Nothing
    , _ctgrTransitGatewayAttachmentId = Nothing
    , _ctgrDryRun = Nothing
    , _ctgrDestinationCidrBlock = pDestinationCidrBlock_
    , _ctgrTransitGatewayRouteTableId = pTransitGatewayRouteTableId_
    }


-- | Indicates whether traffic matching this route is to be dropped.
ctgrBlackhole :: Lens' CreateTransitGatewayRoute (Maybe Bool)
ctgrBlackhole = lens _ctgrBlackhole (\ s a -> s{_ctgrBlackhole = a})

-- | The ID of the attachment.
ctgrTransitGatewayAttachmentId :: Lens' CreateTransitGatewayRoute (Maybe Text)
ctgrTransitGatewayAttachmentId = lens _ctgrTransitGatewayAttachmentId (\ s a -> s{_ctgrTransitGatewayAttachmentId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ctgrDryRun :: Lens' CreateTransitGatewayRoute (Maybe Bool)
ctgrDryRun = lens _ctgrDryRun (\ s a -> s{_ctgrDryRun = a})

-- | The CIDR range used for destination matches. Routing decisions are based on the most specific match.
ctgrDestinationCidrBlock :: Lens' CreateTransitGatewayRoute Text
ctgrDestinationCidrBlock = lens _ctgrDestinationCidrBlock (\ s a -> s{_ctgrDestinationCidrBlock = a})

-- | The ID of the transit gateway route table.
ctgrTransitGatewayRouteTableId :: Lens' CreateTransitGatewayRoute Text
ctgrTransitGatewayRouteTableId = lens _ctgrTransitGatewayRouteTableId (\ s a -> s{_ctgrTransitGatewayRouteTableId = a})

instance AWSRequest CreateTransitGatewayRoute where
        type Rs CreateTransitGatewayRoute =
             CreateTransitGatewayRouteResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateTransitGatewayRouteResponse' <$>
                   (x .@? "route") <*> (pure (fromEnum s)))

instance Hashable CreateTransitGatewayRoute where

instance NFData CreateTransitGatewayRoute where

instance ToHeaders CreateTransitGatewayRoute where
        toHeaders = const mempty

instance ToPath CreateTransitGatewayRoute where
        toPath = const "/"

instance ToQuery CreateTransitGatewayRoute where
        toQuery CreateTransitGatewayRoute'{..}
          = mconcat
              ["Action" =:
                 ("CreateTransitGatewayRoute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Blackhole" =: _ctgrBlackhole,
               "TransitGatewayAttachmentId" =:
                 _ctgrTransitGatewayAttachmentId,
               "DryRun" =: _ctgrDryRun,
               "DestinationCidrBlock" =: _ctgrDestinationCidrBlock,
               "TransitGatewayRouteTableId" =:
                 _ctgrTransitGatewayRouteTableId]

-- | /See:/ 'createTransitGatewayRouteResponse' smart constructor.
data CreateTransitGatewayRouteResponse = CreateTransitGatewayRouteResponse'
  { _ctgrrsRoute          :: !(Maybe TransitGatewayRoute)
  , _ctgrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTransitGatewayRouteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgrrsRoute' - Information about the route.
--
-- * 'ctgrrsResponseStatus' - -- | The response status code.
createTransitGatewayRouteResponse
    :: Int -- ^ 'ctgrrsResponseStatus'
    -> CreateTransitGatewayRouteResponse
createTransitGatewayRouteResponse pResponseStatus_ =
  CreateTransitGatewayRouteResponse'
    {_ctgrrsRoute = Nothing, _ctgrrsResponseStatus = pResponseStatus_}


-- | Information about the route.
ctgrrsRoute :: Lens' CreateTransitGatewayRouteResponse (Maybe TransitGatewayRoute)
ctgrrsRoute = lens _ctgrrsRoute (\ s a -> s{_ctgrrsRoute = a})

-- | -- | The response status code.
ctgrrsResponseStatus :: Lens' CreateTransitGatewayRouteResponse Int
ctgrrsResponseStatus = lens _ctgrrsResponseStatus (\ s a -> s{_ctgrrsResponseStatus = a})

instance NFData CreateTransitGatewayRouteResponse
         where
