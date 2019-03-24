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
-- Module      : Network.AWS.EC2.ReplaceTransitGatewayRoute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the specified route in the specified transit gateway route table.
--
--
module Network.AWS.EC2.ReplaceTransitGatewayRoute
    (
    -- * Creating a Request
      replaceTransitGatewayRoute
    , ReplaceTransitGatewayRoute
    -- * Request Lenses
    , rtgrBlackhole
    , rtgrTransitGatewayAttachmentId
    , rtgrDryRun
    , rtgrDestinationCidrBlock
    , rtgrTransitGatewayRouteTableId

    -- * Destructuring the Response
    , replaceTransitGatewayRouteResponse
    , ReplaceTransitGatewayRouteResponse
    -- * Response Lenses
    , rtgrrsRoute
    , rtgrrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'replaceTransitGatewayRoute' smart constructor.
data ReplaceTransitGatewayRoute = ReplaceTransitGatewayRoute'
  { _rtgrBlackhole                  :: !(Maybe Bool)
  , _rtgrTransitGatewayAttachmentId :: !(Maybe Text)
  , _rtgrDryRun                     :: !(Maybe Bool)
  , _rtgrDestinationCidrBlock       :: !Text
  , _rtgrTransitGatewayRouteTableId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplaceTransitGatewayRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtgrBlackhole' - Indicates whether traffic matching this route is to be dropped.
--
-- * 'rtgrTransitGatewayAttachmentId' - The ID of the attachment.
--
-- * 'rtgrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rtgrDestinationCidrBlock' - The CIDR range used for the destination match. Routing decisions are based on the most specific match.
--
-- * 'rtgrTransitGatewayRouteTableId' - The ID of the route table.
replaceTransitGatewayRoute
    :: Text -- ^ 'rtgrDestinationCidrBlock'
    -> Text -- ^ 'rtgrTransitGatewayRouteTableId'
    -> ReplaceTransitGatewayRoute
replaceTransitGatewayRoute pDestinationCidrBlock_ pTransitGatewayRouteTableId_ =
  ReplaceTransitGatewayRoute'
    { _rtgrBlackhole = Nothing
    , _rtgrTransitGatewayAttachmentId = Nothing
    , _rtgrDryRun = Nothing
    , _rtgrDestinationCidrBlock = pDestinationCidrBlock_
    , _rtgrTransitGatewayRouteTableId = pTransitGatewayRouteTableId_
    }


-- | Indicates whether traffic matching this route is to be dropped.
rtgrBlackhole :: Lens' ReplaceTransitGatewayRoute (Maybe Bool)
rtgrBlackhole = lens _rtgrBlackhole (\ s a -> s{_rtgrBlackhole = a})

-- | The ID of the attachment.
rtgrTransitGatewayAttachmentId :: Lens' ReplaceTransitGatewayRoute (Maybe Text)
rtgrTransitGatewayAttachmentId = lens _rtgrTransitGatewayAttachmentId (\ s a -> s{_rtgrTransitGatewayAttachmentId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rtgrDryRun :: Lens' ReplaceTransitGatewayRoute (Maybe Bool)
rtgrDryRun = lens _rtgrDryRun (\ s a -> s{_rtgrDryRun = a})

-- | The CIDR range used for the destination match. Routing decisions are based on the most specific match.
rtgrDestinationCidrBlock :: Lens' ReplaceTransitGatewayRoute Text
rtgrDestinationCidrBlock = lens _rtgrDestinationCidrBlock (\ s a -> s{_rtgrDestinationCidrBlock = a})

-- | The ID of the route table.
rtgrTransitGatewayRouteTableId :: Lens' ReplaceTransitGatewayRoute Text
rtgrTransitGatewayRouteTableId = lens _rtgrTransitGatewayRouteTableId (\ s a -> s{_rtgrTransitGatewayRouteTableId = a})

instance AWSRequest ReplaceTransitGatewayRoute where
        type Rs ReplaceTransitGatewayRoute =
             ReplaceTransitGatewayRouteResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ReplaceTransitGatewayRouteResponse' <$>
                   (x .@? "route") <*> (pure (fromEnum s)))

instance Hashable ReplaceTransitGatewayRoute where

instance NFData ReplaceTransitGatewayRoute where

instance ToHeaders ReplaceTransitGatewayRoute where
        toHeaders = const mempty

instance ToPath ReplaceTransitGatewayRoute where
        toPath = const "/"

instance ToQuery ReplaceTransitGatewayRoute where
        toQuery ReplaceTransitGatewayRoute'{..}
          = mconcat
              ["Action" =:
                 ("ReplaceTransitGatewayRoute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Blackhole" =: _rtgrBlackhole,
               "TransitGatewayAttachmentId" =:
                 _rtgrTransitGatewayAttachmentId,
               "DryRun" =: _rtgrDryRun,
               "DestinationCidrBlock" =: _rtgrDestinationCidrBlock,
               "TransitGatewayRouteTableId" =:
                 _rtgrTransitGatewayRouteTableId]

-- | /See:/ 'replaceTransitGatewayRouteResponse' smart constructor.
data ReplaceTransitGatewayRouteResponse = ReplaceTransitGatewayRouteResponse'
  { _rtgrrsRoute          :: !(Maybe TransitGatewayRoute)
  , _rtgrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplaceTransitGatewayRouteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtgrrsRoute' - Information about the modified route.
--
-- * 'rtgrrsResponseStatus' - -- | The response status code.
replaceTransitGatewayRouteResponse
    :: Int -- ^ 'rtgrrsResponseStatus'
    -> ReplaceTransitGatewayRouteResponse
replaceTransitGatewayRouteResponse pResponseStatus_ =
  ReplaceTransitGatewayRouteResponse'
    {_rtgrrsRoute = Nothing, _rtgrrsResponseStatus = pResponseStatus_}


-- | Information about the modified route.
rtgrrsRoute :: Lens' ReplaceTransitGatewayRouteResponse (Maybe TransitGatewayRoute)
rtgrrsRoute = lens _rtgrrsRoute (\ s a -> s{_rtgrrsRoute = a})

-- | -- | The response status code.
rtgrrsResponseStatus :: Lens' ReplaceTransitGatewayRouteResponse Int
rtgrrsResponseStatus = lens _rtgrrsResponseStatus (\ s a -> s{_rtgrrsResponseStatus = a})

instance NFData ReplaceTransitGatewayRouteResponse
         where
