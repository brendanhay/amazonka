{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.EnableVGWRoutePropagation
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Enables a virtual private gateway (VGW) to propagate routes to the
-- specified route table of a VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-EnableVGWRoutePropagation.html>
module Network.AWS.EC2.EnableVGWRoutePropagation
    (
    -- * Request
      EnableVGWRoutePropagation
    -- ** Request constructor
    , enableVGWRoutePropagation
    -- ** Request lenses
    , evrpRouteTableId
    , evrpGatewayId

    -- * Response
    , EnableVGWRoutePropagationResponse
    -- ** Response constructor
    , enableVGWRoutePropagationResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableVGWRoutePropagation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'evrpRouteTableId'
--
-- * 'evrpGatewayId'
data EnableVGWRoutePropagation = EnableVGWRoutePropagation'{_evrpRouteTableId :: Text, _evrpGatewayId :: Text} deriving (Eq, Read, Show)

-- | 'EnableVGWRoutePropagation' smart constructor.
enableVGWRoutePropagation :: Text -> Text -> EnableVGWRoutePropagation
enableVGWRoutePropagation pRouteTableId pGatewayId = EnableVGWRoutePropagation'{_evrpRouteTableId = pRouteTableId, _evrpGatewayId = pGatewayId};

-- | The ID of the route table.
evrpRouteTableId :: Lens' EnableVGWRoutePropagation Text
evrpRouteTableId = lens _evrpRouteTableId (\ s a -> s{_evrpRouteTableId = a});

-- | The ID of the virtual private gateway.
evrpGatewayId :: Lens' EnableVGWRoutePropagation Text
evrpGatewayId = lens _evrpGatewayId (\ s a -> s{_evrpGatewayId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest EnableVGWRoutePropagation where
        type Sv EnableVGWRoutePropagation = EC2
        type Rs EnableVGWRoutePropagation =
             EnableVGWRoutePropagationResponse
        request = post
        response
          = receiveNull EnableVGWRoutePropagationResponse'

instance ToHeaders EnableVGWRoutePropagation where
        toHeaders = const mempty

instance ToPath EnableVGWRoutePropagation where
        toPath = const "/"

instance ToQuery EnableVGWRoutePropagation where
        toQuery EnableVGWRoutePropagation'{..}
          = mconcat
              ["Action" =:
                 ("EnableVGWRoutePropagation" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "RouteTableId" =: _evrpRouteTableId,
               "GatewayId" =: _evrpGatewayId]

-- | /See:/ 'enableVGWRoutePropagationResponse' smart constructor.
data EnableVGWRoutePropagationResponse = EnableVGWRoutePropagationResponse' deriving (Eq, Read, Show)

-- | 'EnableVGWRoutePropagationResponse' smart constructor.
enableVGWRoutePropagationResponse :: EnableVGWRoutePropagationResponse
enableVGWRoutePropagationResponse = EnableVGWRoutePropagationResponse';
