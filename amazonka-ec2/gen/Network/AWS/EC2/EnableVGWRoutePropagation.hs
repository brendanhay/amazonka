{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableVGWRoutePropagation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Enables a virtual private gateway (VGW) to propagate routes to the
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
    , evrprqRouteTableId
    , evrprqGatewayId

    -- * Response
    , EnableVGWRoutePropagationResponse
    -- ** Response constructor
    , enableVGWRoutePropagationResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'enableVGWRoutePropagation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'evrprqRouteTableId'
--
-- * 'evrprqGatewayId'
data EnableVGWRoutePropagation = EnableVGWRoutePropagation'
    { _evrprqRouteTableId :: !Text
    , _evrprqGatewayId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableVGWRoutePropagation' smart constructor.
enableVGWRoutePropagation :: Text -> Text -> EnableVGWRoutePropagation
enableVGWRoutePropagation pRouteTableId_ pGatewayId_ =
    EnableVGWRoutePropagation'
    { _evrprqRouteTableId = pRouteTableId_
    , _evrprqGatewayId = pGatewayId_
    }

-- | The ID of the route table.
evrprqRouteTableId :: Lens' EnableVGWRoutePropagation Text
evrprqRouteTableId = lens _evrprqRouteTableId (\ s a -> s{_evrprqRouteTableId = a});

-- | The ID of the virtual private gateway.
evrprqGatewayId :: Lens' EnableVGWRoutePropagation Text
evrprqGatewayId = lens _evrprqGatewayId (\ s a -> s{_evrprqGatewayId = a});

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
               "RouteTableId" =: _evrprqRouteTableId,
               "GatewayId" =: _evrprqGatewayId]

-- | /See:/ 'enableVGWRoutePropagationResponse' smart constructor.
data EnableVGWRoutePropagationResponse =
    EnableVGWRoutePropagationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableVGWRoutePropagationResponse' smart constructor.
enableVGWRoutePropagationResponse :: EnableVGWRoutePropagationResponse
enableVGWRoutePropagationResponse = EnableVGWRoutePropagationResponse'
