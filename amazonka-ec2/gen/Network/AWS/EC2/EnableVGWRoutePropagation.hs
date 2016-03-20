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
-- Module      : Network.AWS.EC2.EnableVGWRoutePropagation
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a virtual private gateway (VGW) to propagate routes to the
-- specified route table of a VPC.
module Network.AWS.EC2.EnableVGWRoutePropagation
    (
    -- * Creating a Request
      enableVGWRoutePropagation
    , EnableVGWRoutePropagation
    -- * Request Lenses
    , evrpRouteTableId
    , evrpGatewayId

    -- * Destructuring the Response
    , enableVGWRoutePropagationResponse
    , EnableVGWRoutePropagationResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'enableVGWRoutePropagation' smart constructor.
data EnableVGWRoutePropagation = EnableVGWRoutePropagation'
    { _evrpRouteTableId :: !Text
    , _evrpGatewayId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnableVGWRoutePropagation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evrpRouteTableId'
--
-- * 'evrpGatewayId'
enableVGWRoutePropagation
    :: Text -- ^ 'evrpRouteTableId'
    -> Text -- ^ 'evrpGatewayId'
    -> EnableVGWRoutePropagation
enableVGWRoutePropagation pRouteTableId_ pGatewayId_ =
    EnableVGWRoutePropagation'
    { _evrpRouteTableId = pRouteTableId_
    , _evrpGatewayId = pGatewayId_
    }

-- | The ID of the route table.
evrpRouteTableId :: Lens' EnableVGWRoutePropagation Text
evrpRouteTableId = lens _evrpRouteTableId (\ s a -> s{_evrpRouteTableId = a});

-- | The ID of the virtual private gateway.
evrpGatewayId :: Lens' EnableVGWRoutePropagation Text
evrpGatewayId = lens _evrpGatewayId (\ s a -> s{_evrpGatewayId = a});

instance AWSRequest EnableVGWRoutePropagation where
        type Rs EnableVGWRoutePropagation =
             EnableVGWRoutePropagationResponse
        request = postQuery eC2
        response
          = receiveNull EnableVGWRoutePropagationResponse'

instance Hashable EnableVGWRoutePropagation

instance ToHeaders EnableVGWRoutePropagation where
        toHeaders = const mempty

instance ToPath EnableVGWRoutePropagation where
        toPath = const "/"

instance ToQuery EnableVGWRoutePropagation where
        toQuery EnableVGWRoutePropagation'{..}
          = mconcat
              ["Action" =:
                 ("EnableVgwRoutePropagation" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "RouteTableId" =: _evrpRouteTableId,
               "GatewayId" =: _evrpGatewayId]

-- | /See:/ 'enableVGWRoutePropagationResponse' smart constructor.
data EnableVGWRoutePropagationResponse =
    EnableVGWRoutePropagationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnableVGWRoutePropagationResponse' with the minimum fields required to make a request.
--
enableVGWRoutePropagationResponse
    :: EnableVGWRoutePropagationResponse
enableVGWRoutePropagationResponse = EnableVGWRoutePropagationResponse'
