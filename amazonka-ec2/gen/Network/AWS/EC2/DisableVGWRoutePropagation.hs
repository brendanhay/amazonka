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
-- Module      : Network.AWS.EC2.DisableVGWRoutePropagation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a virtual private gateway (VGW) from propagating routes to a
-- specified route table of a VPC.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisableVGWRoutePropagation.html AWS API Reference> for DisableVGWRoutePropagation.
module Network.AWS.EC2.DisableVGWRoutePropagation
    (
    -- * Creating a Request
      disableVGWRoutePropagation
    , DisableVGWRoutePropagation
    -- * Request Lenses
    , dvrpRouteTableId
    , dvrpGatewayId

    -- * Destructuring the Response
    , disableVGWRoutePropagationResponse
    , DisableVGWRoutePropagationResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'disableVGWRoutePropagation' smart constructor.
data DisableVGWRoutePropagation = DisableVGWRoutePropagation'
    { _dvrpRouteTableId :: !Text
    , _dvrpGatewayId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisableVGWRoutePropagation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvrpRouteTableId'
--
-- * 'dvrpGatewayId'
disableVGWRoutePropagation
    :: Text -- ^ 'dvrpRouteTableId'
    -> Text -- ^ 'dvrpGatewayId'
    -> DisableVGWRoutePropagation
disableVGWRoutePropagation pRouteTableId_ pGatewayId_ =
    DisableVGWRoutePropagation'
    { _dvrpRouteTableId = pRouteTableId_
    , _dvrpGatewayId = pGatewayId_
    }

-- | The ID of the route table.
dvrpRouteTableId :: Lens' DisableVGWRoutePropagation Text
dvrpRouteTableId = lens _dvrpRouteTableId (\ s a -> s{_dvrpRouteTableId = a});

-- | The ID of the virtual private gateway.
dvrpGatewayId :: Lens' DisableVGWRoutePropagation Text
dvrpGatewayId = lens _dvrpGatewayId (\ s a -> s{_dvrpGatewayId = a});

instance AWSRequest DisableVGWRoutePropagation where
        type Rs DisableVGWRoutePropagation =
             DisableVGWRoutePropagationResponse
        request = postQuery eC2
        response
          = receiveNull DisableVGWRoutePropagationResponse'

instance ToHeaders DisableVGWRoutePropagation where
        toHeaders = const mempty

instance ToPath DisableVGWRoutePropagation where
        toPath = const "/"

instance ToQuery DisableVGWRoutePropagation where
        toQuery DisableVGWRoutePropagation'{..}
          = mconcat
              ["Action" =:
                 ("DisableVgwRoutePropagation" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "RouteTableId" =: _dvrpRouteTableId,
               "GatewayId" =: _dvrpGatewayId]

-- | /See:/ 'disableVGWRoutePropagationResponse' smart constructor.
data DisableVGWRoutePropagationResponse =
    DisableVGWRoutePropagationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisableVGWRoutePropagationResponse' with the minimum fields required to make a request.
--
disableVGWRoutePropagationResponse
    :: DisableVGWRoutePropagationResponse
disableVGWRoutePropagationResponse = DisableVGWRoutePropagationResponse'
