{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DetachVPNGateway
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

-- | Detaches a virtual private gateway from a VPC. You do this if you\'re
-- planning to turn off the VPC and not use it anymore. You can confirm a
-- virtual private gateway has been completely detached from a VPC by
-- describing the virtual private gateway (any attachments to the virtual
-- private gateway are also described).
--
-- You must wait for the attachment\'s state to switch to @detached@ before
-- you can delete the VPC or attach a different VPC to the virtual private
-- gateway.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachVPNGateway.html>
module Network.AWS.EC2.DetachVPNGateway
    (
    -- * Request
      DetachVPNGateway
    -- ** Request constructor
    , detachVPNGateway
    -- ** Request lenses
    , detDryRun
    , detVPNGatewayId
    , detVPCId

    -- * Response
    , DetachVPNGatewayResponse
    -- ** Response constructor
    , detachVPNGatewayResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'detachVPNGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'detDryRun'
--
-- * 'detVPNGatewayId'
--
-- * 'detVPCId'
data DetachVPNGateway = DetachVPNGateway'{_detDryRun :: Maybe Bool, _detVPNGatewayId :: Text, _detVPCId :: Text} deriving (Eq, Read, Show)

-- | 'DetachVPNGateway' smart constructor.
detachVPNGateway :: Text -> Text -> DetachVPNGateway
detachVPNGateway pVPNGatewayId pVPCId = DetachVPNGateway'{_detDryRun = Nothing, _detVPNGatewayId = pVPNGatewayId, _detVPCId = pVPCId};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
detDryRun :: Lens' DetachVPNGateway (Maybe Bool)
detDryRun = lens _detDryRun (\ s a -> s{_detDryRun = a});

-- | The ID of the virtual private gateway.
detVPNGatewayId :: Lens' DetachVPNGateway Text
detVPNGatewayId = lens _detVPNGatewayId (\ s a -> s{_detVPNGatewayId = a});

-- | The ID of the VPC.
detVPCId :: Lens' DetachVPNGateway Text
detVPCId = lens _detVPCId (\ s a -> s{_detVPCId = a});

instance AWSRequest DetachVPNGateway where
        type Sv DetachVPNGateway = EC2
        type Rs DetachVPNGateway = DetachVPNGatewayResponse
        request = post
        response = receiveNull DetachVPNGatewayResponse'

instance ToHeaders DetachVPNGateway where
        toHeaders = const mempty

instance ToPath DetachVPNGateway where
        toPath = const "/"

instance ToQuery DetachVPNGateway where
        toQuery DetachVPNGateway'{..}
          = mconcat
              ["Action" =: ("DetachVPNGateway" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _detDryRun,
               "VpnGatewayId" =: _detVPNGatewayId,
               "VpcId" =: _detVPCId]

-- | /See:/ 'detachVPNGatewayResponse' smart constructor.
data DetachVPNGatewayResponse = DetachVPNGatewayResponse' deriving (Eq, Read, Show)

-- | 'DetachVPNGatewayResponse' smart constructor.
detachVPNGatewayResponse :: DetachVPNGatewayResponse
detachVPNGatewayResponse = DetachVPNGatewayResponse';
