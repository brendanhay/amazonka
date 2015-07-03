{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DetachInternetGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Detaches an Internet gateway from a VPC, disabling connectivity between
-- the Internet and the VPC. The VPC must not contain any running instances
-- with Elastic IP addresses.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachInternetGateway.html>
module Network.AWS.EC2.DetachInternetGateway
    (
    -- * Request
      DetachInternetGateway
    -- ** Request constructor
    , detachInternetGateway
    -- ** Request lenses
    , digDryRun
    , digInternetGatewayId
    , digVPCId

    -- * Response
    , DetachInternetGatewayResponse
    -- ** Response constructor
    , detachInternetGatewayResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachInternetGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'digDryRun'
--
-- * 'digInternetGatewayId'
--
-- * 'digVPCId'
data DetachInternetGateway = DetachInternetGateway'
    { _digDryRun            :: !(Maybe Bool)
    , _digInternetGatewayId :: !Text
    , _digVPCId             :: !Text
    } deriving (Eq,Read,Show)

-- | 'DetachInternetGateway' smart constructor.
detachInternetGateway :: Text -> Text -> DetachInternetGateway
detachInternetGateway pInternetGatewayId pVPCId =
    DetachInternetGateway'
    { _digDryRun = Nothing
    , _digInternetGatewayId = pInternetGatewayId
    , _digVPCId = pVPCId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
digDryRun :: Lens' DetachInternetGateway (Maybe Bool)
digDryRun = lens _digDryRun (\ s a -> s{_digDryRun = a});

-- | The ID of the Internet gateway.
digInternetGatewayId :: Lens' DetachInternetGateway Text
digInternetGatewayId = lens _digInternetGatewayId (\ s a -> s{_digInternetGatewayId = a});

-- | The ID of the VPC.
digVPCId :: Lens' DetachInternetGateway Text
digVPCId = lens _digVPCId (\ s a -> s{_digVPCId = a});

instance AWSRequest DetachInternetGateway where
        type Sv DetachInternetGateway = EC2
        type Rs DetachInternetGateway =
             DetachInternetGatewayResponse
        request = post
        response = receiveNull DetachInternetGatewayResponse'

instance ToHeaders DetachInternetGateway where
        toHeaders = const mempty

instance ToPath DetachInternetGateway where
        toPath = const "/"

instance ToQuery DetachInternetGateway where
        toQuery DetachInternetGateway'{..}
          = mconcat
              ["Action" =: ("DetachInternetGateway" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _digDryRun,
               "InternetGatewayId" =: _digInternetGatewayId,
               "VpcId" =: _digVPCId]

-- | /See:/ 'detachInternetGatewayResponse' smart constructor.
data DetachInternetGatewayResponse =
    DetachInternetGatewayResponse'
    deriving (Eq,Read,Show)

-- | 'DetachInternetGatewayResponse' smart constructor.
detachInternetGatewayResponse :: DetachInternetGatewayResponse
detachInternetGatewayResponse = DetachInternetGatewayResponse'
