{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.AttachInternetGateway
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

-- | Attaches an Internet gateway to a VPC, enabling connectivity between the
-- Internet and the VPC. For more information about your VPC and Internet
-- gateway, see the
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/ Amazon Virtual Private Cloud User Guide>.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachInternetGateway.html>
module Network.AWS.EC2.AttachInternetGateway
    (
    -- * Request
      AttachInternetGateway
    -- ** Request constructor
    , attachInternetGateway
    -- ** Request lenses
    , aigDryRun
    , aigInternetGatewayId
    , aigVPCId

    -- * Response
    , AttachInternetGatewayResponse
    -- ** Response constructor
    , attachInternetGatewayResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachInternetGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aigDryRun'
--
-- * 'aigInternetGatewayId'
--
-- * 'aigVPCId'
data AttachInternetGateway = AttachInternetGateway'
    { _aigDryRun            :: !(Maybe Bool)
    , _aigInternetGatewayId :: !Text
    , _aigVPCId             :: !Text
    } deriving (Eq,Read,Show)

-- | 'AttachInternetGateway' smart constructor.
attachInternetGateway :: Text -> Text -> AttachInternetGateway
attachInternetGateway pInternetGatewayId pVPCId =
    AttachInternetGateway'
    { _aigDryRun = Nothing
    , _aigInternetGatewayId = pInternetGatewayId
    , _aigVPCId = pVPCId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
aigDryRun :: Lens' AttachInternetGateway (Maybe Bool)
aigDryRun = lens _aigDryRun (\ s a -> s{_aigDryRun = a});

-- | The ID of the Internet gateway.
aigInternetGatewayId :: Lens' AttachInternetGateway Text
aigInternetGatewayId = lens _aigInternetGatewayId (\ s a -> s{_aigInternetGatewayId = a});

-- | The ID of the VPC.
aigVPCId :: Lens' AttachInternetGateway Text
aigVPCId = lens _aigVPCId (\ s a -> s{_aigVPCId = a});

instance AWSRequest AttachInternetGateway where
        type Sv AttachInternetGateway = EC2
        type Rs AttachInternetGateway =
             AttachInternetGatewayResponse
        request = post
        response = receiveNull AttachInternetGatewayResponse'

instance ToHeaders AttachInternetGateway where
        toHeaders = const mempty

instance ToPath AttachInternetGateway where
        toPath = const "/"

instance ToQuery AttachInternetGateway where
        toQuery AttachInternetGateway'{..}
          = mconcat
              ["Action" =: ("AttachInternetGateway" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _aigDryRun,
               "InternetGatewayId" =: _aigInternetGatewayId,
               "VpcId" =: _aigVPCId]

-- | /See:/ 'attachInternetGatewayResponse' smart constructor.
data AttachInternetGatewayResponse =
    AttachInternetGatewayResponse'
    deriving (Eq,Read,Show)

-- | 'AttachInternetGatewayResponse' smart constructor.
attachInternetGatewayResponse :: AttachInternetGatewayResponse
attachInternetGatewayResponse = AttachInternetGatewayResponse'
