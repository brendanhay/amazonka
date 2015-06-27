{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.AttachVPNGateway
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

-- | Attaches a virtual private gateway to a VPC. For more information, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding a Hardware Virtual Private Gateway to Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachVPNGateway.html>
module Network.AWS.EC2.AttachVPNGateway
    (
    -- * Request
      AttachVPNGateway
    -- ** Request constructor
    , attachVPNGateway
    -- ** Request lenses
    , avgDryRun
    , avgVPNGatewayId
    , avgVPCId

    -- * Response
    , AttachVPNGatewayResponse
    -- ** Response constructor
    , attachVPNGatewayResponse
    -- ** Response lenses
    , avgrVPCAttachment
    , avgrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachVPNGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avgDryRun'
--
-- * 'avgVPNGatewayId'
--
-- * 'avgVPCId'
data AttachVPNGateway = AttachVPNGateway'
    { _avgDryRun       :: !(Maybe Bool)
    , _avgVPNGatewayId :: !Text
    , _avgVPCId        :: !Text
    } deriving (Eq,Read,Show)

-- | 'AttachVPNGateway' smart constructor.
attachVPNGateway :: Text -> Text -> AttachVPNGateway
attachVPNGateway pVPNGatewayId pVPCId =
    AttachVPNGateway'
    { _avgDryRun = Nothing
    , _avgVPNGatewayId = pVPNGatewayId
    , _avgVPCId = pVPCId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
avgDryRun :: Lens' AttachVPNGateway (Maybe Bool)
avgDryRun = lens _avgDryRun (\ s a -> s{_avgDryRun = a});

-- | The ID of the virtual private gateway.
avgVPNGatewayId :: Lens' AttachVPNGateway Text
avgVPNGatewayId = lens _avgVPNGatewayId (\ s a -> s{_avgVPNGatewayId = a});

-- | The ID of the VPC.
avgVPCId :: Lens' AttachVPNGateway Text
avgVPCId = lens _avgVPCId (\ s a -> s{_avgVPCId = a});

instance AWSRequest AttachVPNGateway where
        type Sv AttachVPNGateway = EC2
        type Rs AttachVPNGateway = AttachVPNGatewayResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 AttachVPNGatewayResponse' <$>
                   (x .@? "attachment") <*> (pure (fromEnum s)))

instance ToHeaders AttachVPNGateway where
        toHeaders = const mempty

instance ToPath AttachVPNGateway where
        toPath = const "/"

instance ToQuery AttachVPNGateway where
        toQuery AttachVPNGateway'{..}
          = mconcat
              ["Action" =: ("AttachVPNGateway" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _avgDryRun,
               "VpnGatewayId" =: _avgVPNGatewayId,
               "VpcId" =: _avgVPCId]

-- | /See:/ 'attachVPNGatewayResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avgrVPCAttachment'
--
-- * 'avgrStatus'
data AttachVPNGatewayResponse = AttachVPNGatewayResponse'
    { _avgrVPCAttachment :: !(Maybe VPCAttachment)
    , _avgrStatus        :: !Int
    } deriving (Eq,Read,Show)

-- | 'AttachVPNGatewayResponse' smart constructor.
attachVPNGatewayResponse :: Int -> AttachVPNGatewayResponse
attachVPNGatewayResponse pStatus =
    AttachVPNGatewayResponse'
    { _avgrVPCAttachment = Nothing
    , _avgrStatus = pStatus
    }

-- | Information about the attachment.
avgrVPCAttachment :: Lens' AttachVPNGatewayResponse (Maybe VPCAttachment)
avgrVPCAttachment = lens _avgrVPCAttachment (\ s a -> s{_avgrVPCAttachment = a});

-- | FIXME: Undocumented member.
avgrStatus :: Lens' AttachVPNGatewayResponse Int
avgrStatus = lens _avgrStatus (\ s a -> s{_avgrStatus = a});
