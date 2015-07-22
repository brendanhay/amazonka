{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AttachVPNGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Attaches a virtual private gateway to a VPC. For more information, see
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
    , avgrqDryRun
    , avgrqVPNGatewayId
    , avgrqVPCId

    -- * Response
    , AttachVPNGatewayResponse
    -- ** Response constructor
    , attachVPNGatewayResponse
    -- ** Response lenses
    , avgrsVPCAttachment
    , avgrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachVPNGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avgrqDryRun'
--
-- * 'avgrqVPNGatewayId'
--
-- * 'avgrqVPCId'
data AttachVPNGateway = AttachVPNGateway'
    { _avgrqDryRun       :: !(Maybe Bool)
    , _avgrqVPNGatewayId :: !Text
    , _avgrqVPCId        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachVPNGateway' smart constructor.
attachVPNGateway :: Text -> Text -> AttachVPNGateway
attachVPNGateway pVPNGatewayId pVPCId =
    AttachVPNGateway'
    { _avgrqDryRun = Nothing
    , _avgrqVPNGatewayId = pVPNGatewayId
    , _avgrqVPCId = pVPCId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
avgrqDryRun :: Lens' AttachVPNGateway (Maybe Bool)
avgrqDryRun = lens _avgrqDryRun (\ s a -> s{_avgrqDryRun = a});

-- | The ID of the virtual private gateway.
avgrqVPNGatewayId :: Lens' AttachVPNGateway Text
avgrqVPNGatewayId = lens _avgrqVPNGatewayId (\ s a -> s{_avgrqVPNGatewayId = a});

-- | The ID of the VPC.
avgrqVPCId :: Lens' AttachVPNGateway Text
avgrqVPCId = lens _avgrqVPCId (\ s a -> s{_avgrqVPCId = a});

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
               "DryRun" =: _avgrqDryRun,
               "VpnGatewayId" =: _avgrqVPNGatewayId,
               "VpcId" =: _avgrqVPCId]

-- | /See:/ 'attachVPNGatewayResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avgrsVPCAttachment'
--
-- * 'avgrsStatus'
data AttachVPNGatewayResponse = AttachVPNGatewayResponse'
    { _avgrsVPCAttachment :: !(Maybe VPCAttachment)
    , _avgrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachVPNGatewayResponse' smart constructor.
attachVPNGatewayResponse :: Int -> AttachVPNGatewayResponse
attachVPNGatewayResponse pStatus =
    AttachVPNGatewayResponse'
    { _avgrsVPCAttachment = Nothing
    , _avgrsStatus = pStatus
    }

-- | Information about the attachment.
avgrsVPCAttachment :: Lens' AttachVPNGatewayResponse (Maybe VPCAttachment)
avgrsVPCAttachment = lens _avgrsVPCAttachment (\ s a -> s{_avgrsVPCAttachment = a});

-- | FIXME: Undocumented member.
avgrsStatus :: Lens' AttachVPNGatewayResponse Int
avgrsStatus = lens _avgrsStatus (\ s a -> s{_avgrsStatus = a});
