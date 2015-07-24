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
    , avgDryRun
    , avgVPNGatewayId
    , avgVPCId

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
-- * 'avgDryRun'
--
-- * 'avgVPNGatewayId'
--
-- * 'avgVPCId'
data AttachVPNGateway = AttachVPNGateway'
    { _avgDryRun       :: !(Maybe Bool)
    , _avgVPNGatewayId :: !Text
    , _avgVPCId        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachVPNGateway' smart constructor.
attachVPNGateway :: Text -> Text -> AttachVPNGateway
attachVPNGateway pVPNGatewayId_ pVPCId_ =
    AttachVPNGateway'
    { _avgDryRun = Nothing
    , _avgVPNGatewayId = pVPNGatewayId_
    , _avgVPCId = pVPCId_
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
        request = post "AttachVPNGateway"
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
-- * 'avgrsVPCAttachment'
--
-- * 'avgrsStatus'
data AttachVPNGatewayResponse = AttachVPNGatewayResponse'
    { _avgrsVPCAttachment :: !(Maybe VPCAttachment)
    , _avgrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachVPNGatewayResponse' smart constructor.
attachVPNGatewayResponse :: Int -> AttachVPNGatewayResponse
attachVPNGatewayResponse pStatus_ =
    AttachVPNGatewayResponse'
    { _avgrsVPCAttachment = Nothing
    , _avgrsStatus = pStatus_
    }

-- | Information about the attachment.
avgrsVPCAttachment :: Lens' AttachVPNGatewayResponse (Maybe VPCAttachment)
avgrsVPCAttachment = lens _avgrsVPCAttachment (\ s a -> s{_avgrsVPCAttachment = a});

-- | FIXME: Undocumented member.
avgrsStatus :: Lens' AttachVPNGatewayResponse Int
avgrsStatus = lens _avgrsStatus (\ s a -> s{_avgrsStatus = a});
