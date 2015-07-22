{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DetachVPNGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Detaches a virtual private gateway from a VPC. You do this if you\'re
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
    , dvpngrqDryRun
    , dvpngrqVPNGatewayId
    , dvpngrqVPCId

    -- * Response
    , DetachVPNGatewayResponse
    -- ** Response constructor
    , detachVPNGatewayResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachVPNGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpngrqDryRun'
--
-- * 'dvpngrqVPNGatewayId'
--
-- * 'dvpngrqVPCId'
data DetachVPNGateway = DetachVPNGateway'
    { _dvpngrqDryRun       :: !(Maybe Bool)
    , _dvpngrqVPNGatewayId :: !Text
    , _dvpngrqVPCId        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachVPNGateway' smart constructor.
detachVPNGateway :: Text -> Text -> DetachVPNGateway
detachVPNGateway pVPNGatewayId pVPCId =
    DetachVPNGateway'
    { _dvpngrqDryRun = Nothing
    , _dvpngrqVPNGatewayId = pVPNGatewayId
    , _dvpngrqVPCId = pVPCId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvpngrqDryRun :: Lens' DetachVPNGateway (Maybe Bool)
dvpngrqDryRun = lens _dvpngrqDryRun (\ s a -> s{_dvpngrqDryRun = a});

-- | The ID of the virtual private gateway.
dvpngrqVPNGatewayId :: Lens' DetachVPNGateway Text
dvpngrqVPNGatewayId = lens _dvpngrqVPNGatewayId (\ s a -> s{_dvpngrqVPNGatewayId = a});

-- | The ID of the VPC.
dvpngrqVPCId :: Lens' DetachVPNGateway Text
dvpngrqVPCId = lens _dvpngrqVPCId (\ s a -> s{_dvpngrqVPCId = a});

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
               "DryRun" =: _dvpngrqDryRun,
               "VpnGatewayId" =: _dvpngrqVPNGatewayId,
               "VpcId" =: _dvpngrqVPCId]

-- | /See:/ 'detachVPNGatewayResponse' smart constructor.
data DetachVPNGatewayResponse =
    DetachVPNGatewayResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachVPNGatewayResponse' smart constructor.
detachVPNGatewayResponse :: DetachVPNGatewayResponse
detachVPNGatewayResponse = DetachVPNGatewayResponse'
