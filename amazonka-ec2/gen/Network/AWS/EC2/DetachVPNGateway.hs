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
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachVPNGateway.html AWS API Reference> for DetachVPNGateway.
module Network.AWS.EC2.DetachVPNGateway
    (
    -- * Creating a Request
      DetachVPNGateway
    , detachVPNGateway
    -- * Request Lenses
    , dvpngDryRun
    , dvpngVPNGatewayId
    , dvpngVPCId

    -- * Destructuring the Response
    , DetachVPNGatewayResponse
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
-- * 'dvpngDryRun'
--
-- * 'dvpngVPNGatewayId'
--
-- * 'dvpngVPCId'
data DetachVPNGateway = DetachVPNGateway'
    { _dvpngDryRun       :: !(Maybe Bool)
    , _dvpngVPNGatewayId :: !Text
    , _dvpngVPCId        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachVPNGateway' smart constructor.
detachVPNGateway :: Text -> Text -> DetachVPNGateway
detachVPNGateway pVPNGatewayId_ pVPCId_ =
    DetachVPNGateway'
    { _dvpngDryRun = Nothing
    , _dvpngVPNGatewayId = pVPNGatewayId_
    , _dvpngVPCId = pVPCId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvpngDryRun :: Lens' DetachVPNGateway (Maybe Bool)
dvpngDryRun = lens _dvpngDryRun (\ s a -> s{_dvpngDryRun = a});

-- | The ID of the virtual private gateway.
dvpngVPNGatewayId :: Lens' DetachVPNGateway Text
dvpngVPNGatewayId = lens _dvpngVPNGatewayId (\ s a -> s{_dvpngVPNGatewayId = a});

-- | The ID of the VPC.
dvpngVPCId :: Lens' DetachVPNGateway Text
dvpngVPCId = lens _dvpngVPCId (\ s a -> s{_dvpngVPCId = a});

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
              ["Action" =: ("DetachVpnGateway" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dvpngDryRun,
               "VpnGatewayId" =: _dvpngVPNGatewayId,
               "VpcId" =: _dvpngVPCId]

-- | /See:/ 'detachVPNGatewayResponse' smart constructor.
data DetachVPNGatewayResponse =
    DetachVPNGatewayResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachVPNGatewayResponse' smart constructor.
detachVPNGatewayResponse :: DetachVPNGatewayResponse
detachVPNGatewayResponse = DetachVPNGatewayResponse'
