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
-- Module      : Network.AWS.EC2.DetachVPNGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a virtual private gateway from a VPC. You do this if you're planning to turn off the VPC and not use it anymore. You can confirm a virtual private gateway has been completely detached from a VPC by describing the virtual private gateway (any attachments to the virtual private gateway are also described).
--
--
-- You must wait for the attachment's state to switch to @detached@ before you can delete the VPC or attach a different VPC to the virtual private gateway.
--
module Network.AWS.EC2.DetachVPNGateway
    (
    -- * Creating a Request
      detachVPNGateway
    , DetachVPNGateway
    -- * Request Lenses
    , dvpngDryRun
    , dvpngVPCId
    , dvpngVPNGatewayId

    -- * Destructuring the Response
    , detachVPNGatewayResponse
    , DetachVPNGatewayResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DetachVpnGateway.
--
--
--
-- /See:/ 'detachVPNGateway' smart constructor.
data DetachVPNGateway = DetachVPNGateway'
  { _dvpngDryRun       :: !(Maybe Bool)
  , _dvpngVPCId        :: !Text
  , _dvpngVPNGatewayId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachVPNGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpngDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvpngVPCId' - The ID of the VPC.
--
-- * 'dvpngVPNGatewayId' - The ID of the virtual private gateway.
detachVPNGateway
    :: Text -- ^ 'dvpngVPCId'
    -> Text -- ^ 'dvpngVPNGatewayId'
    -> DetachVPNGateway
detachVPNGateway pVPCId_ pVPNGatewayId_ =
  DetachVPNGateway'
    { _dvpngDryRun = Nothing
    , _dvpngVPCId = pVPCId_
    , _dvpngVPNGatewayId = pVPNGatewayId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvpngDryRun :: Lens' DetachVPNGateway (Maybe Bool)
dvpngDryRun = lens _dvpngDryRun (\ s a -> s{_dvpngDryRun = a})

-- | The ID of the VPC.
dvpngVPCId :: Lens' DetachVPNGateway Text
dvpngVPCId = lens _dvpngVPCId (\ s a -> s{_dvpngVPCId = a})

-- | The ID of the virtual private gateway.
dvpngVPNGatewayId :: Lens' DetachVPNGateway Text
dvpngVPNGatewayId = lens _dvpngVPNGatewayId (\ s a -> s{_dvpngVPNGatewayId = a})

instance AWSRequest DetachVPNGateway where
        type Rs DetachVPNGateway = DetachVPNGatewayResponse
        request = postQuery ec2
        response = receiveNull DetachVPNGatewayResponse'

instance Hashable DetachVPNGateway where

instance NFData DetachVPNGateway where

instance ToHeaders DetachVPNGateway where
        toHeaders = const mempty

instance ToPath DetachVPNGateway where
        toPath = const "/"

instance ToQuery DetachVPNGateway where
        toQuery DetachVPNGateway'{..}
          = mconcat
              ["Action" =: ("DetachVpnGateway" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dvpngDryRun, "VpcId" =: _dvpngVPCId,
               "VpnGatewayId" =: _dvpngVPNGatewayId]

-- | /See:/ 'detachVPNGatewayResponse' smart constructor.
data DetachVPNGatewayResponse =
  DetachVPNGatewayResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachVPNGatewayResponse' with the minimum fields required to make a request.
--
detachVPNGatewayResponse
    :: DetachVPNGatewayResponse
detachVPNGatewayResponse = DetachVPNGatewayResponse'


instance NFData DetachVPNGatewayResponse where
