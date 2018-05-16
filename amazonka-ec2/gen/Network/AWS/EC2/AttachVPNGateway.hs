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
-- Module      : Network.AWS.EC2.AttachVPNGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a virtual private gateway to a VPC. You can attach one virtual private gateway to one VPC at a time.
--
--
-- For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html AWS Managed VPN Connections> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.AttachVPNGateway
    (
    -- * Creating a Request
      attachVPNGateway
    , AttachVPNGateway
    -- * Request Lenses
    , avgDryRun
    , avgVPCId
    , avgVPNGatewayId

    -- * Destructuring the Response
    , attachVPNGatewayResponse
    , AttachVPNGatewayResponse
    -- * Response Lenses
    , avgrsVPCAttachment
    , avgrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for AttachVpnGateway.
--
--
--
-- /See:/ 'attachVPNGateway' smart constructor.
data AttachVPNGateway = AttachVPNGateway'
  { _avgDryRun       :: !(Maybe Bool)
  , _avgVPCId        :: !Text
  , _avgVPNGatewayId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachVPNGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avgDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'avgVPCId' - The ID of the VPC.
--
-- * 'avgVPNGatewayId' - The ID of the virtual private gateway.
attachVPNGateway
    :: Text -- ^ 'avgVPCId'
    -> Text -- ^ 'avgVPNGatewayId'
    -> AttachVPNGateway
attachVPNGateway pVPCId_ pVPNGatewayId_ =
  AttachVPNGateway'
    { _avgDryRun = Nothing
    , _avgVPCId = pVPCId_
    , _avgVPNGatewayId = pVPNGatewayId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
avgDryRun :: Lens' AttachVPNGateway (Maybe Bool)
avgDryRun = lens _avgDryRun (\ s a -> s{_avgDryRun = a})

-- | The ID of the VPC.
avgVPCId :: Lens' AttachVPNGateway Text
avgVPCId = lens _avgVPCId (\ s a -> s{_avgVPCId = a})

-- | The ID of the virtual private gateway.
avgVPNGatewayId :: Lens' AttachVPNGateway Text
avgVPNGatewayId = lens _avgVPNGatewayId (\ s a -> s{_avgVPNGatewayId = a})

instance AWSRequest AttachVPNGateway where
        type Rs AttachVPNGateway = AttachVPNGatewayResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AttachVPNGatewayResponse' <$>
                   (x .@? "attachment") <*> (pure (fromEnum s)))

instance Hashable AttachVPNGateway where

instance NFData AttachVPNGateway where

instance ToHeaders AttachVPNGateway where
        toHeaders = const mempty

instance ToPath AttachVPNGateway where
        toPath = const "/"

instance ToQuery AttachVPNGateway where
        toQuery AttachVPNGateway'{..}
          = mconcat
              ["Action" =: ("AttachVpnGateway" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _avgDryRun, "VpcId" =: _avgVPCId,
               "VpnGatewayId" =: _avgVPNGatewayId]

-- | Contains the output of AttachVpnGateway.
--
--
--
-- /See:/ 'attachVPNGatewayResponse' smart constructor.
data AttachVPNGatewayResponse = AttachVPNGatewayResponse'
  { _avgrsVPCAttachment  :: !(Maybe VPCAttachment)
  , _avgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachVPNGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avgrsVPCAttachment' - Information about the attachment.
--
-- * 'avgrsResponseStatus' - -- | The response status code.
attachVPNGatewayResponse
    :: Int -- ^ 'avgrsResponseStatus'
    -> AttachVPNGatewayResponse
attachVPNGatewayResponse pResponseStatus_ =
  AttachVPNGatewayResponse'
    {_avgrsVPCAttachment = Nothing, _avgrsResponseStatus = pResponseStatus_}


-- | Information about the attachment.
avgrsVPCAttachment :: Lens' AttachVPNGatewayResponse (Maybe VPCAttachment)
avgrsVPCAttachment = lens _avgrsVPCAttachment (\ s a -> s{_avgrsVPCAttachment = a})

-- | -- | The response status code.
avgrsResponseStatus :: Lens' AttachVPNGatewayResponse Int
avgrsResponseStatus = lens _avgrsResponseStatus (\ s a -> s{_avgrsResponseStatus = a})

instance NFData AttachVPNGatewayResponse where
