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
-- Module      : Network.AWS.EC2.CreateVPNGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a virtual private gateway. A virtual private gateway is the
-- endpoint on the VPC side of your VPN connection. You can create a
-- virtual private gateway before creating the VPC itself.
--
-- For more information about virtual private gateways, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding a Hardware Virtual Private Gateway to Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVPNGateway.html AWS API Reference> for CreateVPNGateway.
module Network.AWS.EC2.CreateVPNGateway
    (
    -- * Creating a Request
      CreateVPNGateway
    , createVPNGateway
    -- * Request Lenses
    , cvgAvailabilityZone
    , cvgDryRun
    , cvgType

    -- * Destructuring the Response
    , CreateVPNGatewayResponse
    , createVPNGatewayResponse
    -- * Response Lenses
    , cvgrsVPNGateway
    , cvgrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createVPNGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvgAvailabilityZone'
--
-- * 'cvgDryRun'
--
-- * 'cvgType'
data CreateVPNGateway = CreateVPNGateway'
    { _cvgAvailabilityZone :: !(Maybe Text)
    , _cvgDryRun           :: !(Maybe Bool)
    , _cvgType             :: !GatewayType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPNGateway' smart constructor.
createVPNGateway :: GatewayType -> CreateVPNGateway
createVPNGateway pType_ =
    CreateVPNGateway'
    { _cvgAvailabilityZone = Nothing
    , _cvgDryRun = Nothing
    , _cvgType = pType_
    }

-- | The Availability Zone for the virtual private gateway.
cvgAvailabilityZone :: Lens' CreateVPNGateway (Maybe Text)
cvgAvailabilityZone = lens _cvgAvailabilityZone (\ s a -> s{_cvgAvailabilityZone = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cvgDryRun :: Lens' CreateVPNGateway (Maybe Bool)
cvgDryRun = lens _cvgDryRun (\ s a -> s{_cvgDryRun = a});

-- | The type of VPN connection this virtual private gateway supports.
cvgType :: Lens' CreateVPNGateway GatewayType
cvgType = lens _cvgType (\ s a -> s{_cvgType = a});

instance AWSRequest CreateVPNGateway where
        type Sv CreateVPNGateway = EC2
        type Rs CreateVPNGateway = CreateVPNGatewayResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateVPNGatewayResponse' <$>
                   (x .@? "vpnGateway") <*> (pure (fromEnum s)))

instance ToHeaders CreateVPNGateway where
        toHeaders = const mempty

instance ToPath CreateVPNGateway where
        toPath = const "/"

instance ToQuery CreateVPNGateway where
        toQuery CreateVPNGateway'{..}
          = mconcat
              ["Action" =: ("CreateVpnGateway" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "AvailabilityZone" =: _cvgAvailabilityZone,
               "DryRun" =: _cvgDryRun, "Type" =: _cvgType]

-- | /See:/ 'createVPNGatewayResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvgrsVPNGateway'
--
-- * 'cvgrsStatus'
data CreateVPNGatewayResponse = CreateVPNGatewayResponse'
    { _cvgrsVPNGateway :: !(Maybe VPNGateway)
    , _cvgrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPNGatewayResponse' smart constructor.
createVPNGatewayResponse :: Int -> CreateVPNGatewayResponse
createVPNGatewayResponse pStatus_ =
    CreateVPNGatewayResponse'
    { _cvgrsVPNGateway = Nothing
    , _cvgrsStatus = pStatus_
    }

-- | Information about the virtual private gateway.
cvgrsVPNGateway :: Lens' CreateVPNGatewayResponse (Maybe VPNGateway)
cvgrsVPNGateway = lens _cvgrsVPNGateway (\ s a -> s{_cvgrsVPNGateway = a});

-- | Undocumented member.
cvgrsStatus :: Lens' CreateVPNGatewayResponse Int
cvgrsStatus = lens _cvgrsStatus (\ s a -> s{_cvgrsStatus = a});
