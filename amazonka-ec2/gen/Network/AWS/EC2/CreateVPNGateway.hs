{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVPNGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVPNGateway.html>
module Network.AWS.EC2.CreateVPNGateway
    (
    -- * Request
      CreateVPNGateway
    -- ** Request constructor
    , createVPNGateway
    -- ** Request lenses
    , cvgrqAvailabilityZone
    , cvgrqDryRun
    , cvgrqType

    -- * Response
    , CreateVPNGatewayResponse
    -- ** Response constructor
    , createVPNGatewayResponse
    -- ** Response lenses
    , cvgrsVPNGateway
    , cvgrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createVPNGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvgrqAvailabilityZone'
--
-- * 'cvgrqDryRun'
--
-- * 'cvgrqType'
data CreateVPNGateway = CreateVPNGateway'
    { _cvgrqAvailabilityZone :: !(Maybe Text)
    , _cvgrqDryRun           :: !(Maybe Bool)
    , _cvgrqType             :: !GatewayType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPNGateway' smart constructor.
createVPNGateway :: GatewayType -> CreateVPNGateway
createVPNGateway pType =
    CreateVPNGateway'
    { _cvgrqAvailabilityZone = Nothing
    , _cvgrqDryRun = Nothing
    , _cvgrqType = pType
    }

-- | The Availability Zone for the virtual private gateway.
cvgrqAvailabilityZone :: Lens' CreateVPNGateway (Maybe Text)
cvgrqAvailabilityZone = lens _cvgrqAvailabilityZone (\ s a -> s{_cvgrqAvailabilityZone = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cvgrqDryRun :: Lens' CreateVPNGateway (Maybe Bool)
cvgrqDryRun = lens _cvgrqDryRun (\ s a -> s{_cvgrqDryRun = a});

-- | The type of VPN connection this virtual private gateway supports.
cvgrqType :: Lens' CreateVPNGateway GatewayType
cvgrqType = lens _cvgrqType (\ s a -> s{_cvgrqType = a});

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
              ["Action" =: ("CreateVPNGateway" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "AvailabilityZone" =: _cvgrqAvailabilityZone,
               "DryRun" =: _cvgrqDryRun, "Type" =: _cvgrqType]

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
createVPNGatewayResponse pStatus =
    CreateVPNGatewayResponse'
    { _cvgrsVPNGateway = Nothing
    , _cvgrsStatus = pStatus
    }

-- | Information about the virtual private gateway.
cvgrsVPNGateway :: Lens' CreateVPNGatewayResponse (Maybe VPNGateway)
cvgrsVPNGateway = lens _cvgrsVPNGateway (\ s a -> s{_cvgrsVPNGateway = a});

-- | FIXME: Undocumented member.
cvgrsStatus :: Lens' CreateVPNGatewayResponse Int
cvgrsStatus = lens _cvgrsStatus (\ s a -> s{_cvgrsStatus = a});
