{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVPNConnection
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPN connection between an existing virtual private gateway and
-- a VPN customer gateway. The only supported connection type is @ipsec.1@.
--
-- The response includes information that you need to give to your network
-- administrator to configure your customer gateway.
--
-- We strongly recommend that you use HTTPS when calling this operation
-- because the response contains sensitive cryptographic information for
-- configuring your customer gateway.
--
-- If you decide to shut down your VPN connection for any reason and later
-- create a new VPN connection, you must reconfigure your customer gateway
-- with the new information returned from this call.
--
-- For more information about VPN connections, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding a Hardware Virtual Private Gateway to Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVPNConnection.html>
module Network.AWS.EC2.CreateVPNConnection
    (
    -- * Request
      CreateVPNConnection
    -- ** Request constructor
    , createVPNConnection
    -- ** Request lenses
    , cvcrqOptions
    , cvcrqDryRun
    , cvcrqType
    , cvcrqCustomerGatewayId
    , cvcrqVPNGatewayId

    -- * Response
    , CreateVPNConnectionResponse
    -- ** Response constructor
    , createVPNConnectionResponse
    -- ** Response lenses
    , cvcrsVPNConnection
    , cvcrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createVPNConnection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvcrqOptions'
--
-- * 'cvcrqDryRun'
--
-- * 'cvcrqType'
--
-- * 'cvcrqCustomerGatewayId'
--
-- * 'cvcrqVPNGatewayId'
data CreateVPNConnection = CreateVPNConnection'
    { _cvcrqOptions           :: !(Maybe VPNConnectionOptionsSpecification)
    , _cvcrqDryRun            :: !(Maybe Bool)
    , _cvcrqType              :: !Text
    , _cvcrqCustomerGatewayId :: !Text
    , _cvcrqVPNGatewayId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPNConnection' smart constructor.
createVPNConnection :: Text -> Text -> Text -> CreateVPNConnection
createVPNConnection pType pCustomerGatewayId pVPNGatewayId =
    CreateVPNConnection'
    { _cvcrqOptions = Nothing
    , _cvcrqDryRun = Nothing
    , _cvcrqType = pType
    , _cvcrqCustomerGatewayId = pCustomerGatewayId
    , _cvcrqVPNGatewayId = pVPNGatewayId
    }

-- | Indicates whether the VPN connection requires static routes. If you are
-- creating a VPN connection for a device that does not support BGP, you
-- must specify @true@.
--
-- Default: @false@
cvcrqOptions :: Lens' CreateVPNConnection (Maybe VPNConnectionOptionsSpecification)
cvcrqOptions = lens _cvcrqOptions (\ s a -> s{_cvcrqOptions = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cvcrqDryRun :: Lens' CreateVPNConnection (Maybe Bool)
cvcrqDryRun = lens _cvcrqDryRun (\ s a -> s{_cvcrqDryRun = a});

-- | The type of VPN connection (@ipsec.1@).
cvcrqType :: Lens' CreateVPNConnection Text
cvcrqType = lens _cvcrqType (\ s a -> s{_cvcrqType = a});

-- | The ID of the customer gateway.
cvcrqCustomerGatewayId :: Lens' CreateVPNConnection Text
cvcrqCustomerGatewayId = lens _cvcrqCustomerGatewayId (\ s a -> s{_cvcrqCustomerGatewayId = a});

-- | The ID of the virtual private gateway.
cvcrqVPNGatewayId :: Lens' CreateVPNConnection Text
cvcrqVPNGatewayId = lens _cvcrqVPNGatewayId (\ s a -> s{_cvcrqVPNGatewayId = a});

instance AWSRequest CreateVPNConnection where
        type Sv CreateVPNConnection = EC2
        type Rs CreateVPNConnection =
             CreateVPNConnectionResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateVPNConnectionResponse' <$>
                   (x .@? "vpnConnection") <*> (pure (fromEnum s)))

instance ToHeaders CreateVPNConnection where
        toHeaders = const mempty

instance ToPath CreateVPNConnection where
        toPath = const "/"

instance ToQuery CreateVPNConnection where
        toQuery CreateVPNConnection'{..}
          = mconcat
              ["Action" =: ("CreateVPNConnection" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Options" =: _cvcrqOptions, "DryRun" =: _cvcrqDryRun,
               "Type" =: _cvcrqType,
               "CustomerGatewayId" =: _cvcrqCustomerGatewayId,
               "VpnGatewayId" =: _cvcrqVPNGatewayId]

-- | /See:/ 'createVPNConnectionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvcrsVPNConnection'
--
-- * 'cvcrsStatus'
data CreateVPNConnectionResponse = CreateVPNConnectionResponse'
    { _cvcrsVPNConnection :: !(Maybe VPNConnection)
    , _cvcrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPNConnectionResponse' smart constructor.
createVPNConnectionResponse :: Int -> CreateVPNConnectionResponse
createVPNConnectionResponse pStatus =
    CreateVPNConnectionResponse'
    { _cvcrsVPNConnection = Nothing
    , _cvcrsStatus = pStatus
    }

-- | Information about the VPN connection.
cvcrsVPNConnection :: Lens' CreateVPNConnectionResponse (Maybe VPNConnection)
cvcrsVPNConnection = lens _cvcrsVPNConnection (\ s a -> s{_cvcrsVPNConnection = a});

-- | FIXME: Undocumented member.
cvcrsStatus :: Lens' CreateVPNConnectionResponse Int
cvcrsStatus = lens _cvcrsStatus (\ s a -> s{_cvcrsStatus = a});
