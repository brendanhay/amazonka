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
    , cvcOptions
    , cvcDryRun
    , cvcType
    , cvcCustomerGatewayId
    , cvcVPNGatewayId

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
-- * 'cvcOptions'
--
-- * 'cvcDryRun'
--
-- * 'cvcType'
--
-- * 'cvcCustomerGatewayId'
--
-- * 'cvcVPNGatewayId'
data CreateVPNConnection = CreateVPNConnection'
    { _cvcOptions           :: !(Maybe VPNConnectionOptionsSpecification)
    , _cvcDryRun            :: !(Maybe Bool)
    , _cvcType              :: !Text
    , _cvcCustomerGatewayId :: !Text
    , _cvcVPNGatewayId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPNConnection' smart constructor.
createVPNConnection :: Text -> Text -> Text -> CreateVPNConnection
createVPNConnection pType_ pCustomerGatewayId_ pVPNGatewayId_ =
    CreateVPNConnection'
    { _cvcOptions = Nothing
    , _cvcDryRun = Nothing
    , _cvcType = pType_
    , _cvcCustomerGatewayId = pCustomerGatewayId_
    , _cvcVPNGatewayId = pVPNGatewayId_
    }

-- | Indicates whether the VPN connection requires static routes. If you are
-- creating a VPN connection for a device that does not support BGP, you
-- must specify @true@.
--
-- Default: @false@
cvcOptions :: Lens' CreateVPNConnection (Maybe VPNConnectionOptionsSpecification)
cvcOptions = lens _cvcOptions (\ s a -> s{_cvcOptions = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cvcDryRun :: Lens' CreateVPNConnection (Maybe Bool)
cvcDryRun = lens _cvcDryRun (\ s a -> s{_cvcDryRun = a});

-- | The type of VPN connection (@ipsec.1@).
cvcType :: Lens' CreateVPNConnection Text
cvcType = lens _cvcType (\ s a -> s{_cvcType = a});

-- | The ID of the customer gateway.
cvcCustomerGatewayId :: Lens' CreateVPNConnection Text
cvcCustomerGatewayId = lens _cvcCustomerGatewayId (\ s a -> s{_cvcCustomerGatewayId = a});

-- | The ID of the virtual private gateway.
cvcVPNGatewayId :: Lens' CreateVPNConnection Text
cvcVPNGatewayId = lens _cvcVPNGatewayId (\ s a -> s{_cvcVPNGatewayId = a});

instance AWSRequest CreateVPNConnection where
        type Sv CreateVPNConnection = EC2
        type Rs CreateVPNConnection =
             CreateVPNConnectionResponse
        request = post "CreateVPNConnection"
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
               "Options" =: _cvcOptions, "DryRun" =: _cvcDryRun,
               "Type" =: _cvcType,
               "CustomerGatewayId" =: _cvcCustomerGatewayId,
               "VpnGatewayId" =: _cvcVPNGatewayId]

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
createVPNConnectionResponse pStatus_ =
    CreateVPNConnectionResponse'
    { _cvcrsVPNConnection = Nothing
    , _cvcrsStatus = pStatus_
    }

-- | Information about the VPN connection.
cvcrsVPNConnection :: Lens' CreateVPNConnectionResponse (Maybe VPNConnection)
cvcrsVPNConnection = lens _cvcrsVPNConnection (\ s a -> s{_cvcrsVPNConnection = a});

-- | FIXME: Undocumented member.
cvcrsStatus :: Lens' CreateVPNConnectionResponse Int
cvcrsStatus = lens _cvcrsStatus (\ s a -> s{_cvcrsStatus = a});
