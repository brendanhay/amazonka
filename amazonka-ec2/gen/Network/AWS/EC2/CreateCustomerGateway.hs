{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateCustomerGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Provides information to AWS about your VPN customer gateway device. The
-- customer gateway is the appliance at your end of the VPN connection.
-- (The device on the AWS side of the VPN connection is the virtual private
-- gateway.) You must provide the Internet-routable IP address of the
-- customer gateway\'s external interface. The IP address must be static
-- and can\'t be behind a device performing network address translation
-- (NAT).
--
-- For devices that use Border Gateway Protocol (BGP), you can also provide
-- the device\'s BGP Autonomous System Number (ASN). You can use an
-- existing ASN assigned to your network. If you don\'t have an ASN
-- already, you can use a private ASN (in the 64512 - 65534 range).
--
-- Amazon EC2 supports all 2-byte ASN numbers in the range of 1 - 65534,
-- with the exception of 7224, which is reserved in the @us-east-1@ region,
-- and 9059, which is reserved in the @eu-west-1@ region.
--
-- For more information about VPN customer gateways, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding a Hardware Virtual Private Gateway to Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- You cannot create more than one customer gateway with the same VPN type,
-- IP address, and BGP ASN parameter values. If you run an identical
-- request more than one time, the first request creates the customer
-- gateway, and subsequent requests return information about the existing
-- customer gateway. The subsequent requests do not create new customer
-- gateway resources.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateCustomerGateway.html>
module Network.AWS.EC2.CreateCustomerGateway
    (
    -- * Request
      CreateCustomerGateway
    -- ** Request constructor
    , createCustomerGateway
    -- ** Request lenses
    , ccgDryRun
    , ccgType
    , ccgPublicIP
    , ccgBGPASN

    -- * Response
    , CreateCustomerGatewayResponse
    -- ** Response constructor
    , createCustomerGatewayResponse
    -- ** Response lenses
    , ccgrsCustomerGateway
    , ccgrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createCustomerGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccgDryRun'
--
-- * 'ccgType'
--
-- * 'ccgPublicIP'
--
-- * 'ccgBGPASN'
data CreateCustomerGateway = CreateCustomerGateway'
    { _ccgDryRun   :: !(Maybe Bool)
    , _ccgType     :: !GatewayType
    , _ccgPublicIP :: !Text
    , _ccgBGPASN   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCustomerGateway' smart constructor.
createCustomerGateway :: GatewayType -> Text -> Int -> CreateCustomerGateway
createCustomerGateway pType_ pPublicIP_ pBGPASN_ =
    CreateCustomerGateway'
    { _ccgDryRun = Nothing
    , _ccgType = pType_
    , _ccgPublicIP = pPublicIP_
    , _ccgBGPASN = pBGPASN_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ccgDryRun :: Lens' CreateCustomerGateway (Maybe Bool)
ccgDryRun = lens _ccgDryRun (\ s a -> s{_ccgDryRun = a});

-- | The type of VPN connection that this customer gateway supports
-- (@ipsec.1@).
ccgType :: Lens' CreateCustomerGateway GatewayType
ccgType = lens _ccgType (\ s a -> s{_ccgType = a});

-- | The Internet-routable IP address for the customer gateway\'s outside
-- interface. The address must be static.
ccgPublicIP :: Lens' CreateCustomerGateway Text
ccgPublicIP = lens _ccgPublicIP (\ s a -> s{_ccgPublicIP = a});

-- | For devices that support BGP, the customer gateway\'s BGP ASN.
--
-- Default: 65000
ccgBGPASN :: Lens' CreateCustomerGateway Int
ccgBGPASN = lens _ccgBGPASN (\ s a -> s{_ccgBGPASN = a});

instance AWSRequest CreateCustomerGateway where
        type Sv CreateCustomerGateway = EC2
        type Rs CreateCustomerGateway =
             CreateCustomerGatewayResponse
        request = post "CreateCustomerGateway"
        response
          = receiveXML
              (\ s h x ->
                 CreateCustomerGatewayResponse' <$>
                   (x .@? "customerGateway") <*> (pure (fromEnum s)))

instance ToHeaders CreateCustomerGateway where
        toHeaders = const mempty

instance ToPath CreateCustomerGateway where
        toPath = const "/"

instance ToQuery CreateCustomerGateway where
        toQuery CreateCustomerGateway'{..}
          = mconcat
              ["Action" =: ("CreateCustomerGateway" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _ccgDryRun, "Type" =: _ccgType,
               "IpAddress" =: _ccgPublicIP, "BgpAsn" =: _ccgBGPASN]

-- | /See:/ 'createCustomerGatewayResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccgrsCustomerGateway'
--
-- * 'ccgrsStatus'
data CreateCustomerGatewayResponse = CreateCustomerGatewayResponse'
    { _ccgrsCustomerGateway :: !(Maybe CustomerGateway)
    , _ccgrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCustomerGatewayResponse' smart constructor.
createCustomerGatewayResponse :: Int -> CreateCustomerGatewayResponse
createCustomerGatewayResponse pStatus_ =
    CreateCustomerGatewayResponse'
    { _ccgrsCustomerGateway = Nothing
    , _ccgrsStatus = pStatus_
    }

-- | Information about the customer gateway.
ccgrsCustomerGateway :: Lens' CreateCustomerGatewayResponse (Maybe CustomerGateway)
ccgrsCustomerGateway = lens _ccgrsCustomerGateway (\ s a -> s{_ccgrsCustomerGateway = a});

-- | FIXME: Undocumented member.
ccgrsStatus :: Lens' CreateCustomerGatewayResponse Int
ccgrsStatus = lens _ccgrsStatus (\ s a -> s{_ccgrsStatus = a});
