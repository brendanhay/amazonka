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
-- Module      : Network.AWS.EC2.CreateCustomerGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information to AWS about your VPN customer gateway device. The customer gateway is the appliance at your end of the VPN connection. (The device on the AWS side of the VPN connection is the virtual private gateway.) You must provide the Internet-routable IP address of the customer gateway's external interface. The IP address must be static and may be behind a device performing network address translation (NAT).
--
--
-- For devices that use Border Gateway Protocol (BGP), you can also provide the device's BGP Autonomous System Number (ASN). You can use an existing ASN assigned to your network. If you don't have an ASN already, you can use a private ASN (in the 64512 - 65534 range).
--
-- For more information about VPN customer gateways, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html AWS Managed VPN Connections> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- /Important:/ You cannot create more than one customer gateway with the same VPN type, IP address, and BGP ASN parameter values. If you run an identical request more than one time, the first request creates the customer gateway, and subsequent requests return information about the existing customer gateway. The subsequent requests do not create new customer gateway resources.
--
module Network.AWS.EC2.CreateCustomerGateway
    (
    -- * Creating a Request
      createCustomerGateway
    , CreateCustomerGateway
    -- * Request Lenses
    , ccgDryRun
    , ccgBGPASN
    , ccgPublicIP
    , ccgType

    -- * Destructuring the Response
    , createCustomerGatewayResponse
    , CreateCustomerGatewayResponse
    -- * Response Lenses
    , ccgrsCustomerGateway
    , ccgrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateCustomerGateway.
--
--
--
-- /See:/ 'createCustomerGateway' smart constructor.
data CreateCustomerGateway = CreateCustomerGateway'
  { _ccgDryRun   :: !(Maybe Bool)
  , _ccgBGPASN   :: !Int
  , _ccgPublicIP :: !Text
  , _ccgType     :: !GatewayType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCustomerGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccgDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ccgBGPASN' - For devices that support BGP, the customer gateway's BGP ASN. Default: 65000
--
-- * 'ccgPublicIP' - The Internet-routable IP address for the customer gateway's outside interface. The address must be static.
--
-- * 'ccgType' - The type of VPN connection that this customer gateway supports (@ipsec.1@ ).
createCustomerGateway
    :: Int -- ^ 'ccgBGPASN'
    -> Text -- ^ 'ccgPublicIP'
    -> GatewayType -- ^ 'ccgType'
    -> CreateCustomerGateway
createCustomerGateway pBGPASN_ pPublicIP_ pType_ =
  CreateCustomerGateway'
    { _ccgDryRun = Nothing
    , _ccgBGPASN = pBGPASN_
    , _ccgPublicIP = pPublicIP_
    , _ccgType = pType_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ccgDryRun :: Lens' CreateCustomerGateway (Maybe Bool)
ccgDryRun = lens _ccgDryRun (\ s a -> s{_ccgDryRun = a})

-- | For devices that support BGP, the customer gateway's BGP ASN. Default: 65000
ccgBGPASN :: Lens' CreateCustomerGateway Int
ccgBGPASN = lens _ccgBGPASN (\ s a -> s{_ccgBGPASN = a})

-- | The Internet-routable IP address for the customer gateway's outside interface. The address must be static.
ccgPublicIP :: Lens' CreateCustomerGateway Text
ccgPublicIP = lens _ccgPublicIP (\ s a -> s{_ccgPublicIP = a})

-- | The type of VPN connection that this customer gateway supports (@ipsec.1@ ).
ccgType :: Lens' CreateCustomerGateway GatewayType
ccgType = lens _ccgType (\ s a -> s{_ccgType = a})

instance AWSRequest CreateCustomerGateway where
        type Rs CreateCustomerGateway =
             CreateCustomerGatewayResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateCustomerGatewayResponse' <$>
                   (x .@? "customerGateway") <*> (pure (fromEnum s)))

instance Hashable CreateCustomerGateway where

instance NFData CreateCustomerGateway where

instance ToHeaders CreateCustomerGateway where
        toHeaders = const mempty

instance ToPath CreateCustomerGateway where
        toPath = const "/"

instance ToQuery CreateCustomerGateway where
        toQuery CreateCustomerGateway'{..}
          = mconcat
              ["Action" =: ("CreateCustomerGateway" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _ccgDryRun, "BgpAsn" =: _ccgBGPASN,
               "IpAddress" =: _ccgPublicIP, "Type" =: _ccgType]

-- | Contains the output of CreateCustomerGateway.
--
--
--
-- /See:/ 'createCustomerGatewayResponse' smart constructor.
data CreateCustomerGatewayResponse = CreateCustomerGatewayResponse'
  { _ccgrsCustomerGateway :: !(Maybe CustomerGateway)
  , _ccgrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCustomerGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccgrsCustomerGateway' - Information about the customer gateway.
--
-- * 'ccgrsResponseStatus' - -- | The response status code.
createCustomerGatewayResponse
    :: Int -- ^ 'ccgrsResponseStatus'
    -> CreateCustomerGatewayResponse
createCustomerGatewayResponse pResponseStatus_ =
  CreateCustomerGatewayResponse'
    {_ccgrsCustomerGateway = Nothing, _ccgrsResponseStatus = pResponseStatus_}


-- | Information about the customer gateway.
ccgrsCustomerGateway :: Lens' CreateCustomerGatewayResponse (Maybe CustomerGateway)
ccgrsCustomerGateway = lens _ccgrsCustomerGateway (\ s a -> s{_ccgrsCustomerGateway = a})

-- | -- | The response status code.
ccgrsResponseStatus :: Lens' CreateCustomerGatewayResponse Int
ccgrsResponseStatus = lens _ccgrsResponseStatus (\ s a -> s{_ccgrsResponseStatus = a})

instance NFData CreateCustomerGatewayResponse where
