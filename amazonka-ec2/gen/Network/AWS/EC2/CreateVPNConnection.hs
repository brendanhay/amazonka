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
-- Module      : Network.AWS.EC2.CreateVPNConnection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPN connection between an existing virtual private gateway and a VPN customer gateway. The only supported connection type is @ipsec.1@ .
--
--
-- The response includes information that you need to give to your network administrator to configure your customer gateway.
--
-- /Important:/ We strongly recommend that you use HTTPS when calling this operation because the response contains sensitive cryptographic information for configuring your customer gateway.
--
-- If you decide to shut down your VPN connection for any reason and later create a new VPN connection, you must reconfigure your customer gateway with the new information returned from this call.
--
-- This is an idempotent operation. If you perform the operation more than once, Amazon EC2 doesn't return an error.
--
-- For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html AWS Managed VPN Connections> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.CreateVPNConnection
    (
    -- * Creating a Request
      createVPNConnection
    , CreateVPNConnection
    -- * Request Lenses
    , cvcOptions
    , cvcDryRun
    , cvcCustomerGatewayId
    , cvcType
    , cvcVPNGatewayId

    -- * Destructuring the Response
    , createVPNConnectionResponse
    , CreateVPNConnectionResponse
    -- * Response Lenses
    , cvcrsVPNConnection
    , cvcrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateVpnConnection.
--
--
--
-- /See:/ 'createVPNConnection' smart constructor.
data CreateVPNConnection = CreateVPNConnection'
  { _cvcOptions           :: !(Maybe VPNConnectionOptionsSpecification)
  , _cvcDryRun            :: !(Maybe Bool)
  , _cvcCustomerGatewayId :: !Text
  , _cvcType              :: !Text
  , _cvcVPNGatewayId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPNConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvcOptions' - The options for the VPN connection.
--
-- * 'cvcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cvcCustomerGatewayId' - The ID of the customer gateway.
--
-- * 'cvcType' - The type of VPN connection (@ipsec.1@ ).
--
-- * 'cvcVPNGatewayId' - The ID of the virtual private gateway.
createVPNConnection
    :: Text -- ^ 'cvcCustomerGatewayId'
    -> Text -- ^ 'cvcType'
    -> Text -- ^ 'cvcVPNGatewayId'
    -> CreateVPNConnection
createVPNConnection pCustomerGatewayId_ pType_ pVPNGatewayId_ =
  CreateVPNConnection'
    { _cvcOptions = Nothing
    , _cvcDryRun = Nothing
    , _cvcCustomerGatewayId = pCustomerGatewayId_
    , _cvcType = pType_
    , _cvcVPNGatewayId = pVPNGatewayId_
    }


-- | The options for the VPN connection.
cvcOptions :: Lens' CreateVPNConnection (Maybe VPNConnectionOptionsSpecification)
cvcOptions = lens _cvcOptions (\ s a -> s{_cvcOptions = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cvcDryRun :: Lens' CreateVPNConnection (Maybe Bool)
cvcDryRun = lens _cvcDryRun (\ s a -> s{_cvcDryRun = a})

-- | The ID of the customer gateway.
cvcCustomerGatewayId :: Lens' CreateVPNConnection Text
cvcCustomerGatewayId = lens _cvcCustomerGatewayId (\ s a -> s{_cvcCustomerGatewayId = a})

-- | The type of VPN connection (@ipsec.1@ ).
cvcType :: Lens' CreateVPNConnection Text
cvcType = lens _cvcType (\ s a -> s{_cvcType = a})

-- | The ID of the virtual private gateway.
cvcVPNGatewayId :: Lens' CreateVPNConnection Text
cvcVPNGatewayId = lens _cvcVPNGatewayId (\ s a -> s{_cvcVPNGatewayId = a})

instance AWSRequest CreateVPNConnection where
        type Rs CreateVPNConnection =
             CreateVPNConnectionResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateVPNConnectionResponse' <$>
                   (x .@? "vpnConnection") <*> (pure (fromEnum s)))

instance Hashable CreateVPNConnection where

instance NFData CreateVPNConnection where

instance ToHeaders CreateVPNConnection where
        toHeaders = const mempty

instance ToPath CreateVPNConnection where
        toPath = const "/"

instance ToQuery CreateVPNConnection where
        toQuery CreateVPNConnection'{..}
          = mconcat
              ["Action" =: ("CreateVpnConnection" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Options" =: _cvcOptions, "DryRun" =: _cvcDryRun,
               "CustomerGatewayId" =: _cvcCustomerGatewayId,
               "Type" =: _cvcType,
               "VpnGatewayId" =: _cvcVPNGatewayId]

-- | Contains the output of CreateVpnConnection.
--
--
--
-- /See:/ 'createVPNConnectionResponse' smart constructor.
data CreateVPNConnectionResponse = CreateVPNConnectionResponse'
  { _cvcrsVPNConnection  :: !(Maybe VPNConnection)
  , _cvcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPNConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvcrsVPNConnection' - Information about the VPN connection.
--
-- * 'cvcrsResponseStatus' - -- | The response status code.
createVPNConnectionResponse
    :: Int -- ^ 'cvcrsResponseStatus'
    -> CreateVPNConnectionResponse
createVPNConnectionResponse pResponseStatus_ =
  CreateVPNConnectionResponse'
    {_cvcrsVPNConnection = Nothing, _cvcrsResponseStatus = pResponseStatus_}


-- | Information about the VPN connection.
cvcrsVPNConnection :: Lens' CreateVPNConnectionResponse (Maybe VPNConnection)
cvcrsVPNConnection = lens _cvcrsVPNConnection (\ s a -> s{_cvcrsVPNConnection = a})

-- | -- | The response status code.
cvcrsResponseStatus :: Lens' CreateVPNConnectionResponse Int
cvcrsResponseStatus = lens _cvcrsResponseStatus (\ s a -> s{_cvcrsResponseStatus = a})

instance NFData CreateVPNConnectionResponse where
