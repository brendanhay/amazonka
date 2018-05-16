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
-- Module      : Network.AWS.EC2.CreateVPCEndpointServiceConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC endpoint service configuration to which service consumers (AWS accounts, IAM users, and IAM roles) can connect. Service consumers can create an interface VPC endpoint to connect to your service.
--
--
-- To create an endpoint service configuration, you must first create a Network Load Balancer for your service. For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/endpoint-service.html VPC Endpoint Services> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.CreateVPCEndpointServiceConfiguration
    (
    -- * Creating a Request
      createVPCEndpointServiceConfiguration
    , CreateVPCEndpointServiceConfiguration
    -- * Request Lenses
    , cvescClientToken
    , cvescAcceptanceRequired
    , cvescDryRun
    , cvescNetworkLoadBalancerARNs

    -- * Destructuring the Response
    , createVPCEndpointServiceConfigurationResponse
    , CreateVPCEndpointServiceConfigurationResponse
    -- * Response Lenses
    , cvescrsClientToken
    , cvescrsServiceConfiguration
    , cvescrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createVPCEndpointServiceConfiguration' smart constructor.
data CreateVPCEndpointServiceConfiguration = CreateVPCEndpointServiceConfiguration'
  { _cvescClientToken             :: !(Maybe Text)
  , _cvescAcceptanceRequired      :: !(Maybe Bool)
  , _cvescDryRun                  :: !(Maybe Bool)
  , _cvescNetworkLoadBalancerARNs :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPCEndpointServiceConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvescClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'cvescAcceptanceRequired' - Indicate whether requests from service consumers to create an endpoint to your service must be accepted. To accept a request, use 'AcceptVpcEndpointConnections' .
--
-- * 'cvescDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cvescNetworkLoadBalancerARNs' - The Amazon Resource Names (ARNs) of one or more Network Load Balancers for your service.
createVPCEndpointServiceConfiguration
    :: CreateVPCEndpointServiceConfiguration
createVPCEndpointServiceConfiguration =
  CreateVPCEndpointServiceConfiguration'
    { _cvescClientToken = Nothing
    , _cvescAcceptanceRequired = Nothing
    , _cvescDryRun = Nothing
    , _cvescNetworkLoadBalancerARNs = mempty
    }


-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
cvescClientToken :: Lens' CreateVPCEndpointServiceConfiguration (Maybe Text)
cvescClientToken = lens _cvescClientToken (\ s a -> s{_cvescClientToken = a})

-- | Indicate whether requests from service consumers to create an endpoint to your service must be accepted. To accept a request, use 'AcceptVpcEndpointConnections' .
cvescAcceptanceRequired :: Lens' CreateVPCEndpointServiceConfiguration (Maybe Bool)
cvescAcceptanceRequired = lens _cvescAcceptanceRequired (\ s a -> s{_cvescAcceptanceRequired = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cvescDryRun :: Lens' CreateVPCEndpointServiceConfiguration (Maybe Bool)
cvescDryRun = lens _cvescDryRun (\ s a -> s{_cvescDryRun = a})

-- | The Amazon Resource Names (ARNs) of one or more Network Load Balancers for your service.
cvescNetworkLoadBalancerARNs :: Lens' CreateVPCEndpointServiceConfiguration [Text]
cvescNetworkLoadBalancerARNs = lens _cvescNetworkLoadBalancerARNs (\ s a -> s{_cvescNetworkLoadBalancerARNs = a}) . _Coerce

instance AWSRequest
           CreateVPCEndpointServiceConfiguration
         where
        type Rs CreateVPCEndpointServiceConfiguration =
             CreateVPCEndpointServiceConfigurationResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateVPCEndpointServiceConfigurationResponse' <$>
                   (x .@? "clientToken") <*>
                     (x .@? "serviceConfiguration")
                     <*> (pure (fromEnum s)))

instance Hashable
           CreateVPCEndpointServiceConfiguration
         where

instance NFData CreateVPCEndpointServiceConfiguration
         where

instance ToHeaders
           CreateVPCEndpointServiceConfiguration
         where
        toHeaders = const mempty

instance ToPath CreateVPCEndpointServiceConfiguration
         where
        toPath = const "/"

instance ToQuery
           CreateVPCEndpointServiceConfiguration
         where
        toQuery CreateVPCEndpointServiceConfiguration'{..}
          = mconcat
              ["Action" =:
                 ("CreateVpcEndpointServiceConfiguration" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ClientToken" =: _cvescClientToken,
               "AcceptanceRequired" =: _cvescAcceptanceRequired,
               "DryRun" =: _cvescDryRun,
               toQueryList "NetworkLoadBalancerArn"
                 _cvescNetworkLoadBalancerARNs]

-- | /See:/ 'createVPCEndpointServiceConfigurationResponse' smart constructor.
data CreateVPCEndpointServiceConfigurationResponse = CreateVPCEndpointServiceConfigurationResponse'
  { _cvescrsClientToken          :: !(Maybe Text)
  , _cvescrsServiceConfiguration :: !(Maybe ServiceConfiguration)
  , _cvescrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPCEndpointServiceConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvescrsClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request.
--
-- * 'cvescrsServiceConfiguration' - Information about the service configuration.
--
-- * 'cvescrsResponseStatus' - -- | The response status code.
createVPCEndpointServiceConfigurationResponse
    :: Int -- ^ 'cvescrsResponseStatus'
    -> CreateVPCEndpointServiceConfigurationResponse
createVPCEndpointServiceConfigurationResponse pResponseStatus_ =
  CreateVPCEndpointServiceConfigurationResponse'
    { _cvescrsClientToken = Nothing
    , _cvescrsServiceConfiguration = Nothing
    , _cvescrsResponseStatus = pResponseStatus_
    }


-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request.
cvescrsClientToken :: Lens' CreateVPCEndpointServiceConfigurationResponse (Maybe Text)
cvescrsClientToken = lens _cvescrsClientToken (\ s a -> s{_cvescrsClientToken = a})

-- | Information about the service configuration.
cvescrsServiceConfiguration :: Lens' CreateVPCEndpointServiceConfigurationResponse (Maybe ServiceConfiguration)
cvescrsServiceConfiguration = lens _cvescrsServiceConfiguration (\ s a -> s{_cvescrsServiceConfiguration = a})

-- | -- | The response status code.
cvescrsResponseStatus :: Lens' CreateVPCEndpointServiceConfigurationResponse Int
cvescrsResponseStatus = lens _cvescrsResponseStatus (\ s a -> s{_cvescrsResponseStatus = a})

instance NFData
           CreateVPCEndpointServiceConfigurationResponse
         where
