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
-- Module      : Network.AWS.EC2.CreateVPCEndpoint
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC endpoint for a specified AWS service. An endpoint enables you to create a private connection between your VPC and another AWS service in your account. You can specify an endpoint policy to attach to the endpoint that will control access to the service from your VPC. You can also specify the VPC route tables that use the endpoint.
--
--
-- Use 'DescribeVpcEndpointServices' to get a list of supported AWS services.
--
module Network.AWS.EC2.CreateVPCEndpoint
    (
    -- * Creating a Request
      createVPCEndpoint
    , CreateVPCEndpoint
    -- * Request Lenses
    , cvePolicyDocument
    , cveClientToken
    , cveDryRun
    , cveRouteTableIds
    , cveServiceName
    , cveVPCId

    -- * Destructuring the Response
    , createVPCEndpointResponse
    , CreateVPCEndpointResponse
    -- * Response Lenses
    , cversClientToken
    , cversVPCEndpoint
    , cversResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for CreateVpcEndpoint.
--
--
--
-- /See:/ 'createVPCEndpoint' smart constructor.
data CreateVPCEndpoint = CreateVPCEndpoint'
    { _cvePolicyDocument :: !(Maybe Text)
    , _cveClientToken    :: !(Maybe Text)
    , _cveDryRun         :: !(Maybe Bool)
    , _cveRouteTableIds  :: !(Maybe [Text])
    , _cveServiceName    :: !Text
    , _cveVPCId          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateVPCEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvePolicyDocument' - A policy to attach to the endpoint that controls access to the service. The policy must be in valid JSON format. If this parameter is not specified, we attach a default policy that allows full access to the service.
--
-- * 'cveClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'cveDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cveRouteTableIds' - One or more route table IDs.
--
-- * 'cveServiceName' - The AWS service name, in the form @com.amazonaws./region/ ./service/ @ . To get a list of available services, use the 'DescribeVpcEndpointServices' request.
--
-- * 'cveVPCId' - The ID of the VPC in which the endpoint will be used.
createVPCEndpoint
    :: Text -- ^ 'cveServiceName'
    -> Text -- ^ 'cveVPCId'
    -> CreateVPCEndpoint
createVPCEndpoint pServiceName_ pVPCId_ =
    CreateVPCEndpoint'
    { _cvePolicyDocument = Nothing
    , _cveClientToken = Nothing
    , _cveDryRun = Nothing
    , _cveRouteTableIds = Nothing
    , _cveServiceName = pServiceName_
    , _cveVPCId = pVPCId_
    }

-- | A policy to attach to the endpoint that controls access to the service. The policy must be in valid JSON format. If this parameter is not specified, we attach a default policy that allows full access to the service.
cvePolicyDocument :: Lens' CreateVPCEndpoint (Maybe Text)
cvePolicyDocument = lens _cvePolicyDocument (\ s a -> s{_cvePolicyDocument = a});

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
cveClientToken :: Lens' CreateVPCEndpoint (Maybe Text)
cveClientToken = lens _cveClientToken (\ s a -> s{_cveClientToken = a});

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cveDryRun :: Lens' CreateVPCEndpoint (Maybe Bool)
cveDryRun = lens _cveDryRun (\ s a -> s{_cveDryRun = a});

-- | One or more route table IDs.
cveRouteTableIds :: Lens' CreateVPCEndpoint [Text]
cveRouteTableIds = lens _cveRouteTableIds (\ s a -> s{_cveRouteTableIds = a}) . _Default . _Coerce;

-- | The AWS service name, in the form @com.amazonaws./region/ ./service/ @ . To get a list of available services, use the 'DescribeVpcEndpointServices' request.
cveServiceName :: Lens' CreateVPCEndpoint Text
cveServiceName = lens _cveServiceName (\ s a -> s{_cveServiceName = a});

-- | The ID of the VPC in which the endpoint will be used.
cveVPCId :: Lens' CreateVPCEndpoint Text
cveVPCId = lens _cveVPCId (\ s a -> s{_cveVPCId = a});

instance AWSRequest CreateVPCEndpoint where
        type Rs CreateVPCEndpoint = CreateVPCEndpointResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateVPCEndpointResponse' <$>
                   (x .@? "clientToken") <*> (x .@? "vpcEndpoint") <*>
                     (pure (fromEnum s)))

instance Hashable CreateVPCEndpoint

instance NFData CreateVPCEndpoint

instance ToHeaders CreateVPCEndpoint where
        toHeaders = const mempty

instance ToPath CreateVPCEndpoint where
        toPath = const "/"

instance ToQuery CreateVPCEndpoint where
        toQuery CreateVPCEndpoint'{..}
          = mconcat
              ["Action" =: ("CreateVpcEndpoint" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "PolicyDocument" =: _cvePolicyDocument,
               "ClientToken" =: _cveClientToken,
               "DryRun" =: _cveDryRun,
               toQuery
                 (toQueryList "RouteTableId" <$> _cveRouteTableIds),
               "ServiceName" =: _cveServiceName,
               "VpcId" =: _cveVPCId]

-- | Contains the output of CreateVpcEndpoint.
--
--
--
-- /See:/ 'createVPCEndpointResponse' smart constructor.
data CreateVPCEndpointResponse = CreateVPCEndpointResponse'
    { _cversClientToken    :: !(Maybe Text)
    , _cversVPCEndpoint    :: !(Maybe VPCEndpoint)
    , _cversResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateVPCEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cversClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request.
--
-- * 'cversVPCEndpoint' - Information about the endpoint.
--
-- * 'cversResponseStatus' - -- | The response status code.
createVPCEndpointResponse
    :: Int -- ^ 'cversResponseStatus'
    -> CreateVPCEndpointResponse
createVPCEndpointResponse pResponseStatus_ =
    CreateVPCEndpointResponse'
    { _cversClientToken = Nothing
    , _cversVPCEndpoint = Nothing
    , _cversResponseStatus = pResponseStatus_
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request.
cversClientToken :: Lens' CreateVPCEndpointResponse (Maybe Text)
cversClientToken = lens _cversClientToken (\ s a -> s{_cversClientToken = a});

-- | Information about the endpoint.
cversVPCEndpoint :: Lens' CreateVPCEndpointResponse (Maybe VPCEndpoint)
cversVPCEndpoint = lens _cversVPCEndpoint (\ s a -> s{_cversVPCEndpoint = a});

-- | -- | The response status code.
cversResponseStatus :: Lens' CreateVPCEndpointResponse Int
cversResponseStatus = lens _cversResponseStatus (\ s a -> s{_cversResponseStatus = a});

instance NFData CreateVPCEndpointResponse
