{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVPCEndpoint
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC endpoint for a specified AWS service. An endpoint enables
-- you to create a private connection between your VPC and another AWS
-- service in your account. You can specify an endpoint policy to attach to
-- the endpoint that will control access to the service from your VPC. You
-- can also specify the VPC route tables that use the endpoint.
--
-- Currently, only endpoints to Amazon S3 are supported.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVPCEndpoint.html>
module Network.AWS.EC2.CreateVPCEndpoint
    (
    -- * Request
      CreateVPCEndpoint
    -- ** Request constructor
    , createVPCEndpoint
    -- ** Request lenses
    , cverqPolicyDocument
    , cverqClientToken
    , cverqDryRun
    , cverqRouteTableIds
    , cverqVPCId
    , cverqServiceName

    -- * Response
    , CreateVPCEndpointResponse
    -- ** Response constructor
    , createVPCEndpointResponse
    -- ** Response lenses
    , cversClientToken
    , cversVPCEndpoint
    , cversStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createVPCEndpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cverqPolicyDocument'
--
-- * 'cverqClientToken'
--
-- * 'cverqDryRun'
--
-- * 'cverqRouteTableIds'
--
-- * 'cverqVPCId'
--
-- * 'cverqServiceName'
data CreateVPCEndpoint = CreateVPCEndpoint'
    { _cverqPolicyDocument :: !(Maybe Text)
    , _cverqClientToken    :: !(Maybe Text)
    , _cverqDryRun         :: !(Maybe Bool)
    , _cverqRouteTableIds  :: !(Maybe [Text])
    , _cverqVPCId          :: !Text
    , _cverqServiceName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPCEndpoint' smart constructor.
createVPCEndpoint :: Text -> Text -> CreateVPCEndpoint
createVPCEndpoint pVPCId pServiceName =
    CreateVPCEndpoint'
    { _cverqPolicyDocument = Nothing
    , _cverqClientToken = Nothing
    , _cverqDryRun = Nothing
    , _cverqRouteTableIds = Nothing
    , _cverqVPCId = pVPCId
    , _cverqServiceName = pServiceName
    }

-- | A policy to attach to the endpoint that controls access to the service.
-- The policy must be in valid JSON format. If this parameter is not
-- specified, we attach a default policy that allows full access to the
-- service.
cverqPolicyDocument :: Lens' CreateVPCEndpoint (Maybe Text)
cverqPolicyDocument = lens _cverqPolicyDocument (\ s a -> s{_cverqPolicyDocument = a});

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
cverqClientToken :: Lens' CreateVPCEndpoint (Maybe Text)
cverqClientToken = lens _cverqClientToken (\ s a -> s{_cverqClientToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cverqDryRun :: Lens' CreateVPCEndpoint (Maybe Bool)
cverqDryRun = lens _cverqDryRun (\ s a -> s{_cverqDryRun = a});

-- | One or more route table IDs.
cverqRouteTableIds :: Lens' CreateVPCEndpoint [Text]
cverqRouteTableIds = lens _cverqRouteTableIds (\ s a -> s{_cverqRouteTableIds = a}) . _Default;

-- | The ID of the VPC in which the endpoint will be used.
cverqVPCId :: Lens' CreateVPCEndpoint Text
cverqVPCId = lens _cverqVPCId (\ s a -> s{_cverqVPCId = a});

-- | The AWS service name, in the form com.amazonaws.\<region>.\<service>. To
-- get a list of available services, use the DescribeVpcEndpointServices
-- request.
cverqServiceName :: Lens' CreateVPCEndpoint Text
cverqServiceName = lens _cverqServiceName (\ s a -> s{_cverqServiceName = a});

instance AWSRequest CreateVPCEndpoint where
        type Sv CreateVPCEndpoint = EC2
        type Rs CreateVPCEndpoint = CreateVPCEndpointResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateVPCEndpointResponse' <$>
                   (x .@? "clientToken") <*> (x .@? "vpcEndpoint") <*>
                     (pure (fromEnum s)))

instance ToHeaders CreateVPCEndpoint where
        toHeaders = const mempty

instance ToPath CreateVPCEndpoint where
        toPath = const "/"

instance ToQuery CreateVPCEndpoint where
        toQuery CreateVPCEndpoint'{..}
          = mconcat
              ["Action" =: ("CreateVPCEndpoint" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "PolicyDocument" =: _cverqPolicyDocument,
               "ClientToken" =: _cverqClientToken,
               "DryRun" =: _cverqDryRun,
               toQuery (toQueryList "item" <$> _cverqRouteTableIds),
               "VpcId" =: _cverqVPCId,
               "ServiceName" =: _cverqServiceName]

-- | /See:/ 'createVPCEndpointResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cversClientToken'
--
-- * 'cversVPCEndpoint'
--
-- * 'cversStatus'
data CreateVPCEndpointResponse = CreateVPCEndpointResponse'
    { _cversClientToken :: !(Maybe Text)
    , _cversVPCEndpoint :: !(Maybe VPCEndpoint)
    , _cversStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPCEndpointResponse' smart constructor.
createVPCEndpointResponse :: Int -> CreateVPCEndpointResponse
createVPCEndpointResponse pStatus =
    CreateVPCEndpointResponse'
    { _cversClientToken = Nothing
    , _cversVPCEndpoint = Nothing
    , _cversStatus = pStatus
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request.
cversClientToken :: Lens' CreateVPCEndpointResponse (Maybe Text)
cversClientToken = lens _cversClientToken (\ s a -> s{_cversClientToken = a});

-- | Information about the endpoint.
cversVPCEndpoint :: Lens' CreateVPCEndpointResponse (Maybe VPCEndpoint)
cversVPCEndpoint = lens _cversVPCEndpoint (\ s a -> s{_cversVPCEndpoint = a});

-- | FIXME: Undocumented member.
cversStatus :: Lens' CreateVPCEndpointResponse Int
cversStatus = lens _cversStatus (\ s a -> s{_cversStatus = a});
