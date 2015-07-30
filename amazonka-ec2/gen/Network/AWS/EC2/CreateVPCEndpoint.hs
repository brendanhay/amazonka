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
    , cvePolicyDocument
    , cveClientToken
    , cveDryRun
    , cveRouteTableIds
    , cveVPCId
    , cveServiceName

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
-- * 'cvePolicyDocument'
--
-- * 'cveClientToken'
--
-- * 'cveDryRun'
--
-- * 'cveRouteTableIds'
--
-- * 'cveVPCId'
--
-- * 'cveServiceName'
data CreateVPCEndpoint = CreateVPCEndpoint'
    { _cvePolicyDocument :: !(Maybe Text)
    , _cveClientToken    :: !(Maybe Text)
    , _cveDryRun         :: !(Maybe Bool)
    , _cveRouteTableIds  :: !(Maybe [Text])
    , _cveVPCId          :: !Text
    , _cveServiceName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVPCEndpoint' smart constructor.
createVPCEndpoint :: Text -> Text -> CreateVPCEndpoint
createVPCEndpoint pVPCId_ pServiceName_ =
    CreateVPCEndpoint'
    { _cvePolicyDocument = Nothing
    , _cveClientToken = Nothing
    , _cveDryRun = Nothing
    , _cveRouteTableIds = Nothing
    , _cveVPCId = pVPCId_
    , _cveServiceName = pServiceName_
    }

-- | A policy to attach to the endpoint that controls access to the service.
-- The policy must be in valid JSON format. If this parameter is not
-- specified, we attach a default policy that allows full access to the
-- service.
cvePolicyDocument :: Lens' CreateVPCEndpoint (Maybe Text)
cvePolicyDocument = lens _cvePolicyDocument (\ s a -> s{_cvePolicyDocument = a});

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
cveClientToken :: Lens' CreateVPCEndpoint (Maybe Text)
cveClientToken = lens _cveClientToken (\ s a -> s{_cveClientToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cveDryRun :: Lens' CreateVPCEndpoint (Maybe Bool)
cveDryRun = lens _cveDryRun (\ s a -> s{_cveDryRun = a});

-- | One or more route table IDs.
cveRouteTableIds :: Lens' CreateVPCEndpoint [Text]
cveRouteTableIds = lens _cveRouteTableIds (\ s a -> s{_cveRouteTableIds = a}) . _Default . _Coerce;

-- | The ID of the VPC in which the endpoint will be used.
cveVPCId :: Lens' CreateVPCEndpoint Text
cveVPCId = lens _cveVPCId (\ s a -> s{_cveVPCId = a});

-- | The AWS service name, in the form @com.amazonaws.region.service@. To get
-- a list of available services, use the DescribeVpcEndpointServices
-- request.
cveServiceName :: Lens' CreateVPCEndpoint Text
cveServiceName = lens _cveServiceName (\ s a -> s{_cveServiceName = a});

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
               "PolicyDocument" =: _cvePolicyDocument,
               "ClientToken" =: _cveClientToken,
               "DryRun" =: _cveDryRun,
               toQuery (toQueryList "item" <$> _cveRouteTableIds),
               "VpcId" =: _cveVPCId,
               "ServiceName" =: _cveServiceName]

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
createVPCEndpointResponse pStatus_ =
    CreateVPCEndpointResponse'
    { _cversClientToken = Nothing
    , _cversVPCEndpoint = Nothing
    , _cversStatus = pStatus_
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
