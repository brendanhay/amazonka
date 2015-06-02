{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateVpcEndpoint
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a VPC endpoint for a specified AWS service. An endpoint enables you
-- to create a private connection between your VPC and another AWS service in
-- your account. You can specify an endpoint policy to attach to the endpoint
-- that will control access to the service from your VPC. You can also specify
-- the VPC route tables that use the endpoint.
--
-- Currently, only endpoints to Amazon S3 are supported.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpcEndpoint.html>
module Network.AWS.EC2.CreateVpcEndpoint
    (
    -- * Request
      CreateVpcEndpoint
    -- ** Request constructor
    , createVpcEndpoint
    -- ** Request lenses
    , cveClientToken
    , cveDryRun
    , cvePolicyDocument
    , cveRouteTableIds
    , cveServiceName
    , cveVpcId

    -- * Response
    , CreateVpcEndpointResponse
    -- ** Response constructor
    , createVpcEndpointResponse
    -- ** Response lenses
    , cverClientToken
    , cverVpcEndpoint
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateVpcEndpoint = CreateVpcEndpoint
    { _cveClientToken    :: Maybe Text
    , _cveDryRun         :: Maybe Bool
    , _cvePolicyDocument :: Maybe Text
    , _cveRouteTableIds  :: List "item" Text
    , _cveServiceName    :: Text
    , _cveVpcId          :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CreateVpcEndpoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cveClientToken' @::@ 'Maybe' 'Text'
--
-- * 'cveDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cvePolicyDocument' @::@ 'Maybe' 'Text'
--
-- * 'cveRouteTableIds' @::@ ['Text']
--
-- * 'cveServiceName' @::@ 'Text'
--
-- * 'cveVpcId' @::@ 'Text'
--
createVpcEndpoint :: Text -- ^ 'cveVpcId'
                  -> Text -- ^ 'cveServiceName'
                  -> CreateVpcEndpoint
createVpcEndpoint p1 p2 = CreateVpcEndpoint
    { _cveVpcId          = p1
    , _cveServiceName    = p2
    , _cveDryRun         = Nothing
    , _cvePolicyDocument = Nothing
    , _cveRouteTableIds  = mempty
    , _cveClientToken    = Nothing
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of
-- the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
cveClientToken :: Lens' CreateVpcEndpoint (Maybe Text)
cveClientToken = lens _cveClientToken (\s a -> s { _cveClientToken = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
cveDryRun :: Lens' CreateVpcEndpoint (Maybe Bool)
cveDryRun = lens _cveDryRun (\s a -> s { _cveDryRun = a })

-- | A policy to attach to the endpoint that controls access to the service. The
-- policy must be in valid JSON format. If this parameter is not specified, we
-- attach a default policy that allows full access to the service.
cvePolicyDocument :: Lens' CreateVpcEndpoint (Maybe Text)
cvePolicyDocument =
    lens _cvePolicyDocument (\s a -> s { _cvePolicyDocument = a })

-- | One or more route table IDs.
cveRouteTableIds :: Lens' CreateVpcEndpoint [Text]
cveRouteTableIds = lens _cveRouteTableIds (\s a -> s { _cveRouteTableIds = a }) . _List

-- | The AWS service name, in the form com.amazonaws.<region>.<service>. To get a
-- list of available services, use the DescribeVpcEndpointServices request.
cveServiceName :: Lens' CreateVpcEndpoint Text
cveServiceName = lens _cveServiceName (\s a -> s { _cveServiceName = a })

-- | The ID of the VPC in which the endpoint will be used.
cveVpcId :: Lens' CreateVpcEndpoint Text
cveVpcId = lens _cveVpcId (\s a -> s { _cveVpcId = a })

data CreateVpcEndpointResponse = CreateVpcEndpointResponse
    { _cverClientToken :: Maybe Text
    , _cverVpcEndpoint :: Maybe VpcEndpoint
    } deriving (Eq, Read, Show)

-- | 'CreateVpcEndpointResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cverClientToken' @::@ 'Maybe' 'Text'
--
-- * 'cverVpcEndpoint' @::@ 'Maybe' 'VpcEndpoint'
--
createVpcEndpointResponse :: CreateVpcEndpointResponse
createVpcEndpointResponse = CreateVpcEndpointResponse
    { _cverVpcEndpoint = Nothing
    , _cverClientToken = Nothing
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of
-- the request.
cverClientToken :: Lens' CreateVpcEndpointResponse (Maybe Text)
cverClientToken = lens _cverClientToken (\s a -> s { _cverClientToken = a })

-- | Information about the endpoint.
cverVpcEndpoint :: Lens' CreateVpcEndpointResponse (Maybe VpcEndpoint)
cverVpcEndpoint = lens _cverVpcEndpoint (\s a -> s { _cverVpcEndpoint = a })

instance ToPath CreateVpcEndpoint where
    toPath = const "/"

instance ToQuery CreateVpcEndpoint where
    toQuery CreateVpcEndpoint{..} = mconcat
        [ "ClientToken"    =? _cveClientToken
        , "DryRun"         =? _cveDryRun
        , "PolicyDocument" =? _cvePolicyDocument
        , "RouteTableId"   `toQueryList` _cveRouteTableIds
        , "ServiceName"    =? _cveServiceName
        , "VpcId"          =? _cveVpcId
        ]

instance ToHeaders CreateVpcEndpoint

instance AWSRequest CreateVpcEndpoint where
    type Sv CreateVpcEndpoint = EC2
    type Rs CreateVpcEndpoint = CreateVpcEndpointResponse

    request  = post "CreateVpcEndpoint"
    response = xmlResponse

instance FromXML CreateVpcEndpointResponse where
    parseXML x = CreateVpcEndpointResponse
        <$> x .@? "clientToken"
        <*> x .@? "vpcEndpoint"
