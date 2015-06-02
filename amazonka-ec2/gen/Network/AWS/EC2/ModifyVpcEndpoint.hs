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

-- Module      : Network.AWS.EC2.ModifyVpcEndpoint
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

-- | Modifies attributes of a specified VPC endpoint. You can modify the policy
-- associated with the endpoint, and you can add and remove route tables
-- associated with the endpoint.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyVpcEndpoint.html>
module Network.AWS.EC2.ModifyVpcEndpoint
    (
    -- * Request
      ModifyVpcEndpoint
    -- ** Request constructor
    , modifyVpcEndpoint
    -- ** Request lenses
    , mveAddRouteTableIds
    , mveDryRun
    , mvePolicyDocument
    , mveRemoveRouteTableIds
    , mveResetPolicy
    , mveVpcEndpointId

    -- * Response
    , ModifyVpcEndpointResponse
    -- ** Response constructor
    , modifyVpcEndpointResponse
    -- ** Response lenses
    , mverReturn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ModifyVpcEndpoint = ModifyVpcEndpoint
    { _mveAddRouteTableIds    :: List "item" Text
    , _mveDryRun              :: Maybe Bool
    , _mvePolicyDocument      :: Maybe Text
    , _mveRemoveRouteTableIds :: List "item" Text
    , _mveResetPolicy         :: Maybe Bool
    , _mveVpcEndpointId       :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ModifyVpcEndpoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mveAddRouteTableIds' @::@ ['Text']
--
-- * 'mveDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'mvePolicyDocument' @::@ 'Maybe' 'Text'
--
-- * 'mveRemoveRouteTableIds' @::@ ['Text']
--
-- * 'mveResetPolicy' @::@ 'Maybe' 'Bool'
--
-- * 'mveVpcEndpointId' @::@ 'Text'
--
modifyVpcEndpoint :: Text -- ^ 'mveVpcEndpointId'
                  -> ModifyVpcEndpoint
modifyVpcEndpoint p1 = ModifyVpcEndpoint
    { _mveVpcEndpointId       = p1
    , _mveDryRun              = Nothing
    , _mveResetPolicy         = Nothing
    , _mvePolicyDocument      = Nothing
    , _mveAddRouteTableIds    = mempty
    , _mveRemoveRouteTableIds = mempty
    }

-- | One or more route tables IDs to associate with the endpoint.
mveAddRouteTableIds :: Lens' ModifyVpcEndpoint [Text]
mveAddRouteTableIds =
    lens _mveAddRouteTableIds (\s a -> s { _mveAddRouteTableIds = a })
        . _List

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
mveDryRun :: Lens' ModifyVpcEndpoint (Maybe Bool)
mveDryRun = lens _mveDryRun (\s a -> s { _mveDryRun = a })

-- | A policy document to attach to the endpoint. The policy must be in valid JSON
-- format.
mvePolicyDocument :: Lens' ModifyVpcEndpoint (Maybe Text)
mvePolicyDocument =
    lens _mvePolicyDocument (\s a -> s { _mvePolicyDocument = a })

-- | One or more route table IDs to disassociate from the endpoint.
mveRemoveRouteTableIds :: Lens' ModifyVpcEndpoint [Text]
mveRemoveRouteTableIds =
    lens _mveRemoveRouteTableIds (\s a -> s { _mveRemoveRouteTableIds = a })
        . _List

-- | Specify 'true' to reset the policy document to the default policy. The default
-- policy allows access to the service.
mveResetPolicy :: Lens' ModifyVpcEndpoint (Maybe Bool)
mveResetPolicy = lens _mveResetPolicy (\s a -> s { _mveResetPolicy = a })

-- | The ID of the endpoint.
mveVpcEndpointId :: Lens' ModifyVpcEndpoint Text
mveVpcEndpointId = lens _mveVpcEndpointId (\s a -> s { _mveVpcEndpointId = a })

newtype ModifyVpcEndpointResponse = ModifyVpcEndpointResponse
    { _mverReturn :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'ModifyVpcEndpointResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mverReturn' @::@ 'Maybe' 'Bool'
--
modifyVpcEndpointResponse :: ModifyVpcEndpointResponse
modifyVpcEndpointResponse = ModifyVpcEndpointResponse
    { _mverReturn = Nothing
    }

-- | Returns 'true' if the request succeeds; otherwise, it returns an error.
mverReturn :: Lens' ModifyVpcEndpointResponse (Maybe Bool)
mverReturn = lens _mverReturn (\s a -> s { _mverReturn = a })

instance ToPath ModifyVpcEndpoint where
    toPath = const "/"

instance ToQuery ModifyVpcEndpoint where
    toQuery ModifyVpcEndpoint{..} = mconcat
        [ "AddRouteTableId"    `toQueryList` _mveAddRouteTableIds
        , "DryRun"             =? _mveDryRun
        , "PolicyDocument"     =? _mvePolicyDocument
        , "RemoveRouteTableId" `toQueryList` _mveRemoveRouteTableIds
        , "ResetPolicy"        =? _mveResetPolicy
        , "VpcEndpointId"      =? _mveVpcEndpointId
        ]

instance ToHeaders ModifyVpcEndpoint

instance AWSRequest ModifyVpcEndpoint where
    type Sv ModifyVpcEndpoint = EC2
    type Rs ModifyVpcEndpoint = ModifyVpcEndpointResponse

    request  = post "ModifyVpcEndpoint"
    response = xmlResponse

instance FromXML ModifyVpcEndpointResponse where
    parseXML x = ModifyVpcEndpointResponse
        <$> x .@? "return"
