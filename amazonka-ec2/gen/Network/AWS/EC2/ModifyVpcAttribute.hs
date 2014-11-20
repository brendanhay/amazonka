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

-- Module      : Network.AWS.EC2.ModifyVpcAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the specified attribute of the specified VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyVpcAttribute.html>
module Network.AWS.EC2.ModifyVpcAttribute
    (
    -- * Request
      ModifyVpcAttribute
    -- ** Request constructor
    , modifyVpcAttribute
    -- ** Request lenses
    , mvaEnableDnsHostnames
    , mvaEnableDnsSupport
    , mvaVpcId

    -- * Response
    , ModifyVpcAttributeResponse
    -- ** Response constructor
    , modifyVpcAttributeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ModifyVpcAttribute = ModifyVpcAttribute
    { _mvaEnableDnsHostnames :: Maybe AttributeBooleanValue
    , _mvaEnableDnsSupport   :: Maybe AttributeBooleanValue
    , _mvaVpcId              :: Text
    } deriving (Eq, Show)

-- | 'ModifyVpcAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mvaEnableDnsHostnames' @::@ 'Maybe' 'AttributeBooleanValue'
--
-- * 'mvaEnableDnsSupport' @::@ 'Maybe' 'AttributeBooleanValue'
--
-- * 'mvaVpcId' @::@ 'Text'
--
modifyVpcAttribute :: Text -- ^ 'mvaVpcId'
                   -> ModifyVpcAttribute
modifyVpcAttribute p1 = ModifyVpcAttribute
    { _mvaVpcId              = p1
    , _mvaEnableDnsSupport   = Nothing
    , _mvaEnableDnsHostnames = Nothing
    }

-- | Indicates whether the instances launched in the VPC get DNS hostnames. If
-- enabled, instances in the VPC get DNS hostnames; otherwise, they do not.
-- You can only enable DNS hostnames if you also enable DNS support.
mvaEnableDnsHostnames :: Lens' ModifyVpcAttribute (Maybe AttributeBooleanValue)
mvaEnableDnsHostnames =
    lens _mvaEnableDnsHostnames (\s a -> s { _mvaEnableDnsHostnames = a })

-- | Indicates whether the DNS resolution is supported for the VPC. If
-- enabled, queries to the Amazon provided DNS server at the 169.254.169.253
-- IP address, or the reserved IP address at the base of the VPC network
-- range "plus two" will succeed. If disabled, the Amazon provided DNS
-- service in the VPC that resolves public DNS hostnames to IP addresses is
-- not enabled.
mvaEnableDnsSupport :: Lens' ModifyVpcAttribute (Maybe AttributeBooleanValue)
mvaEnableDnsSupport =
    lens _mvaEnableDnsSupport (\s a -> s { _mvaEnableDnsSupport = a })

-- | The ID of the VPC.
mvaVpcId :: Lens' ModifyVpcAttribute Text
mvaVpcId = lens _mvaVpcId (\s a -> s { _mvaVpcId = a })

data ModifyVpcAttributeResponse = ModifyVpcAttributeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'ModifyVpcAttributeResponse' constructor.
modifyVpcAttributeResponse :: ModifyVpcAttributeResponse
modifyVpcAttributeResponse = ModifyVpcAttributeResponse

instance ToPath ModifyVpcAttribute where
    toPath = const "/"

instance ToQuery ModifyVpcAttribute where
    toQuery ModifyVpcAttribute{..} = mconcat
        [ "EnableDnsHostnames" =? _mvaEnableDnsHostnames
        , "EnableDnsSupport"   =? _mvaEnableDnsSupport
        , "vpcId"              =? _mvaVpcId
        ]

instance ToHeaders ModifyVpcAttribute

instance AWSRequest ModifyVpcAttribute where
    type Sv ModifyVpcAttribute = EC2
    type Rs ModifyVpcAttribute = ModifyVpcAttributeResponse

    request  = post "ModifyVpcAttribute"
    response = nullResponse ModifyVpcAttributeResponse


Some kind of operator / class to check the types whether to continue?
