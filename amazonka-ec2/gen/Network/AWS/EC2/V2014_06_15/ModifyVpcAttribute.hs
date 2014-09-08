{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ModifyVpcAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the specified attribute of the specified VPC. Example This example
-- disables support for DNS hostnames in the specified VPC.
-- https://ec2.amazonaws.com/?Action=ModifyVpcAttribute
-- &amp;VpcId=vpc-1a2b3c4d &amp;EnableDnsHostnames.Value=false
-- &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.ModifyVpcAttribute
    (
    -- * Request
      ModifyVpcAttribute
    -- ** Request constructor
    , mkModifyVpcAttribute
    -- ** Request lenses
    , mva1VpcId
    , mva1EnableDnsSupport
    , mva1EnableDnsHostnames

    -- * Response
    , ModifyVpcAttributeResponse
    -- ** Response constructor
    , mkModifyVpcAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data ModifyVpcAttribute = ModifyVpcAttribute
    { _mva1VpcId :: Text
    , _mva1EnableDnsSupport :: Maybe AttributeBooleanValue
    , _mva1EnableDnsHostnames :: Maybe AttributeBooleanValue
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyVpcAttribute' request.
mkModifyVpcAttribute :: Text -- ^ 'mva1VpcId'
                     -> ModifyVpcAttribute
mkModifyVpcAttribute p1 = ModifyVpcAttribute
    { _mva1VpcId = p1
    , _mva1EnableDnsSupport = Nothing
    , _mva1EnableDnsHostnames = Nothing
    }

-- | The ID of the VPC.
mva1VpcId :: Lens' ModifyVpcAttribute Text
mva1VpcId = lens _mva1VpcId (\s a -> s { _mva1VpcId = a })

-- | Indicates whether the DNS resolution is supported for the VPC. If this
-- attribute is false, the Amazon provided DNS service in the VPC that
-- resolves public DNS hostnames to IP addresses is not enabled. If this
-- attribute is true, queries to the Amazon provided DNS server at the
-- 169.254.169.253 IP address, or the reserved IP address at the base of the
-- VPC network range "plus two" will succeed.
mva1EnableDnsSupport :: Lens' ModifyVpcAttribute (Maybe AttributeBooleanValue)
mva1EnableDnsSupport =
    lens _mva1EnableDnsSupport (\s a -> s { _mva1EnableDnsSupport = a })

-- | Indicates whether the instances launched in the VPC get DNS hostnames. If
-- this attribute is true, instances in the VPC get DNS hostnames; otherwise,
-- they do not. You can only set enableDnsHostnames to true if you also set
-- the EnableDnsSupport attribute to true.
mva1EnableDnsHostnames :: Lens' ModifyVpcAttribute (Maybe AttributeBooleanValue)
mva1EnableDnsHostnames =
    lens _mva1EnableDnsHostnames (\s a -> s { _mva1EnableDnsHostnames = a })

instance ToQuery ModifyVpcAttribute where
    toQuery = genericQuery def

data ModifyVpcAttributeResponse = ModifyVpcAttributeResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyVpcAttributeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkModifyVpcAttributeResponse :: ModifyVpcAttributeResponse
mkModifyVpcAttributeResponse = ModifyVpcAttributeResponse

instance AWSRequest ModifyVpcAttribute where
    type Sv ModifyVpcAttribute = EC2
    type Rs ModifyVpcAttribute = ModifyVpcAttributeResponse

    request = post "ModifyVpcAttribute"
    response _ = nullaryResponse ModifyVpcAttributeResponse
