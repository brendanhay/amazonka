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
    , modifyVpcAttribute
    -- ** Request lenses
    , mvasVpcId
    , mvasEnableDnsSupport
    , mvasEnableDnsHostnames

    -- * Response
    , ModifyVpcAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyVpcAttribute' request.
modifyVpcAttribute :: Text -- ^ 'mvasVpcId'
                   -> ModifyVpcAttribute
modifyVpcAttribute p1 = ModifyVpcAttribute
    { _mvasVpcId = p1
    , _mvasEnableDnsSupport = Nothing
    , _mvasEnableDnsHostnames = Nothing
    }

data ModifyVpcAttribute = ModifyVpcAttribute
    { _mvasVpcId :: Text
      -- ^ The ID of the VPC.
    , _mvasEnableDnsSupport :: Maybe AttributeBooleanValue
      -- ^ Indicates whether the DNS resolution is supported for the VPC. If
      -- this attribute is false, the Amazon provided DNS service in the
      -- VPC that resolves public DNS hostnames to IP addresses is not
      -- enabled. If this attribute is true, queries to the Amazon
      -- provided DNS server at the 169.254.169.253 IP address, or the
      -- reserved IP address at the base of the VPC network range "plus
      -- two" will succeed.
    , _mvasEnableDnsHostnames :: Maybe AttributeBooleanValue
      -- ^ Indicates whether the instances launched in the VPC get DNS
      -- hostnames. If this attribute is true, instances in the VPC get
      -- DNS hostnames; otherwise, they do not. You can only set
      -- enableDnsHostnames to true if you also set the EnableDnsSupport
      -- attribute to true.
    } deriving (Show, Generic)

-- | The ID of the VPC.
mvasVpcId
    :: Functor f
    => (Text
    -> f (Text))
    -> ModifyVpcAttribute
    -> f ModifyVpcAttribute
mvasVpcId f x =
    (\y -> x { _mvasVpcId = y })
       <$> f (_mvasVpcId x)
{-# INLINE mvasVpcId #-}

-- | Indicates whether the DNS resolution is supported for the VPC. If this
-- attribute is false, the Amazon provided DNS service in the VPC that
-- resolves public DNS hostnames to IP addresses is not enabled. If this
-- attribute is true, queries to the Amazon provided DNS server at the
-- 169.254.169.253 IP address, or the reserved IP address at the base of the
-- VPC network range "plus two" will succeed.
mvasEnableDnsSupport
    :: Functor f
    => (Maybe AttributeBooleanValue
    -> f (Maybe AttributeBooleanValue))
    -> ModifyVpcAttribute
    -> f ModifyVpcAttribute
mvasEnableDnsSupport f x =
    (\y -> x { _mvasEnableDnsSupport = y })
       <$> f (_mvasEnableDnsSupport x)
{-# INLINE mvasEnableDnsSupport #-}

-- | Indicates whether the instances launched in the VPC get DNS hostnames. If
-- this attribute is true, instances in the VPC get DNS hostnames; otherwise,
-- they do not. You can only set enableDnsHostnames to true if you also set
-- the EnableDnsSupport attribute to true.
mvasEnableDnsHostnames
    :: Functor f
    => (Maybe AttributeBooleanValue
    -> f (Maybe AttributeBooleanValue))
    -> ModifyVpcAttribute
    -> f ModifyVpcAttribute
mvasEnableDnsHostnames f x =
    (\y -> x { _mvasEnableDnsHostnames = y })
       <$> f (_mvasEnableDnsHostnames x)
{-# INLINE mvasEnableDnsHostnames #-}

instance ToQuery ModifyVpcAttribute where
    toQuery = genericQuery def

data ModifyVpcAttributeResponse = ModifyVpcAttributeResponse
    deriving (Eq, Show, Generic)

instance AWSRequest ModifyVpcAttribute where
    type Sv ModifyVpcAttribute = EC2
    type Rs ModifyVpcAttribute = ModifyVpcAttributeResponse

    request = post "ModifyVpcAttribute"
    response _ = nullaryResponse ModifyVpcAttributeResponse
