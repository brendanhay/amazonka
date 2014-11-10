{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.CreateVpc
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a VPC with the specified CIDR block. The smallest VPC you can
-- create uses a /28 netmask (16 IP addresses), and the largest uses a /16
-- netmask (65,536 IP addresses). To help you decide how big to make your VPC,
-- see Your VPC and Subnets in the Amazon Virtual Private Cloud User Guide. By
-- default, each instance you launch in the VPC has the default DHCP options,
-- which includes only a default DNS server that we provide
-- (AmazonProvidedDNS). For more information about DHCP options, see DHCP
-- Options Sets in the Amazon Virtual Private Cloud User Guide.
module Network.AWS.EC2.CreateVpc
    (
    -- * Request
      CreateVpc
    -- ** Request constructor
    , createVpc
    -- ** Request lenses
    , cv1CidrBlock
    , cv1DryRun
    , cv1InstanceTenancy

    -- * Response
    , CreateVpcResult
    -- ** Response constructor
    , createVpcResponse
    -- ** Response lenses
    , cvrVpc
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data CreateVpc = CreateVpc
    { _cv1CidrBlock       :: Text
    , _cv1DryRun          :: Maybe Bool
    , _cv1InstanceTenancy :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateVpc' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cv1CidrBlock' @::@ 'Text'
--
-- * 'cv1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cv1InstanceTenancy' @::@ 'Maybe' 'Text'
--
createVpc :: Text -- ^ 'cv1CidrBlock'
          -> CreateVpc
createVpc p1 = CreateVpc
    { _cv1CidrBlock       = p1
    , _cv1DryRun          = Nothing
    , _cv1InstanceTenancy = Nothing
    }

-- | The network range for the VPC, in CIDR notation. For example,
-- 10.0.0.0/16.
cv1CidrBlock :: Lens' CreateVpc Text
cv1CidrBlock = lens _cv1CidrBlock (\s a -> s { _cv1CidrBlock = a })

cv1DryRun :: Lens' CreateVpc (Maybe Bool)
cv1DryRun = lens _cv1DryRun (\s a -> s { _cv1DryRun = a })

-- | The supported tenancy options for instances launched into the VPC. A
-- value of default means that instances can be launched with any tenancy; a
-- value of dedicated means all instances launched into the VPC are launched
-- as dedicated tenancy instances regardless of the tenancy assigned to the
-- instance at launch. Dedicated tenancy instances run on single-tenant
-- hardware. Default: default.
cv1InstanceTenancy :: Lens' CreateVpc (Maybe Text)
cv1InstanceTenancy =
    lens _cv1InstanceTenancy (\s a -> s { _cv1InstanceTenancy = a })

instance ToPath CreateVpc where
    toPath = const "/"

instance ToQuery CreateVpc

newtype CreateVpcResult = CreateVpcResult
    { _cvrVpc :: Maybe Vpc
    } deriving (Eq, Show, Generic)

-- | 'CreateVpcResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvrVpc' @::@ 'Maybe' 'Vpc'
--
createVpcResponse :: CreateVpcResult
createVpcResponse = CreateVpcResult
    { _cvrVpc = Nothing
    }

-- | Information about the VPC.
cvrVpc :: Lens' CreateVpcResult (Maybe Vpc)
cvrVpc = lens _cvrVpc (\s a -> s { _cvrVpc = a })

instance AWSRequest CreateVpc where
    type Sv CreateVpc = EC2
    type Rs CreateVpc = CreateVpcResult

    request  = post "CreateVpc"
    response = xmlResponse $ \h x -> CreateVpcResult
        <$> x %| "vpc"
