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
-- see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Subnets.html
-- Your VPC and Subnets> in the /Amazon Virtual Private Cloud User Guide/. By
-- default, each instance you launch in the VPC has the default DHCP options,
-- which includes only a default DNS server that we provide
-- (AmazonProvidedDNS). For more information about DHCP options, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html
-- DHCP Options Sets> in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpc.html>
module Network.AWS.EC2.CreateVpc
    (
    -- * Request
      CreateVpc
    -- ** Request constructor
    , createVpc
    -- ** Request lenses
    , cvCidrBlock
    , cvDryRun
    , cvInstanceTenancy

    -- * Response
    , CreateVpcResponse
    -- ** Response constructor
    , createVpcResponse
    -- ** Response lenses
    , cvrVpc
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateVpc = CreateVpc
    { _cvCidrBlock       :: Text
    , _cvDryRun          :: Maybe Bool
    , _cvInstanceTenancy :: Maybe Tenancy
    } deriving (Eq, Show)

-- | 'CreateVpc' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvCidrBlock' @::@ 'Text'
--
-- * 'cvDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cvInstanceTenancy' @::@ 'Maybe' 'Tenancy'
--
createVpc :: Text -- ^ 'cvCidrBlock'
          -> CreateVpc
createVpc p1 = CreateVpc
    { _cvCidrBlock       = p1
    , _cvDryRun          = Nothing
    , _cvInstanceTenancy = Nothing
    }

-- | The network range for the VPC, in CIDR notation. For example,
-- '10.0.0.0/16'.
cvCidrBlock :: Lens' CreateVpc Text
cvCidrBlock = lens _cvCidrBlock (\s a -> s { _cvCidrBlock = a })

cvDryRun :: Lens' CreateVpc (Maybe Bool)
cvDryRun = lens _cvDryRun (\s a -> s { _cvDryRun = a })

-- | The supported tenancy options for instances launched into the VPC. A
-- value of 'default' means that instances can be launched with any tenancy;
-- a value of 'dedicated' means all instances launched into the VPC are
-- launched as dedicated tenancy instances regardless of the tenancy
-- assigned to the instance at launch. Dedicated tenancy instances run on
-- single-tenant hardware. Default: 'default'.
cvInstanceTenancy :: Lens' CreateVpc (Maybe Tenancy)
cvInstanceTenancy =
    lens _cvInstanceTenancy (\s a -> s { _cvInstanceTenancy = a })

newtype CreateVpcResponse = CreateVpcResponse
    { _cvrVpc :: Maybe Vpc
    } deriving (Eq, Show)

-- | 'CreateVpcResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvrVpc' @::@ 'Maybe' 'Vpc'
--
createVpcResponse :: CreateVpcResponse
createVpcResponse = CreateVpcResponse
    { _cvrVpc = Nothing
    }

-- | Information about the VPC.
cvrVpc :: Lens' CreateVpcResponse (Maybe Vpc)
cvrVpc = lens _cvrVpc (\s a -> s { _cvrVpc = a })

instance ToPath CreateVpc where
    toPath = const "/"

instance ToQuery CreateVpc where
    toQuery CreateVpc{..} = mconcat
        [ "CidrBlock"       =? _cvCidrBlock
        , "dryRun"          =? _cvDryRun
        , "instanceTenancy" =? _cvInstanceTenancy
        ]

instance ToHeaders CreateVpc

instance AWSRequest CreateVpc where
    type Sv CreateVpc = EC2
    type Rs CreateVpc = CreateVpcResponse

    request  = post "CreateVpc"
    response = xmlResponse

instance FromXML CreateVpcResponse where
    parseXML x = CreateVpcResponse
        <$> x .@? "vpc"
