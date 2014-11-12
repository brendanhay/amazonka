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

-- Module      : Network.AWS.EC2.CreateVpcPeeringConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a VPC peering connection between two VPCs: a requester VPC that
-- you own and a peer VPC with which to create the connection. The peer VPC
-- can belong to another AWS account. The requester VPC and peer VPC cannot
-- have overlapping CIDR blocks. The owner of the peer VPC must accept the
-- peering request to activate the peering connection. The VPC peering
-- connection request expires after 7 days, after which it cannot be accepted
-- or rejected. A CreateVpcPeeringConnection request between VPCs with
-- overlapping CIDR blocks results in the VPC peering connection having a
-- status of failed.
module Network.AWS.EC2.CreateVpcPeeringConnection
    (
    -- * Request
      CreateVpcPeeringConnection
    -- ** Request constructor
    , createVpcPeeringConnection
    -- ** Request lenses
    , cvpcDryRun
    , cvpcPeerOwnerId
    , cvpcPeerVpcId
    , cvpcVpcId

    -- * Response
    , CreateVpcPeeringConnectionResult
    -- ** Response constructor
    , createVpcPeeringConnectionResult
    -- ** Response lenses
    , cvpcrVpcPeeringConnection
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data CreateVpcPeeringConnection = CreateVpcPeeringConnection
    { _cvpcDryRun      :: Maybe Bool
    , _cvpcPeerOwnerId :: Maybe Text
    , _cvpcPeerVpcId   :: Maybe Text
    , _cvpcVpcId       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateVpcPeeringConnection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvpcDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cvpcPeerOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'cvpcPeerVpcId' @::@ 'Maybe' 'Text'
--
-- * 'cvpcVpcId' @::@ 'Maybe' 'Text'
--
createVpcPeeringConnection :: CreateVpcPeeringConnection
createVpcPeeringConnection = CreateVpcPeeringConnection
    { _cvpcDryRun      = Nothing
    , _cvpcVpcId       = Nothing
    , _cvpcPeerVpcId   = Nothing
    , _cvpcPeerOwnerId = Nothing
    }

cvpcDryRun :: Lens' CreateVpcPeeringConnection (Maybe Bool)
cvpcDryRun = lens _cvpcDryRun (\s a -> s { _cvpcDryRun = a })

-- | The AWS account ID of the owner of the peer VPC. Default: Your AWS
-- account ID.
cvpcPeerOwnerId :: Lens' CreateVpcPeeringConnection (Maybe Text)
cvpcPeerOwnerId = lens _cvpcPeerOwnerId (\s a -> s { _cvpcPeerOwnerId = a })

-- | The ID of the VPC with which you are creating the VPC peering connection.
cvpcPeerVpcId :: Lens' CreateVpcPeeringConnection (Maybe Text)
cvpcPeerVpcId = lens _cvpcPeerVpcId (\s a -> s { _cvpcPeerVpcId = a })

-- | The ID of the requester VPC.
cvpcVpcId :: Lens' CreateVpcPeeringConnection (Maybe Text)
cvpcVpcId = lens _cvpcVpcId (\s a -> s { _cvpcVpcId = a })

instance ToQuery CreateVpcPeeringConnection

instance ToPath CreateVpcPeeringConnection where
    toPath = const "/"

newtype CreateVpcPeeringConnectionResult = CreateVpcPeeringConnectionResult
    { _cvpcrVpcPeeringConnection :: Maybe VpcPeeringConnection
    } deriving (Eq, Show, Generic)

-- | 'CreateVpcPeeringConnectionResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvpcrVpcPeeringConnection' @::@ 'Maybe' 'VpcPeeringConnection'
--
createVpcPeeringConnectionResult :: CreateVpcPeeringConnectionResult
createVpcPeeringConnectionResult = CreateVpcPeeringConnectionResult
    { _cvpcrVpcPeeringConnection = Nothing
    }

-- | Information about the VPC peering connection.
cvpcrVpcPeeringConnection :: Lens' CreateVpcPeeringConnectionResult (Maybe VpcPeeringConnection)
cvpcrVpcPeeringConnection =
    lens _cvpcrVpcPeeringConnection
        (\s a -> s { _cvpcrVpcPeeringConnection = a })

instance FromXML CreateVpcPeeringConnectionResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateVpcPeeringConnectionResult"

instance AWSRequest CreateVpcPeeringConnection where
    type Sv CreateVpcPeeringConnection = EC2
    type Rs CreateVpcPeeringConnection = CreateVpcPeeringConnectionResult

    request  = post "CreateVpcPeeringConnection"
    response = xmlResponse $ \h x -> CreateVpcPeeringConnectionResult
        <$> x %| "vpcPeeringConnection"
