{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateVpcPeeringConnection
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
-- status of failed. Example 1 This example requests a peering connection
-- between your VPC (vpc-1a2b3c4d), and a VPC (vpc-a1b2c3d4) that belongs to
-- AWS account 123456789012.
-- https://ec2.amazonaws.com/?Action=CreateVpcPeeringConnection
-- &amp;VpcId=vpc-1a2b3c4d &amp;PeerVpcId=vpc-a1b2c3d4
-- &amp;PeerOwnerId=123456789012 &amp;AUTHPARAMS
-- &lt;CreateVpcPeeringConnectionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpcPeeringConnection&gt;
-- &lt;vpcPeeringConnectionId&gt;pcx-73a5401a&lt;/vpcPeeringConnectionId&gt;
-- &lt;requesterVpcInfo&gt; &lt;ownerId&gt;777788889999&lt;/ownerId&gt;
-- &lt;vpcId&gt;vpc-vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;cidrBlock&gt;10.0.0.0/28&lt;/cidrBlock&gt; &lt;/requesterVpcInfo&gt;
-- &lt;accepterVpcInfo&gt; &lt;ownerId&gt;123456789012&lt;/ownerId&gt;
-- &lt;vpcId&gt;vpc-a1b2c3d4&lt;/vpcId&gt; &lt;/accepterVpcInfo&gt;
-- &lt;status&gt; &lt;code&gt;initiating-request&lt;/code&gt;
-- &lt;message&gt;Initiating Request to 123456789012&lt;/message&gt;
-- &lt;/status&gt;
-- &lt;expirationTime&gt;2014-02-18T14:37:25.000Z&lt;/expirationTime&gt;
-- &lt;tagSet/&gt; &lt;/vpcPeeringConnection&gt;
-- &lt;/CreateVpcPeeringConnectionResponse&gt; Example 2 This example requests
-- a peering connection between your VPCs vpc-1a2b3c4d and vpc-11122233.
-- https://ec2.amazonaws.com/?Action=CreateVpcPeeringConnection
-- &amp;VpcId=vpc-1a2b3c4d &amp;PeerVpcId=vpc-11122233 &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.CreateVpcPeeringConnection
    (
    -- * Request
      CreateVpcPeeringConnection
    -- ** Request constructor
    , mkCreateVpcPeeringConnectionRequest
    -- ** Request lenses
    , cvpcrVpcId
    , cvpcrPeerVpcId
    , cvpcrPeerOwnerId

    -- * Response
    , CreateVpcPeeringConnectionResponse
    -- ** Response lenses
    , cvpcsVpcPeeringConnection
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateVpcPeeringConnection' request.
mkCreateVpcPeeringConnectionRequest :: CreateVpcPeeringConnection
mkCreateVpcPeeringConnectionRequest = CreateVpcPeeringConnection
    { _cvpcrVpcId = Nothing
    , _cvpcrPeerVpcId = Nothing
    , _cvpcrPeerOwnerId = Nothing
    }
{-# INLINE mkCreateVpcPeeringConnectionRequest #-}

data CreateVpcPeeringConnection = CreateVpcPeeringConnection
    { _cvpcrVpcId :: Maybe Text
      -- ^ The ID of the requester VPC.
    , _cvpcrPeerVpcId :: Maybe Text
      -- ^ The ID of the VPC with which you are creating the VPC peering
      -- connection.
    , _cvpcrPeerOwnerId :: Maybe Text
      -- ^ The AWS account ID of the owner of the peer VPC. Default: Your
      -- AWS account ID.
    } deriving (Show, Generic)

-- | The ID of the requester VPC.
cvpcrVpcId :: Lens' CreateVpcPeeringConnection (Maybe Text)
cvpcrVpcId = lens _cvpcrVpcId (\s a -> s { _cvpcrVpcId = a })
{-# INLINE cvpcrVpcId #-}

-- | The ID of the VPC with which you are creating the VPC peering connection.
cvpcrPeerVpcId :: Lens' CreateVpcPeeringConnection (Maybe Text)
cvpcrPeerVpcId = lens _cvpcrPeerVpcId (\s a -> s { _cvpcrPeerVpcId = a })
{-# INLINE cvpcrPeerVpcId #-}

-- | The AWS account ID of the owner of the peer VPC. Default: Your AWS account
-- ID.
cvpcrPeerOwnerId :: Lens' CreateVpcPeeringConnection (Maybe Text)
cvpcrPeerOwnerId = lens _cvpcrPeerOwnerId (\s a -> s { _cvpcrPeerOwnerId = a })
{-# INLINE cvpcrPeerOwnerId #-}

instance ToQuery CreateVpcPeeringConnection where
    toQuery = genericQuery def

newtype CreateVpcPeeringConnectionResponse = CreateVpcPeeringConnectionResponse
    { _cvpcsVpcPeeringConnection :: Maybe VpcPeeringConnection
      -- ^ Information about the VPC peering connection.
    } deriving (Show, Generic)

-- | Information about the VPC peering connection.
cvpcsVpcPeeringConnection :: Lens' CreateVpcPeeringConnectionResponse (Maybe VpcPeeringConnection)
cvpcsVpcPeeringConnection = lens _cvpcsVpcPeeringConnection (\s a -> s { _cvpcsVpcPeeringConnection = a })
{-# INLINE cvpcsVpcPeeringConnection #-}

instance FromXML CreateVpcPeeringConnectionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateVpcPeeringConnection where
    type Sv CreateVpcPeeringConnection = EC2
    type Rs CreateVpcPeeringConnection = CreateVpcPeeringConnectionResponse

    request = post "CreateVpcPeeringConnection"
    response _ = xmlResponse
