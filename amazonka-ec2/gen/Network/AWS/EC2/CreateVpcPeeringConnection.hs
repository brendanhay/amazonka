{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.EC2.CreateVpcPeeringConnection
    (
    -- * Request
      CreateVpcPeeringConnection
    -- ** Request constructor
    , mkCreateVpcPeeringConnection
    -- ** Request lenses
    , cvpcVpcId
    , cvpcPeerVpcId
    , cvpcPeerOwnerId

    -- * Response
    , CreateVpcPeeringConnectionResponse
    -- ** Response constructor
    , mkCreateVpcPeeringConnectionResponse
    -- ** Response lenses
    , cvpcrVpcPeeringConnection
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data CreateVpcPeeringConnection = CreateVpcPeeringConnection
    { _cvpcVpcId :: !(Maybe Text)
    , _cvpcPeerVpcId :: !(Maybe Text)
    , _cvpcPeerOwnerId :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateVpcPeeringConnection' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @PeerVpcId ::@ @Maybe Text@
--
-- * @PeerOwnerId ::@ @Maybe Text@
--
mkCreateVpcPeeringConnection :: CreateVpcPeeringConnection
mkCreateVpcPeeringConnection = CreateVpcPeeringConnection
    { _cvpcVpcId = Nothing
    , _cvpcPeerVpcId = Nothing
    , _cvpcPeerOwnerId = Nothing
    }

-- | The ID of the requester VPC.
cvpcVpcId :: Lens' CreateVpcPeeringConnection (Maybe Text)
cvpcVpcId = lens _cvpcVpcId (\s a -> s { _cvpcVpcId = a })

-- | The ID of the VPC with which you are creating the VPC peering connection.
cvpcPeerVpcId :: Lens' CreateVpcPeeringConnection (Maybe Text)
cvpcPeerVpcId = lens _cvpcPeerVpcId (\s a -> s { _cvpcPeerVpcId = a })

-- | The AWS account ID of the owner of the peer VPC. Default: Your AWS account
-- ID.
cvpcPeerOwnerId :: Lens' CreateVpcPeeringConnection (Maybe Text)
cvpcPeerOwnerId = lens _cvpcPeerOwnerId (\s a -> s { _cvpcPeerOwnerId = a })

instance ToQuery CreateVpcPeeringConnection where
    toQuery = genericQuery def

newtype CreateVpcPeeringConnectionResponse = CreateVpcPeeringConnectionResponse
    { _cvpcrVpcPeeringConnection :: Maybe VpcPeeringConnection
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateVpcPeeringConnectionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcPeeringConnection ::@ @Maybe VpcPeeringConnection@
--
mkCreateVpcPeeringConnectionResponse :: CreateVpcPeeringConnectionResponse
mkCreateVpcPeeringConnectionResponse = CreateVpcPeeringConnectionResponse
    { _cvpcrVpcPeeringConnection = Nothing
    }

-- | Information about the VPC peering connection.
cvpcrVpcPeeringConnection :: Lens' CreateVpcPeeringConnectionResponse (Maybe VpcPeeringConnection)
cvpcrVpcPeeringConnection =
    lens _cvpcrVpcPeeringConnection
         (\s a -> s { _cvpcrVpcPeeringConnection = a })

instance FromXML CreateVpcPeeringConnectionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateVpcPeeringConnection where
    type Sv CreateVpcPeeringConnection = EC2
    type Rs CreateVpcPeeringConnection = CreateVpcPeeringConnectionResponse

    request = post "CreateVpcPeeringConnection"
    response _ = xmlResponse
