{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.DescribeVpcPeeringConnections
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your VPC peering connections. Example 1 This
-- example describes all of your VPC peering connections.
-- https://ec2.amazonaws.com/?Action=DescribeVpcPeeringConnections
-- &amp;AUTHPARAMS &lt;DescribeVpcPeeringConnectionsResponse
-- xmlns=http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpcPeeringConnectionSet&gt; &lt;item&gt;
-- &lt;vpcPeeringConnectionId&gt;pcx-111aaa22&lt;/vpcPeeringConnectionId&gt;
-- &lt;requesterVpcInfo&gt; &lt;ownerId&gt;777788889999&lt;/ownerId&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;cidrBlock&gt;172.31.0.0/16&lt;/cidrBlock&gt; &lt;/requesterVpcInfo&gt;
-- &lt;accepterVpcInfo&gt; &lt;ownerId&gt;123456789012&lt;/ownerId&gt;
-- &lt;vpcId&gt;vpc-aa22cc33&lt;/vpcId&gt; &lt;/accepterVpcInfo&gt;"
-- &lt;status&gt; &lt;code&gt;pending-acceptance&lt;/code&gt;
-- &lt;message&gt;Pending Acceptance by 123456789012&lt;/message&gt;
-- &lt;/status&gt;
-- &lt;expirationTime&gt;2014-02-17T16:00:50.000Z&lt;/expirationTime&gt;
-- &lt;tagSet/&gt; &lt;/item&gt; &lt;/vpcPeeringConnectionSet&gt;
-- &lt;/DescribeVpcPeeringConnectionsResponse&gt; Example 2 This example
-- describes all of your VPC peering connections that are in the
-- pending-acceptance state.
-- https://ec2.amazonaws.com/?Action=DescribeVpcPeeringConnections
-- &amp;Filter.1.Name=status-code &amp;Filter.1.Value=pending-acceptance
-- &amp;AUTHPARAMS Example 3 This example describes all of your VPC peering
-- connections that have the tag Name=Finance or Name=Accounts.
-- https://ec2.amazonaws.com/?Action=DescribeVpcPeeringConnections
-- &amp;Filter.1.Name=tag:Name &amp;Filter.1.Value.1=Finance
-- &amp;Filter.1.Value.2=Accounts &amp;AUTHPARAMS Example 4 This example
-- describes all of the VPC peering connections for your specified VPC,
-- vpc-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=DescribeVpcPeeringConnections
-- &amp;Filter.1.Name=requester-vpc-info.vpc-id
-- &amp;Filter.1.Value=vpc-1a2b3c4d &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_05_01.DescribeVpcPeeringConnections where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DescribeVpcPeeringConnections = DescribeVpcPeeringConnections
    { _dvpctDryRun :: Bool
      -- ^ 
    , _dvpctFilters :: [Filter]
      -- ^ One or more filters. accepter-vpc-info.cidr-block - The CIDR
      -- block of the peer VPC. accepter-vpc-info.owner-id - The AWS
      -- account ID of the owner of the peer VPC. accepter-vpc-info.vpc-id
      -- - The ID of the peer VPC. expiration-time - The expiration date
      -- and time for the VPC peering connection.
      -- requester-vpc-info.cidr-block - The CIDR block of the requester's
      -- VPC. requester-vpc-info.owner-id - The AWS account ID of the
      -- owner of the requester VPC. requester-vpc-info.vpc-id - The ID of
      -- the requester VPC. status-code - The status of the VPC peering
      -- connection (pending-acceptance | failed | expired | provisioning
      -- | active | deleted | rejected). status-message - A message that
      -- provides more information about the status of the VPC peering
      -- connection, if applicable. tag:key=value - The key/value
      -- combination of a tag assigned to the resource. tag-key - The key
      -- of a tag assigned to the resource. This filter is independent of
      -- the tag-value filter. For example, if you use both the filter
      -- "tag-key=Purpose" and the filter "tag-value=X", you get any
      -- resources assigned both the tag key Purpose (regardless of what
      -- the tag's value is), and the tag value X (regardless of what the
      -- tag's key is). If you want to list only resources where Purpose
      -- is X, see the tag:key=value filter. tag-value - The value of a
      -- tag assigned to the resource. This filter is independent of the
      -- tag-key filter. vpc-peering-connection-id - The ID of the VPC
      -- peering connection.
    , _dvpctVpcPeeringConnectionIds :: [Text]
      -- ^ One or more VPC peering connection IDs. Default: Describes all
      -- your VPC peering connections.
    } deriving (Generic)

instance ToQuery DescribeVpcPeeringConnections where
    toQuery = genericToQuery def

instance AWSRequest DescribeVpcPeeringConnections where
    type Sv DescribeVpcPeeringConnections = EC2
    type Rs DescribeVpcPeeringConnections = DescribeVpcPeeringConnectionsResponse

    request = post "DescribeVpcPeeringConnections"

    response _ = xmlResponse

data DescribeVpcPeeringConnectionsResponse = DescribeVpcPeeringConnectionsResponse
    { _dvpcuVpcPeeringConnections :: [VpcPeeringConnection]
      -- ^ Information about the VPC peering connections.
    } deriving (Generic)

instance FromXML DescribeVpcPeeringConnectionsResponse where
    fromXMLOptions = xmlOptions
