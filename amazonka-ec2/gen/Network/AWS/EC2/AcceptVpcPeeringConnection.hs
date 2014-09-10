{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Accept a VPC peering connection request. To accept a request, the VPC
-- peering connection must be in the pending-acceptance state, and you must be
-- the owner of the peer VPC. Use the DescribeVpcPeeringConnections request to
-- view your outstanding VPC peering connection requests. Example This example
-- accepts the specified VPC peering connection request.
-- https://ec2.amazonaws.com/?Action=AcceptVpcPeeringConnection
-- &amp;vpcPeeringConnectionId=pcx-1a2b3c4d &amp;AUTHPARAMS
-- &lt;AcceptVpcPeeringConnectionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpcPeeringConnection&gt;
-- &lt;vpcPeeringConnectionId&gt;pcx-1a2b3c4d&lt;/vpcPeeringConnectionId&gt;
-- &lt;requesterVpcInfo&gt; &lt;ownerId&gt;123456789012&lt;/ownerId&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt;
-- &lt;cidrBlock&gt;10.0.0.0/28&lt;/cidrBlock&gt; &lt;/requesterVpcInfo&gt;
-- &lt;accepterVpcInfo&gt; &lt;ownerId&gt;777788889999&lt;/ownerId&gt;
-- &lt;vpcId&gt;vpc-111aaa22&lt;/vpcId&gt;
-- &lt;cidrBlock&gt;10.0.1.0/28&lt;/cidrBlock&gt; &lt;/accepterVpcInfo&gt;
-- &lt;status&gt; &lt;code&gt;active&lt;/code&gt;
-- &lt;message&gt;Active&lt;/message&gt; &lt;/status&gt; &lt;tagSet/&gt;
-- &lt;/vpcPeeringConnection&gt; &lt;/AcceptVpcPeeringConnectionResponse&gt;".
module Network.AWS.EC2
    (
    -- * Request
      AcceptVpcPeeringConnection
    -- ** Request constructor
    , mkAcceptVpcPeeringConnection
    -- ** Request lenses
    , avpcVpcPeeringConnectionId

    -- * Response
    , AcceptVpcPeeringConnectionResponse
    -- ** Response constructor
    , mkAcceptVpcPeeringConnectionResponse
    -- ** Response lenses
    , avpcrVpcPeeringConnection
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype AcceptVpcPeeringConnection = AcceptVpcPeeringConnection
    { _avpcVpcPeeringConnectionId :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AcceptVpcPeeringConnection' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcPeeringConnectionId ::@ @Maybe Text@
--
mkAcceptVpcPeeringConnection :: AcceptVpcPeeringConnection
mkAcceptVpcPeeringConnection = AcceptVpcPeeringConnection
    { _avpcVpcPeeringConnectionId = Nothing
    }

-- | The ID of the VPC peering connection.
avpcVpcPeeringConnectionId :: Lens' AcceptVpcPeeringConnection (Maybe Text)
avpcVpcPeeringConnectionId =
    lens _avpcVpcPeeringConnectionId
         (\s a -> s { _avpcVpcPeeringConnectionId = a })

instance ToQuery AcceptVpcPeeringConnection where
    toQuery = genericQuery def

newtype AcceptVpcPeeringConnectionResponse = AcceptVpcPeeringConnectionResponse
    { _avpcrVpcPeeringConnection :: Maybe VpcPeeringConnection
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AcceptVpcPeeringConnectionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcPeeringConnection ::@ @Maybe VpcPeeringConnection@
--
mkAcceptVpcPeeringConnectionResponse :: AcceptVpcPeeringConnectionResponse
mkAcceptVpcPeeringConnectionResponse = AcceptVpcPeeringConnectionResponse
    { _avpcrVpcPeeringConnection = Nothing
    }

-- | Information about the VPC peering connection.
avpcrVpcPeeringConnection :: Lens' AcceptVpcPeeringConnectionResponse (Maybe VpcPeeringConnection)
avpcrVpcPeeringConnection =
    lens _avpcrVpcPeeringConnection
         (\s a -> s { _avpcrVpcPeeringConnection = a })

instance FromXML AcceptVpcPeeringConnectionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AcceptVpcPeeringConnection where
    type Sv AcceptVpcPeeringConnection = EC2
    type Rs AcceptVpcPeeringConnection = AcceptVpcPeeringConnectionResponse

    request = post "AcceptVpcPeeringConnection"
    response _ = xmlResponse
