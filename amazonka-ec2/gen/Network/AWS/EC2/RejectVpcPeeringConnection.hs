{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.RejectVpcPeeringConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Rejects a VPC peering connection request. The VPC peering connection must
-- be in the pending-acceptance state. Use the DescribeVpcPeeringConnections
-- request to view your outstanding VPC peering connection requests. Example
-- This example rejects the specified VPC peering connection request.
-- https://ec2.amazonaws.com/?Action=RejectVpcPeeringConnection
-- &amp;vpcPeeringConnectionId=pcx-1a2b3c4d &amp;AUTHPARAMS
-- &lt;RejectVpcPeeringConnectionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/RejectVpcPeeringConnectionResponse&gt;.
module Network.AWS.EC2.RejectVpcPeeringConnection
    (
    -- * Request
      RejectVpcPeeringConnection
    -- ** Request constructor
    , mkRejectVpcPeeringConnection
    -- ** Request lenses
    , rvpcVpcPeeringConnectionId

    -- * Response
    , RejectVpcPeeringConnectionResponse
    -- ** Response constructor
    , mkRejectVpcPeeringConnectionResponse
    -- ** Response lenses
    , rvpcrReturn
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype RejectVpcPeeringConnection = RejectVpcPeeringConnection
    { _rvpcVpcPeeringConnectionId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RejectVpcPeeringConnection' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcPeeringConnectionId ::@ @Text@
--
mkRejectVpcPeeringConnection :: Text -- ^ 'rvpcVpcPeeringConnectionId'
                             -> RejectVpcPeeringConnection
mkRejectVpcPeeringConnection p1 = RejectVpcPeeringConnection
    { _rvpcVpcPeeringConnectionId = p1
    }

-- | The ID of the VPC peering connection.
rvpcVpcPeeringConnectionId :: Lens' RejectVpcPeeringConnection Text
rvpcVpcPeeringConnectionId =
    lens _rvpcVpcPeeringConnectionId
         (\s a -> s { _rvpcVpcPeeringConnectionId = a })

instance ToQuery RejectVpcPeeringConnection where
    toQuery = genericQuery def

newtype RejectVpcPeeringConnectionResponse = RejectVpcPeeringConnectionResponse
    { _rvpcrReturn :: !(Maybe Bool)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RejectVpcPeeringConnectionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Return ::@ @Maybe Bool@
--
mkRejectVpcPeeringConnectionResponse :: RejectVpcPeeringConnectionResponse
mkRejectVpcPeeringConnectionResponse = RejectVpcPeeringConnectionResponse
    { _rvpcrReturn = Nothing
    }

-- | Returns true if the request succeeds; otherwise, it returns an error.
rvpcrReturn :: Lens' RejectVpcPeeringConnectionResponse (Maybe Bool)
rvpcrReturn = lens _rvpcrReturn (\s a -> s { _rvpcrReturn = a })

instance FromXML RejectVpcPeeringConnectionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RejectVpcPeeringConnection where
    type Sv RejectVpcPeeringConnection = EC2
    type Rs RejectVpcPeeringConnection = RejectVpcPeeringConnectionResponse

    request = post "RejectVpcPeeringConnection"
    response _ = xmlResponse
