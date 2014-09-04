{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.RejectVpcPeeringConnection
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
module Network.AWS.EC2.V2014_06_15.RejectVpcPeeringConnection
    (
    -- * Request
      RejectVpcPeeringConnection
    -- ** Request constructor
    , mkRejectVpcPeeringConnectionRequest
    -- ** Request lenses
    , rvpcrVpcPeeringConnectionId

    -- * Response
    , RejectVpcPeeringConnectionResponse
    -- ** Response lenses
    , rvpcsReturn
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RejectVpcPeeringConnection' request.
mkRejectVpcPeeringConnectionRequest :: Text -- ^ 'rvpcrVpcPeeringConnectionId'
                                    -> RejectVpcPeeringConnection
mkRejectVpcPeeringConnectionRequest p1 = RejectVpcPeeringConnection
    { _rvpcrVpcPeeringConnectionId = p1
    }
{-# INLINE mkRejectVpcPeeringConnectionRequest #-}

newtype RejectVpcPeeringConnection = RejectVpcPeeringConnection
    { _rvpcrVpcPeeringConnectionId :: Text
      -- ^ The ID of the VPC peering connection.
    } deriving (Show, Generic)

-- | The ID of the VPC peering connection.
rvpcrVpcPeeringConnectionId :: Lens' RejectVpcPeeringConnection (Text)
rvpcrVpcPeeringConnectionId = lens _rvpcrVpcPeeringConnectionId (\s a -> s { _rvpcrVpcPeeringConnectionId = a })
{-# INLINE rvpcrVpcPeeringConnectionId #-}

instance ToQuery RejectVpcPeeringConnection where
    toQuery = genericQuery def

newtype RejectVpcPeeringConnectionResponse = RejectVpcPeeringConnectionResponse
    { _rvpcsReturn :: Maybe Bool
      -- ^ Returns true if the request succeeds; otherwise, it returns an
      -- error.
    } deriving (Show, Generic)

-- | Returns true if the request succeeds; otherwise, it returns an error.
rvpcsReturn :: Lens' RejectVpcPeeringConnectionResponse (Maybe Bool)
rvpcsReturn = lens _rvpcsReturn (\s a -> s { _rvpcsReturn = a })
{-# INLINE rvpcsReturn #-}

instance FromXML RejectVpcPeeringConnectionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RejectVpcPeeringConnection where
    type Sv RejectVpcPeeringConnection = EC2
    type Rs RejectVpcPeeringConnection = RejectVpcPeeringConnectionResponse

    request = post "RejectVpcPeeringConnection"
    response _ = xmlResponse
