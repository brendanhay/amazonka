{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.RejectVpcPeeringConnection
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
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/RejectVpcPeeringConnectionResponse&gt;.
module Network.AWS.EC2.V2014_05_01.RejectVpcPeeringConnection where

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

data RejectVpcPeeringConnection = RejectVpcPeeringConnection
    { _rvpcrDryRun :: Bool
      -- ^ 
    , _rvpcrVpcPeeringConnectionId :: Text
      -- ^ The ID of the VPC peering connection.
    } deriving (Generic)

instance ToQuery RejectVpcPeeringConnection where
    toQuery = genericToQuery def

instance AWSRequest RejectVpcPeeringConnection where
    type Sv RejectVpcPeeringConnection = EC2
    type Rs RejectVpcPeeringConnection = RejectVpcPeeringConnectionResponse

    request = post "RejectVpcPeeringConnection"

    response _ = xmlResponse

data RejectVpcPeeringConnectionResponse = RejectVpcPeeringConnectionResponse
    { _rvpcsReturn :: Maybe Bool
      -- ^ Returns true if the request succeeds; otherwise, it returns an
      -- error.
    } deriving (Generic)

instance FromXML RejectVpcPeeringConnectionResponse where
    fromXMLOptions = xmlOptions
