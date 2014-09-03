{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateNetworkAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a network ACL in a VPC. Network ACLs provide an optional layer of
-- security (in addition to security groups) for the instances in your VPC.
-- For more information about network ACLs, see Network ACLs in the Amazon
-- Virtual Private Cloud User Guide. Example This example creates a network
-- ACL in the specified VPC. The response includes a default entry for egress,
-- and another for ingress, each with a very high rule number. These are the
-- last entries we process to decide whether traffic is allowed in or out of
-- an associated subnet. If the traffic doesn't match any rules with a lower
-- rule number, then these default entries ultimately deny the traffic.
-- https://ec2.amazonaws.com/?Action=CreateNetworkAcl &amp;VpcId=vpc-11ad4878
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE acl-5fb85d36
-- vpc-11ad4878 false 32767 all deny true 0.0.0.0/0 32767 all deny false
-- 0.0.0.0/0.
module Network.AWS.EC2.V2014_06_15.CreateNetworkAcl where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateNetworkAcl' request.
createNetworkAcl :: Text -- ^ '_cnarVpcId'
                 -> CreateNetworkAcl
createNetworkAcl p1 = CreateNetworkAcl
    { _cnarVpcId = p1
    }

data CreateNetworkAcl = CreateNetworkAcl
    { _cnarVpcId :: Text
      -- ^ The ID of the VPC.
    } deriving (Show, Generic)

makeLenses ''CreateNetworkAcl

instance ToQuery CreateNetworkAcl where
    toQuery = genericQuery def

data CreateNetworkAclResponse = CreateNetworkAclResponse
    { _cnasNetworkAcl :: Maybe NetworkAcl
      -- ^ Information about the network ACL.
    } deriving (Show, Generic)

makeLenses ''CreateNetworkAclResponse

instance FromXML CreateNetworkAclResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateNetworkAcl where
    type Sv CreateNetworkAcl = EC2
    type Rs CreateNetworkAcl = CreateNetworkAclResponse

    request = post "CreateNetworkAcl"
    response _ = xmlResponse
