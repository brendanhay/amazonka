{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.CreateClusterSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new Amazon Redshift subnet group. You must provide a list of one
-- or more subnets in your existing Amazon Virtual Private Cloud (Amazon VPC)
-- when creating Amazon Redshift subnet group. For information about subnet
-- groups, go to Amazon Redshift Cluster Subnet Groups in the Amazon Redshift
-- Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=CreateClusterSubnetGroup &ClusterSubnetGroupName=mysubnetgroup1
-- &Description=My subnet group 1 &SubnetIds.member.1=subnet-756a591f
-- &SubnetIds.member.1=subnet-716a591b &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130129/us-east-1/redshift/aws4_request
-- &x-amz-date=20130129T192820Z
-- &x-amz-signedheaders=content-type;host;x-amz-date vpc-796a5913 My subnet
-- group 1 mysubnetgroup1 Complete Active subnet-756a591f us-east-1c
-- 0a60660f-6a4a-11e2-aad2-71d00c36728e.
module Network.AWS.Redshift.V2012_12_01.CreateClusterSubnetGroup where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

data CreateClusterSubnetGroup = CreateClusterSubnetGroup
    { _ccsgmClusterSubnetGroupName :: Text
      -- ^ The name for the subnet group. Amazon Redshift stores the value
      -- as a lowercase string. Constraints: Must contain no more than 255
      -- alphanumeric characters or hyphens. Must not be "Default". Must
      -- be unique for all subnet groups that are created by your AWS
      -- account. Example: examplesubnetgroup.
    , _ccsgmDescription :: Text
      -- ^ A description for the subnet group.
    , _ccsgmSubnetIds :: [Text]
      -- ^ An array of VPC subnet IDs. A maximum of 20 subnets can be
      -- modified in a single request.
    } deriving (Show, Generic)

makeLenses ''CreateClusterSubnetGroup

instance ToQuery CreateClusterSubnetGroup where
    toQuery = genericQuery def

data CreateClusterSubnetGroupResponse = CreateClusterSubnetGroupResponse
    { _csgwClusterSubnetGroup :: Maybe ClusterSubnetGroup
      -- ^ Describes a subnet group.
    } deriving (Show, Generic)

makeLenses ''CreateClusterSubnetGroupResponse

instance FromXML CreateClusterSubnetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateClusterSubnetGroup where
    type Sv CreateClusterSubnetGroup = Redshift
    type Rs CreateClusterSubnetGroup = CreateClusterSubnetGroupResponse

    request = post "CreateClusterSubnetGroup"
    response _ = xmlResponse
