{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.Redshift.V2012_12_01.CreateClusterSubnetGroup
    (
    -- * Request
      CreateClusterSubnetGroup
    -- ** Request constructor
    , mkCreateClusterSubnetGroup
    -- ** Request lenses
    , ccsg1ClusterSubnetGroupName
    , ccsg1Description
    , ccsg1SubnetIds

    -- * Response
    , CreateClusterSubnetGroupResponse
    -- ** Response constructor
    , mkCreateClusterSubnetGroupResponse
    -- ** Response lenses
    , ccsgrrClusterSubnetGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
data CreateClusterSubnetGroup = CreateClusterSubnetGroup
    { _ccsg1ClusterSubnetGroupName :: Text
    , _ccsg1Description :: Text
    , _ccsg1SubnetIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateClusterSubnetGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterSubnetGroupName ::@ @Text@
--
-- * @Description ::@ @Text@
--
-- * @SubnetIds ::@ @[Text]@
--
mkCreateClusterSubnetGroup :: Text -- ^ 'ccsg1ClusterSubnetGroupName'
                           -> Text -- ^ 'ccsg1Description'
                           -> [Text] -- ^ 'ccsg1SubnetIds'
                           -> CreateClusterSubnetGroup
mkCreateClusterSubnetGroup p1 p2 p3 = CreateClusterSubnetGroup
    { _ccsg1ClusterSubnetGroupName = p1
    , _ccsg1Description = p2
    , _ccsg1SubnetIds = p3
    }

-- | The name for the subnet group. Amazon Redshift stores the value as a
-- lowercase string. Constraints: Must contain no more than 255 alphanumeric
-- characters or hyphens. Must not be "Default". Must be unique for all subnet
-- groups that are created by your AWS account. Example: examplesubnetgroup.
ccsg1ClusterSubnetGroupName :: Lens' CreateClusterSubnetGroup Text
ccsg1ClusterSubnetGroupName =
    lens _ccsg1ClusterSubnetGroupName
         (\s a -> s { _ccsg1ClusterSubnetGroupName = a })

-- | A description for the subnet group.
ccsg1Description :: Lens' CreateClusterSubnetGroup Text
ccsg1Description =
    lens _ccsg1Description (\s a -> s { _ccsg1Description = a })

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
ccsg1SubnetIds :: Lens' CreateClusterSubnetGroup [Text]
ccsg1SubnetIds = lens _ccsg1SubnetIds (\s a -> s { _ccsg1SubnetIds = a })

instance ToQuery CreateClusterSubnetGroup where
    toQuery = genericQuery def

newtype CreateClusterSubnetGroupResponse = CreateClusterSubnetGroupResponse
    { _ccsgrrClusterSubnetGroup :: Maybe ClusterSubnetGroup
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateClusterSubnetGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterSubnetGroup ::@ @Maybe ClusterSubnetGroup@
--
mkCreateClusterSubnetGroupResponse :: CreateClusterSubnetGroupResponse
mkCreateClusterSubnetGroupResponse = CreateClusterSubnetGroupResponse
    { _ccsgrrClusterSubnetGroup = Nothing
    }

-- | Describes a subnet group.
ccsgrrClusterSubnetGroup :: Lens' CreateClusterSubnetGroupResponse (Maybe ClusterSubnetGroup)
ccsgrrClusterSubnetGroup =
    lens _ccsgrrClusterSubnetGroup
         (\s a -> s { _ccsgrrClusterSubnetGroup = a })

instance FromXML CreateClusterSubnetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateClusterSubnetGroup where
    type Sv CreateClusterSubnetGroup = Redshift
    type Rs CreateClusterSubnetGroup = CreateClusterSubnetGroupResponse

    request = post "CreateClusterSubnetGroup"
    response _ = xmlResponse
