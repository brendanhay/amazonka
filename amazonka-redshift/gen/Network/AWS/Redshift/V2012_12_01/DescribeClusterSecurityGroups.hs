{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeClusterSecurityGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about Amazon Redshift security groups. If the name of a
-- security group is specified, the response will contain only information
-- about only that security group. For information about managing security
-- groups, go to Amazon Redshift Cluster Security Groups in the Amazon
-- Redshift Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeClusterSecurityGroups &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T010237Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 0.0.0.0/0 authorized
-- default default my security group securitygroup1
-- 947a8305-64f8-11e2-bec0-17624ad140dd.
module Network.AWS.Redshift.V2012_12_01.DescribeClusterSecurityGroups where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeClusterSecurityGroups' request.
describeClusterSecurityGroups :: DescribeClusterSecurityGroups
describeClusterSecurityGroups = DescribeClusterSecurityGroups
    { _dcsgnMaxRecords = Nothing
    , _dcsgnClusterSecurityGroupName = Nothing
    , _dcsgnMarker = Nothing
    }

data DescribeClusterSecurityGroups = DescribeClusterSecurityGroups
    { _dcsgnMaxRecords :: Maybe Integer
      -- ^ The maximum number of response records to return in each call. If
      -- the number of remaining response records exceeds the specified
      -- MaxRecords value, a value is returned in a marker field of the
      -- response. You can retrieve the next set of records by retrying
      -- the command with the returned marker value. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , _dcsgnClusterSecurityGroupName :: Maybe Text
      -- ^ The name of a cluster security group for which you are requesting
      -- details. You can specify either the Marker parameter or a
      -- ClusterSecurityGroupName parameter, but not both. Example:
      -- securitygroup1.
    , _dcsgnMarker :: Maybe Text
      -- ^ An optional parameter that specifies the starting point to return
      -- a set of response records. When the results of a
      -- DescribeClusterSecurityGroups request exceed the value specified
      -- in MaxRecords, AWS returns a value in the Marker field of the
      -- response. You can retrieve the next set of response records by
      -- providing the returned marker value in the Marker parameter and
      -- retrying the request. Constraints: You can specify either the
      -- ClusterSecurityGroupName parameter or the Marker parameter, but
      -- not both.
    } deriving (Show, Generic)

makeLenses ''DescribeClusterSecurityGroups

instance ToQuery DescribeClusterSecurityGroups where
    toQuery = genericQuery def

data DescribeClusterSecurityGroupsResponse = DescribeClusterSecurityGroupsResponse
    { _csgnClusterSecurityGroups :: [ClusterSecurityGroup]
      -- ^ A list of ClusterSecurityGroup instances.
    , _csgnMarker :: Maybe Text
      -- ^ A value that indicates the starting point for the next set of
      -- response records in a subsequent request. If a value is returned
      -- in a response, you can retrieve the next set of records by
      -- providing this returned marker value in the Marker parameter and
      -- retrying the command. If the Marker field is empty, all response
      -- records have been retrieved for the request.
    } deriving (Show, Generic)

makeLenses ''DescribeClusterSecurityGroupsResponse

instance FromXML DescribeClusterSecurityGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeClusterSecurityGroups where
    type Sv DescribeClusterSecurityGroups = Redshift
    type Rs DescribeClusterSecurityGroups = DescribeClusterSecurityGroupsResponse

    request = post "DescribeClusterSecurityGroups"
    response _ = xmlResponse

instance AWSPager DescribeClusterSecurityGroups where
    next rq rs = (\x -> rq { _dcsgnMarker = Just x })
        <$> (_csgnMarker rs)
