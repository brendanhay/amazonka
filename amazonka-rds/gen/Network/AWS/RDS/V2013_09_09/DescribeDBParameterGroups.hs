{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeDBParameterGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of DBParameterGroup descriptions. If a DBParameterGroupName
-- is specified, the list will contain only the description of the specified
-- DB parameter group. https://rds.amazonaws.com/
-- ?Action=DescribeDBParameterGroups &DBParameterGroupName=myparamsgroup
-- &MaxRecords=100 &Version=2013-05-15 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T17%3A54%3A32.899Z
-- &AWSAccessKeyId= &Signature= mysql5.1 Default parameter group for mysql5.1
-- default.mysql5.1 mysql5.1 My DB Param Group testdbparamgroup
-- cb8d9bb4-a02a-11df-bd60-c955b7d6e8e0.
module Network.AWS.RDS.V2013_09_09.DescribeDBParameterGroups
    (
    -- * Request
      DescribeDBParameterGroups
    -- ** Request constructor
    , mkDescribeDBParameterGroups
    -- ** Request lenses
    , ddbpg1DBParameterGroupName
    , ddbpg1MaxRecords
    , ddbpg1Marker

    -- * Response
    , DescribeDBParameterGroupsResponse
    -- ** Response lenses
    , ddbpgrsMarker
    , ddbpgrsDBParameterGroups
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | 
data DescribeDBParameterGroups = DescribeDBParameterGroups
    { _ddbpg1DBParameterGroupName :: Maybe Text
    , _ddbpg1MaxRecords :: Maybe Integer
    , _ddbpg1Marker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBParameterGroups' request.
mkDescribeDBParameterGroups :: DescribeDBParameterGroups
mkDescribeDBParameterGroups = DescribeDBParameterGroups
    { _ddbpg1DBParameterGroupName = Nothing
    , _ddbpg1MaxRecords = Nothing
    , _ddbpg1Marker = Nothing
    }

-- | The name of a specific DB parameter group to return details for.
-- Constraints: Must be 1 to 255 alphanumeric characters First character must
-- be a letter Cannot end with a hyphen or contain two consecutive hyphens.
ddbpg1DBParameterGroupName :: Lens' DescribeDBParameterGroups (Maybe Text)
ddbpg1DBParameterGroupName =
    lens _ddbpg1DBParameterGroupName
         (\s a -> s { _ddbpg1DBParameterGroupName = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbpg1MaxRecords :: Lens' DescribeDBParameterGroups (Maybe Integer)
ddbpg1MaxRecords =
    lens _ddbpg1MaxRecords (\s a -> s { _ddbpg1MaxRecords = a })

-- | An optional pagination token provided by a previous
-- DescribeDBParameterGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value specified
-- by MaxRecords.
ddbpg1Marker :: Lens' DescribeDBParameterGroups (Maybe Text)
ddbpg1Marker = lens _ddbpg1Marker (\s a -> s { _ddbpg1Marker = a })

instance ToQuery DescribeDBParameterGroups where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- DescribeDBParameterGroups action.
data DescribeDBParameterGroupsResponse = DescribeDBParameterGroupsResponse
    { _ddbpgrsMarker :: Maybe Text
    , _ddbpgrsDBParameterGroups :: [DBParameterGroup]
    } deriving (Show, Generic)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
ddbpgrsMarker :: Lens' DescribeDBParameterGroupsResponse (Maybe Text)
ddbpgrsMarker = lens _ddbpgrsMarker (\s a -> s { _ddbpgrsMarker = a })

-- | A list of DBParameterGroup instances.
ddbpgrsDBParameterGroups :: Lens' DescribeDBParameterGroupsResponse [DBParameterGroup]
ddbpgrsDBParameterGroups =
    lens _ddbpgrsDBParameterGroups
         (\s a -> s { _ddbpgrsDBParameterGroups = a })

instance FromXML DescribeDBParameterGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDBParameterGroups where
    type Sv DescribeDBParameterGroups = RDS
    type Rs DescribeDBParameterGroups = DescribeDBParameterGroupsResponse

    request = post "DescribeDBParameterGroups"
    response _ = xmlResponse

instance AWSPager DescribeDBParameterGroups where
    next rq rs = (\x -> rq & ddbpg1Marker ?~ x)
        <$> (rs ^. ddbpgrsMarker)
