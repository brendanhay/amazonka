{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeDBSecurityGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of DBSecurityGroup descriptions. If a DBSecurityGroupName is
-- specified, the list will contain only the descriptions of the specified DB
-- security group. https://rds.amazonaws.com/ ?Action=DescribeDBSecurityGroups
-- &Version=2013-05-15 &MaxRecords=100 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T19%3A40%3A19.926Z
-- &AWSAccessKeyId= &Signature= authorized myec2securitygroup 054794666394
-- default 127.0.0.1/30 authorized 621567473609 default vpc-1ab2c3d4 My new
-- DBSecurityGroup 192.168.1.1/24 authorized 621567473609 mydbsecuritygroup
-- vpc-1ab2c3d5 My new DBSecurityGroup 621567473609 mydbsecuritygroup4
-- vpc-1ab2c3d6 bbdad154-bf42-11de-86a4-97241dfaadff.
module Network.AWS.RDS.V2013_09_09.DescribeDBSecurityGroups
    (
    -- * Request
      DescribeDBSecurityGroups
    -- ** Request constructor
    , mkDescribeDBSecurityGroupsMessage
    -- ** Request lenses
    , ddbsgoDBSecurityGroupName
    , ddbsgoMaxRecords
    , ddbsgoMarker

    -- * Response
    , DescribeDBSecurityGroupsResponse
    -- ** Response lenses
    , dbsgrMarker
    , dbsgrDBSecurityGroups
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBSecurityGroups' request.
mkDescribeDBSecurityGroupsMessage :: DescribeDBSecurityGroups
mkDescribeDBSecurityGroupsMessage = DescribeDBSecurityGroups
    { _ddbsgoDBSecurityGroupName = Nothing
    , _ddbsgoMaxRecords = Nothing
    , _ddbsgoMarker = Nothing
    }
{-# INLINE mkDescribeDBSecurityGroupsMessage #-}

data DescribeDBSecurityGroups = DescribeDBSecurityGroups
    { _ddbsgoDBSecurityGroupName :: Maybe Text
      -- ^ The name of the DB security group to return details for.
    , _ddbsgoMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results may be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _ddbsgoMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeDBSecurityGroups request. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the
      -- value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The name of the DB security group to return details for.
ddbsgoDBSecurityGroupName :: Lens' DescribeDBSecurityGroups (Maybe Text)
ddbsgoDBSecurityGroupName = lens _ddbsgoDBSecurityGroupName (\s a -> s { _ddbsgoDBSecurityGroupName = a })
{-# INLINE ddbsgoDBSecurityGroupName #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbsgoMaxRecords :: Lens' DescribeDBSecurityGroups (Maybe Integer)
ddbsgoMaxRecords = lens _ddbsgoMaxRecords (\s a -> s { _ddbsgoMaxRecords = a })
{-# INLINE ddbsgoMaxRecords #-}

-- | An optional pagination token provided by a previous
-- DescribeDBSecurityGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value specified
-- by MaxRecords.
ddbsgoMarker :: Lens' DescribeDBSecurityGroups (Maybe Text)
ddbsgoMarker = lens _ddbsgoMarker (\s a -> s { _ddbsgoMarker = a })
{-# INLINE ddbsgoMarker #-}

instance ToQuery DescribeDBSecurityGroups where
    toQuery = genericQuery def

data DescribeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse
    { _dbsgrMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    , _dbsgrDBSecurityGroups :: [DBSecurityGroup]
      -- ^ A list of DBSecurityGroup instances.
    } deriving (Show, Generic)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dbsgrMarker :: Lens' DescribeDBSecurityGroupsResponse (Maybe Text)
dbsgrMarker = lens _dbsgrMarker (\s a -> s { _dbsgrMarker = a })
{-# INLINE dbsgrMarker #-}

-- | A list of DBSecurityGroup instances.
dbsgrDBSecurityGroups :: Lens' DescribeDBSecurityGroupsResponse ([DBSecurityGroup])
dbsgrDBSecurityGroups = lens _dbsgrDBSecurityGroups (\s a -> s { _dbsgrDBSecurityGroups = a })
{-# INLINE dbsgrDBSecurityGroups #-}

instance FromXML DescribeDBSecurityGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDBSecurityGroups where
    type Sv DescribeDBSecurityGroups = RDS
    type Rs DescribeDBSecurityGroups = DescribeDBSecurityGroupsResponse

    request = post "DescribeDBSecurityGroups"
    response _ = xmlResponse

instance AWSPager DescribeDBSecurityGroups where
    next rq rs = (\x -> rq { _ddbsgoMarker = Just x })
        <$> (_dbsgrMarker rs)
