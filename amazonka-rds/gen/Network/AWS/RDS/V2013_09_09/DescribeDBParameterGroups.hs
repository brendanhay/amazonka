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
    , describeDBParameterGroups
    -- ** Request lenses
    , ddbpgnMaxRecords
    , ddbpgnDBParameterGroupName
    , ddbpgnMarker

    -- * Response
    , DescribeDBParameterGroupsResponse
    -- ** Response lenses
    , dbpgmDBParameterGroups
    , dbpgmMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeDBParameterGroups' request.
describeDBParameterGroups :: DescribeDBParameterGroups
describeDBParameterGroups = DescribeDBParameterGroups
    { _ddbpgnMaxRecords = Nothing
    , _ddbpgnDBParameterGroupName = Nothing
    , _ddbpgnMarker = Nothing
    }

data DescribeDBParameterGroups = DescribeDBParameterGroups
    { _ddbpgnMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results may be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _ddbpgnDBParameterGroupName :: Maybe Text
      -- ^ The name of a specific DB parameter group to return details for.
      -- Constraints: Must be 1 to 255 alphanumeric characters First
      -- character must be a letter Cannot end with a hyphen or contain
      -- two consecutive hyphens.
    , _ddbpgnMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeDBParameterGroups request. If this parameter is
      -- specified, the response includes only records beyond the marker,
      -- up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbpgnMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeDBParameterGroups
    -> f DescribeDBParameterGroups
ddbpgnMaxRecords f x =
    (\y -> x { _ddbpgnMaxRecords = y })
       <$> f (_ddbpgnMaxRecords x)
{-# INLINE ddbpgnMaxRecords #-}

-- | The name of a specific DB parameter group to return details for.
-- Constraints: Must be 1 to 255 alphanumeric characters First character must
-- be a letter Cannot end with a hyphen or contain two consecutive hyphens.
ddbpgnDBParameterGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeDBParameterGroups
    -> f DescribeDBParameterGroups
ddbpgnDBParameterGroupName f x =
    (\y -> x { _ddbpgnDBParameterGroupName = y })
       <$> f (_ddbpgnDBParameterGroupName x)
{-# INLINE ddbpgnDBParameterGroupName #-}

-- | An optional pagination token provided by a previous
-- DescribeDBParameterGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value specified
-- by MaxRecords.
ddbpgnMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeDBParameterGroups
    -> f DescribeDBParameterGroups
ddbpgnMarker f x =
    (\y -> x { _ddbpgnMarker = y })
       <$> f (_ddbpgnMarker x)
{-# INLINE ddbpgnMarker #-}

instance ToQuery DescribeDBParameterGroups where
    toQuery = genericQuery def

data DescribeDBParameterGroupsResponse = DescribeDBParameterGroupsResponse
    { _dbpgmDBParameterGroups :: [DBParameterGroup]
      -- ^ A list of DBParameterGroup instances.
    , _dbpgmMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | A list of DBParameterGroup instances.
dbpgmDBParameterGroups
    :: Functor f
    => ([DBParameterGroup]
    -> f ([DBParameterGroup]))
    -> DescribeDBParameterGroupsResponse
    -> f DescribeDBParameterGroupsResponse
dbpgmDBParameterGroups f x =
    (\y -> x { _dbpgmDBParameterGroups = y })
       <$> f (_dbpgmDBParameterGroups x)
{-# INLINE dbpgmDBParameterGroups #-}

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dbpgmMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeDBParameterGroupsResponse
    -> f DescribeDBParameterGroupsResponse
dbpgmMarker f x =
    (\y -> x { _dbpgmMarker = y })
       <$> f (_dbpgmMarker x)
{-# INLINE dbpgmMarker #-}

instance FromXML DescribeDBParameterGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDBParameterGroups where
    type Sv DescribeDBParameterGroups = RDS
    type Rs DescribeDBParameterGroups = DescribeDBParameterGroupsResponse

    request = post "DescribeDBParameterGroups"
    response _ = xmlResponse

instance AWSPager DescribeDBParameterGroups where
    next rq rs = (\x -> rq { _ddbpgnMarker = Just x })
        <$> (_dbpgmMarker rs)
