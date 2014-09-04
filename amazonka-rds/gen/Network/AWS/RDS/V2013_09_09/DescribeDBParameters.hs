{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeDBParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the detailed parameter list for a particular DB parameter group.
-- https://rds.amazonaws.com/ ?Action=DescribeDBParameters
-- &DBParameterGroupName=mydbparametergroup &Source=system &MaxRecords=100
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-11T19%3A31%3A42.262Z &AWSAccessKeyId= &Signature=
-- /rdsdbbin/mysql string system false The MySQL installation base directory.
-- static basedir 32768 integer system true The size of the cache to hold the
-- SQL statements for the binary log during a transaction. dynamic
-- 4096-9223372036854775807 binlog_cache_size
-- 8743f2cf-bf41-11de-8c8e-49155882c409.
module Network.AWS.RDS.V2013_09_09.DescribeDBParameters
    (
    -- * Request
      DescribeDBParameters
    -- ** Request constructor
    , mkDescribeDBParametersMessage
    -- ** Request lenses
    , ddbpmDBParameterGroupName
    , ddbpmSource
    , ddbpmMaxRecords
    , ddbpmMarker

    -- * Response
    , DescribeDBParametersResponse
    -- ** Response lenses
    , dbpgdParameters
    , dbpgdMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBParameters' request.
mkDescribeDBParametersMessage :: Text -- ^ 'ddbpmDBParameterGroupName'
                              -> DescribeDBParameters
mkDescribeDBParametersMessage p1 = DescribeDBParameters
    { _ddbpmDBParameterGroupName = p1
    , _ddbpmSource = Nothing
    , _ddbpmMaxRecords = Nothing
    , _ddbpmMarker = Nothing
    }
{-# INLINE mkDescribeDBParametersMessage #-}

data DescribeDBParameters = DescribeDBParameters
    { _ddbpmDBParameterGroupName :: Text
      -- ^ The name of a specific DB parameter group to return details for.
      -- Constraints: Must be 1 to 255 alphanumeric characters First
      -- character must be a letter Cannot end with a hyphen or contain
      -- two consecutive hyphens.
    , _ddbpmSource :: Maybe Text
      -- ^ The parameter types to return. Default: All parameter types
      -- returned Valid Values: user | system | engine-default.
    , _ddbpmMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results may be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _ddbpmMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeDBParameters request. If this parameter is specified, the
      -- response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    } deriving (Show, Generic)

-- | The name of a specific DB parameter group to return details for.
-- Constraints: Must be 1 to 255 alphanumeric characters First character must
-- be a letter Cannot end with a hyphen or contain two consecutive hyphens.
ddbpmDBParameterGroupName :: Lens' DescribeDBParameters (Text)
ddbpmDBParameterGroupName = lens _ddbpmDBParameterGroupName (\s a -> s { _ddbpmDBParameterGroupName = a })
{-# INLINE ddbpmDBParameterGroupName #-}

-- | The parameter types to return. Default: All parameter types returned Valid
-- Values: user | system | engine-default.
ddbpmSource :: Lens' DescribeDBParameters (Maybe Text)
ddbpmSource = lens _ddbpmSource (\s a -> s { _ddbpmSource = a })
{-# INLINE ddbpmSource #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbpmMaxRecords :: Lens' DescribeDBParameters (Maybe Integer)
ddbpmMaxRecords = lens _ddbpmMaxRecords (\s a -> s { _ddbpmMaxRecords = a })
{-# INLINE ddbpmMaxRecords #-}

-- | An optional pagination token provided by a previous DescribeDBParameters
-- request. If this parameter is specified, the response includes only records
-- beyond the marker, up to the value specified by MaxRecords.
ddbpmMarker :: Lens' DescribeDBParameters (Maybe Text)
ddbpmMarker = lens _ddbpmMarker (\s a -> s { _ddbpmMarker = a })
{-# INLINE ddbpmMarker #-}

instance ToQuery DescribeDBParameters where
    toQuery = genericQuery def

data DescribeDBParametersResponse = DescribeDBParametersResponse
    { _dbpgdParameters :: [Parameter]
      -- ^ A list of Parameter values.
    , _dbpgdMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | A list of Parameter values.
dbpgdParameters :: Lens' DescribeDBParametersResponse ([Parameter])
dbpgdParameters = lens _dbpgdParameters (\s a -> s { _dbpgdParameters = a })
{-# INLINE dbpgdParameters #-}

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dbpgdMarker :: Lens' DescribeDBParametersResponse (Maybe Text)
dbpgdMarker = lens _dbpgdMarker (\s a -> s { _dbpgdMarker = a })
{-# INLINE dbpgdMarker #-}

instance FromXML DescribeDBParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDBParameters where
    type Sv DescribeDBParameters = RDS
    type Rs DescribeDBParameters = DescribeDBParametersResponse

    request = post "DescribeDBParameters"
    response _ = xmlResponse

instance AWSPager DescribeDBParameters where
    next rq rs = (\x -> rq { _ddbpmMarker = Just x })
        <$> (_dbpgdMarker rs)
