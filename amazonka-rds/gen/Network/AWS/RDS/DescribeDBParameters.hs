{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeDBParameters
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
module Network.AWS.RDS.DescribeDBParameters
    (
    -- * Request
      DescribeDBParameters
    -- ** Request constructor
    , describeDBParameters
    -- ** Request lenses
    , ddbpDBParameterGroupName
    , ddbpSource
    , ddbpMaxRecords
    , ddbpMarker

    -- * Response
    , DescribeDBParametersResponse
    -- ** Response constructor
    , describeDBParametersResponse
    -- ** Response lenses
    , ddbprParameters
    , ddbprMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

data DescribeDBParameters = DescribeDBParameters
    { _ddbpDBParameterGroupName :: Text
    , _ddbpSource :: Maybe Text
    , _ddbpMaxRecords :: Maybe Integer
    , _ddbpMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBParameters' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBParameterGroupName ::@ @Text@
--
-- * @Source ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
describeDBParameters :: Text -- ^ 'ddbpDBParameterGroupName'
                     -> DescribeDBParameters
describeDBParameters p1 = DescribeDBParameters
    { _ddbpDBParameterGroupName = p1
    , _ddbpSource = Nothing
    , _ddbpMaxRecords = Nothing
    , _ddbpMarker = Nothing
    }

-- | The name of a specific DB parameter group to return details for.
-- Constraints: Must be 1 to 255 alphanumeric characters First character must
-- be a letter Cannot end with a hyphen or contain two consecutive hyphens.
ddbpDBParameterGroupName :: Lens' DescribeDBParameters Text
ddbpDBParameterGroupName =
    lens _ddbpDBParameterGroupName
         (\s a -> s { _ddbpDBParameterGroupName = a })

-- | The parameter types to return. Default: All parameter types returned Valid
-- Values: user | system | engine-default.
ddbpSource :: Lens' DescribeDBParameters (Maybe Text)
ddbpSource = lens _ddbpSource (\s a -> s { _ddbpSource = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbpMaxRecords :: Lens' DescribeDBParameters (Maybe Integer)
ddbpMaxRecords = lens _ddbpMaxRecords (\s a -> s { _ddbpMaxRecords = a })

-- | An optional pagination token provided by a previous DescribeDBParameters
-- request. If this parameter is specified, the response includes only records
-- beyond the marker, up to the value specified by MaxRecords.
ddbpMarker :: Lens' DescribeDBParameters (Maybe Text)
ddbpMarker = lens _ddbpMarker (\s a -> s { _ddbpMarker = a })

instance ToQuery DescribeDBParameters where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the DescribeDBParameters
-- action.
data DescribeDBParametersResponse = DescribeDBParametersResponse
    { _ddbprParameters :: [Parameter]
    , _ddbprMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBParametersResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Parameters ::@ @[Parameter]@
--
-- * @Marker ::@ @Maybe Text@
--
describeDBParametersResponse :: DescribeDBParametersResponse
describeDBParametersResponse = DescribeDBParametersResponse
    { _ddbprParameters = mempty
    , _ddbprMarker = Nothing
    }

-- | A list of Parameter values.
ddbprParameters :: Lens' DescribeDBParametersResponse [Parameter]
ddbprParameters = lens _ddbprParameters (\s a -> s { _ddbprParameters = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
ddbprMarker :: Lens' DescribeDBParametersResponse (Maybe Text)
ddbprMarker = lens _ddbprMarker (\s a -> s { _ddbprMarker = a })

instance FromXML DescribeDBParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDBParameters where
    type Sv DescribeDBParameters = RDS
    type Rs DescribeDBParameters = DescribeDBParametersResponse

    request = post "DescribeDBParameters"
    response _ = xmlResponse

instance AWSPager DescribeDBParameters where
    next rq rs = (\x -> rq & ddbpMarker ?~ x)
        <$> (rs ^. ddbprMarker)
