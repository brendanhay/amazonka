{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.RDS.DescribeDBInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about provisioned RDS instances. This API supports
-- pagination.
module Network.AWS.RDS.DescribeDBInstances
    (
    -- * Request
      DescribeDBInstances
    -- ** Request constructor
    , describeDBInstances
    -- ** Request lenses
    , ddbi1DBInstanceIdentifier
    , ddbi1Filters
    , ddbi1Marker
    , ddbi1MaxRecords

    -- * Response
    , DescribeDBInstancesResponse
    -- ** Response constructor
    , describeDBInstancesResponse
    -- ** Response lenses
    , ddbirDBInstances
    , ddbirMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeDBInstances = DescribeDBInstances
    { _ddbi1DBInstanceIdentifier :: Maybe Text
    , _ddbi1Filters              :: [Filter]
    , _ddbi1Marker               :: Maybe Text
    , _ddbi1MaxRecords           :: Maybe Int
    } deriving (Eq, Show, Generic)

-- | 'DescribeDBInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbi1DBInstanceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'ddbi1Filters' @::@ ['Filter']
--
-- * 'ddbi1Marker' @::@ 'Maybe' 'Text'
--
-- * 'ddbi1MaxRecords' @::@ 'Maybe' 'Int'
--
describeDBInstances :: DescribeDBInstances
describeDBInstances = DescribeDBInstances
    { _ddbi1DBInstanceIdentifier = Nothing
    , _ddbi1Filters              = mempty
    , _ddbi1MaxRecords           = Nothing
    , _ddbi1Marker               = Nothing
    }

-- | The user-supplied instance identifier. If this parameter is specified,
-- information from only the specific DB instance is returned. This
-- parameter isn't case sensitive. Constraints: Must contain from 1 to 63
-- alphanumeric characters or hyphens First character must be a letter
-- Cannot end with a hyphen or contain two consecutive hyphens.
ddbi1DBInstanceIdentifier :: Lens' DescribeDBInstances (Maybe Text)
ddbi1DBInstanceIdentifier =
    lens _ddbi1DBInstanceIdentifier
        (\s a -> s { _ddbi1DBInstanceIdentifier = a })

-- | This parameter is not currently supported.
ddbi1Filters :: Lens' DescribeDBInstances [Filter]
ddbi1Filters = lens _ddbi1Filters (\s a -> s { _ddbi1Filters = a })

-- | An optional pagination token provided by a previous DescribeDBInstances
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by MaxRecords .
ddbi1Marker :: Lens' DescribeDBInstances (Maybe Text)
ddbi1Marker = lens _ddbi1Marker (\s a -> s { _ddbi1Marker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbi1MaxRecords :: Lens' DescribeDBInstances (Maybe Int)
ddbi1MaxRecords = lens _ddbi1MaxRecords (\s a -> s { _ddbi1MaxRecords = a })

instance ToQuery DescribeDBInstances

instance ToPath DescribeDBInstances where
    toPath = const "/"

data DescribeDBInstancesResponse = DescribeDBInstancesResponse
    { _ddbirDBInstances :: [DBInstance]
    , _ddbirMarker      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeDBInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbirDBInstances' @::@ ['DBInstance']
--
-- * 'ddbirMarker' @::@ 'Maybe' 'Text'
--
describeDBInstancesResponse :: DescribeDBInstancesResponse
describeDBInstancesResponse = DescribeDBInstancesResponse
    { _ddbirMarker      = Nothing
    , _ddbirDBInstances = mempty
    }

-- | A list of DBInstance instances.
ddbirDBInstances :: Lens' DescribeDBInstancesResponse [DBInstance]
ddbirDBInstances = lens _ddbirDBInstances (\s a -> s { _ddbirDBInstances = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords .
ddbirMarker :: Lens' DescribeDBInstancesResponse (Maybe Text)
ddbirMarker = lens _ddbirMarker (\s a -> s { _ddbirMarker = a })

instance AWSRequest DescribeDBInstances where
    type Sv DescribeDBInstances = RDS
    type Rs DescribeDBInstances = DescribeDBInstancesResponse

    request  = post "DescribeDBInstances"
    response = xmlResponse $ \h x -> DescribeDBInstancesResponse
        <$> x %| "DBInstances"
        <*> x %| "Marker"
