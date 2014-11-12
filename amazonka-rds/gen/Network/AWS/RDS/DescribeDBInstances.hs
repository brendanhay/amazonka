{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
      DescribeDBInstancesMessage
    -- ** Request constructor
    , describeDBInstancesMessage
    -- ** Request lenses
    , ddbimDBInstanceIdentifier
    , ddbimFilters
    , ddbimMarker
    , ddbimMaxRecords

    -- * Response
    , DBInstanceMessage
    -- ** Response constructor
    , dbinstanceMessage
    -- ** Response lenses
    , dbimDBInstances
    , dbimMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DescribeDBInstancesMessage = DescribeDBInstancesMessage
    { _ddbimDBInstanceIdentifier :: Maybe Text
    , _ddbimFilters              :: [Filter]
    , _ddbimMarker               :: Maybe Text
    , _ddbimMaxRecords           :: Maybe Int
    } (Eq, Show, Generic)

-- | 'DescribeDBInstancesMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbimDBInstanceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'ddbimFilters' @::@ ['Filter']
--
-- * 'ddbimMarker' @::@ 'Maybe' 'Text'
--
-- * 'ddbimMaxRecords' @::@ 'Maybe' 'Int'
--
describeDBInstancesMessage :: DescribeDBInstancesMessage
describeDBInstancesMessage = DescribeDBInstancesMessage
    { _ddbimDBInstanceIdentifier = Nothing
    , _ddbimFilters              = mempty
    , _ddbimMaxRecords           = Nothing
    , _ddbimMarker               = Nothing
    }

-- | The user-supplied instance identifier. If this parameter is specified,
-- information from only the specific DB instance is returned. This
-- parameter isn't case sensitive. Constraints: Must contain from 1 to 63
-- alphanumeric characters or hyphens First character must be a letter
-- Cannot end with a hyphen or contain two consecutive hyphens.
ddbimDBInstanceIdentifier :: Lens' DescribeDBInstancesMessage (Maybe Text)
ddbimDBInstanceIdentifier =
    lens _ddbimDBInstanceIdentifier
        (\s a -> s { _ddbimDBInstanceIdentifier = a })

-- | This parameter is not currently supported.
ddbimFilters :: Lens' DescribeDBInstancesMessage [Filter]
ddbimFilters = lens _ddbimFilters (\s a -> s { _ddbimFilters = a })

-- | An optional pagination token provided by a previous DescribeDBInstances
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by MaxRecords .
ddbimMarker :: Lens' DescribeDBInstancesMessage (Maybe Text)
ddbimMarker = lens _ddbimMarker (\s a -> s { _ddbimMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbimMaxRecords :: Lens' DescribeDBInstancesMessage (Maybe Int)
ddbimMaxRecords = lens _ddbimMaxRecords (\s a -> s { _ddbimMaxRecords = a })
instance ToQuery DescribeDBInstancesMessage

instance ToPath DescribeDBInstancesMessage where
    toPath = const "/"

data DBInstanceMessage = DBInstanceMessage
    { _dbimDBInstances :: [DBInstance]
    , _dbimMarker      :: Maybe Text
    } (Eq, Show, Generic)

-- | 'DBInstanceMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbimDBInstances' @::@ ['DBInstance']
--
-- * 'dbimMarker' @::@ 'Maybe' 'Text'
--
dbinstanceMessage :: DBInstanceMessage
dbinstanceMessage = DBInstanceMessage
    { _dbimMarker      = Nothing
    , _dbimDBInstances = mempty
    }

-- | A list of DBInstance instances.
dbimDBInstances :: Lens' DBInstanceMessage [DBInstance]
dbimDBInstances = lens _dbimDBInstances (\s a -> s { _dbimDBInstances = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords .
dbimMarker :: Lens' DBInstanceMessage (Maybe Text)
dbimMarker = lens _dbimMarker (\s a -> s { _dbimMarker = a })

instance FromXML DBInstanceMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBInstanceMessage"

instance AWSRequest DescribeDBInstancesMessage where
    type Sv DescribeDBInstancesMessage = RDS
    type Rs DescribeDBInstancesMessage = DBInstanceMessage

    request  = post "DescribeDBInstances"
    response = xmlResponse $ \h x -> DBInstanceMessage
        <$> x %| "DBInstances"
        <*> x %| "Marker"
