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

-- Module      : Network.AWS.RDS.DescribeDBParameterGroups
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
-- DB parameter group.
module Network.AWS.RDS.DescribeDBParameterGroups
    (
    -- * Request
      DescribeDBParameterGroupsMessage
    -- ** Request constructor
    , describeDBParameterGroupsMessage
    -- ** Request lenses
    , ddbpgmDBParameterGroupName
    , ddbpgmFilters
    , ddbpgmMarker
    , ddbpgmMaxRecords

    -- * Response
    , DBParameterGroupsMessage
    -- ** Response constructor
    , dbparameterGroupsMessage
    -- ** Response lenses
    , dbpgmDBParameterGroups
    , dbpgmMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DescribeDBParameterGroupsMessage = DescribeDBParameterGroupsMessage
    { _ddbpgmDBParameterGroupName :: Maybe Text
    , _ddbpgmFilters              :: [Filter]
    , _ddbpgmMarker               :: Maybe Text
    , _ddbpgmMaxRecords           :: Maybe Int
    } deriving (Eq, Show, Generic)

-- | 'DescribeDBParameterGroupsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbpgmDBParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ddbpgmFilters' @::@ ['Filter']
--
-- * 'ddbpgmMarker' @::@ 'Maybe' 'Text'
--
-- * 'ddbpgmMaxRecords' @::@ 'Maybe' 'Int'
--
describeDBParameterGroupsMessage :: DescribeDBParameterGroupsMessage
describeDBParameterGroupsMessage = DescribeDBParameterGroupsMessage
    { _ddbpgmDBParameterGroupName = Nothing
    , _ddbpgmFilters              = mempty
    , _ddbpgmMaxRecords           = Nothing
    , _ddbpgmMarker               = Nothing
    }

-- | The name of a specific DB parameter group to return details for.
-- Constraints: Must be 1 to 255 alphanumeric characters First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
ddbpgmDBParameterGroupName :: Lens' DescribeDBParameterGroupsMessage (Maybe Text)
ddbpgmDBParameterGroupName =
    lens _ddbpgmDBParameterGroupName
        (\s a -> s { _ddbpgmDBParameterGroupName = a })

-- | This parameter is not currently supported.
ddbpgmFilters :: Lens' DescribeDBParameterGroupsMessage [Filter]
ddbpgmFilters = lens _ddbpgmFilters (\s a -> s { _ddbpgmFilters = a })

-- | An optional pagination token provided by a previous
-- DescribeDBParameterGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
ddbpgmMarker :: Lens' DescribeDBParameterGroupsMessage (Maybe Text)
ddbpgmMarker = lens _ddbpgmMarker (\s a -> s { _ddbpgmMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbpgmMaxRecords :: Lens' DescribeDBParameterGroupsMessage (Maybe Int)
ddbpgmMaxRecords = lens _ddbpgmMaxRecords (\s a -> s { _ddbpgmMaxRecords = a })

instance ToQuery DescribeDBParameterGroupsMessage

instance ToPath DescribeDBParameterGroupsMessage where
    toPath = const "/"

data DBParameterGroupsMessage = DBParameterGroupsMessage
    { _dbpgmDBParameterGroups :: [DBParameterGroup]
    , _dbpgmMarker            :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DBParameterGroupsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbpgmDBParameterGroups' @::@ ['DBParameterGroup']
--
-- * 'dbpgmMarker' @::@ 'Maybe' 'Text'
--
dbparameterGroupsMessage :: DBParameterGroupsMessage
dbparameterGroupsMessage = DBParameterGroupsMessage
    { _dbpgmMarker            = Nothing
    , _dbpgmDBParameterGroups = mempty
    }

-- | A list of DBParameterGroup instances.
dbpgmDBParameterGroups :: Lens' DBParameterGroupsMessage [DBParameterGroup]
dbpgmDBParameterGroups =
    lens _dbpgmDBParameterGroups (\s a -> s { _dbpgmDBParameterGroups = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dbpgmMarker :: Lens' DBParameterGroupsMessage (Maybe Text)
dbpgmMarker = lens _dbpgmMarker (\s a -> s { _dbpgmMarker = a })

instance FromXML DBParameterGroupsMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBParameterGroupsMessage"

instance AWSRequest DescribeDBParameterGroupsMessage where
    type Sv DescribeDBParameterGroupsMessage = RDS
    type Rs DescribeDBParameterGroupsMessage = DBParameterGroupsMessage

    request  = post "DescribeDBParameterGroups"
    response = xmlResponse $ \h x -> DBParameterGroupsMessage
        <$> x %| "DBParameterGroups"
        <*> x %| "Marker"
