{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeOptionGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the available option groups.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeOptionGroups.html>
module Network.AWS.RDS.DescribeOptionGroups
    (
    -- * Request
      DescribeOptionGroups
    -- ** Request constructor
    , describeOptionGroups
    -- ** Request lenses
    , dogEngineName
    , dogFilters
    , dogMajorEngineVersion
    , dogMarker
    , dogMaxRecords
    , dogOptionGroupName

    -- * Response
    , DescribeOptionGroupsResponse
    -- ** Response constructor
    , describeOptionGroupsResponse
    -- ** Response lenses
    , dogrMarker
    , dogrOptionGroupsList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeOptionGroups = DescribeOptionGroups
    { _dogEngineName         :: Maybe Text
    , _dogFilters            :: List "Filter" Filter
    , _dogMajorEngineVersion :: Maybe Text
    , _dogMarker             :: Maybe Text
    , _dogMaxRecords         :: Maybe Int
    , _dogOptionGroupName    :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeOptionGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dogEngineName' @::@ 'Maybe' 'Text'
--
-- * 'dogFilters' @::@ ['Filter']
--
-- * 'dogMajorEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'dogMarker' @::@ 'Maybe' 'Text'
--
-- * 'dogMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dogOptionGroupName' @::@ 'Maybe' 'Text'
--
describeOptionGroups :: DescribeOptionGroups
describeOptionGroups = DescribeOptionGroups
    { _dogOptionGroupName    = Nothing
    , _dogFilters            = mempty
    , _dogMarker             = Nothing
    , _dogMaxRecords         = Nothing
    , _dogEngineName         = Nothing
    , _dogMajorEngineVersion = Nothing
    }

-- | Filters the list of option groups to only include groups associated with a
-- specific database engine.
dogEngineName :: Lens' DescribeOptionGroups (Maybe Text)
dogEngineName = lens _dogEngineName (\s a -> s { _dogEngineName = a })

-- | This parameter is not currently supported.
dogFilters :: Lens' DescribeOptionGroups [Filter]
dogFilters = lens _dogFilters (\s a -> s { _dogFilters = a }) . _List

-- | Filters the list of option groups to only include groups associated with a
-- specific database engine version. If specified, then EngineName must also be
-- specified.
dogMajorEngineVersion :: Lens' DescribeOptionGroups (Maybe Text)
dogMajorEngineVersion =
    lens _dogMajorEngineVersion (\s a -> s { _dogMajorEngineVersion = a })

-- | An optional pagination token provided by a previous DescribeOptionGroups
-- request. If this parameter is specified, the response includes only records
-- beyond the marker, up to the value specified by 'MaxRecords'.
dogMarker :: Lens' DescribeOptionGroups (Maybe Text)
dogMarker = lens _dogMarker (\s a -> s { _dogMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified 'MaxRecords' value, a pagination token called a marker
-- is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
dogMaxRecords :: Lens' DescribeOptionGroups (Maybe Int)
dogMaxRecords = lens _dogMaxRecords (\s a -> s { _dogMaxRecords = a })

-- | The name of the option group to describe. Cannot be supplied together with
-- EngineName or MajorEngineVersion.
dogOptionGroupName :: Lens' DescribeOptionGroups (Maybe Text)
dogOptionGroupName =
    lens _dogOptionGroupName (\s a -> s { _dogOptionGroupName = a })

data DescribeOptionGroupsResponse = DescribeOptionGroupsResponse
    { _dogrMarker           :: Maybe Text
    , _dogrOptionGroupsList :: List "OptionGroup" OptionGroup
    } deriving (Eq, Show)

-- | 'DescribeOptionGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dogrMarker' @::@ 'Maybe' 'Text'
--
-- * 'dogrOptionGroupsList' @::@ ['OptionGroup']
--
describeOptionGroupsResponse :: DescribeOptionGroupsResponse
describeOptionGroupsResponse = DescribeOptionGroupsResponse
    { _dogrOptionGroupsList = mempty
    , _dogrMarker           = Nothing
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the marker,
-- up to the value specified by 'MaxRecords'.
dogrMarker :: Lens' DescribeOptionGroupsResponse (Maybe Text)
dogrMarker = lens _dogrMarker (\s a -> s { _dogrMarker = a })

-- | List of option groups.
dogrOptionGroupsList :: Lens' DescribeOptionGroupsResponse [OptionGroup]
dogrOptionGroupsList =
    lens _dogrOptionGroupsList (\s a -> s { _dogrOptionGroupsList = a })
        . _List

instance ToPath DescribeOptionGroups where
    toPath = const "/"

instance ToQuery DescribeOptionGroups where
    toQuery DescribeOptionGroups{..} = mconcat
        [ "EngineName"         =? _dogEngineName
        , "Filters"            =? _dogFilters
        , "MajorEngineVersion" =? _dogMajorEngineVersion
        , "Marker"             =? _dogMarker
        , "MaxRecords"         =? _dogMaxRecords
        , "OptionGroupName"    =? _dogOptionGroupName
        ]

instance ToHeaders DescribeOptionGroups

instance AWSRequest DescribeOptionGroups where
    type Sv DescribeOptionGroups = RDS
    type Rs DescribeOptionGroups = DescribeOptionGroupsResponse

    request  = post "DescribeOptionGroups"
    response = xmlResponse

instance FromXML DescribeOptionGroupsResponse where
    parseXML = withElement "DescribeOptionGroupsResult" $ \x -> DescribeOptionGroupsResponse
        <$> x .@? "Marker"
        <*> x .@  "OptionGroupsList"

instance AWSPager DescribeOptionGroups where
    page rq rs
        | stop (rq ^. dogMarker) = Nothing
        | otherwise = (\x -> rq & dogMarker ?~ x)
            <$> (rs ^. dogrMarker)
