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

-- Module      : Network.AWS.RDS.DescribeOptionGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the available option groups.
module Network.AWS.RDS.DescribeOptionGroups
    (
    -- * Request
      DescribeOptionGroupsMessage
    -- ** Request constructor
    , describeOptionGroups
    -- ** Request lenses
    , dogm1EngineName
    , dogm1Filters
    , dogm1MajorEngineVersion
    , dogm1Marker
    , dogm1MaxRecords
    , dogm1OptionGroupName

    -- * Response
    , OptionGroups
    -- ** Response constructor
    , describeOptionGroupsResponse
    -- ** Response lenses
    , ogMarker
    , ogOptionGroupsList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DescribeOptionGroupsMessage = DescribeOptionGroupsMessage
    { _dogm1EngineName         :: Maybe Text
    , _dogm1Filters            :: [Filter]
    , _dogm1MajorEngineVersion :: Maybe Text
    , _dogm1Marker             :: Maybe Text
    , _dogm1MaxRecords         :: Maybe Int
    , _dogm1OptionGroupName    :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeOptionGroupsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dogm1EngineName' @::@ 'Maybe' 'Text'
--
-- * 'dogm1Filters' @::@ ['Filter']
--
-- * 'dogm1MajorEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'dogm1Marker' @::@ 'Maybe' 'Text'
--
-- * 'dogm1MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dogm1OptionGroupName' @::@ 'Maybe' 'Text'
--
describeOptionGroups :: DescribeOptionGroupsMessage
describeOptionGroups = DescribeOptionGroupsMessage
    { _dogm1OptionGroupName    = Nothing
    , _dogm1Filters            = mempty
    , _dogm1Marker             = Nothing
    , _dogm1MaxRecords         = Nothing
    , _dogm1EngineName         = Nothing
    , _dogm1MajorEngineVersion = Nothing
    }

-- | Filters the list of option groups to only include groups associated with
-- a specific database engine.
dogm1EngineName :: Lens' DescribeOptionGroupsMessage (Maybe Text)
dogm1EngineName = lens _dogm1EngineName (\s a -> s { _dogm1EngineName = a })

-- | This parameter is not currently supported.
dogm1Filters :: Lens' DescribeOptionGroupsMessage [Filter]
dogm1Filters = lens _dogm1Filters (\s a -> s { _dogm1Filters = a })

-- | Filters the list of option groups to only include groups associated with
-- a specific database engine version. If specified, then EngineName must
-- also be specified.
dogm1MajorEngineVersion :: Lens' DescribeOptionGroupsMessage (Maybe Text)
dogm1MajorEngineVersion =
    lens _dogm1MajorEngineVersion (\s a -> s { _dogm1MajorEngineVersion = a })

-- | An optional pagination token provided by a previous DescribeOptionGroups
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by MaxRecords.
dogm1Marker :: Lens' DescribeOptionGroupsMessage (Maybe Text)
dogm1Marker = lens _dogm1Marker (\s a -> s { _dogm1Marker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
dogm1MaxRecords :: Lens' DescribeOptionGroupsMessage (Maybe Int)
dogm1MaxRecords = lens _dogm1MaxRecords (\s a -> s { _dogm1MaxRecords = a })

-- | The name of the option group to describe. Cannot be supplied together
-- with EngineName or MajorEngineVersion.
dogm1OptionGroupName :: Lens' DescribeOptionGroupsMessage (Maybe Text)
dogm1OptionGroupName =
    lens _dogm1OptionGroupName (\s a -> s { _dogm1OptionGroupName = a })

instance ToPath DescribeOptionGroupsMessage where
    toPath = const "/"

instance ToQuery DescribeOptionGroupsMessage

data OptionGroups = OptionGroups
    { _ogMarker           :: Maybe Text
    , _ogOptionGroupsList :: [OptionGroup]
    } deriving (Eq, Show, Generic)

-- | 'OptionGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ogMarker' @::@ 'Maybe' 'Text'
--
-- * 'ogOptionGroupsList' @::@ ['OptionGroup']
--
describeOptionGroupsResponse :: OptionGroups
describeOptionGroupsResponse = OptionGroups
    { _ogOptionGroupsList = mempty
    , _ogMarker           = Nothing
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
ogMarker :: Lens' OptionGroups (Maybe Text)
ogMarker = lens _ogMarker (\s a -> s { _ogMarker = a })

-- | List of option groups.
ogOptionGroupsList :: Lens' OptionGroups [OptionGroup]
ogOptionGroupsList =
    lens _ogOptionGroupsList (\s a -> s { _ogOptionGroupsList = a })

instance AWSRequest DescribeOptionGroupsMessage where
    type Sv DescribeOptionGroupsMessage = RDS
    type Rs DescribeOptionGroupsMessage = OptionGroups

    request  = post "DescribeOptionGroups"
    response = xmlResponse $ \h x -> OptionGroups
        <$> x %| "Marker"
        <*> x %| "OptionGroupsList"
