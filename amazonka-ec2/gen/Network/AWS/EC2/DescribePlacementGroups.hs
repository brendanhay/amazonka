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

-- Module      : Network.AWS.EC2.DescribePlacementGroups
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more of your placement groups. For more information about
-- placement groups and cluster instances, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using_cluster_computing.html Cluster Instances> in the /AmazonElastic Compute Cloud User Guide for Linux/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribePlacementGroups.html>
module Network.AWS.EC2.DescribePlacementGroups
    (
    -- * Request
      DescribePlacementGroups
    -- ** Request constructor
    , describePlacementGroups
    -- ** Request lenses
    , dpg1DryRun
    , dpg1Filters
    , dpg1GroupNames

    -- * Response
    , DescribePlacementGroupsResponse
    -- ** Response constructor
    , describePlacementGroupsResponse
    -- ** Response lenses
    , dpgrPlacementGroups
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribePlacementGroups = DescribePlacementGroups
    { _dpg1DryRun     :: Maybe Bool
    , _dpg1Filters    :: List "Filter" Filter
    , _dpg1GroupNames :: List "groupName" Text
    } deriving (Eq, Read, Show)

-- | 'DescribePlacementGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpg1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dpg1Filters' @::@ ['Filter']
--
-- * 'dpg1GroupNames' @::@ ['Text']
--
describePlacementGroups :: DescribePlacementGroups
describePlacementGroups = DescribePlacementGroups
    { _dpg1DryRun     = Nothing
    , _dpg1GroupNames = mempty
    , _dpg1Filters    = mempty
    }

dpg1DryRun :: Lens' DescribePlacementGroups (Maybe Bool)
dpg1DryRun = lens _dpg1DryRun (\s a -> s { _dpg1DryRun = a })

-- | One or more filters.
--
-- 'group-name' - The name of the placement group.
--
-- 'state' - The state of the placement group ('pending' | 'available' | 'deleting' | 'deleted').
--
-- 'strategy' - The strategy of the placement group ('cluster').
--
--
dpg1Filters :: Lens' DescribePlacementGroups [Filter]
dpg1Filters = lens _dpg1Filters (\s a -> s { _dpg1Filters = a }) . _List

-- | One or more placement group names.
--
-- Default: Describes all your placement groups, or only those otherwise
-- specified.
dpg1GroupNames :: Lens' DescribePlacementGroups [Text]
dpg1GroupNames = lens _dpg1GroupNames (\s a -> s { _dpg1GroupNames = a }) . _List

newtype DescribePlacementGroupsResponse = DescribePlacementGroupsResponse
    { _dpgrPlacementGroups :: List "item" PlacementGroup
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'DescribePlacementGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpgrPlacementGroups' @::@ ['PlacementGroup']
--
describePlacementGroupsResponse :: DescribePlacementGroupsResponse
describePlacementGroupsResponse = DescribePlacementGroupsResponse
    { _dpgrPlacementGroups = mempty
    }

-- | One or more placement groups.
dpgrPlacementGroups :: Lens' DescribePlacementGroupsResponse [PlacementGroup]
dpgrPlacementGroups =
    lens _dpgrPlacementGroups (\s a -> s { _dpgrPlacementGroups = a })
        . _List

instance ToPath DescribePlacementGroups where
    toPath = const "/"

instance ToQuery DescribePlacementGroups where
    toQuery DescribePlacementGroups{..} = mconcat
        [ "DryRun"    =? _dpg1DryRun
        , "Filter"    `toQueryList` _dpg1Filters
        , "GroupName" `toQueryList` _dpg1GroupNames
        ]

instance ToHeaders DescribePlacementGroups

instance AWSRequest DescribePlacementGroups where
    type Sv DescribePlacementGroups = EC2
    type Rs DescribePlacementGroups = DescribePlacementGroupsResponse

    request  = post "DescribePlacementGroups"
    response = xmlResponse

instance FromXML DescribePlacementGroupsResponse where
    parseXML x = DescribePlacementGroupsResponse
        <$> x .@? "placementGroupSet" .!@ mempty
