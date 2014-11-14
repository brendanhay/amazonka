{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.EC2.DescribePlacementGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your placement groups. For more information about
-- placement groups and cluster instances, see Cluster Instances in the Amazon
-- Elastic Compute Cloud User Guide.
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
    , _dpg1Filters    :: [Filter]
    , _dpg1GroupNames :: [Text]
    } deriving (Eq, Show, Generic)

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

-- | One or more filters. group-name - The name of the placement group. state
-- - The state of the placement group (pending | available | deleting |
-- deleted). strategy - The strategy of the placement group (cluster).
dpg1Filters :: Lens' DescribePlacementGroups [Filter]
dpg1Filters = lens _dpg1Filters (\s a -> s { _dpg1Filters = a })

-- | One or more placement group names. Default: Describes all your placement
-- groups, or only those otherwise specified.
dpg1GroupNames :: Lens' DescribePlacementGroups [Text]
dpg1GroupNames = lens _dpg1GroupNames (\s a -> s { _dpg1GroupNames = a })

instance ToQuery DescribePlacementGroups

instance ToPath DescribePlacementGroups where
    toPath = const "/"

newtype DescribePlacementGroupsResponse = DescribePlacementGroupsResponse
    { _dpgrPlacementGroups :: [PlacementGroup]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribePlacementGroupsResponse where
    type Item DescribePlacementGroupsResponse = PlacementGroup

    fromList = DescribePlacementGroupsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dpgrPlacementGroups

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

instance AWSRequest DescribePlacementGroups where
    type Sv DescribePlacementGroups = EC2
    type Rs DescribePlacementGroups = DescribePlacementGroupsResponse

    request  = post "DescribePlacementGroups"
    response = xmlResponse $ \h x -> DescribePlacementGroupsResponse
        <$> x %| "placementGroupSet"
