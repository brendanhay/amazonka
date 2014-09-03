{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribePlacementGroups
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
-- Elastic Compute Cloud User Guide. Example This example describes the
-- placement group named XYZ-cluster.
-- https://ec2.amazonaws.com/?Action=DescribePlacementGroups
-- &amp;GroupName.1=XYZ-cluster &amp;AUTHPARAMS
-- d4904fd9-82c2-4ea5-adfe-a9cc3EXAMPLE XYZ-cluster cluster available Example
-- This example filters the response to include only placement groups that
-- include the string Project in the name.
-- https://ec2.amazonaws.com/?Action=DescribePlacementGroups
-- &amp;Filter.1.Name=group-name &amp;Filter.1.Value=*Project* &amp;AUTHPARAMS
-- d4904fd9-82c2-4ea5-adfe-a9cc3EXAMPLE Project-cluster cluster available.
module Network.AWS.EC2.V2014_06_15.DescribePlacementGroups
    (
    -- * Request
      DescribePlacementGroups
    -- ** Request constructor
    , describePlacementGroups
    -- ** Request lenses
    , dpgsFilters
    , dpgsGroupNames

    -- * Response
    , DescribePlacementGroupsResponse
    -- ** Response lenses
    , dpgtPlacementGroups
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribePlacementGroups' request.
describePlacementGroups :: DescribePlacementGroups
describePlacementGroups = DescribePlacementGroups
    { _dpgsFilters = mempty
    , _dpgsGroupNames = mempty
    }

data DescribePlacementGroups = DescribePlacementGroups
    { _dpgsFilters :: [Filter]
      -- ^ One or more filters. group-name - The name of the placement
      -- group. state - The state of the placement group (pending |
      -- available | deleting | deleted). strategy - The strategy of the
      -- placement group (cluster).
    , _dpgsGroupNames :: [Text]
      -- ^ One or more placement group names. Default: Describes all your
      -- placement groups, or only those otherwise specified.
    } deriving (Show, Generic)

-- | One or more filters. group-name - The name of the placement group. state -
-- The state of the placement group (pending | available | deleting |
-- deleted). strategy - The strategy of the placement group (cluster).
dpgsFilters
    :: Functor f
    => ([Filter]
    -> f ([Filter]))
    -> DescribePlacementGroups
    -> f DescribePlacementGroups
dpgsFilters f x =
    (\y -> x { _dpgsFilters = y })
       <$> f (_dpgsFilters x)
{-# INLINE dpgsFilters #-}

-- | One or more placement group names. Default: Describes all your placement
-- groups, or only those otherwise specified.
dpgsGroupNames
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribePlacementGroups
    -> f DescribePlacementGroups
dpgsGroupNames f x =
    (\y -> x { _dpgsGroupNames = y })
       <$> f (_dpgsGroupNames x)
{-# INLINE dpgsGroupNames #-}

instance ToQuery DescribePlacementGroups where
    toQuery = genericQuery def

data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse
    { _dpgtPlacementGroups :: [PlacementGroup]
      -- ^ One or more placement groups.
    } deriving (Show, Generic)

-- | One or more placement groups.
dpgtPlacementGroups
    :: Functor f
    => ([PlacementGroup]
    -> f ([PlacementGroup]))
    -> DescribePlacementGroupsResponse
    -> f DescribePlacementGroupsResponse
dpgtPlacementGroups f x =
    (\y -> x { _dpgtPlacementGroups = y })
       <$> f (_dpgtPlacementGroups x)
{-# INLINE dpgtPlacementGroups #-}

instance FromXML DescribePlacementGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribePlacementGroups where
    type Sv DescribePlacementGroups = EC2
    type Rs DescribePlacementGroups = DescribePlacementGroupsResponse

    request = post "DescribePlacementGroups"
    response _ = xmlResponse
