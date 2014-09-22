{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.EC2.DescribePlacementGroups
    (
    -- * Request
      DescribePlacementGroups
    -- ** Request constructor
    , describePlacementGroups
    -- ** Request lenses
    , dpg1GroupName
    , dpg1Filter

    -- * Response
    , DescribePlacementGroupsResponse
    -- ** Response constructor
    , describePlacementGroupsResponse
    -- ** Response lenses
    , dpgrItem
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribePlacementGroups = DescribePlacementGroups
    { _dpg1GroupName :: [Text]
    , _dpg1Filter :: [Filter]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribePlacementGroups' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @[Text]@
--
-- * @Filter ::@ @[Filter]@
--
describePlacementGroups :: DescribePlacementGroups
describePlacementGroups = DescribePlacementGroups
    { _dpg1GroupName = mempty
    , _dpg1Filter = mempty
    }

-- | One or more placement group names. Default: Describes all your placement
-- groups, or only those otherwise specified.
dpg1GroupName :: Lens' DescribePlacementGroups [Text]
dpg1GroupName = lens _dpg1GroupName (\s a -> s { _dpg1GroupName = a })

-- | One or more filters. group-name - The name of the placement group. state -
-- The state of the placement group (pending | available | deleting |
-- deleted). strategy - The strategy of the placement group (cluster).
dpg1Filter :: Lens' DescribePlacementGroups [Filter]
dpg1Filter = lens _dpg1Filter (\s a -> s { _dpg1Filter = a })

instance ToQuery DescribePlacementGroups where
    toQuery = genericQuery def

newtype DescribePlacementGroupsResponse = DescribePlacementGroupsResponse
    { _dpgrItem :: [PlacementGroup]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribePlacementGroupsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Item ::@ @[PlacementGroup]@
--
describePlacementGroupsResponse :: DescribePlacementGroupsResponse
describePlacementGroupsResponse = DescribePlacementGroupsResponse
    { _dpgrItem = mempty
    }

-- | One or more placement groups.
dpgrItem :: Lens' DescribePlacementGroupsResponse [PlacementGroup]
dpgrItem = lens _dpgrItem (\s a -> s { _dpgrItem = a })

instance FromXML DescribePlacementGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribePlacementGroups where
    type Sv DescribePlacementGroups = EC2
    type Rs DescribePlacementGroups = DescribePlacementGroupsResponse

    request = post "DescribePlacementGroups"
    response _ = xmlResponse
