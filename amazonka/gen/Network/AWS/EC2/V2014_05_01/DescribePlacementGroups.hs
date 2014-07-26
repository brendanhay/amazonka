{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.DescribePlacementGroups
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
module Network.AWS.EC2.V2014_05_01.DescribePlacementGroups where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DescribePlacementGroups = DescribePlacementGroups
    { _dpgsDryRun :: Bool
      -- ^ 
    , _dpgsFilters :: [Filter]
      -- ^ One or more filters. group-name - The name of the placement
      -- group. state - The state of the placement group (pending |
      -- available | deleting | deleted). strategy - The strategy of the
      -- placement group (cluster).
    , _dpgsGroupNames :: [Text]
      -- ^ One or more placement group names. Default: Describes all your
      -- placement groups, or only those otherwise specified.
    } deriving (Generic)

instance ToQuery DescribePlacementGroups where
    toQuery = genericToQuery def

instance AWSRequest DescribePlacementGroups where
    type Sv DescribePlacementGroups = EC2
    type Rs DescribePlacementGroups = DescribePlacementGroupsResponse

    request = post "DescribePlacementGroups"

    response _ = xmlResponse

data DescribePlacementGroupsResponse = DescribePlacementGroupsResponse
    { _dpgtPlacementGroups :: [PlacementGroup]
      -- ^ One or more placement groups.
    } deriving (Generic)

instance FromXML DescribePlacementGroupsResponse where
    fromXMLOptions = xmlOptions
