{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.V2009_03_31.ListInstanceGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides all available details about the instance groups in a cluster.
module Network.AWS.EMR.V2009_03_31.ListInstanceGroups
    (
    -- * Request
      ListInstanceGroups
    -- ** Request constructor
    , mkListInstanceGroups
    -- ** Request lenses
    , ligClusterId
    , ligMarker

    -- * Response
    , ListInstanceGroupsResponse
    -- ** Response lenses
    , ligrsInstanceGroups
    , ligrsMarker
    ) where

import           Network.AWS.EMR.V2009_03_31.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | This input determines which instance groups to retrieve.
data ListInstanceGroups = ListInstanceGroups
    { _ligClusterId :: Text
    , _ligMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListInstanceGroups' request.
mkListInstanceGroups :: Text -- ^ 'ligClusterId'
                     -> ListInstanceGroups
mkListInstanceGroups p1 = ListInstanceGroups
    { _ligClusterId = p1
    , _ligMarker = Nothing
    }

-- | The identifier of the cluster for which to list the instance groups.
ligClusterId :: Lens' ListInstanceGroups Text
ligClusterId = lens _ligClusterId (\s a -> s { _ligClusterId = a })

-- | The pagination token that indicates the next set of results to retrieve.
ligMarker :: Lens' ListInstanceGroups (Maybe Text)
ligMarker = lens _ligMarker (\s a -> s { _ligMarker = a })

instance ToPath ListInstanceGroups

instance ToQuery ListInstanceGroups

instance ToHeaders ListInstanceGroups

instance ToJSON ListInstanceGroups

-- | This input determines which instance groups to retrieve.
data ListInstanceGroupsResponse = ListInstanceGroupsResponse
    { _ligrsInstanceGroups :: [InstanceGroup]
    , _ligrsMarker :: Maybe Text
    } deriving (Show, Generic)

-- | The list of instance groups for the cluster and given filters.
ligrsInstanceGroups :: Lens' ListInstanceGroupsResponse [InstanceGroup]
ligrsInstanceGroups =
    lens _ligrsInstanceGroups (\s a -> s { _ligrsInstanceGroups = a })

-- | The pagination token that indicates the next set of results to retrieve.
ligrsMarker :: Lens' ListInstanceGroupsResponse (Maybe Text)
ligrsMarker = lens _ligrsMarker (\s a -> s { _ligrsMarker = a })

instance FromJSON ListInstanceGroupsResponse

instance AWSRequest ListInstanceGroups where
    type Sv ListInstanceGroups = EMR
    type Rs ListInstanceGroups = ListInstanceGroupsResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListInstanceGroups where
    next rq rs = (\x -> rq & ligMarker ?~ x) <$> (rs ^. ligrsMarker)

