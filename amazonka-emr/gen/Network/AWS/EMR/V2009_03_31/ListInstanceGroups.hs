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
    , mkListInstanceGroupsInput
    -- ** Request lenses
    , ligiClusterId
    , ligiMarker

    -- * Response
    , ListInstanceGroupsResponse
    -- ** Response lenses
    , ligoInstanceGroups
    , ligoMarker
    ) where

import           Network.AWS.EMR.V2009_03_31.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListInstanceGroups' request.
mkListInstanceGroupsInput :: Text -- ^ 'ligiClusterId'
                          -> ListInstanceGroups
mkListInstanceGroupsInput p1 = ListInstanceGroups
    { _ligiClusterId = p1
    , _ligiMarker = Nothing
    }
{-# INLINE mkListInstanceGroupsInput #-}

data ListInstanceGroups = ListInstanceGroups
    { _ligiClusterId :: Text
      -- ^ The identifier of the cluster for which to list the instance
      -- groups.
    , _ligiMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve.
    } deriving (Show, Generic)

-- | The identifier of the cluster for which to list the instance groups.
ligiClusterId :: Lens' ListInstanceGroups (Text)
ligiClusterId = lens _ligiClusterId (\s a -> s { _ligiClusterId = a })
{-# INLINE ligiClusterId #-}

-- | The pagination token that indicates the next set of results to retrieve.
ligiMarker :: Lens' ListInstanceGroups (Maybe Text)
ligiMarker = lens _ligiMarker (\s a -> s { _ligiMarker = a })
{-# INLINE ligiMarker #-}

instance ToPath ListInstanceGroups

instance ToQuery ListInstanceGroups

instance ToHeaders ListInstanceGroups

instance ToJSON ListInstanceGroups

data ListInstanceGroupsResponse = ListInstanceGroupsResponse
    { _ligoInstanceGroups :: [InstanceGroup]
      -- ^ The list of instance groups for the cluster and given filters.
    , _ligoMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve.
    } deriving (Show, Generic)

-- | The list of instance groups for the cluster and given filters.
ligoInstanceGroups :: Lens' ListInstanceGroupsResponse ([InstanceGroup])
ligoInstanceGroups = lens _ligoInstanceGroups (\s a -> s { _ligoInstanceGroups = a })
{-# INLINE ligoInstanceGroups #-}

-- | The pagination token that indicates the next set of results to retrieve.
ligoMarker :: Lens' ListInstanceGroupsResponse (Maybe Text)
ligoMarker = lens _ligoMarker (\s a -> s { _ligoMarker = a })
{-# INLINE ligoMarker #-}

instance FromJSON ListInstanceGroupsResponse

instance AWSRequest ListInstanceGroups where
    type Sv ListInstanceGroups = EMR
    type Rs ListInstanceGroups = ListInstanceGroupsResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListInstanceGroups where
    next rq rs = (\x -> rq { _ligiMarker = Just x })
        <$> (_ligoMarker rs)
