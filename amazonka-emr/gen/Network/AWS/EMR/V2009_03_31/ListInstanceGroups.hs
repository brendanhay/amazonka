{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.EMR.V2009_03_31.ListInstanceGroups where

import           Network.AWS.EMR.V2009_03_31.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'ListInstanceGroups' request.
listInstanceGroups :: Text -- ^ '_ligiClusterId'
                   -> ListInstanceGroups
listInstanceGroups p1 = ListInstanceGroups
    { _ligiClusterId = p1
    , _ligiMarker = Nothing
    }

data ListInstanceGroups = ListInstanceGroups
    { _ligiClusterId :: Text
      -- ^ The identifier of the cluster for which to list the instance
      -- groups.
    , _ligiMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve.
    } deriving (Show, Generic)

makeLenses ''ListInstanceGroups

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

makeLenses ''ListInstanceGroupsResponse

instance FromJSON ListInstanceGroupsResponse

instance AWSRequest ListInstanceGroups where
    type Sv ListInstanceGroups = EMR
    type Rs ListInstanceGroups = ListInstanceGroupsResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListInstanceGroups where
    next rq rs = (\x -> rq { _ligiMarker = Just x })
        <$> (_ligoMarker rs)
