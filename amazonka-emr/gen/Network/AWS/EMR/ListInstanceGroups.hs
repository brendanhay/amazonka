{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.EMR.ListInstanceGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides all available details about the instance groups in a cluster.
module Network.AWS.EMR.ListInstanceGroups
    (
    -- * Request
      ListInstanceGroups
    -- ** Request constructor
    , listInstanceGroups
    -- ** Request lenses
    , ligClusterId
    , ligMarker

    -- * Response
    , ListInstanceGroupsResponse
    -- ** Response constructor
    , listInstanceGroupsResponse
    -- ** Response lenses
    , ligrInstanceGroups
    , ligrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.EMR.Types
import qualified GHC.Exts

data ListInstanceGroups = ListInstanceGroups
    { _ligClusterId :: Text
    , _ligMarker    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListInstanceGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ligClusterId' @::@ 'Text'
--
-- * 'ligMarker' @::@ 'Maybe' 'Text'
--
listInstanceGroups :: Text -- ^ 'ligClusterId'
                   -> ListInstanceGroups
listInstanceGroups p1 = ListInstanceGroups
    { _ligClusterId = p1
    , _ligMarker    = Nothing
    }

-- | The identifier of the cluster for which to list the instance groups.
ligClusterId :: Lens' ListInstanceGroups Text
ligClusterId = lens _ligClusterId (\s a -> s { _ligClusterId = a })

-- | The pagination token that indicates the next set of results to retrieve.
ligMarker :: Lens' ListInstanceGroups (Maybe Text)
ligMarker = lens _ligMarker (\s a -> s { _ligMarker = a })

instance ToPath ListInstanceGroups where
    toPath = const "/"

instance ToQuery ListInstanceGroups where
    toQuery = const mempty

instance ToHeaders ListInstanceGroups

instance ToBody ListInstanceGroups where
    toBody = toBody . encode . _ligClusterId

data ListInstanceGroupsResponse = ListInstanceGroupsResponse
    { _ligrInstanceGroups :: [InstanceGroup]
    , _ligrMarker         :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListInstanceGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ligrInstanceGroups' @::@ ['InstanceGroup']
--
-- * 'ligrMarker' @::@ 'Maybe' 'Text'
--
listInstanceGroupsResponse :: ListInstanceGroupsResponse
listInstanceGroupsResponse = ListInstanceGroupsResponse
    { _ligrInstanceGroups = mempty
    , _ligrMarker         = Nothing
    }

-- | The list of instance groups for the cluster and given filters.
ligrInstanceGroups :: Lens' ListInstanceGroupsResponse [InstanceGroup]
ligrInstanceGroups =
    lens _ligrInstanceGroups (\s a -> s { _ligrInstanceGroups = a })

-- | The pagination token that indicates the next set of results to retrieve.
ligrMarker :: Lens' ListInstanceGroupsResponse (Maybe Text)
ligrMarker = lens _ligrMarker (\s a -> s { _ligrMarker = a })

instance AWSRequest ListInstanceGroups where
    type Sv ListInstanceGroups = EMR
    type Rs ListInstanceGroups = ListInstanceGroupsResponse

    request  = post
    response = jsonResponse $ \h o -> ListInstanceGroupsResponse
        <$> o .: "InstanceGroups"
        <*> o .: "Marker"
