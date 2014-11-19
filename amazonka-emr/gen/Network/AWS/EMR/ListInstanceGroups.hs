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
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_ListInstanceGroups.html>
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
import Network.AWS.Request.JSON
import Network.AWS.EMR.Types
import qualified GHC.Exts

data ListInstanceGroups = ListInstanceGroups
    { _ligClusterId :: Text
    , _ligMarker    :: Maybe Text
    } deriving (Eq, Ord, Show)

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

data ListInstanceGroupsResponse = ListInstanceGroupsResponse
    { _ligrInstanceGroups :: List "InstanceGroups" InstanceGroup
    , _ligrMarker         :: Maybe Text
    } deriving (Eq, Show)

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
        . _List

-- | The pagination token that indicates the next set of results to retrieve.
ligrMarker :: Lens' ListInstanceGroupsResponse (Maybe Text)
ligrMarker = lens _ligrMarker (\s a -> s { _ligrMarker = a })

instance ToPath ListInstanceGroups where
    toPath = const "/"

instance ToQuery ListInstanceGroups where
    toQuery = const mempty

instance ToHeaders ListInstanceGroups

instance ToJSON ListInstanceGroups where
    toJSON ListInstanceGroups{..} = object
        [ "ClusterId" .= _ligClusterId
        , "Marker"    .= _ligMarker
        ]

instance AWSRequest ListInstanceGroups where
    type Sv ListInstanceGroups = EMR
    type Rs ListInstanceGroups = ListInstanceGroupsResponse

    request  = post "ListInstanceGroups"
    response = jsonResponse

instance FromJSON ListInstanceGroupsResponse where
    parseJSON = withObject "ListInstanceGroupsResponse" $ \o -> ListInstanceGroupsResponse
        <$> o .:  "InstanceGroups"
        <*> o .:? "Marker"

instance AWSPager ListInstanceGroups where
    next rq rs = (\x -> rq & ligMarker ?~ x)
        <$> (rs ^. ligrMarker)
