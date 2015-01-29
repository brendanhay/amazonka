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

-- Module      : Network.AWS.EMR.ListBootstrapActions
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

-- | Provides information about the bootstrap actions associated with a cluster.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_ListBootstrapActions.html>
module Network.AWS.EMR.ListBootstrapActions
    (
    -- * Request
      ListBootstrapActions
    -- ** Request constructor
    , listBootstrapActions
    -- ** Request lenses
    , lbaClusterId
    , lbaMarker

    -- * Response
    , ListBootstrapActionsResponse
    -- ** Response constructor
    , listBootstrapActionsResponse
    -- ** Response lenses
    , lbarBootstrapActions
    , lbarMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.EMR.Types
import qualified GHC.Exts

data ListBootstrapActions = ListBootstrapActions
    { _lbaClusterId :: Text
    , _lbaMarker    :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListBootstrapActions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbaClusterId' @::@ 'Text'
--
-- * 'lbaMarker' @::@ 'Maybe' 'Text'
--
listBootstrapActions :: Text -- ^ 'lbaClusterId'
                     -> ListBootstrapActions
listBootstrapActions p1 = ListBootstrapActions
    { _lbaClusterId = p1
    , _lbaMarker    = Nothing
    }

-- | The cluster identifier for the bootstrap actions to list .
lbaClusterId :: Lens' ListBootstrapActions Text
lbaClusterId = lens _lbaClusterId (\s a -> s { _lbaClusterId = a })

-- | The pagination token that indicates the next set of results to retrieve .
lbaMarker :: Lens' ListBootstrapActions (Maybe Text)
lbaMarker = lens _lbaMarker (\s a -> s { _lbaMarker = a })

data ListBootstrapActionsResponse = ListBootstrapActionsResponse
    { _lbarBootstrapActions :: List "BootstrapActions" Command
    , _lbarMarker           :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ListBootstrapActionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbarBootstrapActions' @::@ ['Command']
--
-- * 'lbarMarker' @::@ 'Maybe' 'Text'
--
listBootstrapActionsResponse :: ListBootstrapActionsResponse
listBootstrapActionsResponse = ListBootstrapActionsResponse
    { _lbarBootstrapActions = mempty
    , _lbarMarker           = Nothing
    }

-- | The bootstrap actions associated with the cluster .
lbarBootstrapActions :: Lens' ListBootstrapActionsResponse [Command]
lbarBootstrapActions =
    lens _lbarBootstrapActions (\s a -> s { _lbarBootstrapActions = a })
        . _List

-- | The pagination token that indicates the next set of results to retrieve .
lbarMarker :: Lens' ListBootstrapActionsResponse (Maybe Text)
lbarMarker = lens _lbarMarker (\s a -> s { _lbarMarker = a })

instance ToPath ListBootstrapActions where
    toPath = const "/"

instance ToQuery ListBootstrapActions where
    toQuery = const mempty

instance ToHeaders ListBootstrapActions

instance ToJSON ListBootstrapActions where
    toJSON ListBootstrapActions{..} = object
        [ "ClusterId" .= _lbaClusterId
        , "Marker"    .= _lbaMarker
        ]

instance AWSRequest ListBootstrapActions where
    type Sv ListBootstrapActions = EMR
    type Rs ListBootstrapActions = ListBootstrapActionsResponse

    request  = post "ListBootstrapActions"
    response = jsonResponse

instance FromJSON ListBootstrapActionsResponse where
    parseJSON = withObject "ListBootstrapActionsResponse" $ \o -> ListBootstrapActionsResponse
        <$> o .:? "BootstrapActions" .!= mempty
        <*> o .:? "Marker"

instance AWSPager ListBootstrapActions where
    page rq rs
        | stop (rs ^. lbarMarker) = Nothing
        | otherwise = (\x -> rq & lbaMarker ?~ x)
            <$> (rs ^. lbarMarker)
