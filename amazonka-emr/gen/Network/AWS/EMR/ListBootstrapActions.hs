{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EMR.ListBootstrapActions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides information about the bootstrap actions associated with a cluster.
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
import Network.AWS.Request
import Network.AWS.EMR.Types

data ListBootstrapActions = ListBootstrapActions
    { _lbaClusterId :: Text
    , _lbaMarker    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

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

instance ToPath ListBootstrapActions where
    toPath = const "/"

instance ToQuery ListBootstrapActions where
    toQuery = const mempty

instance ToHeaders ListBootstrapActions

instance ToBody ListBootstrapActions where
    toBody = toBody . encode . _lbaClusterId

data ListBootstrapActionsResponse = ListBootstrapActionsResponse
    { _lbarBootstrapActions :: [Command]
    , _lbarMarker           :: Maybe Text
    } deriving (Eq, Show, Generic)

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

-- | The pagination token that indicates the next set of results to retrieve .
lbarMarker :: Lens' ListBootstrapActionsResponse (Maybe Text)
lbarMarker = lens _lbarMarker (\s a -> s { _lbarMarker = a })

-- FromJSON

instance AWSRequest ListBootstrapActions where
    type Sv ListBootstrapActions = EMR
    type Rs ListBootstrapActions = ListBootstrapActionsResponse

    request  = post'
    response = jsonResponse $ \h o -> ListBootstrapActionsResponse
        <$> o .: "BootstrapActions"
        <*> o .: "Marker"
