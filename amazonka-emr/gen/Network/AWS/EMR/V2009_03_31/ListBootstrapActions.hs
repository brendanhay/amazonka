{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.V2009_03_31.ListBootstrapActions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides information about the bootstrap actions associated with a cluster.
module Network.AWS.EMR.V2009_03_31.ListBootstrapActions
    (
    -- * Request
      ListBootstrapActions
    -- ** Request constructor
    , mkListBootstrapActions
    -- ** Request lenses
    , lbaClusterId
    , lbaMarker

    -- * Response
    , ListBootstrapActionsResponse
    -- ** Response lenses
    , lbarBootstrapActions
    , lbarMarker
    ) where

import Network.AWS.EMR.V2009_03_31.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | This input determines which bootstrap actions to retrieve.
data ListBootstrapActions = ListBootstrapActions
    { _lbaClusterId :: Text
    , _lbaMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListBootstrapActions' request.
mkListBootstrapActions :: Text -- ^ 'lbaClusterId'
                       -> ListBootstrapActions
mkListBootstrapActions p1 = ListBootstrapActions
    { _lbaClusterId = p1
    , _lbaMarker = Nothing
    }

-- | The cluster identifier for the bootstrap actions to list .
lbaClusterId :: Lens' ListBootstrapActions Text
lbaClusterId = lens _lbaClusterId (\s a -> s { _lbaClusterId = a })

-- | The pagination token that indicates the next set of results to retrieve .
lbaMarker :: Lens' ListBootstrapActions (Maybe Text)
lbaMarker = lens _lbaMarker (\s a -> s { _lbaMarker = a })

instance ToPath ListBootstrapActions

instance ToQuery ListBootstrapActions

instance ToHeaders ListBootstrapActions

instance ToJSON ListBootstrapActions

-- | This output contains the boostrap actions detail .
data ListBootstrapActionsResponse = ListBootstrapActionsResponse
    { _lbarBootstrapActions :: [Command]
    , _lbarMarker :: Maybe Text
    } deriving (Show, Generic)

-- | The bootstrap actions associated with the cluster .
lbarBootstrapActions :: Lens' ListBootstrapActionsResponse [Command]
lbarBootstrapActions =
    lens _lbarBootstrapActions (\s a -> s { _lbarBootstrapActions = a })

-- | The pagination token that indicates the next set of results to retrieve .
lbarMarker :: Lens' ListBootstrapActionsResponse (Maybe Text)
lbarMarker = lens _lbarMarker (\s a -> s { _lbarMarker = a })

instance FromJSON ListBootstrapActionsResponse

instance AWSRequest ListBootstrapActions where
    type Sv ListBootstrapActions = EMR
    type Rs ListBootstrapActions = ListBootstrapActionsResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListBootstrapActions where
    next rq rs = (\x -> rq & lbaMarker ?~ x)
        <$> (rs ^. lbarMarker)
