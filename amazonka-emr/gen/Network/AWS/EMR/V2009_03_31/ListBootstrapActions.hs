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
    , mkListBootstrapActionsInput
    -- ** Request lenses
    , lbaiClusterId
    , lbaiMarker

    -- * Response
    , ListBootstrapActionsResponse
    -- ** Response lenses
    , lbaoBootstrapActions
    , lbaoMarker
    ) where

import           Network.AWS.EMR.V2009_03_31.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListBootstrapActions' request.
mkListBootstrapActionsInput :: Text -- ^ 'lbaiClusterId'
                            -> ListBootstrapActions
mkListBootstrapActionsInput p1 = ListBootstrapActions
    { _lbaiClusterId = p1
    , _lbaiMarker = Nothing
    }
{-# INLINE mkListBootstrapActionsInput #-}

data ListBootstrapActions = ListBootstrapActions
    { _lbaiClusterId :: Text
      -- ^ The cluster identifier for the bootstrap actions to list .
    , _lbaiMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve .
    } deriving (Show, Generic)

-- | The cluster identifier for the bootstrap actions to list .
lbaiClusterId :: Lens' ListBootstrapActions (Text)
lbaiClusterId = lens _lbaiClusterId (\s a -> s { _lbaiClusterId = a })
{-# INLINE lbaiClusterId #-}

-- | The pagination token that indicates the next set of results to retrieve .
lbaiMarker :: Lens' ListBootstrapActions (Maybe Text)
lbaiMarker = lens _lbaiMarker (\s a -> s { _lbaiMarker = a })
{-# INLINE lbaiMarker #-}

instance ToPath ListBootstrapActions

instance ToQuery ListBootstrapActions

instance ToHeaders ListBootstrapActions

instance ToJSON ListBootstrapActions

data ListBootstrapActionsResponse = ListBootstrapActionsResponse
    { _lbaoBootstrapActions :: [Command]
      -- ^ The bootstrap actions associated with the cluster .
    , _lbaoMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve .
    } deriving (Show, Generic)

-- | The bootstrap actions associated with the cluster .
lbaoBootstrapActions :: Lens' ListBootstrapActionsResponse ([Command])
lbaoBootstrapActions = lens _lbaoBootstrapActions (\s a -> s { _lbaoBootstrapActions = a })
{-# INLINE lbaoBootstrapActions #-}

-- | The pagination token that indicates the next set of results to retrieve .
lbaoMarker :: Lens' ListBootstrapActionsResponse (Maybe Text)
lbaoMarker = lens _lbaoMarker (\s a -> s { _lbaoMarker = a })
{-# INLINE lbaoMarker #-}

instance FromJSON ListBootstrapActionsResponse

instance AWSRequest ListBootstrapActions where
    type Sv ListBootstrapActions = EMR
    type Rs ListBootstrapActions = ListBootstrapActionsResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListBootstrapActions where
    next rq rs = (\x -> rq { _lbaiMarker = Just x })
        <$> (_lbaoMarker rs)
