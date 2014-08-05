{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.EMR.V2009_03_31.ListBootstrapActions where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.EMR.V2009_03_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListBootstrapActions' request.
listBootstrapActions :: Text -- ^ '_lbaiClusterId'
                     -> ListBootstrapActions
listBootstrapActions p1 = ListBootstrapActions
    { _lbaiClusterId = p1
    , _lbaiMarker = Nothing
    }

data ListBootstrapActions = ListBootstrapActions
    { _lbaiClusterId :: Text
      -- ^ The cluster identifier for the bootstrap actions to list .
    , _lbaiMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve .
    } deriving (Show, Generic)

makeLenses ''ListBootstrapActions

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

makeLenses ''ListBootstrapActionsResponse

instance FromJSON ListBootstrapActionsResponse

instance AWSRequest ListBootstrapActions where
    type Sv ListBootstrapActions = EMR
    type Rs ListBootstrapActions = ListBootstrapActionsResponse

    request = get
    response _ = undefined

instance AWSPager ListBootstrapActions where
    next rq rs = (\x -> rq { _lbaiMarker = Just x })
        <$> (_lbaoMarker rs)
