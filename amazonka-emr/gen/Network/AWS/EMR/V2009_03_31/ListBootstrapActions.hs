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
    , listBootstrapActions
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

-- | Minimum specification for a 'ListBootstrapActions' request.
listBootstrapActions :: Text -- ^ 'lbaiClusterId'
                     -> ListBootstrapActions
listBootstrapActions p1 = ListBootstrapActions
    { _lbaiClusterId = p1
    , _lbaiMarker = Nothing
    }
{-# INLINE listBootstrapActions #-}

data ListBootstrapActions = ListBootstrapActions
    { _lbaiClusterId :: Text
      -- ^ The cluster identifier for the bootstrap actions to list .
    , _lbaiMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve .
    } deriving (Show, Generic)

-- | The cluster identifier for the bootstrap actions to list .
lbaiClusterId :: Lens' ListBootstrapActions (Text)
lbaiClusterId f x =
    f (_lbaiClusterId x)
        <&> \y -> x { _lbaiClusterId = y }
{-# INLINE lbaiClusterId #-}

-- | The pagination token that indicates the next set of results to retrieve .
lbaiMarker :: Lens' ListBootstrapActions (Maybe Text)
lbaiMarker f x =
    f (_lbaiMarker x)
        <&> \y -> x { _lbaiMarker = y }
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
lbaoBootstrapActions f x =
    f (_lbaoBootstrapActions x)
        <&> \y -> x { _lbaoBootstrapActions = y }
{-# INLINE lbaoBootstrapActions #-}

-- | The pagination token that indicates the next set of results to retrieve .
lbaoMarker :: Lens' ListBootstrapActionsResponse (Maybe Text)
lbaoMarker f x =
    f (_lbaoMarker x)
        <&> \y -> x { _lbaoMarker = y }
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
