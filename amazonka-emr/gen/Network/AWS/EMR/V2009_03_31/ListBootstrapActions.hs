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

data ListBootstrapActions = ListBootstrapActions
    { _lbaiClusterId :: Text
      -- ^ The cluster identifier for the bootstrap actions to list .
    , _lbaiMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve .
    } deriving (Show, Generic)

-- | The cluster identifier for the bootstrap actions to list .
lbaiClusterId
    :: Functor f
    => (Text
    -> f (Text))
    -> ListBootstrapActions
    -> f ListBootstrapActions
lbaiClusterId f x =
    (\y -> x { _lbaiClusterId = y })
       <$> f (_lbaiClusterId x)
{-# INLINE lbaiClusterId #-}

-- | The pagination token that indicates the next set of results to retrieve .
lbaiMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListBootstrapActions
    -> f ListBootstrapActions
lbaiMarker f x =
    (\y -> x { _lbaiMarker = y })
       <$> f (_lbaiMarker x)
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
lbaoBootstrapActions
    :: Functor f
    => ([Command]
    -> f ([Command]))
    -> ListBootstrapActionsResponse
    -> f ListBootstrapActionsResponse
lbaoBootstrapActions f x =
    (\y -> x { _lbaoBootstrapActions = y })
       <$> f (_lbaoBootstrapActions x)
{-# INLINE lbaoBootstrapActions #-}

-- | The pagination token that indicates the next set of results to retrieve .
lbaoMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListBootstrapActionsResponse
    -> f ListBootstrapActionsResponse
lbaoMarker f x =
    (\y -> x { _lbaoMarker = y })
       <$> f (_lbaoMarker x)
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
