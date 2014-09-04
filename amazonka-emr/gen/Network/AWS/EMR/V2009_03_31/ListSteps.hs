{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.V2009_03_31.ListSteps
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides a list of steps for the cluster.
module Network.AWS.EMR.V2009_03_31.ListSteps
    (
    -- * Request
      ListSteps
    -- ** Request constructor
    , listSteps
    -- ** Request lenses
    , lsiClusterId
    , lsiMarker
    , lsiStepStates

    -- * Response
    , ListStepsResponse
    -- ** Response lenses
    , lsoMarker
    , lsoSteps
    ) where

import           Network.AWS.EMR.V2009_03_31.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'ListSteps' request.
listSteps :: Text -- ^ 'lsiClusterId'
          -> ListSteps
listSteps p1 = ListSteps
    { _lsiClusterId = p1
    , _lsiMarker = Nothing
    , _lsiStepStates = mempty
    }
{-# INLINE listSteps #-}

data ListSteps = ListSteps
    { _lsiClusterId :: Text
      -- ^ The identifier of the cluster for which to list the steps.
    , _lsiMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve.
    , _lsiStepStates :: [StepState]
      -- ^ The filter to limit the step list based on certain states.
    } deriving (Show, Generic)

-- | The identifier of the cluster for which to list the steps.
lsiClusterId :: Lens' ListSteps (Text)
lsiClusterId f x =
    f (_lsiClusterId x)
        <&> \y -> x { _lsiClusterId = y }
{-# INLINE lsiClusterId #-}

-- | The pagination token that indicates the next set of results to retrieve.
lsiMarker :: Lens' ListSteps (Maybe Text)
lsiMarker f x =
    f (_lsiMarker x)
        <&> \y -> x { _lsiMarker = y }
{-# INLINE lsiMarker #-}

-- | The filter to limit the step list based on certain states.
lsiStepStates :: Lens' ListSteps ([StepState])
lsiStepStates f x =
    f (_lsiStepStates x)
        <&> \y -> x { _lsiStepStates = y }
{-# INLINE lsiStepStates #-}

instance ToPath ListSteps

instance ToQuery ListSteps

instance ToHeaders ListSteps

instance ToJSON ListSteps

data ListStepsResponse = ListStepsResponse
    { _lsoMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve.
    , _lsoSteps :: [StepSummary]
      -- ^ The filtered list of steps for the cluster.
    } deriving (Show, Generic)

-- | The pagination token that indicates the next set of results to retrieve.
lsoMarker :: Lens' ListStepsResponse (Maybe Text)
lsoMarker f x =
    f (_lsoMarker x)
        <&> \y -> x { _lsoMarker = y }
{-# INLINE lsoMarker #-}

-- | The filtered list of steps for the cluster.
lsoSteps :: Lens' ListStepsResponse ([StepSummary])
lsoSteps f x =
    f (_lsoSteps x)
        <&> \y -> x { _lsoSteps = y }
{-# INLINE lsoSteps #-}

instance FromJSON ListStepsResponse

instance AWSRequest ListSteps where
    type Sv ListSteps = EMR
    type Rs ListSteps = ListStepsResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListSteps where
    next rq rs = (\x -> rq { _lsiMarker = Just x })
        <$> (_lsoMarker rs)
