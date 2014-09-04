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
    , mkListStepsInput
    -- ** Request lenses
    , lsiClusterId
    , lsiStepStates
    , lsiMarker

    -- * Response
    , ListStepsResponse
    -- ** Response lenses
    , lsoSteps
    , lsoMarker
    ) where

import           Network.AWS.EMR.V2009_03_31.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListSteps' request.
mkListStepsInput :: Text -- ^ 'lsiClusterId'
                 -> ListSteps
mkListStepsInput p1 = ListSteps
    { _lsiClusterId = p1
    , _lsiStepStates = mempty
    , _lsiMarker = Nothing
    }
{-# INLINE mkListStepsInput #-}

data ListSteps = ListSteps
    { _lsiClusterId :: Text
      -- ^ The identifier of the cluster for which to list the steps.
    , _lsiStepStates :: [StepState]
      -- ^ The filter to limit the step list based on certain states.
    , _lsiMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve.
    } deriving (Show, Generic)

-- | The identifier of the cluster for which to list the steps.
lsiClusterId :: Lens' ListSteps (Text)
lsiClusterId = lens _lsiClusterId (\s a -> s { _lsiClusterId = a })
{-# INLINE lsiClusterId #-}

-- | The filter to limit the step list based on certain states.
lsiStepStates :: Lens' ListSteps ([StepState])
lsiStepStates = lens _lsiStepStates (\s a -> s { _lsiStepStates = a })
{-# INLINE lsiStepStates #-}

-- | The pagination token that indicates the next set of results to retrieve.
lsiMarker :: Lens' ListSteps (Maybe Text)
lsiMarker = lens _lsiMarker (\s a -> s { _lsiMarker = a })
{-# INLINE lsiMarker #-}

instance ToPath ListSteps

instance ToQuery ListSteps

instance ToHeaders ListSteps

instance ToJSON ListSteps

data ListStepsResponse = ListStepsResponse
    { _lsoSteps :: [StepSummary]
      -- ^ The filtered list of steps for the cluster.
    , _lsoMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve.
    } deriving (Show, Generic)

-- | The filtered list of steps for the cluster.
lsoSteps :: Lens' ListStepsResponse ([StepSummary])
lsoSteps = lens _lsoSteps (\s a -> s { _lsoSteps = a })
{-# INLINE lsoSteps #-}

-- | The pagination token that indicates the next set of results to retrieve.
lsoMarker :: Lens' ListStepsResponse (Maybe Text)
lsoMarker = lens _lsoMarker (\s a -> s { _lsoMarker = a })
{-# INLINE lsoMarker #-}

instance FromJSON ListStepsResponse

instance AWSRequest ListSteps where
    type Sv ListSteps = EMR
    type Rs ListSteps = ListStepsResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListSteps where
    next rq rs = (\x -> rq { _lsiMarker = Just x })
        <$> (_lsoMarker rs)
