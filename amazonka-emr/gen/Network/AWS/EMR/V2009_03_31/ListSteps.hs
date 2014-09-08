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
    , mkListSteps
    -- ** Request lenses
    , lsClusterId
    , lsStepStates
    , lsMarker

    -- * Response
    , ListStepsResponse
    -- ** Response constructor
    , mkListStepsResponse
    -- ** Response lenses
    , lsrSteps
    , lsrMarker
    ) where

import Network.AWS.EMR.V2009_03_31.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | This input determines which steps to list.
data ListSteps = ListSteps
    { _lsClusterId :: Text
    , _lsStepStates :: [StepState]
    , _lsMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListSteps' request.
mkListSteps :: Text -- ^ 'lsClusterId'
            -> ListSteps
mkListSteps p1 = ListSteps
    { _lsClusterId = p1
    , _lsStepStates = mempty
    , _lsMarker = Nothing
    }

-- | The identifier of the cluster for which to list the steps.
lsClusterId :: Lens' ListSteps Text
lsClusterId = lens _lsClusterId (\s a -> s { _lsClusterId = a })

-- | The filter to limit the step list based on certain states.
lsStepStates :: Lens' ListSteps [StepState]
lsStepStates = lens _lsStepStates (\s a -> s { _lsStepStates = a })

-- | The pagination token that indicates the next set of results to retrieve.
lsMarker :: Lens' ListSteps (Maybe Text)
lsMarker = lens _lsMarker (\s a -> s { _lsMarker = a })

instance ToPath ListSteps

instance ToQuery ListSteps

instance ToHeaders ListSteps

instance ToJSON ListSteps

-- | This output contains the list of steps.
data ListStepsResponse = ListStepsResponse
    { _lsrSteps :: [StepSummary]
    , _lsrMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListStepsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkListStepsResponse :: ListStepsResponse
mkListStepsResponse = ListStepsResponse
    { _lsrSteps = mempty
    , _lsrMarker = Nothing
    }

-- | The filtered list of steps for the cluster.
lsrSteps :: Lens' ListStepsResponse [StepSummary]
lsrSteps = lens _lsrSteps (\s a -> s { _lsrSteps = a })

-- | The pagination token that indicates the next set of results to retrieve.
lsrMarker :: Lens' ListStepsResponse (Maybe Text)
lsrMarker = lens _lsrMarker (\s a -> s { _lsrMarker = a })

instance FromJSON ListStepsResponse

instance AWSRequest ListSteps where
    type Sv ListSteps = EMR
    type Rs ListSteps = ListStepsResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListSteps where
    next rq rs = (\x -> rq & lsMarker ?~ x)
        <$> (rs ^. lsrMarker)
