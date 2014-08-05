{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.EMR.V2009_03_31.ListSteps where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.EMR.V2009_03_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListSteps' request.
listSteps :: Text -- ^ '_lsiClusterId'
          -> ListSteps
listSteps p1 = ListSteps
    { _lsiClusterId = p1
    , _lsiMarker = Nothing
    , _lsiStepStates = mempty
    }

data ListSteps = ListSteps
    { _lsiClusterId :: Text
      -- ^ The identifier of the cluster for which to list the steps.
    , _lsiMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve.
    , _lsiStepStates :: [StepState]
      -- ^ The filter to limit the step list based on certain states.
    } deriving (Show, Generic)

makeLenses ''ListSteps

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

makeLenses ''ListStepsResponse

instance FromJSON ListStepsResponse

instance AWSRequest ListSteps where
    type Sv ListSteps = EMR
    type Rs ListSteps = ListStepsResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListSteps where
    next rq rs = (\x -> rq { _lsiMarker = Just x })
        <$> (_lsoMarker rs)
