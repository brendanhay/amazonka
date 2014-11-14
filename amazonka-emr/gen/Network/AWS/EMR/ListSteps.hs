{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.EMR.ListSteps
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides a list of steps for the cluster.
module Network.AWS.EMR.ListSteps
    (
    -- * Request
      ListSteps
    -- ** Request constructor
    , listSteps
    -- ** Request lenses
    , lsClusterId
    , lsMarker
    , lsStepStates

    -- * Response
    , ListStepsResponse
    -- ** Response constructor
    , listStepsResponse
    -- ** Response lenses
    , lsrMarker
    , lsrSteps
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.EMR.Types

data ListSteps = ListSteps
    { _lsClusterId  :: Text
    , _lsMarker     :: Maybe Text
    , _lsStepStates :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListSteps' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsClusterId' @::@ 'Text'
--
-- * 'lsMarker' @::@ 'Maybe' 'Text'
--
-- * 'lsStepStates' @::@ ['Text']
--
listSteps :: Text -- ^ 'lsClusterId'
          -> ListSteps
listSteps p1 = ListSteps
    { _lsClusterId  = p1
    , _lsStepStates = mempty
    , _lsMarker     = Nothing
    }

-- | The identifier of the cluster for which to list the steps.
lsClusterId :: Lens' ListSteps Text
lsClusterId = lens _lsClusterId (\s a -> s { _lsClusterId = a })

-- | The pagination token that indicates the next set of results to retrieve.
lsMarker :: Lens' ListSteps (Maybe Text)
lsMarker = lens _lsMarker (\s a -> s { _lsMarker = a })

-- | The filter to limit the step list based on certain states.
lsStepStates :: Lens' ListSteps [Text]
lsStepStates = lens _lsStepStates (\s a -> s { _lsStepStates = a })

instance ToPath ListSteps where
    toPath = const "/"

instance ToQuery ListSteps where
    toQuery = const mempty

instance ToHeaders ListSteps

instance ToBody ListSteps where
    toBody = toBody . encode . _lsClusterId

data ListStepsResponse = ListStepsResponse
    { _lsrMarker :: Maybe Text
    , _lsrSteps  :: [StepSummary]
    } deriving (Eq, Show, Generic)

-- | 'ListStepsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrMarker' @::@ 'Maybe' 'Text'
--
-- * 'lsrSteps' @::@ ['StepSummary']
--
listStepsResponse :: ListStepsResponse
listStepsResponse = ListStepsResponse
    { _lsrSteps  = mempty
    , _lsrMarker = Nothing
    }

-- | The pagination token that indicates the next set of results to retrieve.
lsrMarker :: Lens' ListStepsResponse (Maybe Text)
lsrMarker = lens _lsrMarker (\s a -> s { _lsrMarker = a })

-- | The filtered list of steps for the cluster.
lsrSteps :: Lens' ListStepsResponse [StepSummary]
lsrSteps = lens _lsrSteps (\s a -> s { _lsrSteps = a })

instance AWSRequest ListSteps where
    type Sv ListSteps = EMR
    type Rs ListSteps = ListStepsResponse

    request  = post
    response = jsonResponse $ \h o -> ListStepsResponse
        <$> o .: "Marker"
        <*> o .: "Steps"
