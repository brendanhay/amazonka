{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.ListSteps
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Provides a list of steps for the cluster.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_ListSteps.html>
module Network.AWS.EMR.ListSteps
    (
    -- * Request
      ListSteps
    -- ** Request constructor
    , listSteps
    -- ** Request lenses
    , lsClusterId
    , lsMarker
    , lsStepIds
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
import Network.AWS.Request.JSON
import Network.AWS.EMR.Types
import qualified GHC.Exts

data ListSteps = ListSteps
    { _lsClusterId  :: Text
    , _lsMarker     :: Maybe Text
    , _lsStepIds    :: List "StepIds" Text
    , _lsStepStates :: List "StepStates" StepState
    } deriving (Eq, Read, Show)

-- | 'ListSteps' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsClusterId' @::@ 'Text'
--
-- * 'lsMarker' @::@ 'Maybe' 'Text'
--
-- * 'lsStepIds' @::@ ['Text']
--
-- * 'lsStepStates' @::@ ['StepState']
--
listSteps :: Text -- ^ 'lsClusterId'
          -> ListSteps
listSteps p1 = ListSteps
    { _lsClusterId  = p1
    , _lsStepStates = mempty
    , _lsStepIds    = mempty
    , _lsMarker     = Nothing
    }

-- | The identifier of the cluster for which to list the steps.
lsClusterId :: Lens' ListSteps Text
lsClusterId = lens _lsClusterId (\s a -> s { _lsClusterId = a })

-- | The pagination token that indicates the next set of results to retrieve.
lsMarker :: Lens' ListSteps (Maybe Text)
lsMarker = lens _lsMarker (\s a -> s { _lsMarker = a })

-- | The filter to limit the step list based on the identifier of the steps.
lsStepIds :: Lens' ListSteps [Text]
lsStepIds = lens _lsStepIds (\s a -> s { _lsStepIds = a }) . _List

-- | The filter to limit the step list based on certain states.
lsStepStates :: Lens' ListSteps [StepState]
lsStepStates = lens _lsStepStates (\s a -> s { _lsStepStates = a }) . _List

data ListStepsResponse = ListStepsResponse
    { _lsrMarker :: Maybe Text
    , _lsrSteps  :: List "Steps" StepSummary
    } deriving (Eq, Read, Show)

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
lsrSteps = lens _lsrSteps (\s a -> s { _lsrSteps = a }) . _List

instance ToPath ListSteps where
    toPath = const "/"

instance ToQuery ListSteps where
    toQuery = const mempty

instance ToHeaders ListSteps

instance ToJSON ListSteps where
    toJSON ListSteps{..} = object
        [ "ClusterId"  .= _lsClusterId
        , "StepStates" .= _lsStepStates
        , "StepIds"    .= _lsStepIds
        , "Marker"     .= _lsMarker
        ]

instance AWSRequest ListSteps where
    type Sv ListSteps = EMR
    type Rs ListSteps = ListStepsResponse

    request  = post "ListSteps"
    response = jsonResponse

instance FromJSON ListStepsResponse where
    parseJSON = withObject "ListStepsResponse" $ \o -> ListStepsResponse
        <$> o .:? "Marker"
        <*> o .:? "Steps" .!= mempty

instance AWSPager ListSteps where
    page rq rs
        | stop (rs ^. lsrMarker) = Nothing
        | otherwise = (\x -> rq & lsMarker ?~ x)
            <$> (rs ^. lsrMarker)
