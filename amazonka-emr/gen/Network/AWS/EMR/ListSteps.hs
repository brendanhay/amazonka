{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListSteps
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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
    , lsStepIds
    , lsStepStates
    , lsMarker
    , lsClusterId

    -- * Response
    , ListStepsResponse
    -- ** Response constructor
    , listStepsResponse
    -- ** Response lenses
    , lsrSteps
    , lsrMarker
    , lsrStatus
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This input determines which steps to list.
--
-- /See:/ 'listSteps' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsStepIds'
--
-- * 'lsStepStates'
--
-- * 'lsMarker'
--
-- * 'lsClusterId'
data ListSteps = ListSteps'
    { _lsStepIds    :: !(Maybe [Text])
    , _lsStepStates :: !(Maybe [StepState])
    , _lsMarker     :: !(Maybe Text)
    , _lsClusterId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListSteps' smart constructor.
listSteps :: Text -> ListSteps
listSteps pClusterId =
    ListSteps'
    { _lsStepIds = Nothing
    , _lsStepStates = Nothing
    , _lsMarker = Nothing
    , _lsClusterId = pClusterId
    }

-- | The filter to limit the step list based on the identifier of the steps.
lsStepIds :: Lens' ListSteps [Text]
lsStepIds = lens _lsStepIds (\ s a -> s{_lsStepIds = a}) . _Default;

-- | The filter to limit the step list based on certain states.
lsStepStates :: Lens' ListSteps [StepState]
lsStepStates = lens _lsStepStates (\ s a -> s{_lsStepStates = a}) . _Default;

-- | The pagination token that indicates the next set of results to retrieve.
lsMarker :: Lens' ListSteps (Maybe Text)
lsMarker = lens _lsMarker (\ s a -> s{_lsMarker = a});

-- | The identifier of the cluster for which to list the steps.
lsClusterId :: Lens' ListSteps Text
lsClusterId = lens _lsClusterId (\ s a -> s{_lsClusterId = a});

instance AWSPager ListSteps where
        page rq rs
          | stop (rs ^. lsrMarker) = Nothing
          | stop (rs ^. lsrSteps) = Nothing
          | otherwise = Just $ rq & lsMarker .~ rs ^. lsrMarker

instance AWSRequest ListSteps where
        type Sv ListSteps = EMR
        type Rs ListSteps = ListStepsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListStepsResponse' <$>
                   (x .?> "Steps" .!@ mempty) <*> (x .?> "Marker") <*>
                     (pure (fromEnum s)))

instance ToHeaders ListSteps where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.ListSteps" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSteps where
        toJSON ListSteps'{..}
          = object
              ["StepIds" .= _lsStepIds,
               "StepStates" .= _lsStepStates, "Marker" .= _lsMarker,
               "ClusterId" .= _lsClusterId]

instance ToPath ListSteps where
        toPath = const "/"

instance ToQuery ListSteps where
        toQuery = const mempty

-- | This output contains the list of steps.
--
-- /See:/ 'listStepsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrSteps'
--
-- * 'lsrMarker'
--
-- * 'lsrStatus'
data ListStepsResponse = ListStepsResponse'
    { _lsrSteps  :: !(Maybe [StepSummary])
    , _lsrMarker :: !(Maybe Text)
    , _lsrStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListStepsResponse' smart constructor.
listStepsResponse :: Int -> ListStepsResponse
listStepsResponse pStatus =
    ListStepsResponse'
    { _lsrSteps = Nothing
    , _lsrMarker = Nothing
    , _lsrStatus = pStatus
    }

-- | The filtered list of steps for the cluster.
lsrSteps :: Lens' ListStepsResponse [StepSummary]
lsrSteps = lens _lsrSteps (\ s a -> s{_lsrSteps = a}) . _Default;

-- | The pagination token that indicates the next set of results to retrieve.
lsrMarker :: Lens' ListStepsResponse (Maybe Text)
lsrMarker = lens _lsrMarker (\ s a -> s{_lsrMarker = a});

-- | FIXME: Undocumented member.
lsrStatus :: Lens' ListStepsResponse Int
lsrStatus = lens _lsrStatus (\ s a -> s{_lsrStatus = a});
