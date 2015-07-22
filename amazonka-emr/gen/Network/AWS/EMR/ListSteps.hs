{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListSteps
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of steps for the cluster.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_ListSteps.html>
module Network.AWS.EMR.ListSteps
    (
    -- * Request
      ListSteps
    -- ** Request constructor
    , listSteps
    -- ** Request lenses
    , lsrqStepIds
    , lsrqStepStates
    , lsrqMarker
    , lsrqClusterId

    -- * Response
    , ListStepsResponse
    -- ** Response constructor
    , listStepsResponse
    -- ** Response lenses
    , lsrsSteps
    , lsrsMarker
    , lsrsStatus
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
-- * 'lsrqStepIds'
--
-- * 'lsrqStepStates'
--
-- * 'lsrqMarker'
--
-- * 'lsrqClusterId'
data ListSteps = ListSteps'
    { _lsrqStepIds    :: !(Maybe [Text])
    , _lsrqStepStates :: !(Maybe [StepState])
    , _lsrqMarker     :: !(Maybe Text)
    , _lsrqClusterId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListSteps' smart constructor.
listSteps :: Text -> ListSteps
listSteps pClusterId_ =
    ListSteps'
    { _lsrqStepIds = Nothing
    , _lsrqStepStates = Nothing
    , _lsrqMarker = Nothing
    , _lsrqClusterId = pClusterId_
    }

-- | The filter to limit the step list based on the identifier of the steps.
lsrqStepIds :: Lens' ListSteps [Text]
lsrqStepIds = lens _lsrqStepIds (\ s a -> s{_lsrqStepIds = a}) . _Default;

-- | The filter to limit the step list based on certain states.
lsrqStepStates :: Lens' ListSteps [StepState]
lsrqStepStates = lens _lsrqStepStates (\ s a -> s{_lsrqStepStates = a}) . _Default;

-- | The pagination token that indicates the next set of results to retrieve.
lsrqMarker :: Lens' ListSteps (Maybe Text)
lsrqMarker = lens _lsrqMarker (\ s a -> s{_lsrqMarker = a});

-- | The identifier of the cluster for which to list the steps.
lsrqClusterId :: Lens' ListSteps Text
lsrqClusterId = lens _lsrqClusterId (\ s a -> s{_lsrqClusterId = a});

instance AWSPager ListSteps where
        page rq rs
          | stop (rs ^. lsrsMarker) = Nothing
          | stop (rs ^. lsrsSteps) = Nothing
          | otherwise =
            Just $ rq & lsrqMarker .~ rs ^. lsrsMarker

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
              ["StepIds" .= _lsrqStepIds,
               "StepStates" .= _lsrqStepStates,
               "Marker" .= _lsrqMarker,
               "ClusterId" .= _lsrqClusterId]

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
-- * 'lsrsSteps'
--
-- * 'lsrsMarker'
--
-- * 'lsrsStatus'
data ListStepsResponse = ListStepsResponse'
    { _lsrsSteps  :: !(Maybe [StepSummary])
    , _lsrsMarker :: !(Maybe Text)
    , _lsrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListStepsResponse' smart constructor.
listStepsResponse :: Int -> ListStepsResponse
listStepsResponse pStatus_ =
    ListStepsResponse'
    { _lsrsSteps = Nothing
    , _lsrsMarker = Nothing
    , _lsrsStatus = pStatus_
    }

-- | The filtered list of steps for the cluster.
lsrsSteps :: Lens' ListStepsResponse [StepSummary]
lsrsSteps = lens _lsrsSteps (\ s a -> s{_lsrsSteps = a}) . _Default;

-- | The pagination token that indicates the next set of results to retrieve.
lsrsMarker :: Lens' ListStepsResponse (Maybe Text)
lsrsMarker = lens _lsrsMarker (\ s a -> s{_lsrsMarker = a});

-- | FIXME: Undocumented member.
lsrsStatus :: Lens' ListStepsResponse Int
lsrsStatus = lens _lsrsStatus (\ s a -> s{_lsrsStatus = a});
