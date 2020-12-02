{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListSteps
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of steps for the cluster in reverse order unless you specify stepIds with the request.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListSteps
    (
    -- * Creating a Request
      listSteps
    , ListSteps
    -- * Request Lenses
    , lsStepIds
    , lsStepStates
    , lsMarker
    , lsClusterId

    -- * Destructuring the Response
    , listStepsResponse
    , ListStepsResponse
    -- * Response Lenses
    , lsrsSteps
    , lsrsMarker
    , lsrsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | This input determines which steps to list.
--
--
--
-- /See:/ 'listSteps' smart constructor.
data ListSteps = ListSteps'
  { _lsStepIds    :: !(Maybe [Text])
  , _lsStepStates :: !(Maybe [StepState])
  , _lsMarker     :: !(Maybe Text)
  , _lsClusterId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSteps' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsStepIds' - The filter to limit the step list based on the identifier of the steps.
--
-- * 'lsStepStates' - The filter to limit the step list based on certain states.
--
-- * 'lsMarker' - The pagination token that indicates the next set of results to retrieve.
--
-- * 'lsClusterId' - The identifier of the cluster for which to list the steps.
listSteps
    :: Text -- ^ 'lsClusterId'
    -> ListSteps
listSteps pClusterId_ =
  ListSteps'
    { _lsStepIds = Nothing
    , _lsStepStates = Nothing
    , _lsMarker = Nothing
    , _lsClusterId = pClusterId_
    }


-- | The filter to limit the step list based on the identifier of the steps.
lsStepIds :: Lens' ListSteps [Text]
lsStepIds = lens _lsStepIds (\ s a -> s{_lsStepIds = a}) . _Default . _Coerce

-- | The filter to limit the step list based on certain states.
lsStepStates :: Lens' ListSteps [StepState]
lsStepStates = lens _lsStepStates (\ s a -> s{_lsStepStates = a}) . _Default . _Coerce

-- | The pagination token that indicates the next set of results to retrieve.
lsMarker :: Lens' ListSteps (Maybe Text)
lsMarker = lens _lsMarker (\ s a -> s{_lsMarker = a})

-- | The identifier of the cluster for which to list the steps.
lsClusterId :: Lens' ListSteps Text
lsClusterId = lens _lsClusterId (\ s a -> s{_lsClusterId = a})

instance AWSPager ListSteps where
        page rq rs
          | stop (rs ^. lsrsMarker) = Nothing
          | stop (rs ^. lsrsSteps) = Nothing
          | otherwise =
            Just $ rq & lsMarker .~ rs ^. lsrsMarker

instance AWSRequest ListSteps where
        type Rs ListSteps = ListStepsResponse
        request = postJSON emr
        response
          = receiveJSON
              (\ s h x ->
                 ListStepsResponse' <$>
                   (x .?> "Steps" .!@ mempty) <*> (x .?> "Marker") <*>
                     (pure (fromEnum s)))

instance Hashable ListSteps where

instance NFData ListSteps where

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
              (catMaybes
                 [("StepIds" .=) <$> _lsStepIds,
                  ("StepStates" .=) <$> _lsStepStates,
                  ("Marker" .=) <$> _lsMarker,
                  Just ("ClusterId" .= _lsClusterId)])

instance ToPath ListSteps where
        toPath = const "/"

instance ToQuery ListSteps where
        toQuery = const mempty

-- | This output contains the list of steps returned in reverse order. This means that the last step is the first element in the list.
--
--
--
-- /See:/ 'listStepsResponse' smart constructor.
data ListStepsResponse = ListStepsResponse'
  { _lsrsSteps          :: !(Maybe [StepSummary])
  , _lsrsMarker         :: !(Maybe Text)
  , _lsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStepsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrsSteps' - The filtered list of steps for the cluster.
--
-- * 'lsrsMarker' - The pagination token that indicates the next set of results to retrieve.
--
-- * 'lsrsResponseStatus' - -- | The response status code.
listStepsResponse
    :: Int -- ^ 'lsrsResponseStatus'
    -> ListStepsResponse
listStepsResponse pResponseStatus_ =
  ListStepsResponse'
    { _lsrsSteps = Nothing
    , _lsrsMarker = Nothing
    , _lsrsResponseStatus = pResponseStatus_
    }


-- | The filtered list of steps for the cluster.
lsrsSteps :: Lens' ListStepsResponse [StepSummary]
lsrsSteps = lens _lsrsSteps (\ s a -> s{_lsrsSteps = a}) . _Default . _Coerce

-- | The pagination token that indicates the next set of results to retrieve.
lsrsMarker :: Lens' ListStepsResponse (Maybe Text)
lsrsMarker = lens _lsrsMarker (\ s a -> s{_lsrsMarker = a})

-- | -- | The response status code.
lsrsResponseStatus :: Lens' ListStepsResponse Int
lsrsResponseStatus = lens _lsrsResponseStatus (\ s a -> s{_lsrsResponseStatus = a})

instance NFData ListStepsResponse where
