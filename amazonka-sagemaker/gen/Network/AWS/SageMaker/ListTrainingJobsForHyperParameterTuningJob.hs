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
-- Module      : Network.AWS.SageMaker.ListTrainingJobsForHyperParameterTuningJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of 'TrainingJobSummary' objects that describe the training jobs that a hyperparameter tuning job launched.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTrainingJobsForHyperParameterTuningJob
    (
    -- * Creating a Request
      listTrainingJobsForHyperParameterTuningJob
    , ListTrainingJobsForHyperParameterTuningJob
    -- * Request Lenses
    , ltjfhptjNextToken
    , ltjfhptjSortOrder
    , ltjfhptjStatusEquals
    , ltjfhptjMaxResults
    , ltjfhptjSortBy
    , ltjfhptjHyperParameterTuningJobName

    -- * Destructuring the Response
    , listTrainingJobsForHyperParameterTuningJobResponse
    , ListTrainingJobsForHyperParameterTuningJobResponse
    -- * Response Lenses
    , ltjfhptjrsNextToken
    , ltjfhptjrsResponseStatus
    , ltjfhptjrsTrainingJobSummaries
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'listTrainingJobsForHyperParameterTuningJob' smart constructor.
data ListTrainingJobsForHyperParameterTuningJob = ListTrainingJobsForHyperParameterTuningJob'
  { _ltjfhptjNextToken                   :: !(Maybe Text)
  , _ltjfhptjSortOrder                   :: !(Maybe SortOrder)
  , _ltjfhptjStatusEquals                :: !(Maybe TrainingJobStatus)
  , _ltjfhptjMaxResults                  :: !(Maybe Nat)
  , _ltjfhptjSortBy                      :: !(Maybe TrainingJobSortByOptions)
  , _ltjfhptjHyperParameterTuningJobName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTrainingJobsForHyperParameterTuningJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltjfhptjNextToken' - If the result of the previous @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
--
-- * 'ltjfhptjSortOrder' - The sort order for results. The default is @Ascending@ .
--
-- * 'ltjfhptjStatusEquals' - A filter that returns only training jobs with the specified status.
--
-- * 'ltjfhptjMaxResults' - The maximum number of training jobs to return. The default value is 10.
--
-- * 'ltjfhptjSortBy' - The field to sort results by. The default is @Name@ . If the value of this field is @FinalObjectiveMetricValue@ , any training jobs that did not return an objective metric are not listed.
--
-- * 'ltjfhptjHyperParameterTuningJobName' - The name of the tuning job whose training jobs you want to list.
listTrainingJobsForHyperParameterTuningJob
    :: Text -- ^ 'ltjfhptjHyperParameterTuningJobName'
    -> ListTrainingJobsForHyperParameterTuningJob
listTrainingJobsForHyperParameterTuningJob pHyperParameterTuningJobName_ =
  ListTrainingJobsForHyperParameterTuningJob'
    { _ltjfhptjNextToken = Nothing
    , _ltjfhptjSortOrder = Nothing
    , _ltjfhptjStatusEquals = Nothing
    , _ltjfhptjMaxResults = Nothing
    , _ltjfhptjSortBy = Nothing
    , _ltjfhptjHyperParameterTuningJobName = pHyperParameterTuningJobName_
    }


-- | If the result of the previous @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
ltjfhptjNextToken :: Lens' ListTrainingJobsForHyperParameterTuningJob (Maybe Text)
ltjfhptjNextToken = lens _ltjfhptjNextToken (\ s a -> s{_ltjfhptjNextToken = a})

-- | The sort order for results. The default is @Ascending@ .
ltjfhptjSortOrder :: Lens' ListTrainingJobsForHyperParameterTuningJob (Maybe SortOrder)
ltjfhptjSortOrder = lens _ltjfhptjSortOrder (\ s a -> s{_ltjfhptjSortOrder = a})

-- | A filter that returns only training jobs with the specified status.
ltjfhptjStatusEquals :: Lens' ListTrainingJobsForHyperParameterTuningJob (Maybe TrainingJobStatus)
ltjfhptjStatusEquals = lens _ltjfhptjStatusEquals (\ s a -> s{_ltjfhptjStatusEquals = a})

-- | The maximum number of training jobs to return. The default value is 10.
ltjfhptjMaxResults :: Lens' ListTrainingJobsForHyperParameterTuningJob (Maybe Natural)
ltjfhptjMaxResults = lens _ltjfhptjMaxResults (\ s a -> s{_ltjfhptjMaxResults = a}) . mapping _Nat

-- | The field to sort results by. The default is @Name@ . If the value of this field is @FinalObjectiveMetricValue@ , any training jobs that did not return an objective metric are not listed.
ltjfhptjSortBy :: Lens' ListTrainingJobsForHyperParameterTuningJob (Maybe TrainingJobSortByOptions)
ltjfhptjSortBy = lens _ltjfhptjSortBy (\ s a -> s{_ltjfhptjSortBy = a})

-- | The name of the tuning job whose training jobs you want to list.
ltjfhptjHyperParameterTuningJobName :: Lens' ListTrainingJobsForHyperParameterTuningJob Text
ltjfhptjHyperParameterTuningJobName = lens _ltjfhptjHyperParameterTuningJobName (\ s a -> s{_ltjfhptjHyperParameterTuningJobName = a})

instance AWSPager
           ListTrainingJobsForHyperParameterTuningJob
         where
        page rq rs
          | stop (rs ^. ltjfhptjrsNextToken) = Nothing
          | stop (rs ^. ltjfhptjrsTrainingJobSummaries) =
            Nothing
          | otherwise =
            Just $ rq &
              ltjfhptjNextToken .~ rs ^. ltjfhptjrsNextToken

instance AWSRequest
           ListTrainingJobsForHyperParameterTuningJob
         where
        type Rs ListTrainingJobsForHyperParameterTuningJob =
             ListTrainingJobsForHyperParameterTuningJobResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 ListTrainingJobsForHyperParameterTuningJobResponse'
                   <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "TrainingJobSummaries" .!@ mempty))

instance Hashable
           ListTrainingJobsForHyperParameterTuningJob
         where

instance NFData
           ListTrainingJobsForHyperParameterTuningJob
         where

instance ToHeaders
           ListTrainingJobsForHyperParameterTuningJob
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.ListTrainingJobsForHyperParameterTuningJob"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           ListTrainingJobsForHyperParameterTuningJob
         where
        toJSON
          ListTrainingJobsForHyperParameterTuningJob'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ltjfhptjNextToken,
                  ("SortOrder" .=) <$> _ltjfhptjSortOrder,
                  ("StatusEquals" .=) <$> _ltjfhptjStatusEquals,
                  ("MaxResults" .=) <$> _ltjfhptjMaxResults,
                  ("SortBy" .=) <$> _ltjfhptjSortBy,
                  Just
                    ("HyperParameterTuningJobName" .=
                       _ltjfhptjHyperParameterTuningJobName)])

instance ToPath
           ListTrainingJobsForHyperParameterTuningJob
         where
        toPath = const "/"

instance ToQuery
           ListTrainingJobsForHyperParameterTuningJob
         where
        toQuery = const mempty

-- | /See:/ 'listTrainingJobsForHyperParameterTuningJobResponse' smart constructor.
data ListTrainingJobsForHyperParameterTuningJobResponse = ListTrainingJobsForHyperParameterTuningJobResponse'
  { _ltjfhptjrsNextToken            :: !(Maybe Text)
  , _ltjfhptjrsResponseStatus       :: !Int
  , _ltjfhptjrsTrainingJobSummaries :: ![HyperParameterTrainingJobSummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTrainingJobsForHyperParameterTuningJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltjfhptjrsNextToken' - If the result of this @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
--
-- * 'ltjfhptjrsResponseStatus' - -- | The response status code.
--
-- * 'ltjfhptjrsTrainingJobSummaries' - A list of 'TrainingJobSummary' objects that describe the training jobs that the @ListTrainingJobsForHyperParameterTuningJob@ request returned.
listTrainingJobsForHyperParameterTuningJobResponse
    :: Int -- ^ 'ltjfhptjrsResponseStatus'
    -> ListTrainingJobsForHyperParameterTuningJobResponse
listTrainingJobsForHyperParameterTuningJobResponse pResponseStatus_ =
  ListTrainingJobsForHyperParameterTuningJobResponse'
    { _ltjfhptjrsNextToken = Nothing
    , _ltjfhptjrsResponseStatus = pResponseStatus_
    , _ltjfhptjrsTrainingJobSummaries = mempty
    }


-- | If the result of this @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
ltjfhptjrsNextToken :: Lens' ListTrainingJobsForHyperParameterTuningJobResponse (Maybe Text)
ltjfhptjrsNextToken = lens _ltjfhptjrsNextToken (\ s a -> s{_ltjfhptjrsNextToken = a})

-- | -- | The response status code.
ltjfhptjrsResponseStatus :: Lens' ListTrainingJobsForHyperParameterTuningJobResponse Int
ltjfhptjrsResponseStatus = lens _ltjfhptjrsResponseStatus (\ s a -> s{_ltjfhptjrsResponseStatus = a})

-- | A list of 'TrainingJobSummary' objects that describe the training jobs that the @ListTrainingJobsForHyperParameterTuningJob@ request returned.
ltjfhptjrsTrainingJobSummaries :: Lens' ListTrainingJobsForHyperParameterTuningJobResponse [HyperParameterTrainingJobSummary]
ltjfhptjrsTrainingJobSummaries = lens _ltjfhptjrsTrainingJobSummaries (\ s a -> s{_ltjfhptjrsTrainingJobSummaries = a}) . _Coerce

instance NFData
           ListTrainingJobsForHyperParameterTuningJobResponse
         where
