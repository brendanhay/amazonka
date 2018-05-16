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
-- Module      : Network.AWS.SageMaker.ListModels
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists models created with the <http://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateModel.html CreateModel> API.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListModels
    (
    -- * Creating a Request
      listModels
    , ListModels
    -- * Request Lenses
    , lmNameContains
    , lmCreationTimeAfter
    , lmNextToken
    , lmSortOrder
    , lmCreationTimeBefore
    , lmMaxResults
    , lmSortBy

    -- * Destructuring the Response
    , listModelsResponse
    , ListModelsResponse
    -- * Response Lenses
    , lmrsNextToken
    , lmrsResponseStatus
    , lmrsModels
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'listModels' smart constructor.
data ListModels = ListModels'
  { _lmNameContains       :: !(Maybe Text)
  , _lmCreationTimeAfter  :: !(Maybe POSIX)
  , _lmNextToken          :: !(Maybe Text)
  , _lmSortOrder          :: !(Maybe OrderKey)
  , _lmCreationTimeBefore :: !(Maybe POSIX)
  , _lmMaxResults         :: !(Maybe Nat)
  , _lmSortBy             :: !(Maybe ModelSortKey)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListModels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmNameContains' - A string in the training job name. This filter returns only models in the training job whose name contains the specified string.
--
-- * 'lmCreationTimeAfter' - A filter that returns only models created after the specified time (timestamp).
--
-- * 'lmNextToken' - If the response to a previous @ListModels@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of models, use the token in the next request.
--
-- * 'lmSortOrder' - The sort order for results. The default is @Ascending@ .
--
-- * 'lmCreationTimeBefore' - A filter that returns only models created before the specified time (timestamp).
--
-- * 'lmMaxResults' - The maximum number of models to return in the response.
--
-- * 'lmSortBy' - Sorts the list of results. The default is @CreationTime@ .
listModels
    :: ListModels
listModels =
  ListModels'
    { _lmNameContains = Nothing
    , _lmCreationTimeAfter = Nothing
    , _lmNextToken = Nothing
    , _lmSortOrder = Nothing
    , _lmCreationTimeBefore = Nothing
    , _lmMaxResults = Nothing
    , _lmSortBy = Nothing
    }


-- | A string in the training job name. This filter returns only models in the training job whose name contains the specified string.
lmNameContains :: Lens' ListModels (Maybe Text)
lmNameContains = lens _lmNameContains (\ s a -> s{_lmNameContains = a})

-- | A filter that returns only models created after the specified time (timestamp).
lmCreationTimeAfter :: Lens' ListModels (Maybe UTCTime)
lmCreationTimeAfter = lens _lmCreationTimeAfter (\ s a -> s{_lmCreationTimeAfter = a}) . mapping _Time

-- | If the response to a previous @ListModels@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of models, use the token in the next request.
lmNextToken :: Lens' ListModels (Maybe Text)
lmNextToken = lens _lmNextToken (\ s a -> s{_lmNextToken = a})

-- | The sort order for results. The default is @Ascending@ .
lmSortOrder :: Lens' ListModels (Maybe OrderKey)
lmSortOrder = lens _lmSortOrder (\ s a -> s{_lmSortOrder = a})

-- | A filter that returns only models created before the specified time (timestamp).
lmCreationTimeBefore :: Lens' ListModels (Maybe UTCTime)
lmCreationTimeBefore = lens _lmCreationTimeBefore (\ s a -> s{_lmCreationTimeBefore = a}) . mapping _Time

-- | The maximum number of models to return in the response.
lmMaxResults :: Lens' ListModels (Maybe Natural)
lmMaxResults = lens _lmMaxResults (\ s a -> s{_lmMaxResults = a}) . mapping _Nat

-- | Sorts the list of results. The default is @CreationTime@ .
lmSortBy :: Lens' ListModels (Maybe ModelSortKey)
lmSortBy = lens _lmSortBy (\ s a -> s{_lmSortBy = a})

instance AWSPager ListModels where
        page rq rs
          | stop (rs ^. lmrsNextToken) = Nothing
          | stop (rs ^. lmrsModels) = Nothing
          | otherwise =
            Just $ rq & lmNextToken .~ rs ^. lmrsNextToken

instance AWSRequest ListModels where
        type Rs ListModels = ListModelsResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 ListModelsResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "Models" .!@ mempty))

instance Hashable ListModels where

instance NFData ListModels where

instance ToHeaders ListModels where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.ListModels" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListModels where
        toJSON ListModels'{..}
          = object
              (catMaybes
                 [("NameContains" .=) <$> _lmNameContains,
                  ("CreationTimeAfter" .=) <$> _lmCreationTimeAfter,
                  ("NextToken" .=) <$> _lmNextToken,
                  ("SortOrder" .=) <$> _lmSortOrder,
                  ("CreationTimeBefore" .=) <$> _lmCreationTimeBefore,
                  ("MaxResults" .=) <$> _lmMaxResults,
                  ("SortBy" .=) <$> _lmSortBy])

instance ToPath ListModels where
        toPath = const "/"

instance ToQuery ListModels where
        toQuery = const mempty

-- | /See:/ 'listModelsResponse' smart constructor.
data ListModelsResponse = ListModelsResponse'
  { _lmrsNextToken      :: !(Maybe Text)
  , _lmrsResponseStatus :: !Int
  , _lmrsModels         :: ![ModelSummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListModelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmrsNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of models, use it in the subsequent request.
--
-- * 'lmrsResponseStatus' - -- | The response status code.
--
-- * 'lmrsModels' - An array of @ModelSummary@ objects, each of which lists a model.
listModelsResponse
    :: Int -- ^ 'lmrsResponseStatus'
    -> ListModelsResponse
listModelsResponse pResponseStatus_ =
  ListModelsResponse'
    { _lmrsNextToken = Nothing
    , _lmrsResponseStatus = pResponseStatus_
    , _lmrsModels = mempty
    }


-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of models, use it in the subsequent request.
lmrsNextToken :: Lens' ListModelsResponse (Maybe Text)
lmrsNextToken = lens _lmrsNextToken (\ s a -> s{_lmrsNextToken = a})

-- | -- | The response status code.
lmrsResponseStatus :: Lens' ListModelsResponse Int
lmrsResponseStatus = lens _lmrsResponseStatus (\ s a -> s{_lmrsResponseStatus = a})

-- | An array of @ModelSummary@ objects, each of which lists a model.
lmrsModels :: Lens' ListModelsResponse [ModelSummary]
lmrsModels = lens _lmrsModels (\ s a -> s{_lmrsModels = a}) . _Coerce

instance NFData ListModelsResponse where
