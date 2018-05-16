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
-- Module      : Network.AWS.CodePipeline.ListPipelineExecutions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a summary of the most recent executions for a pipeline.
--
--
module Network.AWS.CodePipeline.ListPipelineExecutions
    (
    -- * Creating a Request
      listPipelineExecutions
    , ListPipelineExecutions
    -- * Request Lenses
    , lpeNextToken
    , lpeMaxResults
    , lpePipelineName

    -- * Destructuring the Response
    , listPipelineExecutionsResponse
    , ListPipelineExecutionsResponse
    -- * Response Lenses
    , lpersNextToken
    , lpersPipelineExecutionSummaries
    , lpersResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a ListPipelineExecutions action.
--
--
--
-- /See:/ 'listPipelineExecutions' smart constructor.
data ListPipelineExecutions = ListPipelineExecutions'
  { _lpeNextToken    :: !(Maybe Text)
  , _lpeMaxResults   :: !(Maybe Nat)
  , _lpePipelineName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPipelineExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpeNextToken' - The token that was returned from the previous ListPipelineExecutions call, which can be used to return the next set of pipeline executions in the list.
--
-- * 'lpeMaxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value. The available pipeline execution history is limited to the most recent 12 months, based on pipeline execution start times. Default value is 100.
--
-- * 'lpePipelineName' - The name of the pipeline for which you want to get execution summary information.
listPipelineExecutions
    :: Text -- ^ 'lpePipelineName'
    -> ListPipelineExecutions
listPipelineExecutions pPipelineName_ =
  ListPipelineExecutions'
    { _lpeNextToken = Nothing
    , _lpeMaxResults = Nothing
    , _lpePipelineName = pPipelineName_
    }


-- | The token that was returned from the previous ListPipelineExecutions call, which can be used to return the next set of pipeline executions in the list.
lpeNextToken :: Lens' ListPipelineExecutions (Maybe Text)
lpeNextToken = lens _lpeNextToken (\ s a -> s{_lpeNextToken = a})

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value. The available pipeline execution history is limited to the most recent 12 months, based on pipeline execution start times. Default value is 100.
lpeMaxResults :: Lens' ListPipelineExecutions (Maybe Natural)
lpeMaxResults = lens _lpeMaxResults (\ s a -> s{_lpeMaxResults = a}) . mapping _Nat

-- | The name of the pipeline for which you want to get execution summary information.
lpePipelineName :: Lens' ListPipelineExecutions Text
lpePipelineName = lens _lpePipelineName (\ s a -> s{_lpePipelineName = a})

instance AWSRequest ListPipelineExecutions where
        type Rs ListPipelineExecutions =
             ListPipelineExecutionsResponse
        request = postJSON codePipeline
        response
          = receiveJSON
              (\ s h x ->
                 ListPipelineExecutionsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "pipelineExecutionSummaries" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListPipelineExecutions where

instance NFData ListPipelineExecutions where

instance ToHeaders ListPipelineExecutions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.ListPipelineExecutions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListPipelineExecutions where
        toJSON ListPipelineExecutions'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lpeNextToken,
                  ("maxResults" .=) <$> _lpeMaxResults,
                  Just ("pipelineName" .= _lpePipelineName)])

instance ToPath ListPipelineExecutions where
        toPath = const "/"

instance ToQuery ListPipelineExecutions where
        toQuery = const mempty

-- | Represents the output of a ListPipelineExecutions action.
--
--
--
-- /See:/ 'listPipelineExecutionsResponse' smart constructor.
data ListPipelineExecutionsResponse = ListPipelineExecutionsResponse'
  { _lpersNextToken                  :: !(Maybe Text)
  , _lpersPipelineExecutionSummaries :: !(Maybe [PipelineExecutionSummary])
  , _lpersResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPipelineExecutionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpersNextToken' - A token that can be used in the next ListPipelineExecutions call. To view all items in the list, continue to call this operation with each subsequent token until no more nextToken values are returned.
--
-- * 'lpersPipelineExecutionSummaries' - A list of executions in the history of a pipeline.
--
-- * 'lpersResponseStatus' - -- | The response status code.
listPipelineExecutionsResponse
    :: Int -- ^ 'lpersResponseStatus'
    -> ListPipelineExecutionsResponse
listPipelineExecutionsResponse pResponseStatus_ =
  ListPipelineExecutionsResponse'
    { _lpersNextToken = Nothing
    , _lpersPipelineExecutionSummaries = Nothing
    , _lpersResponseStatus = pResponseStatus_
    }


-- | A token that can be used in the next ListPipelineExecutions call. To view all items in the list, continue to call this operation with each subsequent token until no more nextToken values are returned.
lpersNextToken :: Lens' ListPipelineExecutionsResponse (Maybe Text)
lpersNextToken = lens _lpersNextToken (\ s a -> s{_lpersNextToken = a})

-- | A list of executions in the history of a pipeline.
lpersPipelineExecutionSummaries :: Lens' ListPipelineExecutionsResponse [PipelineExecutionSummary]
lpersPipelineExecutionSummaries = lens _lpersPipelineExecutionSummaries (\ s a -> s{_lpersPipelineExecutionSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
lpersResponseStatus :: Lens' ListPipelineExecutionsResponse Int
lpersResponseStatus = lens _lpersResponseStatus (\ s a -> s{_lpersResponseStatus = a})

instance NFData ListPipelineExecutionsResponse where
