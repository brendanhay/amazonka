{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.ListActionExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the action executions that have occurred in a pipeline.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodePipeline.ListActionExecutions
  ( -- * Creating a Request
    listActionExecutions,
    ListActionExecutions,

    -- * Request Lenses
    laeNextToken,
    laeFilter,
    laeMaxResults,
    laePipelineName,

    -- * Destructuring the Response
    listActionExecutionsResponse,
    ListActionExecutionsResponse,

    -- * Response Lenses
    laersActionExecutionDetails,
    laersNextToken,
    laersResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listActionExecutions' smart constructor.
data ListActionExecutions = ListActionExecutions'
  { _laeNextToken ::
      !(Maybe Text),
    _laeFilter :: !(Maybe ActionExecutionFilter),
    _laeMaxResults :: !(Maybe Nat),
    _laePipelineName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListActionExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laeNextToken' - The token that was returned from the previous @ListActionExecutions@ call, which can be used to return the next set of action executions in the list.
--
-- * 'laeFilter' - Input information used to filter action execution history.
--
-- * 'laeMaxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value. Action execution history is retained for up to 12 months, based on action execution start times. Default value is 100.
--
-- * 'laePipelineName' - The name of the pipeline for which you want to list action execution history.
listActionExecutions ::
  -- | 'laePipelineName'
  Text ->
  ListActionExecutions
listActionExecutions pPipelineName_ =
  ListActionExecutions'
    { _laeNextToken = Nothing,
      _laeFilter = Nothing,
      _laeMaxResults = Nothing,
      _laePipelineName = pPipelineName_
    }

-- | The token that was returned from the previous @ListActionExecutions@ call, which can be used to return the next set of action executions in the list.
laeNextToken :: Lens' ListActionExecutions (Maybe Text)
laeNextToken = lens _laeNextToken (\s a -> s {_laeNextToken = a})

-- | Input information used to filter action execution history.
laeFilter :: Lens' ListActionExecutions (Maybe ActionExecutionFilter)
laeFilter = lens _laeFilter (\s a -> s {_laeFilter = a})

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value. Action execution history is retained for up to 12 months, based on action execution start times. Default value is 100.
laeMaxResults :: Lens' ListActionExecutions (Maybe Natural)
laeMaxResults = lens _laeMaxResults (\s a -> s {_laeMaxResults = a}) . mapping _Nat

-- | The name of the pipeline for which you want to list action execution history.
laePipelineName :: Lens' ListActionExecutions Text
laePipelineName = lens _laePipelineName (\s a -> s {_laePipelineName = a})

instance AWSPager ListActionExecutions where
  page rq rs
    | stop (rs ^. laersNextToken) = Nothing
    | stop (rs ^. laersActionExecutionDetails) = Nothing
    | otherwise = Just $ rq & laeNextToken .~ rs ^. laersNextToken

instance AWSRequest ListActionExecutions where
  type Rs ListActionExecutions = ListActionExecutionsResponse
  request = postJSON codePipeline
  response =
    receiveJSON
      ( \s h x ->
          ListActionExecutionsResponse'
            <$> (x .?> "actionExecutionDetails" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListActionExecutions

instance NFData ListActionExecutions

instance ToHeaders ListActionExecutions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodePipeline_20150709.ListActionExecutions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListActionExecutions where
  toJSON ListActionExecutions' {..} =
    object
      ( catMaybes
          [ ("nextToken" .=) <$> _laeNextToken,
            ("filter" .=) <$> _laeFilter,
            ("maxResults" .=) <$> _laeMaxResults,
            Just ("pipelineName" .= _laePipelineName)
          ]
      )

instance ToPath ListActionExecutions where
  toPath = const "/"

instance ToQuery ListActionExecutions where
  toQuery = const mempty

-- | /See:/ 'listActionExecutionsResponse' smart constructor.
data ListActionExecutionsResponse = ListActionExecutionsResponse'
  { _laersActionExecutionDetails ::
      !(Maybe [ActionExecutionDetail]),
    _laersNextToken :: !(Maybe Text),
    _laersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListActionExecutionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laersActionExecutionDetails' - The details for a list of recent executions, such as action execution ID.
--
-- * 'laersNextToken' - If the amount of returned information is significantly large, an identifier is also returned and can be used in a subsequent @ListActionExecutions@ call to return the next set of action executions in the list.
--
-- * 'laersResponseStatus' - -- | The response status code.
listActionExecutionsResponse ::
  -- | 'laersResponseStatus'
  Int ->
  ListActionExecutionsResponse
listActionExecutionsResponse pResponseStatus_ =
  ListActionExecutionsResponse'
    { _laersActionExecutionDetails =
        Nothing,
      _laersNextToken = Nothing,
      _laersResponseStatus = pResponseStatus_
    }

-- | The details for a list of recent executions, such as action execution ID.
laersActionExecutionDetails :: Lens' ListActionExecutionsResponse [ActionExecutionDetail]
laersActionExecutionDetails = lens _laersActionExecutionDetails (\s a -> s {_laersActionExecutionDetails = a}) . _Default . _Coerce

-- | If the amount of returned information is significantly large, an identifier is also returned and can be used in a subsequent @ListActionExecutions@ call to return the next set of action executions in the list.
laersNextToken :: Lens' ListActionExecutionsResponse (Maybe Text)
laersNextToken = lens _laersNextToken (\s a -> s {_laersNextToken = a})

-- | -- | The response status code.
laersResponseStatus :: Lens' ListActionExecutionsResponse Int
laersResponseStatus = lens _laersResponseStatus (\s a -> s {_laersResponseStatus = a})

instance NFData ListActionExecutionsResponse
