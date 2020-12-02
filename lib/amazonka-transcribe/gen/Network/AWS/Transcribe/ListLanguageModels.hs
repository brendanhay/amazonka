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
-- Module      : Network.AWS.Transcribe.ListLanguageModels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides more information about the custom language models you've created. You can use the information in this list to find a specific custom language model. You can then use the operation to get more information about it.
module Network.AWS.Transcribe.ListLanguageModels
  ( -- * Creating a Request
    listLanguageModels,
    ListLanguageModels,

    -- * Request Lenses
    llmNameContains,
    llmNextToken,
    llmStatusEquals,
    llmMaxResults,

    -- * Destructuring the Response
    listLanguageModelsResponse,
    ListLanguageModelsResponse,

    -- * Response Lenses
    llmrsNextToken,
    llmrsModels,
    llmrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'listLanguageModels' smart constructor.
data ListLanguageModels = ListLanguageModels'
  { _llmNameContains ::
      !(Maybe Text),
    _llmNextToken :: !(Maybe Text),
    _llmStatusEquals :: !(Maybe ModelStatus),
    _llmMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListLanguageModels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llmNameContains' - When specified, the custom language model names returned contain the substring you've specified.
--
-- * 'llmNextToken' - When included, fetches the next set of jobs if the result of the previous request was truncated.
--
-- * 'llmStatusEquals' - When specified, returns only custom language models with the specified status. Language models are ordered by creation date, with the newest models first. If you don't specify a status, Amazon Transcribe returns all custom language models ordered by date.
--
-- * 'llmMaxResults' - The maximum number of language models to return in the response. If there are fewer results in the list, the response contains only the actual results.
listLanguageModels ::
  ListLanguageModels
listLanguageModels =
  ListLanguageModels'
    { _llmNameContains = Nothing,
      _llmNextToken = Nothing,
      _llmStatusEquals = Nothing,
      _llmMaxResults = Nothing
    }

-- | When specified, the custom language model names returned contain the substring you've specified.
llmNameContains :: Lens' ListLanguageModels (Maybe Text)
llmNameContains = lens _llmNameContains (\s a -> s {_llmNameContains = a})

-- | When included, fetches the next set of jobs if the result of the previous request was truncated.
llmNextToken :: Lens' ListLanguageModels (Maybe Text)
llmNextToken = lens _llmNextToken (\s a -> s {_llmNextToken = a})

-- | When specified, returns only custom language models with the specified status. Language models are ordered by creation date, with the newest models first. If you don't specify a status, Amazon Transcribe returns all custom language models ordered by date.
llmStatusEquals :: Lens' ListLanguageModels (Maybe ModelStatus)
llmStatusEquals = lens _llmStatusEquals (\s a -> s {_llmStatusEquals = a})

-- | The maximum number of language models to return in the response. If there are fewer results in the list, the response contains only the actual results.
llmMaxResults :: Lens' ListLanguageModels (Maybe Natural)
llmMaxResults = lens _llmMaxResults (\s a -> s {_llmMaxResults = a}) . mapping _Nat

instance AWSRequest ListLanguageModels where
  type Rs ListLanguageModels = ListLanguageModelsResponse
  request = postJSON transcribe
  response =
    receiveJSON
      ( \s h x ->
          ListLanguageModelsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "Models" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListLanguageModels

instance NFData ListLanguageModels

instance ToHeaders ListLanguageModels where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("Transcribe.ListLanguageModels" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListLanguageModels where
  toJSON ListLanguageModels' {..} =
    object
      ( catMaybes
          [ ("NameContains" .=) <$> _llmNameContains,
            ("NextToken" .=) <$> _llmNextToken,
            ("StatusEquals" .=) <$> _llmStatusEquals,
            ("MaxResults" .=) <$> _llmMaxResults
          ]
      )

instance ToPath ListLanguageModels where
  toPath = const "/"

instance ToQuery ListLanguageModels where
  toQuery = const mempty

-- | /See:/ 'listLanguageModelsResponse' smart constructor.
data ListLanguageModelsResponse = ListLanguageModelsResponse'
  { _llmrsNextToken ::
      !(Maybe Text),
    _llmrsModels ::
      !(Maybe [LanguageModel]),
    _llmrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListLanguageModelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llmrsNextToken' - The operation returns a page of jobs at a time. The maximum size of the list is set by the MaxResults parameter. If there are more language models in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the operation to return the next page of language models.
--
-- * 'llmrsModels' - A list of objects containing information about custom language models.
--
-- * 'llmrsResponseStatus' - -- | The response status code.
listLanguageModelsResponse ::
  -- | 'llmrsResponseStatus'
  Int ->
  ListLanguageModelsResponse
listLanguageModelsResponse pResponseStatus_ =
  ListLanguageModelsResponse'
    { _llmrsNextToken = Nothing,
      _llmrsModels = Nothing,
      _llmrsResponseStatus = pResponseStatus_
    }

-- | The operation returns a page of jobs at a time. The maximum size of the list is set by the MaxResults parameter. If there are more language models in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the operation to return the next page of language models.
llmrsNextToken :: Lens' ListLanguageModelsResponse (Maybe Text)
llmrsNextToken = lens _llmrsNextToken (\s a -> s {_llmrsNextToken = a})

-- | A list of objects containing information about custom language models.
llmrsModels :: Lens' ListLanguageModelsResponse [LanguageModel]
llmrsModels = lens _llmrsModels (\s a -> s {_llmrsModels = a}) . _Default . _Coerce

-- | -- | The response status code.
llmrsResponseStatus :: Lens' ListLanguageModelsResponse Int
llmrsResponseStatus = lens _llmrsResponseStatus (\s a -> s {_llmrsResponseStatus = a})

instance NFData ListLanguageModelsResponse
