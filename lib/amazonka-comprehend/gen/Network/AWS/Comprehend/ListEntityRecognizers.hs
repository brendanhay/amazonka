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
-- Module      : Network.AWS.Comprehend.ListEntityRecognizers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the properties of all entity recognizers that you created, including recognizers currently in training. Allows you to filter the list of recognizers based on criteria such as status and submission time. This call returns up to 500 entity recognizers in the list, with a default number of 100 recognizers in the list.
--
--
-- The results of this list are not in any particular order. Please get the list and sort locally if needed.
--
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListEntityRecognizers
  ( -- * Creating a Request
    listEntityRecognizers,
    ListEntityRecognizers,

    -- * Request Lenses
    lerNextToken,
    lerFilter,
    lerMaxResults,

    -- * Destructuring the Response
    listEntityRecognizersResponse,
    ListEntityRecognizersResponse,

    -- * Response Lenses
    lerrsNextToken,
    lerrsEntityRecognizerPropertiesList,
    lerrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listEntityRecognizers' smart constructor.
data ListEntityRecognizers = ListEntityRecognizers'
  { _lerNextToken ::
      !(Maybe Text),
    _lerFilter :: !(Maybe EntityRecognizerFilter),
    _lerMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListEntityRecognizers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lerNextToken' - Identifies the next page of results to return.
--
-- * 'lerFilter' - Filters the list of entities returned. You can filter on @Status@ , @SubmitTimeBefore@ , or @SubmitTimeAfter@ . You can only set one filter at a time.
--
-- * 'lerMaxResults' - The maximum number of results to return on each page. The default is 100.
listEntityRecognizers ::
  ListEntityRecognizers
listEntityRecognizers =
  ListEntityRecognizers'
    { _lerNextToken = Nothing,
      _lerFilter = Nothing,
      _lerMaxResults = Nothing
    }

-- | Identifies the next page of results to return.
lerNextToken :: Lens' ListEntityRecognizers (Maybe Text)
lerNextToken = lens _lerNextToken (\s a -> s {_lerNextToken = a})

-- | Filters the list of entities returned. You can filter on @Status@ , @SubmitTimeBefore@ , or @SubmitTimeAfter@ . You can only set one filter at a time.
lerFilter :: Lens' ListEntityRecognizers (Maybe EntityRecognizerFilter)
lerFilter = lens _lerFilter (\s a -> s {_lerFilter = a})

-- | The maximum number of results to return on each page. The default is 100.
lerMaxResults :: Lens' ListEntityRecognizers (Maybe Natural)
lerMaxResults = lens _lerMaxResults (\s a -> s {_lerMaxResults = a}) . mapping _Nat

instance AWSPager ListEntityRecognizers where
  page rq rs
    | stop (rs ^. lerrsNextToken) = Nothing
    | stop (rs ^. lerrsEntityRecognizerPropertiesList) = Nothing
    | otherwise = Just $ rq & lerNextToken .~ rs ^. lerrsNextToken

instance AWSRequest ListEntityRecognizers where
  type Rs ListEntityRecognizers = ListEntityRecognizersResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          ListEntityRecognizersResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "EntityRecognizerPropertiesList" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListEntityRecognizers

instance NFData ListEntityRecognizers

instance ToHeaders ListEntityRecognizers where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.ListEntityRecognizers" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListEntityRecognizers where
  toJSON ListEntityRecognizers' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lerNextToken,
            ("Filter" .=) <$> _lerFilter,
            ("MaxResults" .=) <$> _lerMaxResults
          ]
      )

instance ToPath ListEntityRecognizers where
  toPath = const "/"

instance ToQuery ListEntityRecognizers where
  toQuery = const mempty

-- | /See:/ 'listEntityRecognizersResponse' smart constructor.
data ListEntityRecognizersResponse = ListEntityRecognizersResponse'
  { _lerrsNextToken ::
      !(Maybe Text),
    _lerrsEntityRecognizerPropertiesList ::
      !( Maybe
           [EntityRecognizerProperties]
       ),
    _lerrsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListEntityRecognizersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lerrsNextToken' - Identifies the next page of results to return.
--
-- * 'lerrsEntityRecognizerPropertiesList' - The list of properties of an entity recognizer.
--
-- * 'lerrsResponseStatus' - -- | The response status code.
listEntityRecognizersResponse ::
  -- | 'lerrsResponseStatus'
  Int ->
  ListEntityRecognizersResponse
listEntityRecognizersResponse pResponseStatus_ =
  ListEntityRecognizersResponse'
    { _lerrsNextToken = Nothing,
      _lerrsEntityRecognizerPropertiesList = Nothing,
      _lerrsResponseStatus = pResponseStatus_
    }

-- | Identifies the next page of results to return.
lerrsNextToken :: Lens' ListEntityRecognizersResponse (Maybe Text)
lerrsNextToken = lens _lerrsNextToken (\s a -> s {_lerrsNextToken = a})

-- | The list of properties of an entity recognizer.
lerrsEntityRecognizerPropertiesList :: Lens' ListEntityRecognizersResponse [EntityRecognizerProperties]
lerrsEntityRecognizerPropertiesList = lens _lerrsEntityRecognizerPropertiesList (\s a -> s {_lerrsEntityRecognizerPropertiesList = a}) . _Default . _Coerce

-- | -- | The response status code.
lerrsResponseStatus :: Lens' ListEntityRecognizersResponse Int
lerrsResponseStatus = lens _lerrsResponseStatus (\s a -> s {_lerrsResponseStatus = a})

instance NFData ListEntityRecognizersResponse
