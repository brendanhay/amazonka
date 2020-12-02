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
-- Module      : Network.AWS.Comprehend.ListDocumentClassifiers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the document classifiers that you have created.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListDocumentClassifiers
  ( -- * Creating a Request
    listDocumentClassifiers,
    ListDocumentClassifiers,

    -- * Request Lenses
    ldcNextToken,
    ldcFilter,
    ldcMaxResults,

    -- * Destructuring the Response
    listDocumentClassifiersResponse,
    ListDocumentClassifiersResponse,

    -- * Response Lenses
    ldcrsNextToken,
    ldcrsDocumentClassifierPropertiesList,
    ldcrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDocumentClassifiers' smart constructor.
data ListDocumentClassifiers = ListDocumentClassifiers'
  { _ldcNextToken ::
      !(Maybe Text),
    _ldcFilter ::
      !(Maybe DocumentClassifierFilter),
    _ldcMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDocumentClassifiers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldcNextToken' - Identifies the next page of results to return.
--
-- * 'ldcFilter' - Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- * 'ldcMaxResults' - The maximum number of results to return in each page. The default is 100.
listDocumentClassifiers ::
  ListDocumentClassifiers
listDocumentClassifiers =
  ListDocumentClassifiers'
    { _ldcNextToken = Nothing,
      _ldcFilter = Nothing,
      _ldcMaxResults = Nothing
    }

-- | Identifies the next page of results to return.
ldcNextToken :: Lens' ListDocumentClassifiers (Maybe Text)
ldcNextToken = lens _ldcNextToken (\s a -> s {_ldcNextToken = a})

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
ldcFilter :: Lens' ListDocumentClassifiers (Maybe DocumentClassifierFilter)
ldcFilter = lens _ldcFilter (\s a -> s {_ldcFilter = a})

-- | The maximum number of results to return in each page. The default is 100.
ldcMaxResults :: Lens' ListDocumentClassifiers (Maybe Natural)
ldcMaxResults = lens _ldcMaxResults (\s a -> s {_ldcMaxResults = a}) . mapping _Nat

instance AWSPager ListDocumentClassifiers where
  page rq rs
    | stop (rs ^. ldcrsNextToken) = Nothing
    | stop (rs ^. ldcrsDocumentClassifierPropertiesList) = Nothing
    | otherwise = Just $ rq & ldcNextToken .~ rs ^. ldcrsNextToken

instance AWSRequest ListDocumentClassifiers where
  type Rs ListDocumentClassifiers = ListDocumentClassifiersResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          ListDocumentClassifiersResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "DocumentClassifierPropertiesList" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListDocumentClassifiers

instance NFData ListDocumentClassifiers

instance ToHeaders ListDocumentClassifiers where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.ListDocumentClassifiers" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListDocumentClassifiers where
  toJSON ListDocumentClassifiers' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _ldcNextToken,
            ("Filter" .=) <$> _ldcFilter,
            ("MaxResults" .=) <$> _ldcMaxResults
          ]
      )

instance ToPath ListDocumentClassifiers where
  toPath = const "/"

instance ToQuery ListDocumentClassifiers where
  toQuery = const mempty

-- | /See:/ 'listDocumentClassifiersResponse' smart constructor.
data ListDocumentClassifiersResponse = ListDocumentClassifiersResponse'
  { _ldcrsNextToken ::
      !(Maybe Text),
    _ldcrsDocumentClassifierPropertiesList ::
      !( Maybe
           [DocumentClassifierProperties]
       ),
    _ldcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDocumentClassifiersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldcrsNextToken' - Identifies the next page of results to return.
--
-- * 'ldcrsDocumentClassifierPropertiesList' - A list containing the properties of each job returned.
--
-- * 'ldcrsResponseStatus' - -- | The response status code.
listDocumentClassifiersResponse ::
  -- | 'ldcrsResponseStatus'
  Int ->
  ListDocumentClassifiersResponse
listDocumentClassifiersResponse pResponseStatus_ =
  ListDocumentClassifiersResponse'
    { _ldcrsNextToken = Nothing,
      _ldcrsDocumentClassifierPropertiesList = Nothing,
      _ldcrsResponseStatus = pResponseStatus_
    }

-- | Identifies the next page of results to return.
ldcrsNextToken :: Lens' ListDocumentClassifiersResponse (Maybe Text)
ldcrsNextToken = lens _ldcrsNextToken (\s a -> s {_ldcrsNextToken = a})

-- | A list containing the properties of each job returned.
ldcrsDocumentClassifierPropertiesList :: Lens' ListDocumentClassifiersResponse [DocumentClassifierProperties]
ldcrsDocumentClassifierPropertiesList = lens _ldcrsDocumentClassifierPropertiesList (\s a -> s {_ldcrsDocumentClassifierPropertiesList = a}) . _Default . _Coerce

-- | -- | The response status code.
ldcrsResponseStatus :: Lens' ListDocumentClassifiersResponse Int
ldcrsResponseStatus = lens _ldcrsResponseStatus (\s a -> s {_ldcrsResponseStatus = a})

instance NFData ListDocumentClassifiersResponse
