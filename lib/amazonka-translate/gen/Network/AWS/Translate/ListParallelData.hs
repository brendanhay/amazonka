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
-- Module      : Network.AWS.Translate.ListParallelData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of your parallel data resources in Amazon Translate.
module Network.AWS.Translate.ListParallelData
  ( -- * Creating a Request
    listParallelData,
    ListParallelData,

    -- * Request Lenses
    lpdNextToken,
    lpdMaxResults,

    -- * Destructuring the Response
    listParallelDataResponse,
    ListParallelDataResponse,

    -- * Response Lenses
    lpdrsParallelDataPropertiesList,
    lpdrsNextToken,
    lpdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Translate.Types

-- | /See:/ 'listParallelData' smart constructor.
data ListParallelData = ListParallelData'
  { _lpdNextToken ::
      !(Maybe Text),
    _lpdMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListParallelData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpdNextToken' - A string that specifies the next page of results to return in a paginated response.
--
-- * 'lpdMaxResults' - The maximum number of parallel data resources returned for each request.
listParallelData ::
  ListParallelData
listParallelData =
  ListParallelData'
    { _lpdNextToken = Nothing,
      _lpdMaxResults = Nothing
    }

-- | A string that specifies the next page of results to return in a paginated response.
lpdNextToken :: Lens' ListParallelData (Maybe Text)
lpdNextToken = lens _lpdNextToken (\s a -> s {_lpdNextToken = a})

-- | The maximum number of parallel data resources returned for each request.
lpdMaxResults :: Lens' ListParallelData (Maybe Natural)
lpdMaxResults = lens _lpdMaxResults (\s a -> s {_lpdMaxResults = a}) . mapping _Nat

instance AWSRequest ListParallelData where
  type Rs ListParallelData = ListParallelDataResponse
  request = postJSON translate
  response =
    receiveJSON
      ( \s h x ->
          ListParallelDataResponse'
            <$> (x .?> "ParallelDataPropertiesList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListParallelData

instance NFData ListParallelData

instance ToHeaders ListParallelData where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSShineFrontendService_20170701.ListParallelData" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListParallelData where
  toJSON ListParallelData' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lpdNextToken,
            ("MaxResults" .=) <$> _lpdMaxResults
          ]
      )

instance ToPath ListParallelData where
  toPath = const "/"

instance ToQuery ListParallelData where
  toQuery = const mempty

-- | /See:/ 'listParallelDataResponse' smart constructor.
data ListParallelDataResponse = ListParallelDataResponse'
  { _lpdrsParallelDataPropertiesList ::
      !(Maybe [ParallelDataProperties]),
    _lpdrsNextToken :: !(Maybe Text),
    _lpdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListParallelDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpdrsParallelDataPropertiesList' - The properties of the parallel data resources returned by this request.
--
-- * 'lpdrsNextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- * 'lpdrsResponseStatus' - -- | The response status code.
listParallelDataResponse ::
  -- | 'lpdrsResponseStatus'
  Int ->
  ListParallelDataResponse
listParallelDataResponse pResponseStatus_ =
  ListParallelDataResponse'
    { _lpdrsParallelDataPropertiesList =
        Nothing,
      _lpdrsNextToken = Nothing,
      _lpdrsResponseStatus = pResponseStatus_
    }

-- | The properties of the parallel data resources returned by this request.
lpdrsParallelDataPropertiesList :: Lens' ListParallelDataResponse [ParallelDataProperties]
lpdrsParallelDataPropertiesList = lens _lpdrsParallelDataPropertiesList (\s a -> s {_lpdrsParallelDataPropertiesList = a}) . _Default . _Coerce

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
lpdrsNextToken :: Lens' ListParallelDataResponse (Maybe Text)
lpdrsNextToken = lens _lpdrsNextToken (\s a -> s {_lpdrsNextToken = a})

-- | -- | The response status code.
lpdrsResponseStatus :: Lens' ListParallelDataResponse Int
lpdrsResponseStatus = lens _lpdrsResponseStatus (\s a -> s {_lpdrsResponseStatus = a})

instance NFData ListParallelDataResponse
