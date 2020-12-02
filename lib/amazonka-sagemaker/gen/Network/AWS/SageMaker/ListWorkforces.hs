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
-- Module      : Network.AWS.SageMaker.ListWorkforces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to list all private and vendor workforces in an AWS Region. Note that you can only have one private workforce per AWS Region.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListWorkforces
  ( -- * Creating a Request
    listWorkforces,
    ListWorkforces,

    -- * Request Lenses
    lwsNameContains,
    lwsNextToken,
    lwsSortOrder,
    lwsMaxResults,
    lwsSortBy,

    -- * Destructuring the Response
    listWorkforcesResponse,
    ListWorkforcesResponse,

    -- * Response Lenses
    lwrsNextToken,
    lwrsResponseStatus,
    lwrsWorkforces,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listWorkforces' smart constructor.
data ListWorkforces = ListWorkforces'
  { _lwsNameContains ::
      !(Maybe Text),
    _lwsNextToken :: !(Maybe Text),
    _lwsSortOrder :: !(Maybe SortOrder),
    _lwsMaxResults :: !(Maybe Nat),
    _lwsSortBy :: !(Maybe ListWorkforcesSortByOptions)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListWorkforces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lwsNameContains' - A filter you can use to search for workforces using part of the workforce name.
--
-- * 'lwsNextToken' - A token to resume pagination.
--
-- * 'lwsSortOrder' - Sort workforces in ascending or descending order.
--
-- * 'lwsMaxResults' - The maximum number of workforces returned in the response.
--
-- * 'lwsSortBy' - Sort workforces using the workforce name or creation date.
listWorkforces ::
  ListWorkforces
listWorkforces =
  ListWorkforces'
    { _lwsNameContains = Nothing,
      _lwsNextToken = Nothing,
      _lwsSortOrder = Nothing,
      _lwsMaxResults = Nothing,
      _lwsSortBy = Nothing
    }

-- | A filter you can use to search for workforces using part of the workforce name.
lwsNameContains :: Lens' ListWorkforces (Maybe Text)
lwsNameContains = lens _lwsNameContains (\s a -> s {_lwsNameContains = a})

-- | A token to resume pagination.
lwsNextToken :: Lens' ListWorkforces (Maybe Text)
lwsNextToken = lens _lwsNextToken (\s a -> s {_lwsNextToken = a})

-- | Sort workforces in ascending or descending order.
lwsSortOrder :: Lens' ListWorkforces (Maybe SortOrder)
lwsSortOrder = lens _lwsSortOrder (\s a -> s {_lwsSortOrder = a})

-- | The maximum number of workforces returned in the response.
lwsMaxResults :: Lens' ListWorkforces (Maybe Natural)
lwsMaxResults = lens _lwsMaxResults (\s a -> s {_lwsMaxResults = a}) . mapping _Nat

-- | Sort workforces using the workforce name or creation date.
lwsSortBy :: Lens' ListWorkforces (Maybe ListWorkforcesSortByOptions)
lwsSortBy = lens _lwsSortBy (\s a -> s {_lwsSortBy = a})

instance AWSPager ListWorkforces where
  page rq rs
    | stop (rs ^. lwrsNextToken) = Nothing
    | stop (rs ^. lwrsWorkforces) = Nothing
    | otherwise = Just $ rq & lwsNextToken .~ rs ^. lwrsNextToken

instance AWSRequest ListWorkforces where
  type Rs ListWorkforces = ListWorkforcesResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListWorkforcesResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "Workforces" .!@ mempty)
      )

instance Hashable ListWorkforces

instance NFData ListWorkforces

instance ToHeaders ListWorkforces where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListWorkforces" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListWorkforces where
  toJSON ListWorkforces' {..} =
    object
      ( catMaybes
          [ ("NameContains" .=) <$> _lwsNameContains,
            ("NextToken" .=) <$> _lwsNextToken,
            ("SortOrder" .=) <$> _lwsSortOrder,
            ("MaxResults" .=) <$> _lwsMaxResults,
            ("SortBy" .=) <$> _lwsSortBy
          ]
      )

instance ToPath ListWorkforces where
  toPath = const "/"

instance ToQuery ListWorkforces where
  toQuery = const mempty

-- | /See:/ 'listWorkforcesResponse' smart constructor.
data ListWorkforcesResponse = ListWorkforcesResponse'
  { _lwrsNextToken ::
      !(Maybe Text),
    _lwrsResponseStatus :: !Int,
    _lwrsWorkforces :: ![Workforce]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListWorkforcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lwrsNextToken' - A token to resume pagination.
--
-- * 'lwrsResponseStatus' - -- | The response status code.
--
-- * 'lwrsWorkforces' - A list containing information about your workforce.
listWorkforcesResponse ::
  -- | 'lwrsResponseStatus'
  Int ->
  ListWorkforcesResponse
listWorkforcesResponse pResponseStatus_ =
  ListWorkforcesResponse'
    { _lwrsNextToken = Nothing,
      _lwrsResponseStatus = pResponseStatus_,
      _lwrsWorkforces = mempty
    }

-- | A token to resume pagination.
lwrsNextToken :: Lens' ListWorkforcesResponse (Maybe Text)
lwrsNextToken = lens _lwrsNextToken (\s a -> s {_lwrsNextToken = a})

-- | -- | The response status code.
lwrsResponseStatus :: Lens' ListWorkforcesResponse Int
lwrsResponseStatus = lens _lwrsResponseStatus (\s a -> s {_lwrsResponseStatus = a})

-- | A list containing information about your workforce.
lwrsWorkforces :: Lens' ListWorkforcesResponse [Workforce]
lwrsWorkforces = lens _lwrsWorkforces (\s a -> s {_lwrsWorkforces = a}) . _Coerce

instance NFData ListWorkforcesResponse
