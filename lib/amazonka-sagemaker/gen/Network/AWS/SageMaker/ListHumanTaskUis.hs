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
-- Module      : Network.AWS.SageMaker.ListHumanTaskUis
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the human task user interfaces in your account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListHumanTaskUis
  ( -- * Creating a Request
    listHumanTaskUis,
    ListHumanTaskUis,

    -- * Request Lenses
    lhtuCreationTimeAfter,
    lhtuNextToken,
    lhtuSortOrder,
    lhtuCreationTimeBefore,
    lhtuMaxResults,

    -- * Destructuring the Response
    listHumanTaskUisResponse,
    ListHumanTaskUisResponse,

    -- * Response Lenses
    lhtursNextToken,
    lhtursResponseStatus,
    lhtursHumanTaskUiSummaries,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listHumanTaskUis' smart constructor.
data ListHumanTaskUis = ListHumanTaskUis'
  { _lhtuCreationTimeAfter ::
      !(Maybe POSIX),
    _lhtuNextToken :: !(Maybe Text),
    _lhtuSortOrder :: !(Maybe SortOrder),
    _lhtuCreationTimeBefore :: !(Maybe POSIX),
    _lhtuMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListHumanTaskUis' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhtuCreationTimeAfter' - A filter that returns only human task user interfaces with a creation time greater than or equal to the specified timestamp.
--
-- * 'lhtuNextToken' - A token to resume pagination.
--
-- * 'lhtuSortOrder' - An optional value that specifies whether you want the results sorted in @Ascending@ or @Descending@ order.
--
-- * 'lhtuCreationTimeBefore' - A filter that returns only human task user interfaces that were created before the specified timestamp.
--
-- * 'lhtuMaxResults' - The total number of items to return. If the total number of available items is more than the value specified in @MaxResults@ , then a @NextToken@ will be provided in the output that you can use to resume pagination.
listHumanTaskUis ::
  ListHumanTaskUis
listHumanTaskUis =
  ListHumanTaskUis'
    { _lhtuCreationTimeAfter = Nothing,
      _lhtuNextToken = Nothing,
      _lhtuSortOrder = Nothing,
      _lhtuCreationTimeBefore = Nothing,
      _lhtuMaxResults = Nothing
    }

-- | A filter that returns only human task user interfaces with a creation time greater than or equal to the specified timestamp.
lhtuCreationTimeAfter :: Lens' ListHumanTaskUis (Maybe UTCTime)
lhtuCreationTimeAfter = lens _lhtuCreationTimeAfter (\s a -> s {_lhtuCreationTimeAfter = a}) . mapping _Time

-- | A token to resume pagination.
lhtuNextToken :: Lens' ListHumanTaskUis (Maybe Text)
lhtuNextToken = lens _lhtuNextToken (\s a -> s {_lhtuNextToken = a})

-- | An optional value that specifies whether you want the results sorted in @Ascending@ or @Descending@ order.
lhtuSortOrder :: Lens' ListHumanTaskUis (Maybe SortOrder)
lhtuSortOrder = lens _lhtuSortOrder (\s a -> s {_lhtuSortOrder = a})

-- | A filter that returns only human task user interfaces that were created before the specified timestamp.
lhtuCreationTimeBefore :: Lens' ListHumanTaskUis (Maybe UTCTime)
lhtuCreationTimeBefore = lens _lhtuCreationTimeBefore (\s a -> s {_lhtuCreationTimeBefore = a}) . mapping _Time

-- | The total number of items to return. If the total number of available items is more than the value specified in @MaxResults@ , then a @NextToken@ will be provided in the output that you can use to resume pagination.
lhtuMaxResults :: Lens' ListHumanTaskUis (Maybe Natural)
lhtuMaxResults = lens _lhtuMaxResults (\s a -> s {_lhtuMaxResults = a}) . mapping _Nat

instance AWSPager ListHumanTaskUis where
  page rq rs
    | stop (rs ^. lhtursNextToken) = Nothing
    | stop (rs ^. lhtursHumanTaskUiSummaries) = Nothing
    | otherwise = Just $ rq & lhtuNextToken .~ rs ^. lhtursNextToken

instance AWSRequest ListHumanTaskUis where
  type Rs ListHumanTaskUis = ListHumanTaskUisResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListHumanTaskUisResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "HumanTaskUiSummaries" .!@ mempty)
      )

instance Hashable ListHumanTaskUis

instance NFData ListHumanTaskUis

instance ToHeaders ListHumanTaskUis where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListHumanTaskUis" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListHumanTaskUis where
  toJSON ListHumanTaskUis' {..} =
    object
      ( catMaybes
          [ ("CreationTimeAfter" .=) <$> _lhtuCreationTimeAfter,
            ("NextToken" .=) <$> _lhtuNextToken,
            ("SortOrder" .=) <$> _lhtuSortOrder,
            ("CreationTimeBefore" .=) <$> _lhtuCreationTimeBefore,
            ("MaxResults" .=) <$> _lhtuMaxResults
          ]
      )

instance ToPath ListHumanTaskUis where
  toPath = const "/"

instance ToQuery ListHumanTaskUis where
  toQuery = const mempty

-- | /See:/ 'listHumanTaskUisResponse' smart constructor.
data ListHumanTaskUisResponse = ListHumanTaskUisResponse'
  { _lhtursNextToken ::
      !(Maybe Text),
    _lhtursResponseStatus :: !Int,
    _lhtursHumanTaskUiSummaries ::
      ![HumanTaskUiSummary]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListHumanTaskUisResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhtursNextToken' - A token to resume pagination.
--
-- * 'lhtursResponseStatus' - -- | The response status code.
--
-- * 'lhtursHumanTaskUiSummaries' - An array of objects describing the human task user interfaces.
listHumanTaskUisResponse ::
  -- | 'lhtursResponseStatus'
  Int ->
  ListHumanTaskUisResponse
listHumanTaskUisResponse pResponseStatus_ =
  ListHumanTaskUisResponse'
    { _lhtursNextToken = Nothing,
      _lhtursResponseStatus = pResponseStatus_,
      _lhtursHumanTaskUiSummaries = mempty
    }

-- | A token to resume pagination.
lhtursNextToken :: Lens' ListHumanTaskUisResponse (Maybe Text)
lhtursNextToken = lens _lhtursNextToken (\s a -> s {_lhtursNextToken = a})

-- | -- | The response status code.
lhtursResponseStatus :: Lens' ListHumanTaskUisResponse Int
lhtursResponseStatus = lens _lhtursResponseStatus (\s a -> s {_lhtursResponseStatus = a})

-- | An array of objects describing the human task user interfaces.
lhtursHumanTaskUiSummaries :: Lens' ListHumanTaskUisResponse [HumanTaskUiSummary]
lhtursHumanTaskUiSummaries = lens _lhtursHumanTaskUiSummaries (\s a -> s {_lhtursHumanTaskUiSummaries = a}) . _Coerce

instance NFData ListHumanTaskUisResponse
