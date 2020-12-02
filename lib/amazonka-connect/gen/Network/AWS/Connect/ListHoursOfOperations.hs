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
-- Module      : Network.AWS.Connect.ListHoursOfOperations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the hours of operation for the specified Amazon Connect instance.
--
--
-- For more information about hours of operation, see <https://docs.aws.amazon.com/connect/latest/adminguide/set-hours-operation.html Set the Hours of Operation for a Queue> in the /Amazon Connect Administrator Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListHoursOfOperations
  ( -- * Creating a Request
    listHoursOfOperations,
    ListHoursOfOperations,

    -- * Request Lenses
    lhooNextToken,
    lhooMaxResults,
    lhooInstanceId,

    -- * Destructuring the Response
    listHoursOfOperationsResponse,
    ListHoursOfOperationsResponse,

    -- * Response Lenses
    lhoorsNextToken,
    lhoorsHoursOfOperationSummaryList,
    lhoorsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listHoursOfOperations' smart constructor.
data ListHoursOfOperations = ListHoursOfOperations'
  { _lhooNextToken ::
      !(Maybe Text),
    _lhooMaxResults :: !(Maybe Nat),
    _lhooInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListHoursOfOperations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhooNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'lhooMaxResults' - The maximimum number of results to return per page.
--
-- * 'lhooInstanceId' - The identifier of the Amazon Connect instance.
listHoursOfOperations ::
  -- | 'lhooInstanceId'
  Text ->
  ListHoursOfOperations
listHoursOfOperations pInstanceId_ =
  ListHoursOfOperations'
    { _lhooNextToken = Nothing,
      _lhooMaxResults = Nothing,
      _lhooInstanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
lhooNextToken :: Lens' ListHoursOfOperations (Maybe Text)
lhooNextToken = lens _lhooNextToken (\s a -> s {_lhooNextToken = a})

-- | The maximimum number of results to return per page.
lhooMaxResults :: Lens' ListHoursOfOperations (Maybe Natural)
lhooMaxResults = lens _lhooMaxResults (\s a -> s {_lhooMaxResults = a}) . mapping _Nat

-- | The identifier of the Amazon Connect instance.
lhooInstanceId :: Lens' ListHoursOfOperations Text
lhooInstanceId = lens _lhooInstanceId (\s a -> s {_lhooInstanceId = a})

instance AWSPager ListHoursOfOperations where
  page rq rs
    | stop (rs ^. lhoorsNextToken) = Nothing
    | stop (rs ^. lhoorsHoursOfOperationSummaryList) = Nothing
    | otherwise = Just $ rq & lhooNextToken .~ rs ^. lhoorsNextToken

instance AWSRequest ListHoursOfOperations where
  type Rs ListHoursOfOperations = ListHoursOfOperationsResponse
  request = get connect
  response =
    receiveJSON
      ( \s h x ->
          ListHoursOfOperationsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "HoursOfOperationSummaryList" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListHoursOfOperations

instance NFData ListHoursOfOperations

instance ToHeaders ListHoursOfOperations where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListHoursOfOperations where
  toPath ListHoursOfOperations' {..} =
    mconcat ["/hours-of-operations-summary/", toBS _lhooInstanceId]

instance ToQuery ListHoursOfOperations where
  toQuery ListHoursOfOperations' {..} =
    mconcat
      ["nextToken" =: _lhooNextToken, "maxResults" =: _lhooMaxResults]

-- | /See:/ 'listHoursOfOperationsResponse' smart constructor.
data ListHoursOfOperationsResponse = ListHoursOfOperationsResponse'
  { _lhoorsNextToken ::
      !(Maybe Text),
    _lhoorsHoursOfOperationSummaryList ::
      !( Maybe
           [HoursOfOperationSummary]
       ),
    _lhoorsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListHoursOfOperationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhoorsNextToken' - If there are additional results, this is the token for the next set of results.
--
-- * 'lhoorsHoursOfOperationSummaryList' - Information about the hours of operation.
--
-- * 'lhoorsResponseStatus' - -- | The response status code.
listHoursOfOperationsResponse ::
  -- | 'lhoorsResponseStatus'
  Int ->
  ListHoursOfOperationsResponse
listHoursOfOperationsResponse pResponseStatus_ =
  ListHoursOfOperationsResponse'
    { _lhoorsNextToken = Nothing,
      _lhoorsHoursOfOperationSummaryList = Nothing,
      _lhoorsResponseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
lhoorsNextToken :: Lens' ListHoursOfOperationsResponse (Maybe Text)
lhoorsNextToken = lens _lhoorsNextToken (\s a -> s {_lhoorsNextToken = a})

-- | Information about the hours of operation.
lhoorsHoursOfOperationSummaryList :: Lens' ListHoursOfOperationsResponse [HoursOfOperationSummary]
lhoorsHoursOfOperationSummaryList = lens _lhoorsHoursOfOperationSummaryList (\s a -> s {_lhoorsHoursOfOperationSummaryList = a}) . _Default . _Coerce

-- | -- | The response status code.
lhoorsResponseStatus :: Lens' ListHoursOfOperationsResponse Int
lhoorsResponseStatus = lens _lhoorsResponseStatus (\s a -> s {_lhoorsResponseStatus = a})

instance NFData ListHoursOfOperationsResponse
