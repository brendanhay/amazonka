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
-- Module      : Network.AWS.MediaPackage.ListHarvestJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of HarvestJob records.
--
-- This operation returns paginated results.
module Network.AWS.MediaPackage.ListHarvestJobs
  ( -- * Creating a Request
    listHarvestJobs,
    ListHarvestJobs,

    -- * Request Lenses
    lhjIncludeStatus,
    lhjNextToken,
    lhjIncludeChannelId,
    lhjMaxResults,

    -- * Destructuring the Response
    listHarvestJobsResponse,
    ListHarvestJobsResponse,

    -- * Response Lenses
    lhjrsHarvestJobs,
    lhjrsNextToken,
    lhjrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listHarvestJobs' smart constructor.
data ListHarvestJobs = ListHarvestJobs'
  { _lhjIncludeStatus ::
      !(Maybe Text),
    _lhjNextToken :: !(Maybe Text),
    _lhjIncludeChannelId :: !(Maybe Text),
    _lhjMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListHarvestJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhjIncludeStatus' - When specified, the request will return only HarvestJobs in the given status.
--
-- * 'lhjNextToken' - A token used to resume pagination from the end of a previous request.
--
-- * 'lhjIncludeChannelId' - When specified, the request will return only HarvestJobs associated with the given Channel ID.
--
-- * 'lhjMaxResults' - The upper bound on the number of records to return.
listHarvestJobs ::
  ListHarvestJobs
listHarvestJobs =
  ListHarvestJobs'
    { _lhjIncludeStatus = Nothing,
      _lhjNextToken = Nothing,
      _lhjIncludeChannelId = Nothing,
      _lhjMaxResults = Nothing
    }

-- | When specified, the request will return only HarvestJobs in the given status.
lhjIncludeStatus :: Lens' ListHarvestJobs (Maybe Text)
lhjIncludeStatus = lens _lhjIncludeStatus (\s a -> s {_lhjIncludeStatus = a})

-- | A token used to resume pagination from the end of a previous request.
lhjNextToken :: Lens' ListHarvestJobs (Maybe Text)
lhjNextToken = lens _lhjNextToken (\s a -> s {_lhjNextToken = a})

-- | When specified, the request will return only HarvestJobs associated with the given Channel ID.
lhjIncludeChannelId :: Lens' ListHarvestJobs (Maybe Text)
lhjIncludeChannelId = lens _lhjIncludeChannelId (\s a -> s {_lhjIncludeChannelId = a})

-- | The upper bound on the number of records to return.
lhjMaxResults :: Lens' ListHarvestJobs (Maybe Natural)
lhjMaxResults = lens _lhjMaxResults (\s a -> s {_lhjMaxResults = a}) . mapping _Nat

instance AWSPager ListHarvestJobs where
  page rq rs
    | stop (rs ^. lhjrsNextToken) = Nothing
    | stop (rs ^. lhjrsHarvestJobs) = Nothing
    | otherwise = Just $ rq & lhjNextToken .~ rs ^. lhjrsNextToken

instance AWSRequest ListHarvestJobs where
  type Rs ListHarvestJobs = ListHarvestJobsResponse
  request = get mediaPackage
  response =
    receiveJSON
      ( \s h x ->
          ListHarvestJobsResponse'
            <$> (x .?> "harvestJobs" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListHarvestJobs

instance NFData ListHarvestJobs

instance ToHeaders ListHarvestJobs where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListHarvestJobs where
  toPath = const "/harvest_jobs"

instance ToQuery ListHarvestJobs where
  toQuery ListHarvestJobs' {..} =
    mconcat
      [ "includeStatus" =: _lhjIncludeStatus,
        "nextToken" =: _lhjNextToken,
        "includeChannelId" =: _lhjIncludeChannelId,
        "maxResults" =: _lhjMaxResults
      ]

-- | /See:/ 'listHarvestJobsResponse' smart constructor.
data ListHarvestJobsResponse = ListHarvestJobsResponse'
  { _lhjrsHarvestJobs ::
      !(Maybe [HarvestJob]),
    _lhjrsNextToken :: !(Maybe Text),
    _lhjrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListHarvestJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhjrsHarvestJobs' - A list of HarvestJob records.
--
-- * 'lhjrsNextToken' - A token that can be used to resume pagination from the end of the collection.
--
-- * 'lhjrsResponseStatus' - -- | The response status code.
listHarvestJobsResponse ::
  -- | 'lhjrsResponseStatus'
  Int ->
  ListHarvestJobsResponse
listHarvestJobsResponse pResponseStatus_ =
  ListHarvestJobsResponse'
    { _lhjrsHarvestJobs = Nothing,
      _lhjrsNextToken = Nothing,
      _lhjrsResponseStatus = pResponseStatus_
    }

-- | A list of HarvestJob records.
lhjrsHarvestJobs :: Lens' ListHarvestJobsResponse [HarvestJob]
lhjrsHarvestJobs = lens _lhjrsHarvestJobs (\s a -> s {_lhjrsHarvestJobs = a}) . _Default . _Coerce

-- | A token that can be used to resume pagination from the end of the collection.
lhjrsNextToken :: Lens' ListHarvestJobsResponse (Maybe Text)
lhjrsNextToken = lens _lhjrsNextToken (\s a -> s {_lhjrsNextToken = a})

-- | -- | The response status code.
lhjrsResponseStatus :: Lens' ListHarvestJobsResponse Int
lhjrsResponseStatus = lens _lhjrsResponseStatus (\s a -> s {_lhjrsResponseStatus = a})

instance NFData ListHarvestJobsResponse
