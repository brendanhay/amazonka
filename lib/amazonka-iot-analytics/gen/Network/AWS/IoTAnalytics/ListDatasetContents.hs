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
-- Module      : Network.AWS.IoTAnalytics.ListDatasetContents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about data set contents that have been created.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoTAnalytics.ListDatasetContents
  ( -- * Creating a Request
    listDatasetContents,
    ListDatasetContents,

    -- * Request Lenses
    ldcNextToken,
    ldcScheduledBefore,
    ldcMaxResults,
    ldcScheduledOnOrAfter,
    ldcDatasetName,

    -- * Destructuring the Response
    listDatasetContentsResponse,
    ListDatasetContentsResponse,

    -- * Response Lenses
    ldcrsDatasetContentSummaries,
    ldcrsNextToken,
    ldcrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDatasetContents' smart constructor.
data ListDatasetContents = ListDatasetContents'
  { _ldcNextToken ::
      !(Maybe Text),
    _ldcScheduledBefore :: !(Maybe POSIX),
    _ldcMaxResults :: !(Maybe Nat),
    _ldcScheduledOnOrAfter :: !(Maybe POSIX),
    _ldcDatasetName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDatasetContents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldcNextToken' - The token for the next set of results.
--
-- * 'ldcScheduledBefore' - A filter to limit results to those data set contents whose creation is scheduled before the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
--
-- * 'ldcMaxResults' - The maximum number of results to return in this request.
--
-- * 'ldcScheduledOnOrAfter' - A filter to limit results to those data set contents whose creation is scheduled on or after the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
--
-- * 'ldcDatasetName' - The name of the data set whose contents information you want to list.
listDatasetContents ::
  -- | 'ldcDatasetName'
  Text ->
  ListDatasetContents
listDatasetContents pDatasetName_ =
  ListDatasetContents'
    { _ldcNextToken = Nothing,
      _ldcScheduledBefore = Nothing,
      _ldcMaxResults = Nothing,
      _ldcScheduledOnOrAfter = Nothing,
      _ldcDatasetName = pDatasetName_
    }

-- | The token for the next set of results.
ldcNextToken :: Lens' ListDatasetContents (Maybe Text)
ldcNextToken = lens _ldcNextToken (\s a -> s {_ldcNextToken = a})

-- | A filter to limit results to those data set contents whose creation is scheduled before the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
ldcScheduledBefore :: Lens' ListDatasetContents (Maybe UTCTime)
ldcScheduledBefore = lens _ldcScheduledBefore (\s a -> s {_ldcScheduledBefore = a}) . mapping _Time

-- | The maximum number of results to return in this request.
ldcMaxResults :: Lens' ListDatasetContents (Maybe Natural)
ldcMaxResults = lens _ldcMaxResults (\s a -> s {_ldcMaxResults = a}) . mapping _Nat

-- | A filter to limit results to those data set contents whose creation is scheduled on or after the given time. See the field @triggers.schedule@ in the @CreateDataset@ request. (timestamp)
ldcScheduledOnOrAfter :: Lens' ListDatasetContents (Maybe UTCTime)
ldcScheduledOnOrAfter = lens _ldcScheduledOnOrAfter (\s a -> s {_ldcScheduledOnOrAfter = a}) . mapping _Time

-- | The name of the data set whose contents information you want to list.
ldcDatasetName :: Lens' ListDatasetContents Text
ldcDatasetName = lens _ldcDatasetName (\s a -> s {_ldcDatasetName = a})

instance AWSPager ListDatasetContents where
  page rq rs
    | stop (rs ^. ldcrsNextToken) = Nothing
    | stop (rs ^. ldcrsDatasetContentSummaries) = Nothing
    | otherwise = Just $ rq & ldcNextToken .~ rs ^. ldcrsNextToken

instance AWSRequest ListDatasetContents where
  type Rs ListDatasetContents = ListDatasetContentsResponse
  request = get ioTAnalytics
  response =
    receiveJSON
      ( \s h x ->
          ListDatasetContentsResponse'
            <$> (x .?> "datasetContentSummaries" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListDatasetContents

instance NFData ListDatasetContents

instance ToHeaders ListDatasetContents where
  toHeaders = const mempty

instance ToPath ListDatasetContents where
  toPath ListDatasetContents' {..} =
    mconcat ["/datasets/", toBS _ldcDatasetName, "/contents"]

instance ToQuery ListDatasetContents where
  toQuery ListDatasetContents' {..} =
    mconcat
      [ "nextToken" =: _ldcNextToken,
        "scheduledBefore" =: _ldcScheduledBefore,
        "maxResults" =: _ldcMaxResults,
        "scheduledOnOrAfter" =: _ldcScheduledOnOrAfter
      ]

-- | /See:/ 'listDatasetContentsResponse' smart constructor.
data ListDatasetContentsResponse = ListDatasetContentsResponse'
  { _ldcrsDatasetContentSummaries ::
      !(Maybe [DatasetContentSummary]),
    _ldcrsNextToken :: !(Maybe Text),
    _ldcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDatasetContentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldcrsDatasetContentSummaries' - Summary information about data set contents that have been created.
--
-- * 'ldcrsNextToken' - The token to retrieve the next set of results, or @null@ if there are no more results.
--
-- * 'ldcrsResponseStatus' - -- | The response status code.
listDatasetContentsResponse ::
  -- | 'ldcrsResponseStatus'
  Int ->
  ListDatasetContentsResponse
listDatasetContentsResponse pResponseStatus_ =
  ListDatasetContentsResponse'
    { _ldcrsDatasetContentSummaries =
        Nothing,
      _ldcrsNextToken = Nothing,
      _ldcrsResponseStatus = pResponseStatus_
    }

-- | Summary information about data set contents that have been created.
ldcrsDatasetContentSummaries :: Lens' ListDatasetContentsResponse [DatasetContentSummary]
ldcrsDatasetContentSummaries = lens _ldcrsDatasetContentSummaries (\s a -> s {_ldcrsDatasetContentSummaries = a}) . _Default . _Coerce

-- | The token to retrieve the next set of results, or @null@ if there are no more results.
ldcrsNextToken :: Lens' ListDatasetContentsResponse (Maybe Text)
ldcrsNextToken = lens _ldcrsNextToken (\s a -> s {_ldcrsNextToken = a})

-- | -- | The response status code.
ldcrsResponseStatus :: Lens' ListDatasetContentsResponse Int
ldcrsResponseStatus = lens _ldcrsResponseStatus (\s a -> s {_ldcrsResponseStatus = a})

instance NFData ListDatasetContentsResponse
