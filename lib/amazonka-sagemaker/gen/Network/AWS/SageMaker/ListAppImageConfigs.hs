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
-- Module      : Network.AWS.SageMaker.ListAppImageConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AppImageConfigs in your account and their properties. The list can be filtered by creation time or modified time, and whether the AppImageConfig name contains a specified string.
module Network.AWS.SageMaker.ListAppImageConfigs
  ( -- * Creating a Request
    listAppImageConfigs,
    ListAppImageConfigs,

    -- * Request Lenses
    laicNameContains,
    laicCreationTimeAfter,
    laicModifiedTimeAfter,
    laicNextToken,
    laicSortOrder,
    laicCreationTimeBefore,
    laicModifiedTimeBefore,
    laicMaxResults,
    laicSortBy,

    -- * Destructuring the Response
    listAppImageConfigsResponse,
    ListAppImageConfigsResponse,

    -- * Response Lenses
    laicrsAppImageConfigs,
    laicrsNextToken,
    laicrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listAppImageConfigs' smart constructor.
data ListAppImageConfigs = ListAppImageConfigs'
  { _laicNameContains ::
      !(Maybe Text),
    _laicCreationTimeAfter :: !(Maybe POSIX),
    _laicModifiedTimeAfter :: !(Maybe POSIX),
    _laicNextToken :: !(Maybe Text),
    _laicSortOrder :: !(Maybe SortOrder),
    _laicCreationTimeBefore :: !(Maybe POSIX),
    _laicModifiedTimeBefore :: !(Maybe POSIX),
    _laicMaxResults :: !(Maybe Nat),
    _laicSortBy :: !(Maybe AppImageConfigSortKey)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAppImageConfigs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laicNameContains' - A filter that returns only AppImageConfigs whose name contains the specified string.
--
-- * 'laicCreationTimeAfter' - A filter that returns only AppImageConfigs created on or after the specified time.
--
-- * 'laicModifiedTimeAfter' - A filter that returns only AppImageConfigs modified on or after the specified time.
--
-- * 'laicNextToken' - If the previous call to @ListImages@ didn't return the full set of AppImageConfigs, the call returns a token for getting the next set of AppImageConfigs.
--
-- * 'laicSortOrder' - The sort order. The default value is @Descending@ .
--
-- * 'laicCreationTimeBefore' - A filter that returns only AppImageConfigs created on or before the specified time.
--
-- * 'laicModifiedTimeBefore' - A filter that returns only AppImageConfigs modified on or before the specified time.
--
-- * 'laicMaxResults' - The maximum number of AppImageConfigs to return in the response. The default value is 10.
--
-- * 'laicSortBy' - The property used to sort results. The default value is @CreationTime@ .
listAppImageConfigs ::
  ListAppImageConfigs
listAppImageConfigs =
  ListAppImageConfigs'
    { _laicNameContains = Nothing,
      _laicCreationTimeAfter = Nothing,
      _laicModifiedTimeAfter = Nothing,
      _laicNextToken = Nothing,
      _laicSortOrder = Nothing,
      _laicCreationTimeBefore = Nothing,
      _laicModifiedTimeBefore = Nothing,
      _laicMaxResults = Nothing,
      _laicSortBy = Nothing
    }

-- | A filter that returns only AppImageConfigs whose name contains the specified string.
laicNameContains :: Lens' ListAppImageConfigs (Maybe Text)
laicNameContains = lens _laicNameContains (\s a -> s {_laicNameContains = a})

-- | A filter that returns only AppImageConfigs created on or after the specified time.
laicCreationTimeAfter :: Lens' ListAppImageConfigs (Maybe UTCTime)
laicCreationTimeAfter = lens _laicCreationTimeAfter (\s a -> s {_laicCreationTimeAfter = a}) . mapping _Time

-- | A filter that returns only AppImageConfigs modified on or after the specified time.
laicModifiedTimeAfter :: Lens' ListAppImageConfigs (Maybe UTCTime)
laicModifiedTimeAfter = lens _laicModifiedTimeAfter (\s a -> s {_laicModifiedTimeAfter = a}) . mapping _Time

-- | If the previous call to @ListImages@ didn't return the full set of AppImageConfigs, the call returns a token for getting the next set of AppImageConfigs.
laicNextToken :: Lens' ListAppImageConfigs (Maybe Text)
laicNextToken = lens _laicNextToken (\s a -> s {_laicNextToken = a})

-- | The sort order. The default value is @Descending@ .
laicSortOrder :: Lens' ListAppImageConfigs (Maybe SortOrder)
laicSortOrder = lens _laicSortOrder (\s a -> s {_laicSortOrder = a})

-- | A filter that returns only AppImageConfigs created on or before the specified time.
laicCreationTimeBefore :: Lens' ListAppImageConfigs (Maybe UTCTime)
laicCreationTimeBefore = lens _laicCreationTimeBefore (\s a -> s {_laicCreationTimeBefore = a}) . mapping _Time

-- | A filter that returns only AppImageConfigs modified on or before the specified time.
laicModifiedTimeBefore :: Lens' ListAppImageConfigs (Maybe UTCTime)
laicModifiedTimeBefore = lens _laicModifiedTimeBefore (\s a -> s {_laicModifiedTimeBefore = a}) . mapping _Time

-- | The maximum number of AppImageConfigs to return in the response. The default value is 10.
laicMaxResults :: Lens' ListAppImageConfigs (Maybe Natural)
laicMaxResults = lens _laicMaxResults (\s a -> s {_laicMaxResults = a}) . mapping _Nat

-- | The property used to sort results. The default value is @CreationTime@ .
laicSortBy :: Lens' ListAppImageConfigs (Maybe AppImageConfigSortKey)
laicSortBy = lens _laicSortBy (\s a -> s {_laicSortBy = a})

instance AWSRequest ListAppImageConfigs where
  type Rs ListAppImageConfigs = ListAppImageConfigsResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListAppImageConfigsResponse'
            <$> (x .?> "AppImageConfigs" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListAppImageConfigs

instance NFData ListAppImageConfigs

instance ToHeaders ListAppImageConfigs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListAppImageConfigs" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListAppImageConfigs where
  toJSON ListAppImageConfigs' {..} =
    object
      ( catMaybes
          [ ("NameContains" .=) <$> _laicNameContains,
            ("CreationTimeAfter" .=) <$> _laicCreationTimeAfter,
            ("ModifiedTimeAfter" .=) <$> _laicModifiedTimeAfter,
            ("NextToken" .=) <$> _laicNextToken,
            ("SortOrder" .=) <$> _laicSortOrder,
            ("CreationTimeBefore" .=) <$> _laicCreationTimeBefore,
            ("ModifiedTimeBefore" .=) <$> _laicModifiedTimeBefore,
            ("MaxResults" .=) <$> _laicMaxResults,
            ("SortBy" .=) <$> _laicSortBy
          ]
      )

instance ToPath ListAppImageConfigs where
  toPath = const "/"

instance ToQuery ListAppImageConfigs where
  toQuery = const mempty

-- | /See:/ 'listAppImageConfigsResponse' smart constructor.
data ListAppImageConfigsResponse = ListAppImageConfigsResponse'
  { _laicrsAppImageConfigs ::
      !(Maybe [AppImageConfigDetails]),
    _laicrsNextToken :: !(Maybe Text),
    _laicrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAppImageConfigsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laicrsAppImageConfigs' - A list of AppImageConfigs and their properties.
--
-- * 'laicrsNextToken' - A token for getting the next set of AppImageConfigs, if there are any.
--
-- * 'laicrsResponseStatus' - -- | The response status code.
listAppImageConfigsResponse ::
  -- | 'laicrsResponseStatus'
  Int ->
  ListAppImageConfigsResponse
listAppImageConfigsResponse pResponseStatus_ =
  ListAppImageConfigsResponse'
    { _laicrsAppImageConfigs = Nothing,
      _laicrsNextToken = Nothing,
      _laicrsResponseStatus = pResponseStatus_
    }

-- | A list of AppImageConfigs and their properties.
laicrsAppImageConfigs :: Lens' ListAppImageConfigsResponse [AppImageConfigDetails]
laicrsAppImageConfigs = lens _laicrsAppImageConfigs (\s a -> s {_laicrsAppImageConfigs = a}) . _Default . _Coerce

-- | A token for getting the next set of AppImageConfigs, if there are any.
laicrsNextToken :: Lens' ListAppImageConfigsResponse (Maybe Text)
laicrsNextToken = lens _laicrsNextToken (\s a -> s {_laicrsNextToken = a})

-- | -- | The response status code.
laicrsResponseStatus :: Lens' ListAppImageConfigsResponse Int
laicrsResponseStatus = lens _laicrsResponseStatus (\s a -> s {_laicrsResponseStatus = a})

instance NFData ListAppImageConfigsResponse
