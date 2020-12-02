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
-- Module      : Network.AWS.SageMaker.ListApps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists apps.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListApps
  ( -- * Creating a Request
    listApps,
    ListApps,

    -- * Request Lenses
    laDomainIdEquals,
    laNextToken,
    laSortOrder,
    laUserProfileNameEquals,
    laMaxResults,
    laSortBy,

    -- * Destructuring the Response
    listAppsResponse,
    ListAppsResponse,

    -- * Response Lenses
    larsApps,
    larsNextToken,
    larsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listApps' smart constructor.
data ListApps = ListApps'
  { _laDomainIdEquals :: !(Maybe Text),
    _laNextToken :: !(Maybe Text),
    _laSortOrder :: !(Maybe SortOrder),
    _laUserProfileNameEquals :: !(Maybe Text),
    _laMaxResults :: !(Maybe Nat),
    _laSortBy :: !(Maybe AppSortKey)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListApps' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laDomainIdEquals' - A parameter to search for the domain ID.
--
-- * 'laNextToken' - If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- * 'laSortOrder' - The sort order for the results. The default is Ascending.
--
-- * 'laUserProfileNameEquals' - A parameter to search by user profile name.
--
-- * 'laMaxResults' - Returns a list up to a specified limit.
--
-- * 'laSortBy' - The parameter by which to sort the results. The default is CreationTime.
listApps ::
  ListApps
listApps =
  ListApps'
    { _laDomainIdEquals = Nothing,
      _laNextToken = Nothing,
      _laSortOrder = Nothing,
      _laUserProfileNameEquals = Nothing,
      _laMaxResults = Nothing,
      _laSortBy = Nothing
    }

-- | A parameter to search for the domain ID.
laDomainIdEquals :: Lens' ListApps (Maybe Text)
laDomainIdEquals = lens _laDomainIdEquals (\s a -> s {_laDomainIdEquals = a})

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
laNextToken :: Lens' ListApps (Maybe Text)
laNextToken = lens _laNextToken (\s a -> s {_laNextToken = a})

-- | The sort order for the results. The default is Ascending.
laSortOrder :: Lens' ListApps (Maybe SortOrder)
laSortOrder = lens _laSortOrder (\s a -> s {_laSortOrder = a})

-- | A parameter to search by user profile name.
laUserProfileNameEquals :: Lens' ListApps (Maybe Text)
laUserProfileNameEquals = lens _laUserProfileNameEquals (\s a -> s {_laUserProfileNameEquals = a})

-- | Returns a list up to a specified limit.
laMaxResults :: Lens' ListApps (Maybe Natural)
laMaxResults = lens _laMaxResults (\s a -> s {_laMaxResults = a}) . mapping _Nat

-- | The parameter by which to sort the results. The default is CreationTime.
laSortBy :: Lens' ListApps (Maybe AppSortKey)
laSortBy = lens _laSortBy (\s a -> s {_laSortBy = a})

instance AWSPager ListApps where
  page rq rs
    | stop (rs ^. larsNextToken) = Nothing
    | stop (rs ^. larsApps) = Nothing
    | otherwise = Just $ rq & laNextToken .~ rs ^. larsNextToken

instance AWSRequest ListApps where
  type Rs ListApps = ListAppsResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListAppsResponse'
            <$> (x .?> "Apps" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListApps

instance NFData ListApps

instance ToHeaders ListApps where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListApps" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListApps where
  toJSON ListApps' {..} =
    object
      ( catMaybes
          [ ("DomainIdEquals" .=) <$> _laDomainIdEquals,
            ("NextToken" .=) <$> _laNextToken,
            ("SortOrder" .=) <$> _laSortOrder,
            ("UserProfileNameEquals" .=) <$> _laUserProfileNameEquals,
            ("MaxResults" .=) <$> _laMaxResults,
            ("SortBy" .=) <$> _laSortBy
          ]
      )

instance ToPath ListApps where
  toPath = const "/"

instance ToQuery ListApps where
  toQuery = const mempty

-- | /See:/ 'listAppsResponse' smart constructor.
data ListAppsResponse = ListAppsResponse'
  { _larsApps ::
      !(Maybe [AppDetails]),
    _larsNextToken :: !(Maybe Text),
    _larsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAppsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsApps' - The list of apps.
--
-- * 'larsNextToken' - If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- * 'larsResponseStatus' - -- | The response status code.
listAppsResponse ::
  -- | 'larsResponseStatus'
  Int ->
  ListAppsResponse
listAppsResponse pResponseStatus_ =
  ListAppsResponse'
    { _larsApps = Nothing,
      _larsNextToken = Nothing,
      _larsResponseStatus = pResponseStatus_
    }

-- | The list of apps.
larsApps :: Lens' ListAppsResponse [AppDetails]
larsApps = lens _larsApps (\s a -> s {_larsApps = a}) . _Default . _Coerce

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
larsNextToken :: Lens' ListAppsResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\s a -> s {_larsNextToken = a})

-- | -- | The response status code.
larsResponseStatus :: Lens' ListAppsResponse Int
larsResponseStatus = lens _larsResponseStatus (\s a -> s {_larsResponseStatus = a})

instance NFData ListAppsResponse
