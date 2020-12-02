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
-- Module      : Network.AWS.SageMaker.ListUserProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists user profiles.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListUserProfiles
  ( -- * Creating a Request
    listUserProfiles,
    ListUserProfiles,

    -- * Request Lenses
    lupDomainIdEquals,
    lupUserProfileNameContains,
    lupNextToken,
    lupSortOrder,
    lupMaxResults,
    lupSortBy,

    -- * Destructuring the Response
    listUserProfilesResponse,
    ListUserProfilesResponse,

    -- * Response Lenses
    luprsUserProfiles,
    luprsNextToken,
    luprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listUserProfiles' smart constructor.
data ListUserProfiles = ListUserProfiles'
  { _lupDomainIdEquals ::
      !(Maybe Text),
    _lupUserProfileNameContains :: !(Maybe Text),
    _lupNextToken :: !(Maybe Text),
    _lupSortOrder :: !(Maybe SortOrder),
    _lupMaxResults :: !(Maybe Nat),
    _lupSortBy :: !(Maybe UserProfileSortKey)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListUserProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lupDomainIdEquals' - A parameter by which to filter the results.
--
-- * 'lupUserProfileNameContains' - A parameter by which to filter the results.
--
-- * 'lupNextToken' - If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- * 'lupSortOrder' - The sort order for the results. The default is Ascending.
--
-- * 'lupMaxResults' - Returns a list up to a specified limit.
--
-- * 'lupSortBy' - The parameter by which to sort the results. The default is CreationTime.
listUserProfiles ::
  ListUserProfiles
listUserProfiles =
  ListUserProfiles'
    { _lupDomainIdEquals = Nothing,
      _lupUserProfileNameContains = Nothing,
      _lupNextToken = Nothing,
      _lupSortOrder = Nothing,
      _lupMaxResults = Nothing,
      _lupSortBy = Nothing
    }

-- | A parameter by which to filter the results.
lupDomainIdEquals :: Lens' ListUserProfiles (Maybe Text)
lupDomainIdEquals = lens _lupDomainIdEquals (\s a -> s {_lupDomainIdEquals = a})

-- | A parameter by which to filter the results.
lupUserProfileNameContains :: Lens' ListUserProfiles (Maybe Text)
lupUserProfileNameContains = lens _lupUserProfileNameContains (\s a -> s {_lupUserProfileNameContains = a})

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
lupNextToken :: Lens' ListUserProfiles (Maybe Text)
lupNextToken = lens _lupNextToken (\s a -> s {_lupNextToken = a})

-- | The sort order for the results. The default is Ascending.
lupSortOrder :: Lens' ListUserProfiles (Maybe SortOrder)
lupSortOrder = lens _lupSortOrder (\s a -> s {_lupSortOrder = a})

-- | Returns a list up to a specified limit.
lupMaxResults :: Lens' ListUserProfiles (Maybe Natural)
lupMaxResults = lens _lupMaxResults (\s a -> s {_lupMaxResults = a}) . mapping _Nat

-- | The parameter by which to sort the results. The default is CreationTime.
lupSortBy :: Lens' ListUserProfiles (Maybe UserProfileSortKey)
lupSortBy = lens _lupSortBy (\s a -> s {_lupSortBy = a})

instance AWSPager ListUserProfiles where
  page rq rs
    | stop (rs ^. luprsNextToken) = Nothing
    | stop (rs ^. luprsUserProfiles) = Nothing
    | otherwise = Just $ rq & lupNextToken .~ rs ^. luprsNextToken

instance AWSRequest ListUserProfiles where
  type Rs ListUserProfiles = ListUserProfilesResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListUserProfilesResponse'
            <$> (x .?> "UserProfiles" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListUserProfiles

instance NFData ListUserProfiles

instance ToHeaders ListUserProfiles where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListUserProfiles" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListUserProfiles where
  toJSON ListUserProfiles' {..} =
    object
      ( catMaybes
          [ ("DomainIdEquals" .=) <$> _lupDomainIdEquals,
            ("UserProfileNameContains" .=) <$> _lupUserProfileNameContains,
            ("NextToken" .=) <$> _lupNextToken,
            ("SortOrder" .=) <$> _lupSortOrder,
            ("MaxResults" .=) <$> _lupMaxResults,
            ("SortBy" .=) <$> _lupSortBy
          ]
      )

instance ToPath ListUserProfiles where
  toPath = const "/"

instance ToQuery ListUserProfiles where
  toQuery = const mempty

-- | /See:/ 'listUserProfilesResponse' smart constructor.
data ListUserProfilesResponse = ListUserProfilesResponse'
  { _luprsUserProfiles ::
      !(Maybe [UserProfileDetails]),
    _luprsNextToken :: !(Maybe Text),
    _luprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListUserProfilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luprsUserProfiles' - The list of user profiles.
--
-- * 'luprsNextToken' - If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- * 'luprsResponseStatus' - -- | The response status code.
listUserProfilesResponse ::
  -- | 'luprsResponseStatus'
  Int ->
  ListUserProfilesResponse
listUserProfilesResponse pResponseStatus_ =
  ListUserProfilesResponse'
    { _luprsUserProfiles = Nothing,
      _luprsNextToken = Nothing,
      _luprsResponseStatus = pResponseStatus_
    }

-- | The list of user profiles.
luprsUserProfiles :: Lens' ListUserProfilesResponse [UserProfileDetails]
luprsUserProfiles = lens _luprsUserProfiles (\s a -> s {_luprsUserProfiles = a}) . _Default . _Coerce

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
luprsNextToken :: Lens' ListUserProfilesResponse (Maybe Text)
luprsNextToken = lens _luprsNextToken (\s a -> s {_luprsNextToken = a})

-- | -- | The response status code.
luprsResponseStatus :: Lens' ListUserProfilesResponse Int
luprsResponseStatus = lens _luprsResponseStatus (\s a -> s {_luprsResponseStatus = a})

instance NFData ListUserProfilesResponse
