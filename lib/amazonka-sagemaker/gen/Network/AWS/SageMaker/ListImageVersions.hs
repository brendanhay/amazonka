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
-- Module      : Network.AWS.SageMaker.ListImageVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a specified image and their properties. The list can be filtered by creation time or modified time.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListImageVersions
  ( -- * Creating a Request
    listImageVersions,
    ListImageVersions,

    -- * Request Lenses
    livLastModifiedTimeBefore,
    livCreationTimeAfter,
    livNextToken,
    livSortOrder,
    livLastModifiedTimeAfter,
    livCreationTimeBefore,
    livMaxResults,
    livSortBy,
    livImageName,

    -- * Destructuring the Response
    listImageVersionsResponse,
    ListImageVersionsResponse,

    -- * Response Lenses
    livrsNextToken,
    livrsImageVersions,
    livrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listImageVersions' smart constructor.
data ListImageVersions = ListImageVersions'
  { _livLastModifiedTimeBefore ::
      !(Maybe POSIX),
    _livCreationTimeAfter :: !(Maybe POSIX),
    _livNextToken :: !(Maybe Text),
    _livSortOrder :: !(Maybe ImageVersionSortOrder),
    _livLastModifiedTimeAfter :: !(Maybe POSIX),
    _livCreationTimeBefore :: !(Maybe POSIX),
    _livMaxResults :: !(Maybe Nat),
    _livSortBy :: !(Maybe ImageVersionSortBy),
    _livImageName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListImageVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'livLastModifiedTimeBefore' - A filter that returns only versions modified on or before the specified time.
--
-- * 'livCreationTimeAfter' - A filter that returns only versions created on or after the specified time.
--
-- * 'livNextToken' - If the previous call to @ListImageVersions@ didn't return the full set of versions, the call returns a token for getting the next set of versions.
--
-- * 'livSortOrder' - The sort order. The default value is @DESCENDING@ .
--
-- * 'livLastModifiedTimeAfter' - A filter that returns only versions modified on or after the specified time.
--
-- * 'livCreationTimeBefore' - A filter that returns only versions created on or before the specified time.
--
-- * 'livMaxResults' - The maximum number of versions to return in the response. The default value is 10.
--
-- * 'livSortBy' - The property used to sort results. The default value is @CREATION_TIME@ .
--
-- * 'livImageName' - The name of the image to list the versions of.
listImageVersions ::
  -- | 'livImageName'
  Text ->
  ListImageVersions
listImageVersions pImageName_ =
  ListImageVersions'
    { _livLastModifiedTimeBefore = Nothing,
      _livCreationTimeAfter = Nothing,
      _livNextToken = Nothing,
      _livSortOrder = Nothing,
      _livLastModifiedTimeAfter = Nothing,
      _livCreationTimeBefore = Nothing,
      _livMaxResults = Nothing,
      _livSortBy = Nothing,
      _livImageName = pImageName_
    }

-- | A filter that returns only versions modified on or before the specified time.
livLastModifiedTimeBefore :: Lens' ListImageVersions (Maybe UTCTime)
livLastModifiedTimeBefore = lens _livLastModifiedTimeBefore (\s a -> s {_livLastModifiedTimeBefore = a}) . mapping _Time

-- | A filter that returns only versions created on or after the specified time.
livCreationTimeAfter :: Lens' ListImageVersions (Maybe UTCTime)
livCreationTimeAfter = lens _livCreationTimeAfter (\s a -> s {_livCreationTimeAfter = a}) . mapping _Time

-- | If the previous call to @ListImageVersions@ didn't return the full set of versions, the call returns a token for getting the next set of versions.
livNextToken :: Lens' ListImageVersions (Maybe Text)
livNextToken = lens _livNextToken (\s a -> s {_livNextToken = a})

-- | The sort order. The default value is @DESCENDING@ .
livSortOrder :: Lens' ListImageVersions (Maybe ImageVersionSortOrder)
livSortOrder = lens _livSortOrder (\s a -> s {_livSortOrder = a})

-- | A filter that returns only versions modified on or after the specified time.
livLastModifiedTimeAfter :: Lens' ListImageVersions (Maybe UTCTime)
livLastModifiedTimeAfter = lens _livLastModifiedTimeAfter (\s a -> s {_livLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only versions created on or before the specified time.
livCreationTimeBefore :: Lens' ListImageVersions (Maybe UTCTime)
livCreationTimeBefore = lens _livCreationTimeBefore (\s a -> s {_livCreationTimeBefore = a}) . mapping _Time

-- | The maximum number of versions to return in the response. The default value is 10.
livMaxResults :: Lens' ListImageVersions (Maybe Natural)
livMaxResults = lens _livMaxResults (\s a -> s {_livMaxResults = a}) . mapping _Nat

-- | The property used to sort results. The default value is @CREATION_TIME@ .
livSortBy :: Lens' ListImageVersions (Maybe ImageVersionSortBy)
livSortBy = lens _livSortBy (\s a -> s {_livSortBy = a})

-- | The name of the image to list the versions of.
livImageName :: Lens' ListImageVersions Text
livImageName = lens _livImageName (\s a -> s {_livImageName = a})

instance AWSPager ListImageVersions where
  page rq rs
    | stop (rs ^. livrsNextToken) = Nothing
    | stop (rs ^. livrsImageVersions) = Nothing
    | otherwise = Just $ rq & livNextToken .~ rs ^. livrsNextToken

instance AWSRequest ListImageVersions where
  type Rs ListImageVersions = ListImageVersionsResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListImageVersionsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "ImageVersions" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListImageVersions

instance NFData ListImageVersions

instance ToHeaders ListImageVersions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListImageVersions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListImageVersions where
  toJSON ListImageVersions' {..} =
    object
      ( catMaybes
          [ ("LastModifiedTimeBefore" .=) <$> _livLastModifiedTimeBefore,
            ("CreationTimeAfter" .=) <$> _livCreationTimeAfter,
            ("NextToken" .=) <$> _livNextToken,
            ("SortOrder" .=) <$> _livSortOrder,
            ("LastModifiedTimeAfter" .=) <$> _livLastModifiedTimeAfter,
            ("CreationTimeBefore" .=) <$> _livCreationTimeBefore,
            ("MaxResults" .=) <$> _livMaxResults,
            ("SortBy" .=) <$> _livSortBy,
            Just ("ImageName" .= _livImageName)
          ]
      )

instance ToPath ListImageVersions where
  toPath = const "/"

instance ToQuery ListImageVersions where
  toQuery = const mempty

-- | /See:/ 'listImageVersionsResponse' smart constructor.
data ListImageVersionsResponse = ListImageVersionsResponse'
  { _livrsNextToken ::
      !(Maybe Text),
    _livrsImageVersions ::
      !(Maybe [ImageVersion]),
    _livrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListImageVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'livrsNextToken' - A token for getting the next set of versions, if there are any.
--
-- * 'livrsImageVersions' - A list of versions and their properties.
--
-- * 'livrsResponseStatus' - -- | The response status code.
listImageVersionsResponse ::
  -- | 'livrsResponseStatus'
  Int ->
  ListImageVersionsResponse
listImageVersionsResponse pResponseStatus_ =
  ListImageVersionsResponse'
    { _livrsNextToken = Nothing,
      _livrsImageVersions = Nothing,
      _livrsResponseStatus = pResponseStatus_
    }

-- | A token for getting the next set of versions, if there are any.
livrsNextToken :: Lens' ListImageVersionsResponse (Maybe Text)
livrsNextToken = lens _livrsNextToken (\s a -> s {_livrsNextToken = a})

-- | A list of versions and their properties.
livrsImageVersions :: Lens' ListImageVersionsResponse [ImageVersion]
livrsImageVersions = lens _livrsImageVersions (\s a -> s {_livrsImageVersions = a}) . _Default . _Coerce

-- | -- | The response status code.
livrsResponseStatus :: Lens' ListImageVersionsResponse Int
livrsResponseStatus = lens _livrsResponseStatus (\s a -> s {_livrsResponseStatus = a})

instance NFData ListImageVersionsResponse
