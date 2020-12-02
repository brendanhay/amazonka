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
-- Module      : Network.AWS.SageMaker.ListImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the images in your account and their properties. The list can be filtered by creation time or modified time, and whether the image name contains a specified string.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListImages
  ( -- * Creating a Request
    listImages,
    ListImages,

    -- * Request Lenses
    liNameContains,
    liLastModifiedTimeBefore,
    liCreationTimeAfter,
    liNextToken,
    liSortOrder,
    liLastModifiedTimeAfter,
    liCreationTimeBefore,
    liMaxResults,
    liSortBy,

    -- * Destructuring the Response
    listImagesResponse,
    ListImagesResponse,

    -- * Response Lenses
    lirsImages,
    lirsNextToken,
    lirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listImages' smart constructor.
data ListImages = ListImages'
  { _liNameContains :: !(Maybe Text),
    _liLastModifiedTimeBefore :: !(Maybe POSIX),
    _liCreationTimeAfter :: !(Maybe POSIX),
    _liNextToken :: !(Maybe Text),
    _liSortOrder :: !(Maybe ImageSortOrder),
    _liLastModifiedTimeAfter :: !(Maybe POSIX),
    _liCreationTimeBefore :: !(Maybe POSIX),
    _liMaxResults :: !(Maybe Nat),
    _liSortBy :: !(Maybe ImageSortBy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListImages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liNameContains' - A filter that returns only images whose name contains the specified string.
--
-- * 'liLastModifiedTimeBefore' - A filter that returns only images modified on or before the specified time.
--
-- * 'liCreationTimeAfter' - A filter that returns only images created on or after the specified time.
--
-- * 'liNextToken' - If the previous call to @ListImages@ didn't return the full set of images, the call returns a token for getting the next set of images.
--
-- * 'liSortOrder' - The sort order. The default value is @DESCENDING@ .
--
-- * 'liLastModifiedTimeAfter' - A filter that returns only images modified on or after the specified time.
--
-- * 'liCreationTimeBefore' - A filter that returns only images created on or before the specified time.
--
-- * 'liMaxResults' - The maximum number of images to return in the response. The default value is 10.
--
-- * 'liSortBy' - The property used to sort results. The default value is @CREATION_TIME@ .
listImages ::
  ListImages
listImages =
  ListImages'
    { _liNameContains = Nothing,
      _liLastModifiedTimeBefore = Nothing,
      _liCreationTimeAfter = Nothing,
      _liNextToken = Nothing,
      _liSortOrder = Nothing,
      _liLastModifiedTimeAfter = Nothing,
      _liCreationTimeBefore = Nothing,
      _liMaxResults = Nothing,
      _liSortBy = Nothing
    }

-- | A filter that returns only images whose name contains the specified string.
liNameContains :: Lens' ListImages (Maybe Text)
liNameContains = lens _liNameContains (\s a -> s {_liNameContains = a})

-- | A filter that returns only images modified on or before the specified time.
liLastModifiedTimeBefore :: Lens' ListImages (Maybe UTCTime)
liLastModifiedTimeBefore = lens _liLastModifiedTimeBefore (\s a -> s {_liLastModifiedTimeBefore = a}) . mapping _Time

-- | A filter that returns only images created on or after the specified time.
liCreationTimeAfter :: Lens' ListImages (Maybe UTCTime)
liCreationTimeAfter = lens _liCreationTimeAfter (\s a -> s {_liCreationTimeAfter = a}) . mapping _Time

-- | If the previous call to @ListImages@ didn't return the full set of images, the call returns a token for getting the next set of images.
liNextToken :: Lens' ListImages (Maybe Text)
liNextToken = lens _liNextToken (\s a -> s {_liNextToken = a})

-- | The sort order. The default value is @DESCENDING@ .
liSortOrder :: Lens' ListImages (Maybe ImageSortOrder)
liSortOrder = lens _liSortOrder (\s a -> s {_liSortOrder = a})

-- | A filter that returns only images modified on or after the specified time.
liLastModifiedTimeAfter :: Lens' ListImages (Maybe UTCTime)
liLastModifiedTimeAfter = lens _liLastModifiedTimeAfter (\s a -> s {_liLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only images created on or before the specified time.
liCreationTimeBefore :: Lens' ListImages (Maybe UTCTime)
liCreationTimeBefore = lens _liCreationTimeBefore (\s a -> s {_liCreationTimeBefore = a}) . mapping _Time

-- | The maximum number of images to return in the response. The default value is 10.
liMaxResults :: Lens' ListImages (Maybe Natural)
liMaxResults = lens _liMaxResults (\s a -> s {_liMaxResults = a}) . mapping _Nat

-- | The property used to sort results. The default value is @CREATION_TIME@ .
liSortBy :: Lens' ListImages (Maybe ImageSortBy)
liSortBy = lens _liSortBy (\s a -> s {_liSortBy = a})

instance AWSPager ListImages where
  page rq rs
    | stop (rs ^. lirsNextToken) = Nothing
    | stop (rs ^. lirsImages) = Nothing
    | otherwise = Just $ rq & liNextToken .~ rs ^. lirsNextToken

instance AWSRequest ListImages where
  type Rs ListImages = ListImagesResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListImagesResponse'
            <$> (x .?> "Images" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListImages

instance NFData ListImages

instance ToHeaders ListImages where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListImages" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListImages where
  toJSON ListImages' {..} =
    object
      ( catMaybes
          [ ("NameContains" .=) <$> _liNameContains,
            ("LastModifiedTimeBefore" .=) <$> _liLastModifiedTimeBefore,
            ("CreationTimeAfter" .=) <$> _liCreationTimeAfter,
            ("NextToken" .=) <$> _liNextToken,
            ("SortOrder" .=) <$> _liSortOrder,
            ("LastModifiedTimeAfter" .=) <$> _liLastModifiedTimeAfter,
            ("CreationTimeBefore" .=) <$> _liCreationTimeBefore,
            ("MaxResults" .=) <$> _liMaxResults,
            ("SortBy" .=) <$> _liSortBy
          ]
      )

instance ToPath ListImages where
  toPath = const "/"

instance ToQuery ListImages where
  toQuery = const mempty

-- | /See:/ 'listImagesResponse' smart constructor.
data ListImagesResponse = ListImagesResponse'
  { _lirsImages ::
      !(Maybe [Image]),
    _lirsNextToken :: !(Maybe Text),
    _lirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListImagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lirsImages' - A list of images and their properties.
--
-- * 'lirsNextToken' - A token for getting the next set of images, if there are any.
--
-- * 'lirsResponseStatus' - -- | The response status code.
listImagesResponse ::
  -- | 'lirsResponseStatus'
  Int ->
  ListImagesResponse
listImagesResponse pResponseStatus_ =
  ListImagesResponse'
    { _lirsImages = Nothing,
      _lirsNextToken = Nothing,
      _lirsResponseStatus = pResponseStatus_
    }

-- | A list of images and their properties.
lirsImages :: Lens' ListImagesResponse [Image]
lirsImages = lens _lirsImages (\s a -> s {_lirsImages = a}) . _Default . _Coerce

-- | A token for getting the next set of images, if there are any.
lirsNextToken :: Lens' ListImagesResponse (Maybe Text)
lirsNextToken = lens _lirsNextToken (\s a -> s {_lirsNextToken = a})

-- | -- | The response status code.
lirsResponseStatus :: Lens' ListImagesResponse Int
lirsResponseStatus = lens _lirsResponseStatus (\s a -> s {_lirsResponseStatus = a})

instance NFData ListImagesResponse
