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
-- Module      : Network.AWS.SageMaker.ListModelPackages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the model packages that have been created.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListModelPackages
  ( -- * Creating a Request
    listModelPackages,
    ListModelPackages,

    -- * Request Lenses
    lmpNameContains,
    lmpCreationTimeAfter,
    lmpNextToken,
    lmpSortOrder,
    lmpCreationTimeBefore,
    lmpMaxResults,
    lmpSortBy,

    -- * Destructuring the Response
    listModelPackagesResponse,
    ListModelPackagesResponse,

    -- * Response Lenses
    lmprsNextToken,
    lmprsResponseStatus,
    lmprsModelPackageSummaryList,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listModelPackages' smart constructor.
data ListModelPackages = ListModelPackages'
  { _lmpNameContains ::
      !(Maybe Text),
    _lmpCreationTimeAfter :: !(Maybe POSIX),
    _lmpNextToken :: !(Maybe Text),
    _lmpSortOrder :: !(Maybe SortOrder),
    _lmpCreationTimeBefore :: !(Maybe POSIX),
    _lmpMaxResults :: !(Maybe Nat),
    _lmpSortBy :: !(Maybe ModelPackageSortBy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListModelPackages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmpNameContains' - A string in the model package name. This filter returns only model packages whose name contains the specified string.
--
-- * 'lmpCreationTimeAfter' - A filter that returns only model packages created after the specified time (timestamp).
--
-- * 'lmpNextToken' - If the response to a previous @ListModelPackages@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of model packages, use the token in the next request.
--
-- * 'lmpSortOrder' - The sort order for the results. The default is @Ascending@ .
--
-- * 'lmpCreationTimeBefore' - A filter that returns only model packages created before the specified time (timestamp).
--
-- * 'lmpMaxResults' - The maximum number of model packages to return in the response.
--
-- * 'lmpSortBy' - The parameter by which to sort the results. The default is @CreationTime@ .
listModelPackages ::
  ListModelPackages
listModelPackages =
  ListModelPackages'
    { _lmpNameContains = Nothing,
      _lmpCreationTimeAfter = Nothing,
      _lmpNextToken = Nothing,
      _lmpSortOrder = Nothing,
      _lmpCreationTimeBefore = Nothing,
      _lmpMaxResults = Nothing,
      _lmpSortBy = Nothing
    }

-- | A string in the model package name. This filter returns only model packages whose name contains the specified string.
lmpNameContains :: Lens' ListModelPackages (Maybe Text)
lmpNameContains = lens _lmpNameContains (\s a -> s {_lmpNameContains = a})

-- | A filter that returns only model packages created after the specified time (timestamp).
lmpCreationTimeAfter :: Lens' ListModelPackages (Maybe UTCTime)
lmpCreationTimeAfter = lens _lmpCreationTimeAfter (\s a -> s {_lmpCreationTimeAfter = a}) . mapping _Time

-- | If the response to a previous @ListModelPackages@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of model packages, use the token in the next request.
lmpNextToken :: Lens' ListModelPackages (Maybe Text)
lmpNextToken = lens _lmpNextToken (\s a -> s {_lmpNextToken = a})

-- | The sort order for the results. The default is @Ascending@ .
lmpSortOrder :: Lens' ListModelPackages (Maybe SortOrder)
lmpSortOrder = lens _lmpSortOrder (\s a -> s {_lmpSortOrder = a})

-- | A filter that returns only model packages created before the specified time (timestamp).
lmpCreationTimeBefore :: Lens' ListModelPackages (Maybe UTCTime)
lmpCreationTimeBefore = lens _lmpCreationTimeBefore (\s a -> s {_lmpCreationTimeBefore = a}) . mapping _Time

-- | The maximum number of model packages to return in the response.
lmpMaxResults :: Lens' ListModelPackages (Maybe Natural)
lmpMaxResults = lens _lmpMaxResults (\s a -> s {_lmpMaxResults = a}) . mapping _Nat

-- | The parameter by which to sort the results. The default is @CreationTime@ .
lmpSortBy :: Lens' ListModelPackages (Maybe ModelPackageSortBy)
lmpSortBy = lens _lmpSortBy (\s a -> s {_lmpSortBy = a})

instance AWSPager ListModelPackages where
  page rq rs
    | stop (rs ^. lmprsNextToken) = Nothing
    | stop (rs ^. lmprsModelPackageSummaryList) = Nothing
    | otherwise = Just $ rq & lmpNextToken .~ rs ^. lmprsNextToken

instance AWSRequest ListModelPackages where
  type Rs ListModelPackages = ListModelPackagesResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListModelPackagesResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "ModelPackageSummaryList" .!@ mempty)
      )

instance Hashable ListModelPackages

instance NFData ListModelPackages

instance ToHeaders ListModelPackages where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListModelPackages" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListModelPackages where
  toJSON ListModelPackages' {..} =
    object
      ( catMaybes
          [ ("NameContains" .=) <$> _lmpNameContains,
            ("CreationTimeAfter" .=) <$> _lmpCreationTimeAfter,
            ("NextToken" .=) <$> _lmpNextToken,
            ("SortOrder" .=) <$> _lmpSortOrder,
            ("CreationTimeBefore" .=) <$> _lmpCreationTimeBefore,
            ("MaxResults" .=) <$> _lmpMaxResults,
            ("SortBy" .=) <$> _lmpSortBy
          ]
      )

instance ToPath ListModelPackages where
  toPath = const "/"

instance ToQuery ListModelPackages where
  toQuery = const mempty

-- | /See:/ 'listModelPackagesResponse' smart constructor.
data ListModelPackagesResponse = ListModelPackagesResponse'
  { _lmprsNextToken ::
      !(Maybe Text),
    _lmprsResponseStatus :: !Int,
    _lmprsModelPackageSummaryList ::
      ![ModelPackageSummary]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListModelPackagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmprsNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of model packages, use it in the subsequent request.
--
-- * 'lmprsResponseStatus' - -- | The response status code.
--
-- * 'lmprsModelPackageSummaryList' - An array of @ModelPackageSummary@ objects, each of which lists a model package.
listModelPackagesResponse ::
  -- | 'lmprsResponseStatus'
  Int ->
  ListModelPackagesResponse
listModelPackagesResponse pResponseStatus_ =
  ListModelPackagesResponse'
    { _lmprsNextToken = Nothing,
      _lmprsResponseStatus = pResponseStatus_,
      _lmprsModelPackageSummaryList = mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of model packages, use it in the subsequent request.
lmprsNextToken :: Lens' ListModelPackagesResponse (Maybe Text)
lmprsNextToken = lens _lmprsNextToken (\s a -> s {_lmprsNextToken = a})

-- | -- | The response status code.
lmprsResponseStatus :: Lens' ListModelPackagesResponse Int
lmprsResponseStatus = lens _lmprsResponseStatus (\s a -> s {_lmprsResponseStatus = a})

-- | An array of @ModelPackageSummary@ objects, each of which lists a model package.
lmprsModelPackageSummaryList :: Lens' ListModelPackagesResponse [ModelPackageSummary]
lmprsModelPackageSummaryList = lens _lmprsModelPackageSummaryList (\s a -> s {_lmprsModelPackageSummaryList = a}) . _Coerce

instance NFData ListModelPackagesResponse
