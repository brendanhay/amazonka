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
-- Module      : Network.AWS.ElasticSearch.DescribePackages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all packages available to Amazon ES. Includes options for filtering, limiting the number of results, and pagination.
module Network.AWS.ElasticSearch.DescribePackages
  ( -- * Creating a Request
    describePackages,
    DescribePackages,

    -- * Request Lenses
    dpFilters,
    dpNextToken,
    dpMaxResults,

    -- * Destructuring the Response
    describePackagesResponse,
    DescribePackagesResponse,

    -- * Response Lenses
    dprsPackageDetailsList,
    dprsNextToken,
    dprsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for request parameters to @'DescribePackage' @ operation.
--
--
--
-- /See:/ 'describePackages' smart constructor.
data DescribePackages = DescribePackages'
  { _dpFilters ::
      !(Maybe [DescribePackagesFilter]),
    _dpNextToken :: !(Maybe Text),
    _dpMaxResults :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribePackages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpFilters' - Only returns packages that match the @DescribePackagesFilterList@ values.
--
-- * 'dpNextToken' - Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
--
-- * 'dpMaxResults' - Limits results to a maximum number of packages.
describePackages ::
  DescribePackages
describePackages =
  DescribePackages'
    { _dpFilters = Nothing,
      _dpNextToken = Nothing,
      _dpMaxResults = Nothing
    }

-- | Only returns packages that match the @DescribePackagesFilterList@ values.
dpFilters :: Lens' DescribePackages [DescribePackagesFilter]
dpFilters = lens _dpFilters (\s a -> s {_dpFilters = a}) . _Default . _Coerce

-- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
dpNextToken :: Lens' DescribePackages (Maybe Text)
dpNextToken = lens _dpNextToken (\s a -> s {_dpNextToken = a})

-- | Limits results to a maximum number of packages.
dpMaxResults :: Lens' DescribePackages (Maybe Int)
dpMaxResults = lens _dpMaxResults (\s a -> s {_dpMaxResults = a})

instance AWSRequest DescribePackages where
  type Rs DescribePackages = DescribePackagesResponse
  request = postJSON elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          DescribePackagesResponse'
            <$> (x .?> "PackageDetailsList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribePackages

instance NFData DescribePackages

instance ToHeaders DescribePackages where
  toHeaders = const mempty

instance ToJSON DescribePackages where
  toJSON DescribePackages' {..} =
    object
      ( catMaybes
          [ ("Filters" .=) <$> _dpFilters,
            ("NextToken" .=) <$> _dpNextToken,
            ("MaxResults" .=) <$> _dpMaxResults
          ]
      )

instance ToPath DescribePackages where
  toPath = const "/2015-01-01/packages/describe"

instance ToQuery DescribePackages where
  toQuery = const mempty

-- | Container for response returned by @'DescribePackages' @ operation.
--
--
--
-- /See:/ 'describePackagesResponse' smart constructor.
data DescribePackagesResponse = DescribePackagesResponse'
  { _dprsPackageDetailsList ::
      !(Maybe [PackageDetails]),
    _dprsNextToken :: !(Maybe Text),
    _dprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribePackagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsPackageDetailsList' - List of @PackageDetails@ objects.
--
-- * 'dprsNextToken' - Undocumented member.
--
-- * 'dprsResponseStatus' - -- | The response status code.
describePackagesResponse ::
  -- | 'dprsResponseStatus'
  Int ->
  DescribePackagesResponse
describePackagesResponse pResponseStatus_ =
  DescribePackagesResponse'
    { _dprsPackageDetailsList = Nothing,
      _dprsNextToken = Nothing,
      _dprsResponseStatus = pResponseStatus_
    }

-- | List of @PackageDetails@ objects.
dprsPackageDetailsList :: Lens' DescribePackagesResponse [PackageDetails]
dprsPackageDetailsList = lens _dprsPackageDetailsList (\s a -> s {_dprsPackageDetailsList = a}) . _Default . _Coerce

-- | Undocumented member.
dprsNextToken :: Lens' DescribePackagesResponse (Maybe Text)
dprsNextToken = lens _dprsNextToken (\s a -> s {_dprsNextToken = a})

-- | -- | The response status code.
dprsResponseStatus :: Lens' DescribePackagesResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\s a -> s {_dprsResponseStatus = a})

instance NFData DescribePackagesResponse
