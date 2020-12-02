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
-- Module      : Network.AWS.ElasticSearch.ListPackagesForDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all packages associated with the Amazon ES domain.
module Network.AWS.ElasticSearch.ListPackagesForDomain
  ( -- * Creating a Request
    listPackagesForDomain,
    ListPackagesForDomain,

    -- * Request Lenses
    lpfdNextToken,
    lpfdMaxResults,
    lpfdDomainName,

    -- * Destructuring the Response
    listPackagesForDomainResponse,
    ListPackagesForDomainResponse,

    -- * Response Lenses
    lpfdrsDomainPackageDetailsList,
    lpfdrsNextToken,
    lpfdrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for request parameters to @'ListPackagesForDomain' @ operation.
--
--
--
-- /See:/ 'listPackagesForDomain' smart constructor.
data ListPackagesForDomain = ListPackagesForDomain'
  { _lpfdNextToken ::
      !(Maybe Text),
    _lpfdMaxResults :: !(Maybe Int),
    _lpfdDomainName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPackagesForDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpfdNextToken' - Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
--
-- * 'lpfdMaxResults' - Limits results to a maximum number of packages.
--
-- * 'lpfdDomainName' - The name of the domain for which you want to list associated packages.
listPackagesForDomain ::
  -- | 'lpfdDomainName'
  Text ->
  ListPackagesForDomain
listPackagesForDomain pDomainName_ =
  ListPackagesForDomain'
    { _lpfdNextToken = Nothing,
      _lpfdMaxResults = Nothing,
      _lpfdDomainName = pDomainName_
    }

-- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
lpfdNextToken :: Lens' ListPackagesForDomain (Maybe Text)
lpfdNextToken = lens _lpfdNextToken (\s a -> s {_lpfdNextToken = a})

-- | Limits results to a maximum number of packages.
lpfdMaxResults :: Lens' ListPackagesForDomain (Maybe Int)
lpfdMaxResults = lens _lpfdMaxResults (\s a -> s {_lpfdMaxResults = a})

-- | The name of the domain for which you want to list associated packages.
lpfdDomainName :: Lens' ListPackagesForDomain Text
lpfdDomainName = lens _lpfdDomainName (\s a -> s {_lpfdDomainName = a})

instance AWSRequest ListPackagesForDomain where
  type Rs ListPackagesForDomain = ListPackagesForDomainResponse
  request = get elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          ListPackagesForDomainResponse'
            <$> (x .?> "DomainPackageDetailsList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListPackagesForDomain

instance NFData ListPackagesForDomain

instance ToHeaders ListPackagesForDomain where
  toHeaders = const mempty

instance ToPath ListPackagesForDomain where
  toPath ListPackagesForDomain' {..} =
    mconcat
      ["/2015-01-01/domain/", toBS _lpfdDomainName, "/packages"]

instance ToQuery ListPackagesForDomain where
  toQuery ListPackagesForDomain' {..} =
    mconcat
      ["nextToken" =: _lpfdNextToken, "maxResults" =: _lpfdMaxResults]

-- | Container for response parameters to @'ListPackagesForDomain' @ operation.
--
--
--
-- /See:/ 'listPackagesForDomainResponse' smart constructor.
data ListPackagesForDomainResponse = ListPackagesForDomainResponse'
  { _lpfdrsDomainPackageDetailsList ::
      !(Maybe [DomainPackageDetails]),
    _lpfdrsNextToken ::
      !(Maybe Text),
    _lpfdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPackagesForDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpfdrsDomainPackageDetailsList' - List of @DomainPackageDetails@ objects.
--
-- * 'lpfdrsNextToken' - Pagination token that needs to be supplied to the next call to get the next page of results.
--
-- * 'lpfdrsResponseStatus' - -- | The response status code.
listPackagesForDomainResponse ::
  -- | 'lpfdrsResponseStatus'
  Int ->
  ListPackagesForDomainResponse
listPackagesForDomainResponse pResponseStatus_ =
  ListPackagesForDomainResponse'
    { _lpfdrsDomainPackageDetailsList =
        Nothing,
      _lpfdrsNextToken = Nothing,
      _lpfdrsResponseStatus = pResponseStatus_
    }

-- | List of @DomainPackageDetails@ objects.
lpfdrsDomainPackageDetailsList :: Lens' ListPackagesForDomainResponse [DomainPackageDetails]
lpfdrsDomainPackageDetailsList = lens _lpfdrsDomainPackageDetailsList (\s a -> s {_lpfdrsDomainPackageDetailsList = a}) . _Default . _Coerce

-- | Pagination token that needs to be supplied to the next call to get the next page of results.
lpfdrsNextToken :: Lens' ListPackagesForDomainResponse (Maybe Text)
lpfdrsNextToken = lens _lpfdrsNextToken (\s a -> s {_lpfdrsNextToken = a})

-- | -- | The response status code.
lpfdrsResponseStatus :: Lens' ListPackagesForDomainResponse Int
lpfdrsResponseStatus = lens _lpfdrsResponseStatus (\s a -> s {_lpfdrsResponseStatus = a})

instance NFData ListPackagesForDomainResponse
