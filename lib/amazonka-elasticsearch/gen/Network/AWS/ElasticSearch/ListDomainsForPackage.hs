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
-- Module      : Network.AWS.ElasticSearch.ListDomainsForPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Amazon ES domains associated with the package.
module Network.AWS.ElasticSearch.ListDomainsForPackage
  ( -- * Creating a Request
    listDomainsForPackage,
    ListDomainsForPackage,

    -- * Request Lenses
    ldfpNextToken,
    ldfpMaxResults,
    ldfpPackageId,

    -- * Destructuring the Response
    listDomainsForPackageResponse,
    ListDomainsForPackageResponse,

    -- * Response Lenses
    ldfprsDomainPackageDetailsList,
    ldfprsNextToken,
    ldfprsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for request parameters to @'ListDomainsForPackage' @ operation.
--
--
--
-- /See:/ 'listDomainsForPackage' smart constructor.
data ListDomainsForPackage = ListDomainsForPackage'
  { _ldfpNextToken ::
      !(Maybe Text),
    _ldfpMaxResults :: !(Maybe Int),
    _ldfpPackageId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDomainsForPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldfpNextToken' - Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
--
-- * 'ldfpMaxResults' - Limits results to a maximum number of domains.
--
-- * 'ldfpPackageId' - The package for which to list domains.
listDomainsForPackage ::
  -- | 'ldfpPackageId'
  Text ->
  ListDomainsForPackage
listDomainsForPackage pPackageId_ =
  ListDomainsForPackage'
    { _ldfpNextToken = Nothing,
      _ldfpMaxResults = Nothing,
      _ldfpPackageId = pPackageId_
    }

-- | Used for pagination. Only necessary if a previous API call includes a non-null NextToken value. If provided, returns results for the next page.
ldfpNextToken :: Lens' ListDomainsForPackage (Maybe Text)
ldfpNextToken = lens _ldfpNextToken (\s a -> s {_ldfpNextToken = a})

-- | Limits results to a maximum number of domains.
ldfpMaxResults :: Lens' ListDomainsForPackage (Maybe Int)
ldfpMaxResults = lens _ldfpMaxResults (\s a -> s {_ldfpMaxResults = a})

-- | The package for which to list domains.
ldfpPackageId :: Lens' ListDomainsForPackage Text
ldfpPackageId = lens _ldfpPackageId (\s a -> s {_ldfpPackageId = a})

instance AWSRequest ListDomainsForPackage where
  type Rs ListDomainsForPackage = ListDomainsForPackageResponse
  request = get elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          ListDomainsForPackageResponse'
            <$> (x .?> "DomainPackageDetailsList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListDomainsForPackage

instance NFData ListDomainsForPackage

instance ToHeaders ListDomainsForPackage where
  toHeaders = const mempty

instance ToPath ListDomainsForPackage where
  toPath ListDomainsForPackage' {..} =
    mconcat
      ["/2015-01-01/packages/", toBS _ldfpPackageId, "/domains"]

instance ToQuery ListDomainsForPackage where
  toQuery ListDomainsForPackage' {..} =
    mconcat
      ["nextToken" =: _ldfpNextToken, "maxResults" =: _ldfpMaxResults]

-- | Container for response parameters to @'ListDomainsForPackage' @ operation.
--
--
--
-- /See:/ 'listDomainsForPackageResponse' smart constructor.
data ListDomainsForPackageResponse = ListDomainsForPackageResponse'
  { _ldfprsDomainPackageDetailsList ::
      !(Maybe [DomainPackageDetails]),
    _ldfprsNextToken ::
      !(Maybe Text),
    _ldfprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDomainsForPackageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldfprsDomainPackageDetailsList' - List of @DomainPackageDetails@ objects.
--
-- * 'ldfprsNextToken' - Undocumented member.
--
-- * 'ldfprsResponseStatus' - -- | The response status code.
listDomainsForPackageResponse ::
  -- | 'ldfprsResponseStatus'
  Int ->
  ListDomainsForPackageResponse
listDomainsForPackageResponse pResponseStatus_ =
  ListDomainsForPackageResponse'
    { _ldfprsDomainPackageDetailsList =
        Nothing,
      _ldfprsNextToken = Nothing,
      _ldfprsResponseStatus = pResponseStatus_
    }

-- | List of @DomainPackageDetails@ objects.
ldfprsDomainPackageDetailsList :: Lens' ListDomainsForPackageResponse [DomainPackageDetails]
ldfprsDomainPackageDetailsList = lens _ldfprsDomainPackageDetailsList (\s a -> s {_ldfprsDomainPackageDetailsList = a}) . _Default . _Coerce

-- | Undocumented member.
ldfprsNextToken :: Lens' ListDomainsForPackageResponse (Maybe Text)
ldfprsNextToken = lens _ldfprsNextToken (\s a -> s {_ldfprsNextToken = a})

-- | -- | The response status code.
ldfprsResponseStatus :: Lens' ListDomainsForPackageResponse Int
ldfprsResponseStatus = lens _ldfprsResponseStatus (\s a -> s {_ldfprsResponseStatus = a})

instance NFData ListDomainsForPackageResponse
