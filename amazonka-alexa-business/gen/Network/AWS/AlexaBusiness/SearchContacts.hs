{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SearchContacts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches contacts and lists the ones that meet a set of filter and sort criteria.
--
--
module Network.AWS.AlexaBusiness.SearchContacts
    (
    -- * Creating a Request
      searchContacts
    , SearchContacts
    -- * Request Lenses
    , scFilters
    , scSortCriteria
    , scNextToken
    , scMaxResults

    -- * Destructuring the Response
    , searchContactsResponse
    , SearchContactsResponse
    -- * Response Lenses
    , scrsNextToken
    , scrsContacts
    , scrsTotalCount
    , scrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'searchContacts' smart constructor.
data SearchContacts = SearchContacts'
  { _scFilters      :: !(Maybe [Filter])
  , _scSortCriteria :: !(Maybe [Sort])
  , _scNextToken    :: !(Maybe Text)
  , _scMaxResults   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchContacts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scFilters' - The filters to use to list a specified set of address books. The supported filter keys are DisplayName, FirstName, LastName, and AddressBookArns.
--
-- * 'scSortCriteria' - The sort order to use in listing the specified set of contacts. The supported sort keys are DisplayName, FirstName, and LastName.
--
-- * 'scNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults.
--
-- * 'scMaxResults' - The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
searchContacts
    :: SearchContacts
searchContacts =
  SearchContacts'
    { _scFilters = Nothing
    , _scSortCriteria = Nothing
    , _scNextToken = Nothing
    , _scMaxResults = Nothing
    }


-- | The filters to use to list a specified set of address books. The supported filter keys are DisplayName, FirstName, LastName, and AddressBookArns.
scFilters :: Lens' SearchContacts [Filter]
scFilters = lens _scFilters (\ s a -> s{_scFilters = a}) . _Default . _Coerce

-- | The sort order to use in listing the specified set of contacts. The supported sort keys are DisplayName, FirstName, and LastName.
scSortCriteria :: Lens' SearchContacts [Sort]
scSortCriteria = lens _scSortCriteria (\ s a -> s{_scSortCriteria = a}) . _Default . _Coerce

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults.
scNextToken :: Lens' SearchContacts (Maybe Text)
scNextToken = lens _scNextToken (\ s a -> s{_scNextToken = a})

-- | The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
scMaxResults :: Lens' SearchContacts (Maybe Natural)
scMaxResults = lens _scMaxResults (\ s a -> s{_scMaxResults = a}) . mapping _Nat

instance AWSRequest SearchContacts where
        type Rs SearchContacts = SearchContactsResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 SearchContactsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Contacts" .!@ mempty)
                     <*> (x .?> "TotalCount")
                     <*> (pure (fromEnum s)))

instance Hashable SearchContacts where

instance NFData SearchContacts where

instance ToHeaders SearchContacts where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.SearchContacts" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SearchContacts where
        toJSON SearchContacts'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _scFilters,
                  ("SortCriteria" .=) <$> _scSortCriteria,
                  ("NextToken" .=) <$> _scNextToken,
                  ("MaxResults" .=) <$> _scMaxResults])

instance ToPath SearchContacts where
        toPath = const "/"

instance ToQuery SearchContacts where
        toQuery = const mempty

-- | /See:/ 'searchContactsResponse' smart constructor.
data SearchContactsResponse = SearchContactsResponse'
  { _scrsNextToken      :: !(Maybe Text)
  , _scrsContacts       :: !(Maybe [ContactData])
  , _scrsTotalCount     :: !(Maybe Int)
  , _scrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchContactsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scrsNextToken' - The token returned to indicate that there is more data available.
--
-- * 'scrsContacts' - The contacts that meet the specified set of filter criteria, in sort order.
--
-- * 'scrsTotalCount' - The total number of contacts returned.
--
-- * 'scrsResponseStatus' - -- | The response status code.
searchContactsResponse
    :: Int -- ^ 'scrsResponseStatus'
    -> SearchContactsResponse
searchContactsResponse pResponseStatus_ =
  SearchContactsResponse'
    { _scrsNextToken = Nothing
    , _scrsContacts = Nothing
    , _scrsTotalCount = Nothing
    , _scrsResponseStatus = pResponseStatus_
    }


-- | The token returned to indicate that there is more data available.
scrsNextToken :: Lens' SearchContactsResponse (Maybe Text)
scrsNextToken = lens _scrsNextToken (\ s a -> s{_scrsNextToken = a})

-- | The contacts that meet the specified set of filter criteria, in sort order.
scrsContacts :: Lens' SearchContactsResponse [ContactData]
scrsContacts = lens _scrsContacts (\ s a -> s{_scrsContacts = a}) . _Default . _Coerce

-- | The total number of contacts returned.
scrsTotalCount :: Lens' SearchContactsResponse (Maybe Int)
scrsTotalCount = lens _scrsTotalCount (\ s a -> s{_scrsTotalCount = a})

-- | -- | The response status code.
scrsResponseStatus :: Lens' SearchContactsResponse Int
scrsResponseStatus = lens _scrsResponseStatus (\ s a -> s{_scrsResponseStatus = a})

instance NFData SearchContactsResponse where
