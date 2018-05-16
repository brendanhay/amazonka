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
-- Module      : Network.AWS.AlexaBusiness.SearchAddressBooks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches address books and lists the ones that meet a set of filter and sort criteria.
--
--
module Network.AWS.AlexaBusiness.SearchAddressBooks
    (
    -- * Creating a Request
      searchAddressBooks
    , SearchAddressBooks
    -- * Request Lenses
    , sabFilters
    , sabSortCriteria
    , sabNextToken
    , sabMaxResults

    -- * Destructuring the Response
    , searchAddressBooksResponse
    , SearchAddressBooksResponse
    -- * Response Lenses
    , sabrsNextToken
    , sabrsAddressBooks
    , sabrsTotalCount
    , sabrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'searchAddressBooks' smart constructor.
data SearchAddressBooks = SearchAddressBooks'
  { _sabFilters      :: !(Maybe [Filter])
  , _sabSortCriteria :: !(Maybe [Sort])
  , _sabNextToken    :: !(Maybe Text)
  , _sabMaxResults   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchAddressBooks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sabFilters' - The filters to use to list a specified set of address books. The supported filter key is AddressBookName.
--
-- * 'sabSortCriteria' - The sort order to use in listing the specified set of address books. The supported sort key is AddressBookName.
--
-- * 'sabNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults.
--
-- * 'sabMaxResults' - The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
searchAddressBooks
    :: SearchAddressBooks
searchAddressBooks =
  SearchAddressBooks'
    { _sabFilters = Nothing
    , _sabSortCriteria = Nothing
    , _sabNextToken = Nothing
    , _sabMaxResults = Nothing
    }


-- | The filters to use to list a specified set of address books. The supported filter key is AddressBookName.
sabFilters :: Lens' SearchAddressBooks [Filter]
sabFilters = lens _sabFilters (\ s a -> s{_sabFilters = a}) . _Default . _Coerce

-- | The sort order to use in listing the specified set of address books. The supported sort key is AddressBookName.
sabSortCriteria :: Lens' SearchAddressBooks [Sort]
sabSortCriteria = lens _sabSortCriteria (\ s a -> s{_sabSortCriteria = a}) . _Default . _Coerce

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults.
sabNextToken :: Lens' SearchAddressBooks (Maybe Text)
sabNextToken = lens _sabNextToken (\ s a -> s{_sabNextToken = a})

-- | The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
sabMaxResults :: Lens' SearchAddressBooks (Maybe Natural)
sabMaxResults = lens _sabMaxResults (\ s a -> s{_sabMaxResults = a}) . mapping _Nat

instance AWSRequest SearchAddressBooks where
        type Rs SearchAddressBooks =
             SearchAddressBooksResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 SearchAddressBooksResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "AddressBooks" .!@ mempty)
                     <*> (x .?> "TotalCount")
                     <*> (pure (fromEnum s)))

instance Hashable SearchAddressBooks where

instance NFData SearchAddressBooks where

instance ToHeaders SearchAddressBooks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.SearchAddressBooks" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SearchAddressBooks where
        toJSON SearchAddressBooks'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _sabFilters,
                  ("SortCriteria" .=) <$> _sabSortCriteria,
                  ("NextToken" .=) <$> _sabNextToken,
                  ("MaxResults" .=) <$> _sabMaxResults])

instance ToPath SearchAddressBooks where
        toPath = const "/"

instance ToQuery SearchAddressBooks where
        toQuery = const mempty

-- | /See:/ 'searchAddressBooksResponse' smart constructor.
data SearchAddressBooksResponse = SearchAddressBooksResponse'
  { _sabrsNextToken      :: !(Maybe Text)
  , _sabrsAddressBooks   :: !(Maybe [AddressBookData])
  , _sabrsTotalCount     :: !(Maybe Int)
  , _sabrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchAddressBooksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sabrsNextToken' - The token returned to indicate that there is more data available.
--
-- * 'sabrsAddressBooks' - The address books that meet the specified set of filter criteria, in sort order.
--
-- * 'sabrsTotalCount' - The total number of address books returned.
--
-- * 'sabrsResponseStatus' - -- | The response status code.
searchAddressBooksResponse
    :: Int -- ^ 'sabrsResponseStatus'
    -> SearchAddressBooksResponse
searchAddressBooksResponse pResponseStatus_ =
  SearchAddressBooksResponse'
    { _sabrsNextToken = Nothing
    , _sabrsAddressBooks = Nothing
    , _sabrsTotalCount = Nothing
    , _sabrsResponseStatus = pResponseStatus_
    }


-- | The token returned to indicate that there is more data available.
sabrsNextToken :: Lens' SearchAddressBooksResponse (Maybe Text)
sabrsNextToken = lens _sabrsNextToken (\ s a -> s{_sabrsNextToken = a})

-- | The address books that meet the specified set of filter criteria, in sort order.
sabrsAddressBooks :: Lens' SearchAddressBooksResponse [AddressBookData]
sabrsAddressBooks = lens _sabrsAddressBooks (\ s a -> s{_sabrsAddressBooks = a}) . _Default . _Coerce

-- | The total number of address books returned.
sabrsTotalCount :: Lens' SearchAddressBooksResponse (Maybe Int)
sabrsTotalCount = lens _sabrsTotalCount (\ s a -> s{_sabrsTotalCount = a})

-- | -- | The response status code.
sabrsResponseStatus :: Lens' SearchAddressBooksResponse Int
sabrsResponseStatus = lens _sabrsResponseStatus (\ s a -> s{_sabrsResponseStatus = a})

instance NFData SearchAddressBooksResponse where
