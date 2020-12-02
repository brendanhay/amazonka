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
-- Module      : Network.AWS.SSM.ListComplianceItems
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a specified resource ID, this API action returns a list of compliance statuses for different resource types. Currently, you can only specify one resource ID per call. List results depend on the criteria specified in the filter.
--
--
module Network.AWS.SSM.ListComplianceItems
    (
    -- * Creating a Request
      listComplianceItems
    , ListComplianceItems
    -- * Request Lenses
    , lResourceIds
    , lFilters
    , lNextToken
    , lMaxResults
    , lResourceTypes

    -- * Destructuring the Response
    , listComplianceItemsResponse
    , ListComplianceItemsResponse
    -- * Response Lenses
    , lcirsComplianceItems
    , lcirsNextToken
    , lcirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'listComplianceItems' smart constructor.
data ListComplianceItems = ListComplianceItems'
  { _lResourceIds   :: !(Maybe (List1 Text))
  , _lFilters       :: !(Maybe [ComplianceStringFilter])
  , _lNextToken     :: !(Maybe Text)
  , _lMaxResults    :: !(Maybe Nat)
  , _lResourceTypes :: !(Maybe (List1 Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListComplianceItems' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lResourceIds' - The ID for the resources from which to get compliance information. Currently, you can only specify one resource ID.
--
-- * 'lFilters' - One or more compliance filters. Use a filter to return a more specific list of results.
--
-- * 'lNextToken' - A token to start the list. Use this token to get the next set of results.
--
-- * 'lMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'lResourceTypes' - The type of resource from which to get compliance information. Currently, the only supported resource type is @ManagedInstance@ .
listComplianceItems
    :: ListComplianceItems
listComplianceItems =
  ListComplianceItems'
    { _lResourceIds = Nothing
    , _lFilters = Nothing
    , _lNextToken = Nothing
    , _lMaxResults = Nothing
    , _lResourceTypes = Nothing
    }


-- | The ID for the resources from which to get compliance information. Currently, you can only specify one resource ID.
lResourceIds :: Lens' ListComplianceItems (Maybe (NonEmpty Text))
lResourceIds = lens _lResourceIds (\ s a -> s{_lResourceIds = a}) . mapping _List1

-- | One or more compliance filters. Use a filter to return a more specific list of results.
lFilters :: Lens' ListComplianceItems [ComplianceStringFilter]
lFilters = lens _lFilters (\ s a -> s{_lFilters = a}) . _Default . _Coerce

-- | A token to start the list. Use this token to get the next set of results.
lNextToken :: Lens' ListComplianceItems (Maybe Text)
lNextToken = lens _lNextToken (\ s a -> s{_lNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
lMaxResults :: Lens' ListComplianceItems (Maybe Natural)
lMaxResults = lens _lMaxResults (\ s a -> s{_lMaxResults = a}) . mapping _Nat

-- | The type of resource from which to get compliance information. Currently, the only supported resource type is @ManagedInstance@ .
lResourceTypes :: Lens' ListComplianceItems (Maybe (NonEmpty Text))
lResourceTypes = lens _lResourceTypes (\ s a -> s{_lResourceTypes = a}) . mapping _List1

instance AWSRequest ListComplianceItems where
        type Rs ListComplianceItems =
             ListComplianceItemsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 ListComplianceItemsResponse' <$>
                   (x .?> "ComplianceItems" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListComplianceItems where

instance NFData ListComplianceItems where

instance ToHeaders ListComplianceItems where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.ListComplianceItems" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListComplianceItems where
        toJSON ListComplianceItems'{..}
          = object
              (catMaybes
                 [("ResourceIds" .=) <$> _lResourceIds,
                  ("Filters" .=) <$> _lFilters,
                  ("NextToken" .=) <$> _lNextToken,
                  ("MaxResults" .=) <$> _lMaxResults,
                  ("ResourceTypes" .=) <$> _lResourceTypes])

instance ToPath ListComplianceItems where
        toPath = const "/"

instance ToQuery ListComplianceItems where
        toQuery = const mempty

-- | /See:/ 'listComplianceItemsResponse' smart constructor.
data ListComplianceItemsResponse = ListComplianceItemsResponse'
  { _lcirsComplianceItems :: !(Maybe [ComplianceItem])
  , _lcirsNextToken       :: !(Maybe Text)
  , _lcirsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListComplianceItemsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcirsComplianceItems' - A list of compliance information for the specified resource ID.
--
-- * 'lcirsNextToken' - The token for the next set of items to return. Use this token to get the next set of results.
--
-- * 'lcirsResponseStatus' - -- | The response status code.
listComplianceItemsResponse
    :: Int -- ^ 'lcirsResponseStatus'
    -> ListComplianceItemsResponse
listComplianceItemsResponse pResponseStatus_ =
  ListComplianceItemsResponse'
    { _lcirsComplianceItems = Nothing
    , _lcirsNextToken = Nothing
    , _lcirsResponseStatus = pResponseStatus_
    }


-- | A list of compliance information for the specified resource ID.
lcirsComplianceItems :: Lens' ListComplianceItemsResponse [ComplianceItem]
lcirsComplianceItems = lens _lcirsComplianceItems (\ s a -> s{_lcirsComplianceItems = a}) . _Default . _Coerce

-- | The token for the next set of items to return. Use this token to get the next set of results.
lcirsNextToken :: Lens' ListComplianceItemsResponse (Maybe Text)
lcirsNextToken = lens _lcirsNextToken (\ s a -> s{_lcirsNextToken = a})

-- | -- | The response status code.
lcirsResponseStatus :: Lens' ListComplianceItemsResponse Int
lcirsResponseStatus = lens _lcirsResponseStatus (\ s a -> s{_lcirsResponseStatus = a})

instance NFData ListComplianceItemsResponse where
