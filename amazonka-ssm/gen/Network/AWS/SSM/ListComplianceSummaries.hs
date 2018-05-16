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
-- Module      : Network.AWS.SSM.ListComplianceSummaries
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a summary count of compliant and non-compliant resources for a compliance type. For example, this call can return State Manager associations, patches, or custom compliance types according to the filter criteria that you specify.
--
--
module Network.AWS.SSM.ListComplianceSummaries
    (
    -- * Creating a Request
      listComplianceSummaries
    , ListComplianceSummaries
    -- * Request Lenses
    , lcsFilters
    , lcsNextToken
    , lcsMaxResults

    -- * Destructuring the Response
    , listComplianceSummariesResponse
    , ListComplianceSummariesResponse
    -- * Response Lenses
    , lcsrsNextToken
    , lcsrsComplianceSummaryItems
    , lcsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'listComplianceSummaries' smart constructor.
data ListComplianceSummaries = ListComplianceSummaries'
  { _lcsFilters    :: !(Maybe [ComplianceStringFilter])
  , _lcsNextToken  :: !(Maybe Text)
  , _lcsMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListComplianceSummaries' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcsFilters' - One or more compliance or inventory filters. Use a filter to return a more specific list of results.
--
-- * 'lcsNextToken' - A token to start the list. Use this token to get the next set of results.
--
-- * 'lcsMaxResults' - The maximum number of items to return for this call. Currently, you can specify null or 50. The call also returns a token that you can specify in a subsequent call to get the next set of results.
listComplianceSummaries
    :: ListComplianceSummaries
listComplianceSummaries =
  ListComplianceSummaries'
    {_lcsFilters = Nothing, _lcsNextToken = Nothing, _lcsMaxResults = Nothing}


-- | One or more compliance or inventory filters. Use a filter to return a more specific list of results.
lcsFilters :: Lens' ListComplianceSummaries [ComplianceStringFilter]
lcsFilters = lens _lcsFilters (\ s a -> s{_lcsFilters = a}) . _Default . _Coerce

-- | A token to start the list. Use this token to get the next set of results.
lcsNextToken :: Lens' ListComplianceSummaries (Maybe Text)
lcsNextToken = lens _lcsNextToken (\ s a -> s{_lcsNextToken = a})

-- | The maximum number of items to return for this call. Currently, you can specify null or 50. The call also returns a token that you can specify in a subsequent call to get the next set of results.
lcsMaxResults :: Lens' ListComplianceSummaries (Maybe Natural)
lcsMaxResults = lens _lcsMaxResults (\ s a -> s{_lcsMaxResults = a}) . mapping _Nat

instance AWSRequest ListComplianceSummaries where
        type Rs ListComplianceSummaries =
             ListComplianceSummariesResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 ListComplianceSummariesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "ComplianceSummaryItems" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListComplianceSummaries where

instance NFData ListComplianceSummaries where

instance ToHeaders ListComplianceSummaries where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.ListComplianceSummaries" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListComplianceSummaries where
        toJSON ListComplianceSummaries'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _lcsFilters,
                  ("NextToken" .=) <$> _lcsNextToken,
                  ("MaxResults" .=) <$> _lcsMaxResults])

instance ToPath ListComplianceSummaries where
        toPath = const "/"

instance ToQuery ListComplianceSummaries where
        toQuery = const mempty

-- | /See:/ 'listComplianceSummariesResponse' smart constructor.
data ListComplianceSummariesResponse = ListComplianceSummariesResponse'
  { _lcsrsNextToken              :: !(Maybe Text)
  , _lcsrsComplianceSummaryItems :: !(Maybe [ComplianceSummaryItem])
  , _lcsrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListComplianceSummariesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcsrsNextToken' - The token for the next set of items to return. Use this token to get the next set of results.
--
-- * 'lcsrsComplianceSummaryItems' - A list of compliant and non-compliant summary counts based on compliance types. For example, this call returns State Manager associations, patches, or custom compliance types according to the filter criteria that you specified.
--
-- * 'lcsrsResponseStatus' - -- | The response status code.
listComplianceSummariesResponse
    :: Int -- ^ 'lcsrsResponseStatus'
    -> ListComplianceSummariesResponse
listComplianceSummariesResponse pResponseStatus_ =
  ListComplianceSummariesResponse'
    { _lcsrsNextToken = Nothing
    , _lcsrsComplianceSummaryItems = Nothing
    , _lcsrsResponseStatus = pResponseStatus_
    }


-- | The token for the next set of items to return. Use this token to get the next set of results.
lcsrsNextToken :: Lens' ListComplianceSummariesResponse (Maybe Text)
lcsrsNextToken = lens _lcsrsNextToken (\ s a -> s{_lcsrsNextToken = a})

-- | A list of compliant and non-compliant summary counts based on compliance types. For example, this call returns State Manager associations, patches, or custom compliance types according to the filter criteria that you specified.
lcsrsComplianceSummaryItems :: Lens' ListComplianceSummariesResponse [ComplianceSummaryItem]
lcsrsComplianceSummaryItems = lens _lcsrsComplianceSummaryItems (\ s a -> s{_lcsrsComplianceSummaryItems = a}) . _Default . _Coerce

-- | -- | The response status code.
lcsrsResponseStatus :: Lens' ListComplianceSummariesResponse Int
lcsrsResponseStatus = lens _lcsrsResponseStatus (\ s a -> s{_lcsrsResponseStatus = a})

instance NFData ListComplianceSummariesResponse where
