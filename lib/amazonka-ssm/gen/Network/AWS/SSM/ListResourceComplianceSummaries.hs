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
-- Module      : Network.AWS.SSM.ListResourceComplianceSummaries
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a resource-level summary count. The summary includes information about compliant and non-compliant statuses and detailed compliance-item severity counts, according to the filter criteria you specify.
--
--
module Network.AWS.SSM.ListResourceComplianceSummaries
    (
    -- * Creating a Request
      listResourceComplianceSummaries
    , ListResourceComplianceSummaries
    -- * Request Lenses
    , lrcsFilters
    , lrcsNextToken
    , lrcsMaxResults

    -- * Destructuring the Response
    , listResourceComplianceSummariesResponse
    , ListResourceComplianceSummariesResponse
    -- * Response Lenses
    , lrcsrsResourceComplianceSummaryItems
    , lrcsrsNextToken
    , lrcsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'listResourceComplianceSummaries' smart constructor.
data ListResourceComplianceSummaries = ListResourceComplianceSummaries'
  { _lrcsFilters    :: !(Maybe [ComplianceStringFilter])
  , _lrcsNextToken  :: !(Maybe Text)
  , _lrcsMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourceComplianceSummaries' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrcsFilters' - One or more filters. Use a filter to return a more specific list of results.
--
-- * 'lrcsNextToken' - A token to start the list. Use this token to get the next set of results.
--
-- * 'lrcsMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
listResourceComplianceSummaries
    :: ListResourceComplianceSummaries
listResourceComplianceSummaries =
  ListResourceComplianceSummaries'
    { _lrcsFilters = Nothing
    , _lrcsNextToken = Nothing
    , _lrcsMaxResults = Nothing
    }


-- | One or more filters. Use a filter to return a more specific list of results.
lrcsFilters :: Lens' ListResourceComplianceSummaries [ComplianceStringFilter]
lrcsFilters = lens _lrcsFilters (\ s a -> s{_lrcsFilters = a}) . _Default . _Coerce

-- | A token to start the list. Use this token to get the next set of results.
lrcsNextToken :: Lens' ListResourceComplianceSummaries (Maybe Text)
lrcsNextToken = lens _lrcsNextToken (\ s a -> s{_lrcsNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
lrcsMaxResults :: Lens' ListResourceComplianceSummaries (Maybe Natural)
lrcsMaxResults = lens _lrcsMaxResults (\ s a -> s{_lrcsMaxResults = a}) . mapping _Nat

instance AWSRequest ListResourceComplianceSummaries
         where
        type Rs ListResourceComplianceSummaries =
             ListResourceComplianceSummariesResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 ListResourceComplianceSummariesResponse' <$>
                   (x .?> "ResourceComplianceSummaryItems" .!@ mempty)
                     <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListResourceComplianceSummaries
         where

instance NFData ListResourceComplianceSummaries where

instance ToHeaders ListResourceComplianceSummaries
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.ListResourceComplianceSummaries" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListResourceComplianceSummaries where
        toJSON ListResourceComplianceSummaries'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _lrcsFilters,
                  ("NextToken" .=) <$> _lrcsNextToken,
                  ("MaxResults" .=) <$> _lrcsMaxResults])

instance ToPath ListResourceComplianceSummaries where
        toPath = const "/"

instance ToQuery ListResourceComplianceSummaries
         where
        toQuery = const mempty

-- | /See:/ 'listResourceComplianceSummariesResponse' smart constructor.
data ListResourceComplianceSummariesResponse = ListResourceComplianceSummariesResponse'
  { _lrcsrsResourceComplianceSummaryItems :: !(Maybe [ResourceComplianceSummaryItem])
  , _lrcsrsNextToken :: !(Maybe Text)
  , _lrcsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourceComplianceSummariesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrcsrsResourceComplianceSummaryItems' - A summary count for specified or targeted managed instances. Summary count includes information about compliant and non-compliant State Manager associations, patch status, or custom items according to the filter criteria that you specify.
--
-- * 'lrcsrsNextToken' - The token for the next set of items to return. Use this token to get the next set of results.
--
-- * 'lrcsrsResponseStatus' - -- | The response status code.
listResourceComplianceSummariesResponse
    :: Int -- ^ 'lrcsrsResponseStatus'
    -> ListResourceComplianceSummariesResponse
listResourceComplianceSummariesResponse pResponseStatus_ =
  ListResourceComplianceSummariesResponse'
    { _lrcsrsResourceComplianceSummaryItems = Nothing
    , _lrcsrsNextToken = Nothing
    , _lrcsrsResponseStatus = pResponseStatus_
    }


-- | A summary count for specified or targeted managed instances. Summary count includes information about compliant and non-compliant State Manager associations, patch status, or custom items according to the filter criteria that you specify.
lrcsrsResourceComplianceSummaryItems :: Lens' ListResourceComplianceSummariesResponse [ResourceComplianceSummaryItem]
lrcsrsResourceComplianceSummaryItems = lens _lrcsrsResourceComplianceSummaryItems (\ s a -> s{_lrcsrsResourceComplianceSummaryItems = a}) . _Default . _Coerce

-- | The token for the next set of items to return. Use this token to get the next set of results.
lrcsrsNextToken :: Lens' ListResourceComplianceSummariesResponse (Maybe Text)
lrcsrsNextToken = lens _lrcsrsNextToken (\ s a -> s{_lrcsrsNextToken = a})

-- | -- | The response status code.
lrcsrsResponseStatus :: Lens' ListResourceComplianceSummariesResponse Int
lrcsrsResponseStatus = lens _lrcsrsResponseStatus (\ s a -> s{_lrcsrsResponseStatus = a})

instance NFData
           ListResourceComplianceSummariesResponse
         where
