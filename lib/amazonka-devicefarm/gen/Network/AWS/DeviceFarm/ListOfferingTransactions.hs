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
-- Module      : Network.AWS.DeviceFarm.ListOfferingTransactions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all historical purchases, renewals, and system renewal transactions for an AWS account. The list is paginated and ordered by a descending timestamp (most recent transactions are first). The API returns a @NotEligible@ error if the user is not permitted to invoke the operation. Please contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> if you believe that you should be able to invoke this operation.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListOfferingTransactions
    (
    -- * Creating a Request
      listOfferingTransactions
    , ListOfferingTransactions
    -- * Request Lenses
    , lotNextToken

    -- * Destructuring the Response
    , listOfferingTransactionsResponse
    , ListOfferingTransactionsResponse
    -- * Response Lenses
    , lotrsOfferingTransactions
    , lotrsNextToken
    , lotrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to list the offering transaction history.
--
--
--
-- /See:/ 'listOfferingTransactions' smart constructor.
newtype ListOfferingTransactions = ListOfferingTransactions'
  { _lotNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOfferingTransactions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lotNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
listOfferingTransactions
    :: ListOfferingTransactions
listOfferingTransactions = ListOfferingTransactions' {_lotNextToken = Nothing}


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lotNextToken :: Lens' ListOfferingTransactions (Maybe Text)
lotNextToken = lens _lotNextToken (\ s a -> s{_lotNextToken = a})

instance AWSPager ListOfferingTransactions where
        page rq rs
          | stop (rs ^. lotrsNextToken) = Nothing
          | stop (rs ^. lotrsOfferingTransactions) = Nothing
          | otherwise =
            Just $ rq & lotNextToken .~ rs ^. lotrsNextToken

instance AWSRequest ListOfferingTransactions where
        type Rs ListOfferingTransactions =
             ListOfferingTransactionsResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 ListOfferingTransactionsResponse' <$>
                   (x .?> "offeringTransactions" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListOfferingTransactions where

instance NFData ListOfferingTransactions where

instance ToHeaders ListOfferingTransactions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListOfferingTransactions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListOfferingTransactions where
        toJSON ListOfferingTransactions'{..}
          = object
              (catMaybes [("nextToken" .=) <$> _lotNextToken])

instance ToPath ListOfferingTransactions where
        toPath = const "/"

instance ToQuery ListOfferingTransactions where
        toQuery = const mempty

-- | Returns the transaction log of the specified offerings.
--
--
--
-- /See:/ 'listOfferingTransactionsResponse' smart constructor.
data ListOfferingTransactionsResponse = ListOfferingTransactionsResponse'
  { _lotrsOfferingTransactions :: !(Maybe [OfferingTransaction])
  , _lotrsNextToken            :: !(Maybe Text)
  , _lotrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOfferingTransactionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lotrsOfferingTransactions' - The audit log of subscriptions you have purchased and modified through AWS Device Farm.
--
-- * 'lotrsNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lotrsResponseStatus' - -- | The response status code.
listOfferingTransactionsResponse
    :: Int -- ^ 'lotrsResponseStatus'
    -> ListOfferingTransactionsResponse
listOfferingTransactionsResponse pResponseStatus_ =
  ListOfferingTransactionsResponse'
    { _lotrsOfferingTransactions = Nothing
    , _lotrsNextToken = Nothing
    , _lotrsResponseStatus = pResponseStatus_
    }


-- | The audit log of subscriptions you have purchased and modified through AWS Device Farm.
lotrsOfferingTransactions :: Lens' ListOfferingTransactionsResponse [OfferingTransaction]
lotrsOfferingTransactions = lens _lotrsOfferingTransactions (\ s a -> s{_lotrsOfferingTransactions = a}) . _Default . _Coerce

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lotrsNextToken :: Lens' ListOfferingTransactionsResponse (Maybe Text)
lotrsNextToken = lens _lotrsNextToken (\ s a -> s{_lotrsNextToken = a})

-- | -- | The response status code.
lotrsResponseStatus :: Lens' ListOfferingTransactionsResponse Int
lotrsResponseStatus = lens _lotrsResponseStatus (\ s a -> s{_lotrsResponseStatus = a})

instance NFData ListOfferingTransactionsResponse
         where
