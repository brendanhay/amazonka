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
-- Module      : Network.AWS.Budgets.DescribeNotificationsForBudget
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the notifications associated with a budget.
--
--
module Network.AWS.Budgets.DescribeNotificationsForBudget
    (
    -- * Creating a Request
      describeNotificationsForBudget
    , DescribeNotificationsForBudget
    -- * Request Lenses
    , dnfbNextToken
    , dnfbMaxResults
    , dnfbAccountId
    , dnfbBudgetName

    -- * Destructuring the Response
    , describeNotificationsForBudgetResponse
    , DescribeNotificationsForBudgetResponse
    -- * Response Lenses
    , dnfbrsNextToken
    , dnfbrsNotifications
    , dnfbrsResponseStatus
    ) where

import Network.AWS.Budgets.Types
import Network.AWS.Budgets.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request of DescribeNotificationsForBudget
--
--
--
-- /See:/ 'describeNotificationsForBudget' smart constructor.
data DescribeNotificationsForBudget = DescribeNotificationsForBudget'
  { _dnfbNextToken  :: !(Maybe Text)
  , _dnfbMaxResults :: !(Maybe Nat)
  , _dnfbAccountId  :: !Text
  , _dnfbBudgetName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNotificationsForBudget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnfbNextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- * 'dnfbMaxResults' - Optional integer. Specifies the maximum number of results to return in response.
--
-- * 'dnfbAccountId' - The @accountId@ that is associated with the budget whose notifications you want descriptions of.
--
-- * 'dnfbBudgetName' - The name of the budget whose notifications you want descriptions of.
describeNotificationsForBudget
    :: Text -- ^ 'dnfbAccountId'
    -> Text -- ^ 'dnfbBudgetName'
    -> DescribeNotificationsForBudget
describeNotificationsForBudget pAccountId_ pBudgetName_ =
  DescribeNotificationsForBudget'
    { _dnfbNextToken = Nothing
    , _dnfbMaxResults = Nothing
    , _dnfbAccountId = pAccountId_
    , _dnfbBudgetName = pBudgetName_
    }


-- | The pagination token that indicates the next set of results to retrieve.
dnfbNextToken :: Lens' DescribeNotificationsForBudget (Maybe Text)
dnfbNextToken = lens _dnfbNextToken (\ s a -> s{_dnfbNextToken = a})

-- | Optional integer. Specifies the maximum number of results to return in response.
dnfbMaxResults :: Lens' DescribeNotificationsForBudget (Maybe Natural)
dnfbMaxResults = lens _dnfbMaxResults (\ s a -> s{_dnfbMaxResults = a}) . mapping _Nat

-- | The @accountId@ that is associated with the budget whose notifications you want descriptions of.
dnfbAccountId :: Lens' DescribeNotificationsForBudget Text
dnfbAccountId = lens _dnfbAccountId (\ s a -> s{_dnfbAccountId = a})

-- | The name of the budget whose notifications you want descriptions of.
dnfbBudgetName :: Lens' DescribeNotificationsForBudget Text
dnfbBudgetName = lens _dnfbBudgetName (\ s a -> s{_dnfbBudgetName = a})

instance AWSRequest DescribeNotificationsForBudget
         where
        type Rs DescribeNotificationsForBudget =
             DescribeNotificationsForBudgetResponse
        request = postJSON budgets
        response
          = receiveJSON
              (\ s h x ->
                 DescribeNotificationsForBudgetResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Notifications" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeNotificationsForBudget
         where

instance NFData DescribeNotificationsForBudget where

instance ToHeaders DescribeNotificationsForBudget
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSBudgetServiceGateway.DescribeNotificationsForBudget"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeNotificationsForBudget where
        toJSON DescribeNotificationsForBudget'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dnfbNextToken,
                  ("MaxResults" .=) <$> _dnfbMaxResults,
                  Just ("AccountId" .= _dnfbAccountId),
                  Just ("BudgetName" .= _dnfbBudgetName)])

instance ToPath DescribeNotificationsForBudget where
        toPath = const "/"

instance ToQuery DescribeNotificationsForBudget where
        toQuery = const mempty

-- | Response of GetNotificationsForBudget
--
--
--
-- /See:/ 'describeNotificationsForBudgetResponse' smart constructor.
data DescribeNotificationsForBudgetResponse = DescribeNotificationsForBudgetResponse'
  { _dnfbrsNextToken      :: !(Maybe Text)
  , _dnfbrsNotifications  :: !(Maybe [Notification])
  , _dnfbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNotificationsForBudgetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnfbrsNextToken' - The pagination token that indicates the next set of results that you can retrieve.
--
-- * 'dnfbrsNotifications' - A list of notifications associated with a budget.
--
-- * 'dnfbrsResponseStatus' - -- | The response status code.
describeNotificationsForBudgetResponse
    :: Int -- ^ 'dnfbrsResponseStatus'
    -> DescribeNotificationsForBudgetResponse
describeNotificationsForBudgetResponse pResponseStatus_ =
  DescribeNotificationsForBudgetResponse'
    { _dnfbrsNextToken = Nothing
    , _dnfbrsNotifications = Nothing
    , _dnfbrsResponseStatus = pResponseStatus_
    }


-- | The pagination token that indicates the next set of results that you can retrieve.
dnfbrsNextToken :: Lens' DescribeNotificationsForBudgetResponse (Maybe Text)
dnfbrsNextToken = lens _dnfbrsNextToken (\ s a -> s{_dnfbrsNextToken = a})

-- | A list of notifications associated with a budget.
dnfbrsNotifications :: Lens' DescribeNotificationsForBudgetResponse [Notification]
dnfbrsNotifications = lens _dnfbrsNotifications (\ s a -> s{_dnfbrsNotifications = a}) . _Default . _Coerce

-- | -- | The response status code.
dnfbrsResponseStatus :: Lens' DescribeNotificationsForBudgetResponse Int
dnfbrsResponseStatus = lens _dnfbrsResponseStatus (\ s a -> s{_dnfbrsResponseStatus = a})

instance NFData
           DescribeNotificationsForBudgetResponse
         where
