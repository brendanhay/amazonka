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
-- Module      : Network.AWS.Budgets.DescribeBudgetPerformanceHistory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the history for @DAILY@ , @MONTHLY@ , and @QUARTERLY@ budgets. Budget history isn't available for @ANNUAL@ budgets.
--
--
module Network.AWS.Budgets.DescribeBudgetPerformanceHistory
    (
    -- * Creating a Request
      describeBudgetPerformanceHistory
    , DescribeBudgetPerformanceHistory
    -- * Request Lenses
    , dbphTimePeriod
    , dbphNextToken
    , dbphMaxResults
    , dbphAccountId
    , dbphBudgetName

    -- * Destructuring the Response
    , describeBudgetPerformanceHistoryResponse
    , DescribeBudgetPerformanceHistoryResponse
    -- * Response Lenses
    , dbphrsBudgetPerformanceHistory
    , dbphrsNextToken
    , dbphrsResponseStatus
    ) where

import Network.AWS.Budgets.Types
import Network.AWS.Budgets.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBudgetPerformanceHistory' smart constructor.
data DescribeBudgetPerformanceHistory = DescribeBudgetPerformanceHistory'
  { _dbphTimePeriod :: !(Maybe TimePeriod)
  , _dbphNextToken  :: !(Maybe Text)
  , _dbphMaxResults :: !(Maybe Nat)
  , _dbphAccountId  :: !Text
  , _dbphBudgetName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBudgetPerformanceHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbphTimePeriod' - Retrieves how often the budget went into an @ALARM@ state for the specified time period.
--
-- * 'dbphNextToken' - Undocumented member.
--
-- * 'dbphMaxResults' - Undocumented member.
--
-- * 'dbphAccountId' - Undocumented member.
--
-- * 'dbphBudgetName' - Undocumented member.
describeBudgetPerformanceHistory
    :: Text -- ^ 'dbphAccountId'
    -> Text -- ^ 'dbphBudgetName'
    -> DescribeBudgetPerformanceHistory
describeBudgetPerformanceHistory pAccountId_ pBudgetName_ =
  DescribeBudgetPerformanceHistory'
    { _dbphTimePeriod = Nothing
    , _dbphNextToken = Nothing
    , _dbphMaxResults = Nothing
    , _dbphAccountId = pAccountId_
    , _dbphBudgetName = pBudgetName_
    }


-- | Retrieves how often the budget went into an @ALARM@ state for the specified time period.
dbphTimePeriod :: Lens' DescribeBudgetPerformanceHistory (Maybe TimePeriod)
dbphTimePeriod = lens _dbphTimePeriod (\ s a -> s{_dbphTimePeriod = a})

-- | Undocumented member.
dbphNextToken :: Lens' DescribeBudgetPerformanceHistory (Maybe Text)
dbphNextToken = lens _dbphNextToken (\ s a -> s{_dbphNextToken = a})

-- | Undocumented member.
dbphMaxResults :: Lens' DescribeBudgetPerformanceHistory (Maybe Natural)
dbphMaxResults = lens _dbphMaxResults (\ s a -> s{_dbphMaxResults = a}) . mapping _Nat

-- | Undocumented member.
dbphAccountId :: Lens' DescribeBudgetPerformanceHistory Text
dbphAccountId = lens _dbphAccountId (\ s a -> s{_dbphAccountId = a})

-- | Undocumented member.
dbphBudgetName :: Lens' DescribeBudgetPerformanceHistory Text
dbphBudgetName = lens _dbphBudgetName (\ s a -> s{_dbphBudgetName = a})

instance AWSRequest DescribeBudgetPerformanceHistory
         where
        type Rs DescribeBudgetPerformanceHistory =
             DescribeBudgetPerformanceHistoryResponse
        request = postJSON budgets
        response
          = receiveJSON
              (\ s h x ->
                 DescribeBudgetPerformanceHistoryResponse' <$>
                   (x .?> "BudgetPerformanceHistory") <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeBudgetPerformanceHistory
         where

instance NFData DescribeBudgetPerformanceHistory
         where

instance ToHeaders DescribeBudgetPerformanceHistory
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSBudgetServiceGateway.DescribeBudgetPerformanceHistory"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeBudgetPerformanceHistory
         where
        toJSON DescribeBudgetPerformanceHistory'{..}
          = object
              (catMaybes
                 [("TimePeriod" .=) <$> _dbphTimePeriod,
                  ("NextToken" .=) <$> _dbphNextToken,
                  ("MaxResults" .=) <$> _dbphMaxResults,
                  Just ("AccountId" .= _dbphAccountId),
                  Just ("BudgetName" .= _dbphBudgetName)])

instance ToPath DescribeBudgetPerformanceHistory
         where
        toPath = const "/"

instance ToQuery DescribeBudgetPerformanceHistory
         where
        toQuery = const mempty

-- | /See:/ 'describeBudgetPerformanceHistoryResponse' smart constructor.
data DescribeBudgetPerformanceHistoryResponse = DescribeBudgetPerformanceHistoryResponse'
  { _dbphrsBudgetPerformanceHistory :: !(Maybe BudgetPerformanceHistory)
  , _dbphrsNextToken                :: !(Maybe Text)
  , _dbphrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBudgetPerformanceHistoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbphrsBudgetPerformanceHistory' - The history of how often the budget has gone into an @ALARM@ state. For @DAILY@ budgets, the history saves the state of the budget for the last 60 days. For @MONTHLY@ budgets, the history saves the state of the budget for the current month plus the last 12 months. For @QUARTERLY@ budgets, the history saves the state of the budget for the last four quarters.
--
-- * 'dbphrsNextToken' - Undocumented member.
--
-- * 'dbphrsResponseStatus' - -- | The response status code.
describeBudgetPerformanceHistoryResponse
    :: Int -- ^ 'dbphrsResponseStatus'
    -> DescribeBudgetPerformanceHistoryResponse
describeBudgetPerformanceHistoryResponse pResponseStatus_ =
  DescribeBudgetPerformanceHistoryResponse'
    { _dbphrsBudgetPerformanceHistory = Nothing
    , _dbphrsNextToken = Nothing
    , _dbphrsResponseStatus = pResponseStatus_
    }


-- | The history of how often the budget has gone into an @ALARM@ state. For @DAILY@ budgets, the history saves the state of the budget for the last 60 days. For @MONTHLY@ budgets, the history saves the state of the budget for the current month plus the last 12 months. For @QUARTERLY@ budgets, the history saves the state of the budget for the last four quarters.
dbphrsBudgetPerformanceHistory :: Lens' DescribeBudgetPerformanceHistoryResponse (Maybe BudgetPerformanceHistory)
dbphrsBudgetPerformanceHistory = lens _dbphrsBudgetPerformanceHistory (\ s a -> s{_dbphrsBudgetPerformanceHistory = a})

-- | Undocumented member.
dbphrsNextToken :: Lens' DescribeBudgetPerformanceHistoryResponse (Maybe Text)
dbphrsNextToken = lens _dbphrsNextToken (\ s a -> s{_dbphrsNextToken = a})

-- | -- | The response status code.
dbphrsResponseStatus :: Lens' DescribeBudgetPerformanceHistoryResponse Int
dbphrsResponseStatus = lens _dbphrsResponseStatus (\ s a -> s{_dbphrsResponseStatus = a})

instance NFData
           DescribeBudgetPerformanceHistoryResponse
         where
