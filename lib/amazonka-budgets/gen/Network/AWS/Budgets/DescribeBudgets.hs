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
-- Module      : Network.AWS.Budgets.DescribeBudgets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the budgets associated with an account.
--
--
module Network.AWS.Budgets.DescribeBudgets
    (
    -- * Creating a Request
      describeBudgets
    , DescribeBudgets
    -- * Request Lenses
    , dbNextToken
    , dbMaxResults
    , dbAccountId

    -- * Destructuring the Response
    , describeBudgetsResponse
    , DescribeBudgetsResponse
    -- * Response Lenses
    , dbrsNextToken
    , dbrsBudgets
    , dbrsResponseStatus
    ) where

import Network.AWS.Budgets.Types
import Network.AWS.Budgets.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request of DescribeBudgets
--
--
--
-- /See:/ 'describeBudgets' smart constructor.
data DescribeBudgets = DescribeBudgets'
  { _dbNextToken  :: !(Maybe Text)
  , _dbMaxResults :: !(Maybe Nat)
  , _dbAccountId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBudgets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbNextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- * 'dbMaxResults' - Optional integer. Specifies the maximum number of results to return in response.
--
-- * 'dbAccountId' - The @accountId@ that is associated with the budgets that you want descriptions of.
describeBudgets
    :: Text -- ^ 'dbAccountId'
    -> DescribeBudgets
describeBudgets pAccountId_ =
  DescribeBudgets'
    { _dbNextToken = Nothing
    , _dbMaxResults = Nothing
    , _dbAccountId = pAccountId_
    }


-- | The pagination token that indicates the next set of results to retrieve.
dbNextToken :: Lens' DescribeBudgets (Maybe Text)
dbNextToken = lens _dbNextToken (\ s a -> s{_dbNextToken = a})

-- | Optional integer. Specifies the maximum number of results to return in response.
dbMaxResults :: Lens' DescribeBudgets (Maybe Natural)
dbMaxResults = lens _dbMaxResults (\ s a -> s{_dbMaxResults = a}) . mapping _Nat

-- | The @accountId@ that is associated with the budgets that you want descriptions of.
dbAccountId :: Lens' DescribeBudgets Text
dbAccountId = lens _dbAccountId (\ s a -> s{_dbAccountId = a})

instance AWSRequest DescribeBudgets where
        type Rs DescribeBudgets = DescribeBudgetsResponse
        request = postJSON budgets
        response
          = receiveJSON
              (\ s h x ->
                 DescribeBudgetsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Budgets" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeBudgets where

instance NFData DescribeBudgets where

instance ToHeaders DescribeBudgets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSBudgetServiceGateway.DescribeBudgets" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeBudgets where
        toJSON DescribeBudgets'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dbNextToken,
                  ("MaxResults" .=) <$> _dbMaxResults,
                  Just ("AccountId" .= _dbAccountId)])

instance ToPath DescribeBudgets where
        toPath = const "/"

instance ToQuery DescribeBudgets where
        toQuery = const mempty

-- | Response of DescribeBudgets
--
--
--
-- /See:/ 'describeBudgetsResponse' smart constructor.
data DescribeBudgetsResponse = DescribeBudgetsResponse'
  { _dbrsNextToken      :: !(Maybe Text)
  , _dbrsBudgets        :: !(Maybe [Budget])
  , _dbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBudgetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbrsNextToken' - The pagination token that indicates the next set of results that you can retrieve.
--
-- * 'dbrsBudgets' - A list of budgets.
--
-- * 'dbrsResponseStatus' - -- | The response status code.
describeBudgetsResponse
    :: Int -- ^ 'dbrsResponseStatus'
    -> DescribeBudgetsResponse
describeBudgetsResponse pResponseStatus_ =
  DescribeBudgetsResponse'
    { _dbrsNextToken = Nothing
    , _dbrsBudgets = Nothing
    , _dbrsResponseStatus = pResponseStatus_
    }


-- | The pagination token that indicates the next set of results that you can retrieve.
dbrsNextToken :: Lens' DescribeBudgetsResponse (Maybe Text)
dbrsNextToken = lens _dbrsNextToken (\ s a -> s{_dbrsNextToken = a})

-- | A list of budgets.
dbrsBudgets :: Lens' DescribeBudgetsResponse [Budget]
dbrsBudgets = lens _dbrsBudgets (\ s a -> s{_dbrsBudgets = a}) . _Default . _Coerce

-- | -- | The response status code.
dbrsResponseStatus :: Lens' DescribeBudgetsResponse Int
dbrsResponseStatus = lens _dbrsResponseStatus (\ s a -> s{_dbrsResponseStatus = a})

instance NFData DescribeBudgetsResponse where
