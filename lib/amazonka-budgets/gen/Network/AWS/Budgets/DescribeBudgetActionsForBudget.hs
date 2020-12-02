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
-- Module      : Network.AWS.Budgets.DescribeBudgetActionsForBudget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all of the budget actions for a budget.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeBudgetActionsForBudget
  ( -- * Creating a Request
    describeBudgetActionsForBudget,
    DescribeBudgetActionsForBudget,

    -- * Request Lenses
    dbafbNextToken,
    dbafbMaxResults,
    dbafbAccountId,
    dbafbBudgetName,

    -- * Destructuring the Response
    describeBudgetActionsForBudgetResponse,
    DescribeBudgetActionsForBudgetResponse,

    -- * Response Lenses
    dbafbrsNextToken,
    dbafbrsResponseStatus,
    dbafbrsActions,
  )
where

import Network.AWS.Budgets.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBudgetActionsForBudget' smart constructor.
data DescribeBudgetActionsForBudget = DescribeBudgetActionsForBudget'
  { _dbafbNextToken ::
      !(Maybe Text),
    _dbafbMaxResults ::
      !(Maybe Nat),
    _dbafbAccountId :: !Text,
    _dbafbBudgetName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeBudgetActionsForBudget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbafbNextToken' - Undocumented member.
--
-- * 'dbafbMaxResults' - Undocumented member.
--
-- * 'dbafbAccountId' - Undocumented member.
--
-- * 'dbafbBudgetName' - Undocumented member.
describeBudgetActionsForBudget ::
  -- | 'dbafbAccountId'
  Text ->
  -- | 'dbafbBudgetName'
  Text ->
  DescribeBudgetActionsForBudget
describeBudgetActionsForBudget pAccountId_ pBudgetName_ =
  DescribeBudgetActionsForBudget'
    { _dbafbNextToken = Nothing,
      _dbafbMaxResults = Nothing,
      _dbafbAccountId = pAccountId_,
      _dbafbBudgetName = pBudgetName_
    }

-- | Undocumented member.
dbafbNextToken :: Lens' DescribeBudgetActionsForBudget (Maybe Text)
dbafbNextToken = lens _dbafbNextToken (\s a -> s {_dbafbNextToken = a})

-- | Undocumented member.
dbafbMaxResults :: Lens' DescribeBudgetActionsForBudget (Maybe Natural)
dbafbMaxResults = lens _dbafbMaxResults (\s a -> s {_dbafbMaxResults = a}) . mapping _Nat

-- | Undocumented member.
dbafbAccountId :: Lens' DescribeBudgetActionsForBudget Text
dbafbAccountId = lens _dbafbAccountId (\s a -> s {_dbafbAccountId = a})

-- | Undocumented member.
dbafbBudgetName :: Lens' DescribeBudgetActionsForBudget Text
dbafbBudgetName = lens _dbafbBudgetName (\s a -> s {_dbafbBudgetName = a})

instance AWSPager DescribeBudgetActionsForBudget where
  page rq rs
    | stop (rs ^. dbafbrsNextToken) = Nothing
    | stop (rs ^. dbafbrsActions) = Nothing
    | otherwise = Just $ rq & dbafbNextToken .~ rs ^. dbafbrsNextToken

instance AWSRequest DescribeBudgetActionsForBudget where
  type
    Rs DescribeBudgetActionsForBudget =
      DescribeBudgetActionsForBudgetResponse
  request = postJSON budgets
  response =
    receiveJSON
      ( \s h x ->
          DescribeBudgetActionsForBudgetResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "Actions" .!@ mempty)
      )

instance Hashable DescribeBudgetActionsForBudget

instance NFData DescribeBudgetActionsForBudget

instance ToHeaders DescribeBudgetActionsForBudget where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSBudgetServiceGateway.DescribeBudgetActionsForBudget" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeBudgetActionsForBudget where
  toJSON DescribeBudgetActionsForBudget' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _dbafbNextToken,
            ("MaxResults" .=) <$> _dbafbMaxResults,
            Just ("AccountId" .= _dbafbAccountId),
            Just ("BudgetName" .= _dbafbBudgetName)
          ]
      )

instance ToPath DescribeBudgetActionsForBudget where
  toPath = const "/"

instance ToQuery DescribeBudgetActionsForBudget where
  toQuery = const mempty

-- | /See:/ 'describeBudgetActionsForBudgetResponse' smart constructor.
data DescribeBudgetActionsForBudgetResponse = DescribeBudgetActionsForBudgetResponse'
  { _dbafbrsNextToken ::
      !(Maybe Text),
    _dbafbrsResponseStatus ::
      !Int,
    _dbafbrsActions ::
      ![Action]
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeBudgetActionsForBudgetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbafbrsNextToken' - Undocumented member.
--
-- * 'dbafbrsResponseStatus' - -- | The response status code.
--
-- * 'dbafbrsActions' - A list of the budget action resources information.
describeBudgetActionsForBudgetResponse ::
  -- | 'dbafbrsResponseStatus'
  Int ->
  DescribeBudgetActionsForBudgetResponse
describeBudgetActionsForBudgetResponse pResponseStatus_ =
  DescribeBudgetActionsForBudgetResponse'
    { _dbafbrsNextToken =
        Nothing,
      _dbafbrsResponseStatus = pResponseStatus_,
      _dbafbrsActions = mempty
    }

-- | Undocumented member.
dbafbrsNextToken :: Lens' DescribeBudgetActionsForBudgetResponse (Maybe Text)
dbafbrsNextToken = lens _dbafbrsNextToken (\s a -> s {_dbafbrsNextToken = a})

-- | -- | The response status code.
dbafbrsResponseStatus :: Lens' DescribeBudgetActionsForBudgetResponse Int
dbafbrsResponseStatus = lens _dbafbrsResponseStatus (\s a -> s {_dbafbrsResponseStatus = a})

-- | A list of the budget action resources information.
dbafbrsActions :: Lens' DescribeBudgetActionsForBudgetResponse [Action]
dbafbrsActions = lens _dbafbrsActions (\s a -> s {_dbafbrsActions = a}) . _Coerce

instance NFData DescribeBudgetActionsForBudgetResponse
