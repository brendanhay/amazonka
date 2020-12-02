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
-- Module      : Network.AWS.Budgets.DescribeBudgetActionsForAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all of the budget actions for an account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeBudgetActionsForAccount
  ( -- * Creating a Request
    describeBudgetActionsForAccount,
    DescribeBudgetActionsForAccount,

    -- * Request Lenses
    dbafaNextToken,
    dbafaMaxResults,
    dbafaAccountId,

    -- * Destructuring the Response
    describeBudgetActionsForAccountResponse,
    DescribeBudgetActionsForAccountResponse,

    -- * Response Lenses
    dbafarsNextToken,
    dbafarsResponseStatus,
    dbafarsActions,
  )
where

import Network.AWS.Budgets.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBudgetActionsForAccount' smart constructor.
data DescribeBudgetActionsForAccount = DescribeBudgetActionsForAccount'
  { _dbafaNextToken ::
      !(Maybe Text),
    _dbafaMaxResults ::
      !(Maybe Nat),
    _dbafaAccountId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeBudgetActionsForAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbafaNextToken' - Undocumented member.
--
-- * 'dbafaMaxResults' - Undocumented member.
--
-- * 'dbafaAccountId' - Undocumented member.
describeBudgetActionsForAccount ::
  -- | 'dbafaAccountId'
  Text ->
  DescribeBudgetActionsForAccount
describeBudgetActionsForAccount pAccountId_ =
  DescribeBudgetActionsForAccount'
    { _dbafaNextToken = Nothing,
      _dbafaMaxResults = Nothing,
      _dbafaAccountId = pAccountId_
    }

-- | Undocumented member.
dbafaNextToken :: Lens' DescribeBudgetActionsForAccount (Maybe Text)
dbafaNextToken = lens _dbafaNextToken (\s a -> s {_dbafaNextToken = a})

-- | Undocumented member.
dbafaMaxResults :: Lens' DescribeBudgetActionsForAccount (Maybe Natural)
dbafaMaxResults = lens _dbafaMaxResults (\s a -> s {_dbafaMaxResults = a}) . mapping _Nat

-- | Undocumented member.
dbafaAccountId :: Lens' DescribeBudgetActionsForAccount Text
dbafaAccountId = lens _dbafaAccountId (\s a -> s {_dbafaAccountId = a})

instance AWSPager DescribeBudgetActionsForAccount where
  page rq rs
    | stop (rs ^. dbafarsNextToken) = Nothing
    | stop (rs ^. dbafarsActions) = Nothing
    | otherwise = Just $ rq & dbafaNextToken .~ rs ^. dbafarsNextToken

instance AWSRequest DescribeBudgetActionsForAccount where
  type
    Rs DescribeBudgetActionsForAccount =
      DescribeBudgetActionsForAccountResponse
  request = postJSON budgets
  response =
    receiveJSON
      ( \s h x ->
          DescribeBudgetActionsForAccountResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "Actions" .!@ mempty)
      )

instance Hashable DescribeBudgetActionsForAccount

instance NFData DescribeBudgetActionsForAccount

instance ToHeaders DescribeBudgetActionsForAccount where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSBudgetServiceGateway.DescribeBudgetActionsForAccount" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeBudgetActionsForAccount where
  toJSON DescribeBudgetActionsForAccount' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _dbafaNextToken,
            ("MaxResults" .=) <$> _dbafaMaxResults,
            Just ("AccountId" .= _dbafaAccountId)
          ]
      )

instance ToPath DescribeBudgetActionsForAccount where
  toPath = const "/"

instance ToQuery DescribeBudgetActionsForAccount where
  toQuery = const mempty

-- | /See:/ 'describeBudgetActionsForAccountResponse' smart constructor.
data DescribeBudgetActionsForAccountResponse = DescribeBudgetActionsForAccountResponse'
  { _dbafarsNextToken ::
      !( Maybe
           Text
       ),
    _dbafarsResponseStatus ::
      !Int,
    _dbafarsActions ::
      ![Action]
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeBudgetActionsForAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbafarsNextToken' - Undocumented member.
--
-- * 'dbafarsResponseStatus' - -- | The response status code.
--
-- * 'dbafarsActions' - A list of the budget action resources information.
describeBudgetActionsForAccountResponse ::
  -- | 'dbafarsResponseStatus'
  Int ->
  DescribeBudgetActionsForAccountResponse
describeBudgetActionsForAccountResponse pResponseStatus_ =
  DescribeBudgetActionsForAccountResponse'
    { _dbafarsNextToken =
        Nothing,
      _dbafarsResponseStatus = pResponseStatus_,
      _dbafarsActions = mempty
    }

-- | Undocumented member.
dbafarsNextToken :: Lens' DescribeBudgetActionsForAccountResponse (Maybe Text)
dbafarsNextToken = lens _dbafarsNextToken (\s a -> s {_dbafarsNextToken = a})

-- | -- | The response status code.
dbafarsResponseStatus :: Lens' DescribeBudgetActionsForAccountResponse Int
dbafarsResponseStatus = lens _dbafarsResponseStatus (\s a -> s {_dbafarsResponseStatus = a})

-- | A list of the budget action resources information.
dbafarsActions :: Lens' DescribeBudgetActionsForAccountResponse [Action]
dbafarsActions = lens _dbafarsActions (\s a -> s {_dbafarsActions = a}) . _Coerce

instance NFData DescribeBudgetActionsForAccountResponse
