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
-- Module      : Network.AWS.Budgets.DescribeBudgetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a budget action detail.
module Network.AWS.Budgets.DescribeBudgetAction
  ( -- * Creating a Request
    describeBudgetAction,
    DescribeBudgetAction,

    -- * Request Lenses
    dbabAccountId,
    dbabBudgetName,
    dbabActionId,

    -- * Destructuring the Response
    describeBudgetActionResponse,
    DescribeBudgetActionResponse,

    -- * Response Lenses
    dbarsResponseStatus,
    dbarsAccountId,
    dbarsBudgetName,
    dbarsAction,
  )
where

import Network.AWS.Budgets.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBudgetAction' smart constructor.
data DescribeBudgetAction = DescribeBudgetAction'
  { _dbabAccountId ::
      !Text,
    _dbabBudgetName :: !Text,
    _dbabActionId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeBudgetAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbabAccountId' - Undocumented member.
--
-- * 'dbabBudgetName' - Undocumented member.
--
-- * 'dbabActionId' - A system-generated universally unique identifier (UUID) for the action.
describeBudgetAction ::
  -- | 'dbabAccountId'
  Text ->
  -- | 'dbabBudgetName'
  Text ->
  -- | 'dbabActionId'
  Text ->
  DescribeBudgetAction
describeBudgetAction pAccountId_ pBudgetName_ pActionId_ =
  DescribeBudgetAction'
    { _dbabAccountId = pAccountId_,
      _dbabBudgetName = pBudgetName_,
      _dbabActionId = pActionId_
    }

-- | Undocumented member.
dbabAccountId :: Lens' DescribeBudgetAction Text
dbabAccountId = lens _dbabAccountId (\s a -> s {_dbabAccountId = a})

-- | Undocumented member.
dbabBudgetName :: Lens' DescribeBudgetAction Text
dbabBudgetName = lens _dbabBudgetName (\s a -> s {_dbabBudgetName = a})

-- | A system-generated universally unique identifier (UUID) for the action.
dbabActionId :: Lens' DescribeBudgetAction Text
dbabActionId = lens _dbabActionId (\s a -> s {_dbabActionId = a})

instance AWSRequest DescribeBudgetAction where
  type Rs DescribeBudgetAction = DescribeBudgetActionResponse
  request = postJSON budgets
  response =
    receiveJSON
      ( \s h x ->
          DescribeBudgetActionResponse'
            <$> (pure (fromEnum s))
            <*> (x .:> "AccountId")
            <*> (x .:> "BudgetName")
            <*> (x .:> "Action")
      )

instance Hashable DescribeBudgetAction

instance NFData DescribeBudgetAction

instance ToHeaders DescribeBudgetAction where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSBudgetServiceGateway.DescribeBudgetAction" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeBudgetAction where
  toJSON DescribeBudgetAction' {..} =
    object
      ( catMaybes
          [ Just ("AccountId" .= _dbabAccountId),
            Just ("BudgetName" .= _dbabBudgetName),
            Just ("ActionId" .= _dbabActionId)
          ]
      )

instance ToPath DescribeBudgetAction where
  toPath = const "/"

instance ToQuery DescribeBudgetAction where
  toQuery = const mempty

-- | /See:/ 'describeBudgetActionResponse' smart constructor.
data DescribeBudgetActionResponse = DescribeBudgetActionResponse'
  { _dbarsResponseStatus ::
      !Int,
    _dbarsAccountId :: !Text,
    _dbarsBudgetName :: !Text,
    _dbarsAction :: !Action
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeBudgetActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbarsResponseStatus' - -- | The response status code.
--
-- * 'dbarsAccountId' - Undocumented member.
--
-- * 'dbarsBudgetName' - Undocumented member.
--
-- * 'dbarsAction' - A budget action resource.
describeBudgetActionResponse ::
  -- | 'dbarsResponseStatus'
  Int ->
  -- | 'dbarsAccountId'
  Text ->
  -- | 'dbarsBudgetName'
  Text ->
  -- | 'dbarsAction'
  Action ->
  DescribeBudgetActionResponse
describeBudgetActionResponse
  pResponseStatus_
  pAccountId_
  pBudgetName_
  pAction_ =
    DescribeBudgetActionResponse'
      { _dbarsResponseStatus =
          pResponseStatus_,
        _dbarsAccountId = pAccountId_,
        _dbarsBudgetName = pBudgetName_,
        _dbarsAction = pAction_
      }

-- | -- | The response status code.
dbarsResponseStatus :: Lens' DescribeBudgetActionResponse Int
dbarsResponseStatus = lens _dbarsResponseStatus (\s a -> s {_dbarsResponseStatus = a})

-- | Undocumented member.
dbarsAccountId :: Lens' DescribeBudgetActionResponse Text
dbarsAccountId = lens _dbarsAccountId (\s a -> s {_dbarsAccountId = a})

-- | Undocumented member.
dbarsBudgetName :: Lens' DescribeBudgetActionResponse Text
dbarsBudgetName = lens _dbarsBudgetName (\s a -> s {_dbarsBudgetName = a})

-- | A budget action resource.
dbarsAction :: Lens' DescribeBudgetActionResponse Action
dbarsAction = lens _dbarsAction (\s a -> s {_dbarsAction = a})

instance NFData DescribeBudgetActionResponse
