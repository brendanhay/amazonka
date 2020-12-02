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
-- Module      : Network.AWS.Budgets.ExecuteBudgetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Executes a budget action.
module Network.AWS.Budgets.ExecuteBudgetAction
  ( -- * Creating a Request
    executeBudgetAction,
    ExecuteBudgetAction,

    -- * Request Lenses
    ebaAccountId,
    ebaBudgetName,
    ebaActionId,
    ebaExecutionType,

    -- * Destructuring the Response
    executeBudgetActionResponse,
    ExecuteBudgetActionResponse,

    -- * Response Lenses
    ebarsResponseStatus,
    ebarsAccountId,
    ebarsBudgetName,
    ebarsActionId,
    ebarsExecutionType,
  )
where

import Network.AWS.Budgets.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'executeBudgetAction' smart constructor.
data ExecuteBudgetAction = ExecuteBudgetAction'
  { _ebaAccountId ::
      !Text,
    _ebaBudgetName :: !Text,
    _ebaActionId :: !Text,
    _ebaExecutionType :: !ExecutionType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecuteBudgetAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebaAccountId' - Undocumented member.
--
-- * 'ebaBudgetName' - Undocumented member.
--
-- * 'ebaActionId' - A system-generated universally unique identifier (UUID) for the action.
--
-- * 'ebaExecutionType' - The type of execution.
executeBudgetAction ::
  -- | 'ebaAccountId'
  Text ->
  -- | 'ebaBudgetName'
  Text ->
  -- | 'ebaActionId'
  Text ->
  -- | 'ebaExecutionType'
  ExecutionType ->
  ExecuteBudgetAction
executeBudgetAction
  pAccountId_
  pBudgetName_
  pActionId_
  pExecutionType_ =
    ExecuteBudgetAction'
      { _ebaAccountId = pAccountId_,
        _ebaBudgetName = pBudgetName_,
        _ebaActionId = pActionId_,
        _ebaExecutionType = pExecutionType_
      }

-- | Undocumented member.
ebaAccountId :: Lens' ExecuteBudgetAction Text
ebaAccountId = lens _ebaAccountId (\s a -> s {_ebaAccountId = a})

-- | Undocumented member.
ebaBudgetName :: Lens' ExecuteBudgetAction Text
ebaBudgetName = lens _ebaBudgetName (\s a -> s {_ebaBudgetName = a})

-- | A system-generated universally unique identifier (UUID) for the action.
ebaActionId :: Lens' ExecuteBudgetAction Text
ebaActionId = lens _ebaActionId (\s a -> s {_ebaActionId = a})

-- | The type of execution.
ebaExecutionType :: Lens' ExecuteBudgetAction ExecutionType
ebaExecutionType = lens _ebaExecutionType (\s a -> s {_ebaExecutionType = a})

instance AWSRequest ExecuteBudgetAction where
  type Rs ExecuteBudgetAction = ExecuteBudgetActionResponse
  request = postJSON budgets
  response =
    receiveJSON
      ( \s h x ->
          ExecuteBudgetActionResponse'
            <$> (pure (fromEnum s))
            <*> (x .:> "AccountId")
            <*> (x .:> "BudgetName")
            <*> (x .:> "ActionId")
            <*> (x .:> "ExecutionType")
      )

instance Hashable ExecuteBudgetAction

instance NFData ExecuteBudgetAction

instance ToHeaders ExecuteBudgetAction where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSBudgetServiceGateway.ExecuteBudgetAction" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ExecuteBudgetAction where
  toJSON ExecuteBudgetAction' {..} =
    object
      ( catMaybes
          [ Just ("AccountId" .= _ebaAccountId),
            Just ("BudgetName" .= _ebaBudgetName),
            Just ("ActionId" .= _ebaActionId),
            Just ("ExecutionType" .= _ebaExecutionType)
          ]
      )

instance ToPath ExecuteBudgetAction where
  toPath = const "/"

instance ToQuery ExecuteBudgetAction where
  toQuery = const mempty

-- | /See:/ 'executeBudgetActionResponse' smart constructor.
data ExecuteBudgetActionResponse = ExecuteBudgetActionResponse'
  { _ebarsResponseStatus ::
      !Int,
    _ebarsAccountId :: !Text,
    _ebarsBudgetName :: !Text,
    _ebarsActionId :: !Text,
    _ebarsExecutionType ::
      !ExecutionType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecuteBudgetActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebarsResponseStatus' - -- | The response status code.
--
-- * 'ebarsAccountId' - Undocumented member.
--
-- * 'ebarsBudgetName' - Undocumented member.
--
-- * 'ebarsActionId' - A system-generated universally unique identifier (UUID) for the action.
--
-- * 'ebarsExecutionType' - The type of execution.
executeBudgetActionResponse ::
  -- | 'ebarsResponseStatus'
  Int ->
  -- | 'ebarsAccountId'
  Text ->
  -- | 'ebarsBudgetName'
  Text ->
  -- | 'ebarsActionId'
  Text ->
  -- | 'ebarsExecutionType'
  ExecutionType ->
  ExecuteBudgetActionResponse
executeBudgetActionResponse
  pResponseStatus_
  pAccountId_
  pBudgetName_
  pActionId_
  pExecutionType_ =
    ExecuteBudgetActionResponse'
      { _ebarsResponseStatus =
          pResponseStatus_,
        _ebarsAccountId = pAccountId_,
        _ebarsBudgetName = pBudgetName_,
        _ebarsActionId = pActionId_,
        _ebarsExecutionType = pExecutionType_
      }

-- | -- | The response status code.
ebarsResponseStatus :: Lens' ExecuteBudgetActionResponse Int
ebarsResponseStatus = lens _ebarsResponseStatus (\s a -> s {_ebarsResponseStatus = a})

-- | Undocumented member.
ebarsAccountId :: Lens' ExecuteBudgetActionResponse Text
ebarsAccountId = lens _ebarsAccountId (\s a -> s {_ebarsAccountId = a})

-- | Undocumented member.
ebarsBudgetName :: Lens' ExecuteBudgetActionResponse Text
ebarsBudgetName = lens _ebarsBudgetName (\s a -> s {_ebarsBudgetName = a})

-- | A system-generated universally unique identifier (UUID) for the action.
ebarsActionId :: Lens' ExecuteBudgetActionResponse Text
ebarsActionId = lens _ebarsActionId (\s a -> s {_ebarsActionId = a})

-- | The type of execution.
ebarsExecutionType :: Lens' ExecuteBudgetActionResponse ExecutionType
ebarsExecutionType = lens _ebarsExecutionType (\s a -> s {_ebarsExecutionType = a})

instance NFData ExecuteBudgetActionResponse
