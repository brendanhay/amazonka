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
-- Module      : Network.AWS.Budgets.DeleteBudgetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a budget action.
module Network.AWS.Budgets.DeleteBudgetAction
  ( -- * Creating a Request
    deleteBudgetAction,
    DeleteBudgetAction,

    -- * Request Lenses
    dbaAccountId,
    dbaBudgetName,
    dbaActionId,

    -- * Destructuring the Response
    deleteBudgetActionResponse,
    DeleteBudgetActionResponse,

    -- * Response Lenses
    drsResponseStatus,
    drsAccountId,
    drsBudgetName,
    drsAction,
  )
where

import Network.AWS.Budgets.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBudgetAction' smart constructor.
data DeleteBudgetAction = DeleteBudgetAction'
  { _dbaAccountId ::
      !Text,
    _dbaBudgetName :: !Text,
    _dbaActionId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBudgetAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbaAccountId' - Undocumented member.
--
-- * 'dbaBudgetName' - Undocumented member.
--
-- * 'dbaActionId' - A system-generated universally unique identifier (UUID) for the action.
deleteBudgetAction ::
  -- | 'dbaAccountId'
  Text ->
  -- | 'dbaBudgetName'
  Text ->
  -- | 'dbaActionId'
  Text ->
  DeleteBudgetAction
deleteBudgetAction pAccountId_ pBudgetName_ pActionId_ =
  DeleteBudgetAction'
    { _dbaAccountId = pAccountId_,
      _dbaBudgetName = pBudgetName_,
      _dbaActionId = pActionId_
    }

-- | Undocumented member.
dbaAccountId :: Lens' DeleteBudgetAction Text
dbaAccountId = lens _dbaAccountId (\s a -> s {_dbaAccountId = a})

-- | Undocumented member.
dbaBudgetName :: Lens' DeleteBudgetAction Text
dbaBudgetName = lens _dbaBudgetName (\s a -> s {_dbaBudgetName = a})

-- | A system-generated universally unique identifier (UUID) for the action.
dbaActionId :: Lens' DeleteBudgetAction Text
dbaActionId = lens _dbaActionId (\s a -> s {_dbaActionId = a})

instance AWSRequest DeleteBudgetAction where
  type Rs DeleteBudgetAction = DeleteBudgetActionResponse
  request = postJSON budgets
  response =
    receiveJSON
      ( \s h x ->
          DeleteBudgetActionResponse'
            <$> (pure (fromEnum s))
            <*> (x .:> "AccountId")
            <*> (x .:> "BudgetName")
            <*> (x .:> "Action")
      )

instance Hashable DeleteBudgetAction

instance NFData DeleteBudgetAction

instance ToHeaders DeleteBudgetAction where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSBudgetServiceGateway.DeleteBudgetAction" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteBudgetAction where
  toJSON DeleteBudgetAction' {..} =
    object
      ( catMaybes
          [ Just ("AccountId" .= _dbaAccountId),
            Just ("BudgetName" .= _dbaBudgetName),
            Just ("ActionId" .= _dbaActionId)
          ]
      )

instance ToPath DeleteBudgetAction where
  toPath = const "/"

instance ToQuery DeleteBudgetAction where
  toQuery = const mempty

-- | /See:/ 'deleteBudgetActionResponse' smart constructor.
data DeleteBudgetActionResponse = DeleteBudgetActionResponse'
  { _drsResponseStatus ::
      !Int,
    _drsAccountId :: !Text,
    _drsBudgetName :: !Text,
    _drsAction :: !Action
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBudgetActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
--
-- * 'drsAccountId' - Undocumented member.
--
-- * 'drsBudgetName' - Undocumented member.
--
-- * 'drsAction' - Undocumented member.
deleteBudgetActionResponse ::
  -- | 'drsResponseStatus'
  Int ->
  -- | 'drsAccountId'
  Text ->
  -- | 'drsBudgetName'
  Text ->
  -- | 'drsAction'
  Action ->
  DeleteBudgetActionResponse
deleteBudgetActionResponse
  pResponseStatus_
  pAccountId_
  pBudgetName_
  pAction_ =
    DeleteBudgetActionResponse'
      { _drsResponseStatus =
          pResponseStatus_,
        _drsAccountId = pAccountId_,
        _drsBudgetName = pBudgetName_,
        _drsAction = pAction_
      }

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteBudgetActionResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

-- | Undocumented member.
drsAccountId :: Lens' DeleteBudgetActionResponse Text
drsAccountId = lens _drsAccountId (\s a -> s {_drsAccountId = a})

-- | Undocumented member.
drsBudgetName :: Lens' DeleteBudgetActionResponse Text
drsBudgetName = lens _drsBudgetName (\s a -> s {_drsBudgetName = a})

-- | Undocumented member.
drsAction :: Lens' DeleteBudgetActionResponse Action
drsAction = lens _drsAction (\s a -> s {_drsAction = a})

instance NFData DeleteBudgetActionResponse
