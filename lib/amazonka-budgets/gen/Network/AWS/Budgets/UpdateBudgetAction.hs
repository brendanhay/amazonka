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
-- Module      : Network.AWS.Budgets.UpdateBudgetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a budget action.
module Network.AWS.Budgets.UpdateBudgetAction
  ( -- * Creating a Request
    updateBudgetAction,
    UpdateBudgetAction,

    -- * Request Lenses
    ubaDefinition,
    ubaExecutionRoleARN,
    ubaActionThreshold,
    ubaNotificationType,
    ubaApprovalModel,
    ubaSubscribers,
    ubaAccountId,
    ubaBudgetName,
    ubaActionId,

    -- * Destructuring the Response
    updateBudgetActionResponse,
    UpdateBudgetActionResponse,

    -- * Response Lenses
    ubarsResponseStatus,
    ubarsAccountId,
    ubarsBudgetName,
    ubarsOldAction,
    ubarsNewAction,
  )
where

import Network.AWS.Budgets.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateBudgetAction' smart constructor.
data UpdateBudgetAction = UpdateBudgetAction'
  { _ubaDefinition ::
      !(Maybe Definition),
    _ubaExecutionRoleARN :: !(Maybe Text),
    _ubaActionThreshold :: !(Maybe ActionThreshold),
    _ubaNotificationType :: !(Maybe NotificationType),
    _ubaApprovalModel :: !(Maybe ApprovalModel),
    _ubaSubscribers :: !(Maybe (List1 Subscriber)),
    _ubaAccountId :: !Text,
    _ubaBudgetName :: !Text,
    _ubaActionId :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateBudgetAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubaDefinition' - Undocumented member.
--
-- * 'ubaExecutionRoleARN' - The role passed for action execution and reversion. Roles and actions must be in the same account.
--
-- * 'ubaActionThreshold' - Undocumented member.
--
-- * 'ubaNotificationType' - Undocumented member.
--
-- * 'ubaApprovalModel' - This specifies if the action needs manual or automatic approval.
--
-- * 'ubaSubscribers' - Undocumented member.
--
-- * 'ubaAccountId' - Undocumented member.
--
-- * 'ubaBudgetName' - Undocumented member.
--
-- * 'ubaActionId' - A system-generated universally unique identifier (UUID) for the action.
updateBudgetAction ::
  -- | 'ubaAccountId'
  Text ->
  -- | 'ubaBudgetName'
  Text ->
  -- | 'ubaActionId'
  Text ->
  UpdateBudgetAction
updateBudgetAction pAccountId_ pBudgetName_ pActionId_ =
  UpdateBudgetAction'
    { _ubaDefinition = Nothing,
      _ubaExecutionRoleARN = Nothing,
      _ubaActionThreshold = Nothing,
      _ubaNotificationType = Nothing,
      _ubaApprovalModel = Nothing,
      _ubaSubscribers = Nothing,
      _ubaAccountId = pAccountId_,
      _ubaBudgetName = pBudgetName_,
      _ubaActionId = pActionId_
    }

-- | Undocumented member.
ubaDefinition :: Lens' UpdateBudgetAction (Maybe Definition)
ubaDefinition = lens _ubaDefinition (\s a -> s {_ubaDefinition = a})

-- | The role passed for action execution and reversion. Roles and actions must be in the same account.
ubaExecutionRoleARN :: Lens' UpdateBudgetAction (Maybe Text)
ubaExecutionRoleARN = lens _ubaExecutionRoleARN (\s a -> s {_ubaExecutionRoleARN = a})

-- | Undocumented member.
ubaActionThreshold :: Lens' UpdateBudgetAction (Maybe ActionThreshold)
ubaActionThreshold = lens _ubaActionThreshold (\s a -> s {_ubaActionThreshold = a})

-- | Undocumented member.
ubaNotificationType :: Lens' UpdateBudgetAction (Maybe NotificationType)
ubaNotificationType = lens _ubaNotificationType (\s a -> s {_ubaNotificationType = a})

-- | This specifies if the action needs manual or automatic approval.
ubaApprovalModel :: Lens' UpdateBudgetAction (Maybe ApprovalModel)
ubaApprovalModel = lens _ubaApprovalModel (\s a -> s {_ubaApprovalModel = a})

-- | Undocumented member.
ubaSubscribers :: Lens' UpdateBudgetAction (Maybe (NonEmpty Subscriber))
ubaSubscribers = lens _ubaSubscribers (\s a -> s {_ubaSubscribers = a}) . mapping _List1

-- | Undocumented member.
ubaAccountId :: Lens' UpdateBudgetAction Text
ubaAccountId = lens _ubaAccountId (\s a -> s {_ubaAccountId = a})

-- | Undocumented member.
ubaBudgetName :: Lens' UpdateBudgetAction Text
ubaBudgetName = lens _ubaBudgetName (\s a -> s {_ubaBudgetName = a})

-- | A system-generated universally unique identifier (UUID) for the action.
ubaActionId :: Lens' UpdateBudgetAction Text
ubaActionId = lens _ubaActionId (\s a -> s {_ubaActionId = a})

instance AWSRequest UpdateBudgetAction where
  type Rs UpdateBudgetAction = UpdateBudgetActionResponse
  request = postJSON budgets
  response =
    receiveJSON
      ( \s h x ->
          UpdateBudgetActionResponse'
            <$> (pure (fromEnum s))
            <*> (x .:> "AccountId")
            <*> (x .:> "BudgetName")
            <*> (x .:> "OldAction")
            <*> (x .:> "NewAction")
      )

instance Hashable UpdateBudgetAction

instance NFData UpdateBudgetAction

instance ToHeaders UpdateBudgetAction where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSBudgetServiceGateway.UpdateBudgetAction" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateBudgetAction where
  toJSON UpdateBudgetAction' {..} =
    object
      ( catMaybes
          [ ("Definition" .=) <$> _ubaDefinition,
            ("ExecutionRoleArn" .=) <$> _ubaExecutionRoleARN,
            ("ActionThreshold" .=) <$> _ubaActionThreshold,
            ("NotificationType" .=) <$> _ubaNotificationType,
            ("ApprovalModel" .=) <$> _ubaApprovalModel,
            ("Subscribers" .=) <$> _ubaSubscribers,
            Just ("AccountId" .= _ubaAccountId),
            Just ("BudgetName" .= _ubaBudgetName),
            Just ("ActionId" .= _ubaActionId)
          ]
      )

instance ToPath UpdateBudgetAction where
  toPath = const "/"

instance ToQuery UpdateBudgetAction where
  toQuery = const mempty

-- | /See:/ 'updateBudgetActionResponse' smart constructor.
data UpdateBudgetActionResponse = UpdateBudgetActionResponse'
  { _ubarsResponseStatus ::
      !Int,
    _ubarsAccountId :: !Text,
    _ubarsBudgetName :: !Text,
    _ubarsOldAction :: !Action,
    _ubarsNewAction :: !Action
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateBudgetActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubarsResponseStatus' - -- | The response status code.
--
-- * 'ubarsAccountId' - Undocumented member.
--
-- * 'ubarsBudgetName' - Undocumented member.
--
-- * 'ubarsOldAction' - The previous action resource information.
--
-- * 'ubarsNewAction' - The updated action resource information.
updateBudgetActionResponse ::
  -- | 'ubarsResponseStatus'
  Int ->
  -- | 'ubarsAccountId'
  Text ->
  -- | 'ubarsBudgetName'
  Text ->
  -- | 'ubarsOldAction'
  Action ->
  -- | 'ubarsNewAction'
  Action ->
  UpdateBudgetActionResponse
updateBudgetActionResponse
  pResponseStatus_
  pAccountId_
  pBudgetName_
  pOldAction_
  pNewAction_ =
    UpdateBudgetActionResponse'
      { _ubarsResponseStatus =
          pResponseStatus_,
        _ubarsAccountId = pAccountId_,
        _ubarsBudgetName = pBudgetName_,
        _ubarsOldAction = pOldAction_,
        _ubarsNewAction = pNewAction_
      }

-- | -- | The response status code.
ubarsResponseStatus :: Lens' UpdateBudgetActionResponse Int
ubarsResponseStatus = lens _ubarsResponseStatus (\s a -> s {_ubarsResponseStatus = a})

-- | Undocumented member.
ubarsAccountId :: Lens' UpdateBudgetActionResponse Text
ubarsAccountId = lens _ubarsAccountId (\s a -> s {_ubarsAccountId = a})

-- | Undocumented member.
ubarsBudgetName :: Lens' UpdateBudgetActionResponse Text
ubarsBudgetName = lens _ubarsBudgetName (\s a -> s {_ubarsBudgetName = a})

-- | The previous action resource information.
ubarsOldAction :: Lens' UpdateBudgetActionResponse Action
ubarsOldAction = lens _ubarsOldAction (\s a -> s {_ubarsOldAction = a})

-- | The updated action resource information.
ubarsNewAction :: Lens' UpdateBudgetActionResponse Action
ubarsNewAction = lens _ubarsNewAction (\s a -> s {_ubarsNewAction = a})

instance NFData UpdateBudgetActionResponse
