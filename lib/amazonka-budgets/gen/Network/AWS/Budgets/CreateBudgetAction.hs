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
-- Module      : Network.AWS.Budgets.CreateBudgetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a budget action.
module Network.AWS.Budgets.CreateBudgetAction
  ( -- * Creating a Request
    createBudgetAction,
    CreateBudgetAction,

    -- * Request Lenses
    cbaAccountId,
    cbaBudgetName,
    cbaNotificationType,
    cbaActionType,
    cbaActionThreshold,
    cbaDefinition,
    cbaExecutionRoleARN,
    cbaApprovalModel,
    cbaSubscribers,

    -- * Destructuring the Response
    createBudgetActionResponse,
    CreateBudgetActionResponse,

    -- * Response Lenses
    cbarsResponseStatus,
    cbarsAccountId,
    cbarsBudgetName,
    cbarsActionId,
  )
where

import Network.AWS.Budgets.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createBudgetAction' smart constructor.
data CreateBudgetAction = CreateBudgetAction'
  { _cbaAccountId ::
      !Text,
    _cbaBudgetName :: !Text,
    _cbaNotificationType :: !NotificationType,
    _cbaActionType :: !ActionType,
    _cbaActionThreshold :: !ActionThreshold,
    _cbaDefinition :: !Definition,
    _cbaExecutionRoleARN :: !Text,
    _cbaApprovalModel :: !ApprovalModel,
    _cbaSubscribers :: !(List1 Subscriber)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateBudgetAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbaAccountId' - Undocumented member.
--
-- * 'cbaBudgetName' - Undocumented member.
--
-- * 'cbaNotificationType' - Undocumented member.
--
-- * 'cbaActionType' - The type of action. This defines the type of tasks that can be carried out by this action. This field also determines the format for definition.
--
-- * 'cbaActionThreshold' - Undocumented member.
--
-- * 'cbaDefinition' - Undocumented member.
--
-- * 'cbaExecutionRoleARN' - The role passed for action execution and reversion. Roles and actions must be in the same account.
--
-- * 'cbaApprovalModel' - This specifies if the action needs manual or automatic approval.
--
-- * 'cbaSubscribers' - Undocumented member.
createBudgetAction ::
  -- | 'cbaAccountId'
  Text ->
  -- | 'cbaBudgetName'
  Text ->
  -- | 'cbaNotificationType'
  NotificationType ->
  -- | 'cbaActionType'
  ActionType ->
  -- | 'cbaActionThreshold'
  ActionThreshold ->
  -- | 'cbaDefinition'
  Definition ->
  -- | 'cbaExecutionRoleARN'
  Text ->
  -- | 'cbaApprovalModel'
  ApprovalModel ->
  -- | 'cbaSubscribers'
  NonEmpty Subscriber ->
  CreateBudgetAction
createBudgetAction
  pAccountId_
  pBudgetName_
  pNotificationType_
  pActionType_
  pActionThreshold_
  pDefinition_
  pExecutionRoleARN_
  pApprovalModel_
  pSubscribers_ =
    CreateBudgetAction'
      { _cbaAccountId = pAccountId_,
        _cbaBudgetName = pBudgetName_,
        _cbaNotificationType = pNotificationType_,
        _cbaActionType = pActionType_,
        _cbaActionThreshold = pActionThreshold_,
        _cbaDefinition = pDefinition_,
        _cbaExecutionRoleARN = pExecutionRoleARN_,
        _cbaApprovalModel = pApprovalModel_,
        _cbaSubscribers = _List1 # pSubscribers_
      }

-- | Undocumented member.
cbaAccountId :: Lens' CreateBudgetAction Text
cbaAccountId = lens _cbaAccountId (\s a -> s {_cbaAccountId = a})

-- | Undocumented member.
cbaBudgetName :: Lens' CreateBudgetAction Text
cbaBudgetName = lens _cbaBudgetName (\s a -> s {_cbaBudgetName = a})

-- | Undocumented member.
cbaNotificationType :: Lens' CreateBudgetAction NotificationType
cbaNotificationType = lens _cbaNotificationType (\s a -> s {_cbaNotificationType = a})

-- | The type of action. This defines the type of tasks that can be carried out by this action. This field also determines the format for definition.
cbaActionType :: Lens' CreateBudgetAction ActionType
cbaActionType = lens _cbaActionType (\s a -> s {_cbaActionType = a})

-- | Undocumented member.
cbaActionThreshold :: Lens' CreateBudgetAction ActionThreshold
cbaActionThreshold = lens _cbaActionThreshold (\s a -> s {_cbaActionThreshold = a})

-- | Undocumented member.
cbaDefinition :: Lens' CreateBudgetAction Definition
cbaDefinition = lens _cbaDefinition (\s a -> s {_cbaDefinition = a})

-- | The role passed for action execution and reversion. Roles and actions must be in the same account.
cbaExecutionRoleARN :: Lens' CreateBudgetAction Text
cbaExecutionRoleARN = lens _cbaExecutionRoleARN (\s a -> s {_cbaExecutionRoleARN = a})

-- | This specifies if the action needs manual or automatic approval.
cbaApprovalModel :: Lens' CreateBudgetAction ApprovalModel
cbaApprovalModel = lens _cbaApprovalModel (\s a -> s {_cbaApprovalModel = a})

-- | Undocumented member.
cbaSubscribers :: Lens' CreateBudgetAction (NonEmpty Subscriber)
cbaSubscribers = lens _cbaSubscribers (\s a -> s {_cbaSubscribers = a}) . _List1

instance AWSRequest CreateBudgetAction where
  type Rs CreateBudgetAction = CreateBudgetActionResponse
  request = postJSON budgets
  response =
    receiveJSON
      ( \s h x ->
          CreateBudgetActionResponse'
            <$> (pure (fromEnum s))
            <*> (x .:> "AccountId")
            <*> (x .:> "BudgetName")
            <*> (x .:> "ActionId")
      )

instance Hashable CreateBudgetAction

instance NFData CreateBudgetAction

instance ToHeaders CreateBudgetAction where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSBudgetServiceGateway.CreateBudgetAction" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateBudgetAction where
  toJSON CreateBudgetAction' {..} =
    object
      ( catMaybes
          [ Just ("AccountId" .= _cbaAccountId),
            Just ("BudgetName" .= _cbaBudgetName),
            Just ("NotificationType" .= _cbaNotificationType),
            Just ("ActionType" .= _cbaActionType),
            Just ("ActionThreshold" .= _cbaActionThreshold),
            Just ("Definition" .= _cbaDefinition),
            Just ("ExecutionRoleArn" .= _cbaExecutionRoleARN),
            Just ("ApprovalModel" .= _cbaApprovalModel),
            Just ("Subscribers" .= _cbaSubscribers)
          ]
      )

instance ToPath CreateBudgetAction where
  toPath = const "/"

instance ToQuery CreateBudgetAction where
  toQuery = const mempty

-- | /See:/ 'createBudgetActionResponse' smart constructor.
data CreateBudgetActionResponse = CreateBudgetActionResponse'
  { _cbarsResponseStatus ::
      !Int,
    _cbarsAccountId :: !Text,
    _cbarsBudgetName :: !Text,
    _cbarsActionId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateBudgetActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbarsResponseStatus' - -- | The response status code.
--
-- * 'cbarsAccountId' - Undocumented member.
--
-- * 'cbarsBudgetName' - Undocumented member.
--
-- * 'cbarsActionId' - A system-generated universally unique identifier (UUID) for the action.
createBudgetActionResponse ::
  -- | 'cbarsResponseStatus'
  Int ->
  -- | 'cbarsAccountId'
  Text ->
  -- | 'cbarsBudgetName'
  Text ->
  -- | 'cbarsActionId'
  Text ->
  CreateBudgetActionResponse
createBudgetActionResponse
  pResponseStatus_
  pAccountId_
  pBudgetName_
  pActionId_ =
    CreateBudgetActionResponse'
      { _cbarsResponseStatus =
          pResponseStatus_,
        _cbarsAccountId = pAccountId_,
        _cbarsBudgetName = pBudgetName_,
        _cbarsActionId = pActionId_
      }

-- | -- | The response status code.
cbarsResponseStatus :: Lens' CreateBudgetActionResponse Int
cbarsResponseStatus = lens _cbarsResponseStatus (\s a -> s {_cbarsResponseStatus = a})

-- | Undocumented member.
cbarsAccountId :: Lens' CreateBudgetActionResponse Text
cbarsAccountId = lens _cbarsAccountId (\s a -> s {_cbarsAccountId = a})

-- | Undocumented member.
cbarsBudgetName :: Lens' CreateBudgetActionResponse Text
cbarsBudgetName = lens _cbarsBudgetName (\s a -> s {_cbarsBudgetName = a})

-- | A system-generated universally unique identifier (UUID) for the action.
cbarsActionId :: Lens' CreateBudgetActionResponse Text
cbarsActionId = lens _cbarsActionId (\s a -> s {_cbarsActionId = a})

instance NFData CreateBudgetActionResponse
