{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.Action where

import Network.AWS.Budgets.Types.ActionStatus
import Network.AWS.Budgets.Types.ActionThreshold
import Network.AWS.Budgets.Types.ActionType
import Network.AWS.Budgets.Types.ApprovalModel
import Network.AWS.Budgets.Types.Definition
import Network.AWS.Budgets.Types.NotificationType
import Network.AWS.Budgets.Types.Subscriber
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A budget action resource.
--
--
--
-- /See:/ 'action' smart constructor.
data Action = Action'
  { _aActionId :: !Text,
    _aBudgetName :: !Text,
    _aNotificationType :: !NotificationType,
    _aActionType :: !ActionType,
    _aActionThreshold :: !ActionThreshold,
    _aDefinition :: !Definition,
    _aExecutionRoleARN :: !Text,
    _aApprovalModel :: !ApprovalModel,
    _aStatus :: !ActionStatus,
    _aSubscribers :: !(List1 Subscriber)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Action' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aActionId' - A system-generated universally unique identifier (UUID) for the action.
--
-- * 'aBudgetName' - Undocumented member.
--
-- * 'aNotificationType' - Undocumented member.
--
-- * 'aActionType' - The type of action. This defines the type of tasks that can be carried out by this action. This field also determines the format for definition.
--
-- * 'aActionThreshold' - The trigger threshold of the action.
--
-- * 'aDefinition' - Where you specify all of the type-specific parameters.
--
-- * 'aExecutionRoleARN' - The role passed for action execution and reversion. Roles and actions must be in the same account.
--
-- * 'aApprovalModel' - This specifies if the action needs manual or automatic approval.
--
-- * 'aStatus' - The status of action.
--
-- * 'aSubscribers' - Undocumented member.
action ::
  -- | 'aActionId'
  Text ->
  -- | 'aBudgetName'
  Text ->
  -- | 'aNotificationType'
  NotificationType ->
  -- | 'aActionType'
  ActionType ->
  -- | 'aActionThreshold'
  ActionThreshold ->
  -- | 'aDefinition'
  Definition ->
  -- | 'aExecutionRoleARN'
  Text ->
  -- | 'aApprovalModel'
  ApprovalModel ->
  -- | 'aStatus'
  ActionStatus ->
  -- | 'aSubscribers'
  NonEmpty Subscriber ->
  Action
action
  pActionId_
  pBudgetName_
  pNotificationType_
  pActionType_
  pActionThreshold_
  pDefinition_
  pExecutionRoleARN_
  pApprovalModel_
  pStatus_
  pSubscribers_ =
    Action'
      { _aActionId = pActionId_,
        _aBudgetName = pBudgetName_,
        _aNotificationType = pNotificationType_,
        _aActionType = pActionType_,
        _aActionThreshold = pActionThreshold_,
        _aDefinition = pDefinition_,
        _aExecutionRoleARN = pExecutionRoleARN_,
        _aApprovalModel = pApprovalModel_,
        _aStatus = pStatus_,
        _aSubscribers = _List1 # pSubscribers_
      }

-- | A system-generated universally unique identifier (UUID) for the action.
aActionId :: Lens' Action Text
aActionId = lens _aActionId (\s a -> s {_aActionId = a})

-- | Undocumented member.
aBudgetName :: Lens' Action Text
aBudgetName = lens _aBudgetName (\s a -> s {_aBudgetName = a})

-- | Undocumented member.
aNotificationType :: Lens' Action NotificationType
aNotificationType = lens _aNotificationType (\s a -> s {_aNotificationType = a})

-- | The type of action. This defines the type of tasks that can be carried out by this action. This field also determines the format for definition.
aActionType :: Lens' Action ActionType
aActionType = lens _aActionType (\s a -> s {_aActionType = a})

-- | The trigger threshold of the action.
aActionThreshold :: Lens' Action ActionThreshold
aActionThreshold = lens _aActionThreshold (\s a -> s {_aActionThreshold = a})

-- | Where you specify all of the type-specific parameters.
aDefinition :: Lens' Action Definition
aDefinition = lens _aDefinition (\s a -> s {_aDefinition = a})

-- | The role passed for action execution and reversion. Roles and actions must be in the same account.
aExecutionRoleARN :: Lens' Action Text
aExecutionRoleARN = lens _aExecutionRoleARN (\s a -> s {_aExecutionRoleARN = a})

-- | This specifies if the action needs manual or automatic approval.
aApprovalModel :: Lens' Action ApprovalModel
aApprovalModel = lens _aApprovalModel (\s a -> s {_aApprovalModel = a})

-- | The status of action.
aStatus :: Lens' Action ActionStatus
aStatus = lens _aStatus (\s a -> s {_aStatus = a})

-- | Undocumented member.
aSubscribers :: Lens' Action (NonEmpty Subscriber)
aSubscribers = lens _aSubscribers (\s a -> s {_aSubscribers = a}) . _List1

instance FromJSON Action where
  parseJSON =
    withObject
      "Action"
      ( \x ->
          Action'
            <$> (x .: "ActionId")
            <*> (x .: "BudgetName")
            <*> (x .: "NotificationType")
            <*> (x .: "ActionType")
            <*> (x .: "ActionThreshold")
            <*> (x .: "Definition")
            <*> (x .: "ExecutionRoleArn")
            <*> (x .: "ApprovalModel")
            <*> (x .: "Status")
            <*> (x .: "Subscribers")
      )

instance Hashable Action

instance NFData Action
