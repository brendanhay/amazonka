{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ManagedActionHistoryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ManagedActionHistoryItem where

import Network.AWS.ElasticBeanstalk.Types.ActionHistoryStatus
import Network.AWS.ElasticBeanstalk.Types.ActionType
import Network.AWS.ElasticBeanstalk.Types.FailureType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The record of a completed or failed managed action.
--
--
--
-- /See:/ 'managedActionHistoryItem' smart constructor.
data ManagedActionHistoryItem = ManagedActionHistoryItem'
  { _mahiStatus ::
      !(Maybe ActionHistoryStatus),
    _mahiFailureType :: !(Maybe FailureType),
    _mahiActionId :: !(Maybe Text),
    _mahiFailureDescription :: !(Maybe Text),
    _mahiFinishedTime :: !(Maybe ISO8601),
    _mahiActionDescription :: !(Maybe Text),
    _mahiExecutedTime :: !(Maybe ISO8601),
    _mahiActionType :: !(Maybe ActionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ManagedActionHistoryItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mahiStatus' - The status of the action.
--
-- * 'mahiFailureType' - If the action failed, the type of failure.
--
-- * 'mahiActionId' - A unique identifier for the managed action.
--
-- * 'mahiFailureDescription' - If the action failed, a description of the failure.
--
-- * 'mahiFinishedTime' - The date and time that the action finished executing.
--
-- * 'mahiActionDescription' - A description of the managed action.
--
-- * 'mahiExecutedTime' - The date and time that the action started executing.
--
-- * 'mahiActionType' - The type of the managed action.
managedActionHistoryItem ::
  ManagedActionHistoryItem
managedActionHistoryItem =
  ManagedActionHistoryItem'
    { _mahiStatus = Nothing,
      _mahiFailureType = Nothing,
      _mahiActionId = Nothing,
      _mahiFailureDescription = Nothing,
      _mahiFinishedTime = Nothing,
      _mahiActionDescription = Nothing,
      _mahiExecutedTime = Nothing,
      _mahiActionType = Nothing
    }

-- | The status of the action.
mahiStatus :: Lens' ManagedActionHistoryItem (Maybe ActionHistoryStatus)
mahiStatus = lens _mahiStatus (\s a -> s {_mahiStatus = a})

-- | If the action failed, the type of failure.
mahiFailureType :: Lens' ManagedActionHistoryItem (Maybe FailureType)
mahiFailureType = lens _mahiFailureType (\s a -> s {_mahiFailureType = a})

-- | A unique identifier for the managed action.
mahiActionId :: Lens' ManagedActionHistoryItem (Maybe Text)
mahiActionId = lens _mahiActionId (\s a -> s {_mahiActionId = a})

-- | If the action failed, a description of the failure.
mahiFailureDescription :: Lens' ManagedActionHistoryItem (Maybe Text)
mahiFailureDescription = lens _mahiFailureDescription (\s a -> s {_mahiFailureDescription = a})

-- | The date and time that the action finished executing.
mahiFinishedTime :: Lens' ManagedActionHistoryItem (Maybe UTCTime)
mahiFinishedTime = lens _mahiFinishedTime (\s a -> s {_mahiFinishedTime = a}) . mapping _Time

-- | A description of the managed action.
mahiActionDescription :: Lens' ManagedActionHistoryItem (Maybe Text)
mahiActionDescription = lens _mahiActionDescription (\s a -> s {_mahiActionDescription = a})

-- | The date and time that the action started executing.
mahiExecutedTime :: Lens' ManagedActionHistoryItem (Maybe UTCTime)
mahiExecutedTime = lens _mahiExecutedTime (\s a -> s {_mahiExecutedTime = a}) . mapping _Time

-- | The type of the managed action.
mahiActionType :: Lens' ManagedActionHistoryItem (Maybe ActionType)
mahiActionType = lens _mahiActionType (\s a -> s {_mahiActionType = a})

instance FromXML ManagedActionHistoryItem where
  parseXML x =
    ManagedActionHistoryItem'
      <$> (x .@? "Status")
      <*> (x .@? "FailureType")
      <*> (x .@? "ActionId")
      <*> (x .@? "FailureDescription")
      <*> (x .@? "FinishedTime")
      <*> (x .@? "ActionDescription")
      <*> (x .@? "ExecutedTime")
      <*> (x .@? "ActionType")

instance Hashable ManagedActionHistoryItem

instance NFData ManagedActionHistoryItem
