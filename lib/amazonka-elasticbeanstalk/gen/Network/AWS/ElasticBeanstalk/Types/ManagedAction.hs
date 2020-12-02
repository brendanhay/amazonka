{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ManagedAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ManagedAction where

import Network.AWS.ElasticBeanstalk.Types.ActionStatus
import Network.AWS.ElasticBeanstalk.Types.ActionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The record of an upcoming or in-progress managed action.
--
--
--
-- /See:/ 'managedAction' smart constructor.
data ManagedAction = ManagedAction'
  { _maStatus ::
      !(Maybe ActionStatus),
    _maActionId :: !(Maybe Text),
    _maWindowStartTime :: !(Maybe ISO8601),
    _maActionDescription :: !(Maybe Text),
    _maActionType :: !(Maybe ActionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ManagedAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maStatus' - The status of the managed action. If the action is @Scheduled@ , you can apply it immediately with 'ApplyEnvironmentManagedAction' .
--
-- * 'maActionId' - A unique identifier for the managed action.
--
-- * 'maWindowStartTime' - The start time of the maintenance window in which the managed action will execute.
--
-- * 'maActionDescription' - A description of the managed action.
--
-- * 'maActionType' - The type of managed action.
managedAction ::
  ManagedAction
managedAction =
  ManagedAction'
    { _maStatus = Nothing,
      _maActionId = Nothing,
      _maWindowStartTime = Nothing,
      _maActionDescription = Nothing,
      _maActionType = Nothing
    }

-- | The status of the managed action. If the action is @Scheduled@ , you can apply it immediately with 'ApplyEnvironmentManagedAction' .
maStatus :: Lens' ManagedAction (Maybe ActionStatus)
maStatus = lens _maStatus (\s a -> s {_maStatus = a})

-- | A unique identifier for the managed action.
maActionId :: Lens' ManagedAction (Maybe Text)
maActionId = lens _maActionId (\s a -> s {_maActionId = a})

-- | The start time of the maintenance window in which the managed action will execute.
maWindowStartTime :: Lens' ManagedAction (Maybe UTCTime)
maWindowStartTime = lens _maWindowStartTime (\s a -> s {_maWindowStartTime = a}) . mapping _Time

-- | A description of the managed action.
maActionDescription :: Lens' ManagedAction (Maybe Text)
maActionDescription = lens _maActionDescription (\s a -> s {_maActionDescription = a})

-- | The type of managed action.
maActionType :: Lens' ManagedAction (Maybe ActionType)
maActionType = lens _maActionType (\s a -> s {_maActionType = a})

instance FromXML ManagedAction where
  parseXML x =
    ManagedAction'
      <$> (x .@? "Status")
      <*> (x .@? "ActionId")
      <*> (x .@? "WindowStartTime")
      <*> (x .@? "ActionDescription")
      <*> (x .@? "ActionType")

instance Hashable ManagedAction

instance NFData ManagedAction
