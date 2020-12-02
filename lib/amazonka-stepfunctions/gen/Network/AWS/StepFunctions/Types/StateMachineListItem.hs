{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.StateMachineListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.StateMachineListItem where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StepFunctions.Types.StateMachineType

-- | Contains details about the state machine.
--
--
--
-- /See:/ 'stateMachineListItem' smart constructor.
data StateMachineListItem = StateMachineListItem'
  { _smliStateMachineARN ::
      !Text,
    _smliName :: !Text,
    _smliType :: !StateMachineType,
    _smliCreationDate :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StateMachineListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smliStateMachineARN' - The Amazon Resource Name (ARN) that identifies the state machine.
--
-- * 'smliName' - The name of the state machine. A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
--
-- * 'smliType' -
--
-- * 'smliCreationDate' - The date the state machine is created.
stateMachineListItem ::
  -- | 'smliStateMachineARN'
  Text ->
  -- | 'smliName'
  Text ->
  -- | 'smliType'
  StateMachineType ->
  -- | 'smliCreationDate'
  UTCTime ->
  StateMachineListItem
stateMachineListItem pStateMachineARN_ pName_ pType_ pCreationDate_ =
  StateMachineListItem'
    { _smliStateMachineARN = pStateMachineARN_,
      _smliName = pName_,
      _smliType = pType_,
      _smliCreationDate = _Time # pCreationDate_
    }

-- | The Amazon Resource Name (ARN) that identifies the state machine.
smliStateMachineARN :: Lens' StateMachineListItem Text
smliStateMachineARN = lens _smliStateMachineARN (\s a -> s {_smliStateMachineARN = a})

-- | The name of the state machine. A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
smliName :: Lens' StateMachineListItem Text
smliName = lens _smliName (\s a -> s {_smliName = a})

-- |
smliType :: Lens' StateMachineListItem StateMachineType
smliType = lens _smliType (\s a -> s {_smliType = a})

-- | The date the state machine is created.
smliCreationDate :: Lens' StateMachineListItem UTCTime
smliCreationDate = lens _smliCreationDate (\s a -> s {_smliCreationDate = a}) . _Time

instance FromJSON StateMachineListItem where
  parseJSON =
    withObject
      "StateMachineListItem"
      ( \x ->
          StateMachineListItem'
            <$> (x .: "stateMachineArn")
            <*> (x .: "name")
            <*> (x .: "type")
            <*> (x .: "creationDate")
      )

instance Hashable StateMachineListItem

instance NFData StateMachineListItem
