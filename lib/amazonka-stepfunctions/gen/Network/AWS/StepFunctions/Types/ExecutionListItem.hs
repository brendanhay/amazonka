{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionListItem where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StepFunctions.Types.ExecutionStatus

-- | Contains details about an execution.
--
--
--
-- /See:/ 'executionListItem' smart constructor.
data ExecutionListItem = ExecutionListItem'
  { _eliStopDate ::
      !(Maybe POSIX),
    _eliExecutionARN :: !Text,
    _eliStateMachineARN :: !Text,
    _eliName :: !Text,
    _eliStatus :: !ExecutionStatus,
    _eliStartDate :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecutionListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eliStopDate' - If the execution already ended, the date the execution stopped.
--
-- * 'eliExecutionARN' - The Amazon Resource Name (ARN) that identifies the execution.
--
-- * 'eliStateMachineARN' - The Amazon Resource Name (ARN) of the executed state machine.
--
-- * 'eliName' - The name of the execution. A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
--
-- * 'eliStatus' - The current status of the execution.
--
-- * 'eliStartDate' - The date the execution started.
executionListItem ::
  -- | 'eliExecutionARN'
  Text ->
  -- | 'eliStateMachineARN'
  Text ->
  -- | 'eliName'
  Text ->
  -- | 'eliStatus'
  ExecutionStatus ->
  -- | 'eliStartDate'
  UTCTime ->
  ExecutionListItem
executionListItem
  pExecutionARN_
  pStateMachineARN_
  pName_
  pStatus_
  pStartDate_ =
    ExecutionListItem'
      { _eliStopDate = Nothing,
        _eliExecutionARN = pExecutionARN_,
        _eliStateMachineARN = pStateMachineARN_,
        _eliName = pName_,
        _eliStatus = pStatus_,
        _eliStartDate = _Time # pStartDate_
      }

-- | If the execution already ended, the date the execution stopped.
eliStopDate :: Lens' ExecutionListItem (Maybe UTCTime)
eliStopDate = lens _eliStopDate (\s a -> s {_eliStopDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) that identifies the execution.
eliExecutionARN :: Lens' ExecutionListItem Text
eliExecutionARN = lens _eliExecutionARN (\s a -> s {_eliExecutionARN = a})

-- | The Amazon Resource Name (ARN) of the executed state machine.
eliStateMachineARN :: Lens' ExecutionListItem Text
eliStateMachineARN = lens _eliStateMachineARN (\s a -> s {_eliStateMachineARN = a})

-- | The name of the execution. A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
eliName :: Lens' ExecutionListItem Text
eliName = lens _eliName (\s a -> s {_eliName = a})

-- | The current status of the execution.
eliStatus :: Lens' ExecutionListItem ExecutionStatus
eliStatus = lens _eliStatus (\s a -> s {_eliStatus = a})

-- | The date the execution started.
eliStartDate :: Lens' ExecutionListItem UTCTime
eliStartDate = lens _eliStartDate (\s a -> s {_eliStartDate = a}) . _Time

instance FromJSON ExecutionListItem where
  parseJSON =
    withObject
      "ExecutionListItem"
      ( \x ->
          ExecutionListItem'
            <$> (x .:? "stopDate")
            <*> (x .: "executionArn")
            <*> (x .: "stateMachineArn")
            <*> (x .: "name")
            <*> (x .: "status")
            <*> (x .: "startDate")
      )

instance Hashable ExecutionListItem

instance NFData ExecutionListItem
