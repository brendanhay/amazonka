{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.StateExitedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.StateExitedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about an exit from a state during an execution.
--
--
--
-- /See:/ 'stateExitedEventDetails' smart constructor.
data StateExitedEventDetails = StateExitedEventDetails'
  { _seedOutput ::
      !(Maybe (Sensitive Text)),
    _seedOutputDetails ::
      !(Maybe HistoryEventExecutionDataDetails),
    _seedName :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'StateExitedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seedOutput' - The JSON output data of the state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- * 'seedOutputDetails' - Contains details about the output of an execution history event.
--
-- * 'seedName' - The name of the state. A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
stateExitedEventDetails ::
  -- | 'seedName'
  Text ->
  StateExitedEventDetails
stateExitedEventDetails pName_ =
  StateExitedEventDetails'
    { _seedOutput = Nothing,
      _seedOutputDetails = Nothing,
      _seedName = pName_
    }

-- | The JSON output data of the state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
seedOutput :: Lens' StateExitedEventDetails (Maybe Text)
seedOutput = lens _seedOutput (\s a -> s {_seedOutput = a}) . mapping _Sensitive

-- | Contains details about the output of an execution history event.
seedOutputDetails :: Lens' StateExitedEventDetails (Maybe HistoryEventExecutionDataDetails)
seedOutputDetails = lens _seedOutputDetails (\s a -> s {_seedOutputDetails = a})

-- | The name of the state. A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
seedName :: Lens' StateExitedEventDetails Text
seedName = lens _seedName (\s a -> s {_seedName = a})

instance FromJSON StateExitedEventDetails where
  parseJSON =
    withObject
      "StateExitedEventDetails"
      ( \x ->
          StateExitedEventDetails'
            <$> (x .:? "output") <*> (x .:? "outputDetails") <*> (x .: "name")
      )

instance Hashable StateExitedEventDetails

instance NFData StateExitedEventDetails
