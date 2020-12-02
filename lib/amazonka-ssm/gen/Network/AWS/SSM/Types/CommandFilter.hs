{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.CommandFilterKey

-- | Describes a command filter.
--
--
--
-- /See:/ 'commandFilter' smart constructor.
data CommandFilter = CommandFilter'
  { _cfKey :: !CommandFilterKey,
    _cfValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CommandFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfKey' - The name of the filter.
--
-- * 'cfValue' - The filter value. Valid values for each filter key are as follows:     * __InvokedAfter__ : Specify a timestamp to limit your results. For example, specify @2018-07-07T00:00:00Z@ to see a list of command executions occurring July 7, 2018, and later.     * __InvokedBefore__ : Specify a timestamp to limit your results. For example, specify @2018-07-07T00:00:00Z@ to see a list of command executions from before July 7, 2018.     * __Status__ : Specify a valid command status to see a list of all command executions with that status. Status values you can specify include:     * @Pending@      * @InProgress@      * @Success@      * @Cancelled@      * @Failed@      * @TimedOut@      * @Cancelling@      * __DocumentName__ : Specify name of the SSM document for which you want to see command execution results. For example, specify @AWS-RunPatchBaseline@ to see command executions that used this SSM document to perform security patching operations on instances.      * __ExecutionStage__ : Specify one of the following values:     * @Executing@ : Returns a list of command executions that are currently still running.     * @Complete@ : Returns a list of command executions that have already completed.
commandFilter ::
  -- | 'cfKey'
  CommandFilterKey ->
  -- | 'cfValue'
  Text ->
  CommandFilter
commandFilter pKey_ pValue_ =
  CommandFilter' {_cfKey = pKey_, _cfValue = pValue_}

-- | The name of the filter.
cfKey :: Lens' CommandFilter CommandFilterKey
cfKey = lens _cfKey (\s a -> s {_cfKey = a})

-- | The filter value. Valid values for each filter key are as follows:     * __InvokedAfter__ : Specify a timestamp to limit your results. For example, specify @2018-07-07T00:00:00Z@ to see a list of command executions occurring July 7, 2018, and later.     * __InvokedBefore__ : Specify a timestamp to limit your results. For example, specify @2018-07-07T00:00:00Z@ to see a list of command executions from before July 7, 2018.     * __Status__ : Specify a valid command status to see a list of all command executions with that status. Status values you can specify include:     * @Pending@      * @InProgress@      * @Success@      * @Cancelled@      * @Failed@      * @TimedOut@      * @Cancelling@      * __DocumentName__ : Specify name of the SSM document for which you want to see command execution results. For example, specify @AWS-RunPatchBaseline@ to see command executions that used this SSM document to perform security patching operations on instances.      * __ExecutionStage__ : Specify one of the following values:     * @Executing@ : Returns a list of command executions that are currently still running.     * @Complete@ : Returns a list of command executions that have already completed.
cfValue :: Lens' CommandFilter Text
cfValue = lens _cfValue (\s a -> s {_cfValue = a})

instance Hashable CommandFilter

instance NFData CommandFilter

instance ToJSON CommandFilter where
  toJSON CommandFilter' {..} =
    object
      (catMaybes [Just ("key" .= _cfKey), Just ("value" .= _cfValue)])
