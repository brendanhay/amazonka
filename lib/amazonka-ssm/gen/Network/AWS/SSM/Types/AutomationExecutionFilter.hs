{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecutionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecutionFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.AutomationExecutionFilterKey

-- | A filter used to match specific automation executions. This is used to limit the scope of Automation execution information returned.
--
--
--
-- /See:/ 'automationExecutionFilter' smart constructor.
data AutomationExecutionFilter = AutomationExecutionFilter'
  { _autKey ::
      !AutomationExecutionFilterKey,
    _autValues :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutomationExecutionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'autKey' - One or more keys to limit the results. Valid filter keys include the following: DocumentNamePrefix, ExecutionStatus, ExecutionId, ParentExecutionId, CurrentAction, StartTimeBefore, StartTimeAfter, TargetResourceGroup.
--
-- * 'autValues' - The values used to limit the execution information associated with the filter's key.
automationExecutionFilter ::
  -- | 'autKey'
  AutomationExecutionFilterKey ->
  -- | 'autValues'
  NonEmpty Text ->
  AutomationExecutionFilter
automationExecutionFilter pKey_ pValues_ =
  AutomationExecutionFilter'
    { _autKey = pKey_,
      _autValues = _List1 # pValues_
    }

-- | One or more keys to limit the results. Valid filter keys include the following: DocumentNamePrefix, ExecutionStatus, ExecutionId, ParentExecutionId, CurrentAction, StartTimeBefore, StartTimeAfter, TargetResourceGroup.
autKey :: Lens' AutomationExecutionFilter AutomationExecutionFilterKey
autKey = lens _autKey (\s a -> s {_autKey = a})

-- | The values used to limit the execution information associated with the filter's key.
autValues :: Lens' AutomationExecutionFilter (NonEmpty Text)
autValues = lens _autValues (\s a -> s {_autValues = a}) . _List1

instance Hashable AutomationExecutionFilter

instance NFData AutomationExecutionFilter

instance ToJSON AutomationExecutionFilter where
  toJSON AutomationExecutionFilter' {..} =
    object
      ( catMaybes
          [Just ("Key" .= _autKey), Just ("Values" .= _autValues)]
      )
