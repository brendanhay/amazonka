{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.StepExecutionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.StepExecutionFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.StepExecutionFilterKey

-- | A filter to limit the amount of step execution information returned by the call.
--
--
--
-- /See:/ 'stepExecutionFilter' smart constructor.
data StepExecutionFilter = StepExecutionFilter'
  { _sefKey ::
      !StepExecutionFilterKey,
    _sefValues :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StepExecutionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sefKey' - One or more keys to limit the results. Valid filter keys include the following: StepName, Action, StepExecutionId, StepExecutionStatus, StartTimeBefore, StartTimeAfter.
--
-- * 'sefValues' - The values of the filter key.
stepExecutionFilter ::
  -- | 'sefKey'
  StepExecutionFilterKey ->
  -- | 'sefValues'
  NonEmpty Text ->
  StepExecutionFilter
stepExecutionFilter pKey_ pValues_ =
  StepExecutionFilter'
    { _sefKey = pKey_,
      _sefValues = _List1 # pValues_
    }

-- | One or more keys to limit the results. Valid filter keys include the following: StepName, Action, StepExecutionId, StepExecutionStatus, StartTimeBefore, StartTimeAfter.
sefKey :: Lens' StepExecutionFilter StepExecutionFilterKey
sefKey = lens _sefKey (\s a -> s {_sefKey = a})

-- | The values of the filter key.
sefValues :: Lens' StepExecutionFilter (NonEmpty Text)
sefValues = lens _sefValues (\s a -> s {_sefValues = a}) . _List1

instance Hashable StepExecutionFilter

instance NFData StepExecutionFilter

instance ToJSON StepExecutionFilter where
  toJSON StepExecutionFilter' {..} =
    object
      ( catMaybes
          [Just ("Key" .= _sefKey), Just ("Values" .= _sefValues)]
      )
