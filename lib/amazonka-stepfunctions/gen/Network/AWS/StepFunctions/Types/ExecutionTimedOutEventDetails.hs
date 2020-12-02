{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionTimedOutEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionTimedOutEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about the execution timeout that occurred during the execution.
--
--
--
-- /See:/ 'executionTimedOutEventDetails' smart constructor.
data ExecutionTimedOutEventDetails = ExecutionTimedOutEventDetails'
  { _etoedError ::
      !(Maybe (Sensitive Text)),
    _etoedCause ::
      !(Maybe (Sensitive Text))
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecutionTimedOutEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etoedError' - The error code of the failure.
--
-- * 'etoedCause' - A more detailed explanation of the cause of the timeout.
executionTimedOutEventDetails ::
  ExecutionTimedOutEventDetails
executionTimedOutEventDetails =
  ExecutionTimedOutEventDetails'
    { _etoedError = Nothing,
      _etoedCause = Nothing
    }

-- | The error code of the failure.
etoedError :: Lens' ExecutionTimedOutEventDetails (Maybe Text)
etoedError = lens _etoedError (\s a -> s {_etoedError = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the timeout.
etoedCause :: Lens' ExecutionTimedOutEventDetails (Maybe Text)
etoedCause = lens _etoedCause (\s a -> s {_etoedCause = a}) . mapping _Sensitive

instance FromJSON ExecutionTimedOutEventDetails where
  parseJSON =
    withObject
      "ExecutionTimedOutEventDetails"
      ( \x ->
          ExecutionTimedOutEventDetails'
            <$> (x .:? "error") <*> (x .:? "cause")
      )

instance Hashable ExecutionTimedOutEventDetails

instance NFData ExecutionTimedOutEventDetails
