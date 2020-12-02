{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionAbortedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionAbortedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about an abort of an execution.
--
--
--
-- /See:/ 'executionAbortedEventDetails' smart constructor.
data ExecutionAbortedEventDetails = ExecutionAbortedEventDetails'
  { _eaedError ::
      !(Maybe (Sensitive Text)),
    _eaedCause ::
      !(Maybe (Sensitive Text))
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecutionAbortedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eaedError' - The error code of the failure.
--
-- * 'eaedCause' - A more detailed explanation of the cause of the failure.
executionAbortedEventDetails ::
  ExecutionAbortedEventDetails
executionAbortedEventDetails =
  ExecutionAbortedEventDetails'
    { _eaedError = Nothing,
      _eaedCause = Nothing
    }

-- | The error code of the failure.
eaedError :: Lens' ExecutionAbortedEventDetails (Maybe Text)
eaedError = lens _eaedError (\s a -> s {_eaedError = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the failure.
eaedCause :: Lens' ExecutionAbortedEventDetails (Maybe Text)
eaedCause = lens _eaedCause (\s a -> s {_eaedCause = a}) . mapping _Sensitive

instance FromJSON ExecutionAbortedEventDetails where
  parseJSON =
    withObject
      "ExecutionAbortedEventDetails"
      ( \x ->
          ExecutionAbortedEventDetails'
            <$> (x .:? "error") <*> (x .:? "cause")
      )

instance Hashable ExecutionAbortedEventDetails

instance NFData ExecutionAbortedEventDetails
