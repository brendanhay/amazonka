{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionFailedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about an execution failure event.
--
--
--
-- /See:/ 'executionFailedEventDetails' smart constructor.
data ExecutionFailedEventDetails = ExecutionFailedEventDetails'
  { _efedError ::
      !(Maybe (Sensitive Text)),
    _efedCause ::
      !(Maybe (Sensitive Text))
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecutionFailedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efedError' - The error code of the failure.
--
-- * 'efedCause' - A more detailed explanation of the cause of the failure.
executionFailedEventDetails ::
  ExecutionFailedEventDetails
executionFailedEventDetails =
  ExecutionFailedEventDetails'
    { _efedError = Nothing,
      _efedCause = Nothing
    }

-- | The error code of the failure.
efedError :: Lens' ExecutionFailedEventDetails (Maybe Text)
efedError = lens _efedError (\s a -> s {_efedError = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the failure.
efedCause :: Lens' ExecutionFailedEventDetails (Maybe Text)
efedCause = lens _efedCause (\s a -> s {_efedCause = a}) . mapping _Sensitive

instance FromJSON ExecutionFailedEventDetails where
  parseJSON =
    withObject
      "ExecutionFailedEventDetails"
      ( \x ->
          ExecutionFailedEventDetails'
            <$> (x .:? "error") <*> (x .:? "cause")
      )

instance Hashable ExecutionFailedEventDetails

instance NFData ExecutionFailedEventDetails
