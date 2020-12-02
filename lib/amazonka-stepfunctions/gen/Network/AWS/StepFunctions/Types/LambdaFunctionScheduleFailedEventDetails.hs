{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about a failed lambda function schedule event that occurred during an execution.
--
--
--
-- /See:/ 'lambdaFunctionScheduleFailedEventDetails' smart constructor.
data LambdaFunctionScheduleFailedEventDetails = LambdaFunctionScheduleFailedEventDetails'
  { _lError ::
      !( Maybe
           ( Sensitive
               Text
           )
       ),
    _lCause ::
      !( Maybe
           ( Sensitive
               Text
           )
       )
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaFunctionScheduleFailedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lError' - The error code of the failure.
--
-- * 'lCause' - A more detailed explanation of the cause of the failure.
lambdaFunctionScheduleFailedEventDetails ::
  LambdaFunctionScheduleFailedEventDetails
lambdaFunctionScheduleFailedEventDetails =
  LambdaFunctionScheduleFailedEventDetails'
    { _lError = Nothing,
      _lCause = Nothing
    }

-- | The error code of the failure.
lError :: Lens' LambdaFunctionScheduleFailedEventDetails (Maybe Text)
lError = lens _lError (\s a -> s {_lError = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the failure.
lCause :: Lens' LambdaFunctionScheduleFailedEventDetails (Maybe Text)
lCause = lens _lCause (\s a -> s {_lCause = a}) . mapping _Sensitive

instance FromJSON LambdaFunctionScheduleFailedEventDetails where
  parseJSON =
    withObject
      "LambdaFunctionScheduleFailedEventDetails"
      ( \x ->
          LambdaFunctionScheduleFailedEventDetails'
            <$> (x .:? "error") <*> (x .:? "cause")
      )

instance Hashable LambdaFunctionScheduleFailedEventDetails

instance NFData LambdaFunctionScheduleFailedEventDetails
