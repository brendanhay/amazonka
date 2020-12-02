{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionStartFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionStartFailedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about a lambda function that failed to start during an execution.
--
--
--
-- /See:/ 'lambdaFunctionStartFailedEventDetails' smart constructor.
data LambdaFunctionStartFailedEventDetails = LambdaFunctionStartFailedEventDetails'
  { _lfsfedError ::
      !( Maybe
           ( Sensitive
               Text
           )
       ),
    _lfsfedCause ::
      !( Maybe
           ( Sensitive
               Text
           )
       )
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaFunctionStartFailedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfsfedError' - The error code of the failure.
--
-- * 'lfsfedCause' - A more detailed explanation of the cause of the failure.
lambdaFunctionStartFailedEventDetails ::
  LambdaFunctionStartFailedEventDetails
lambdaFunctionStartFailedEventDetails =
  LambdaFunctionStartFailedEventDetails'
    { _lfsfedError = Nothing,
      _lfsfedCause = Nothing
    }

-- | The error code of the failure.
lfsfedError :: Lens' LambdaFunctionStartFailedEventDetails (Maybe Text)
lfsfedError = lens _lfsfedError (\s a -> s {_lfsfedError = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the failure.
lfsfedCause :: Lens' LambdaFunctionStartFailedEventDetails (Maybe Text)
lfsfedCause = lens _lfsfedCause (\s a -> s {_lfsfedCause = a}) . mapping _Sensitive

instance FromJSON LambdaFunctionStartFailedEventDetails where
  parseJSON =
    withObject
      "LambdaFunctionStartFailedEventDetails"
      ( \x ->
          LambdaFunctionStartFailedEventDetails'
            <$> (x .:? "error") <*> (x .:? "cause")
      )

instance Hashable LambdaFunctionStartFailedEventDetails

instance NFData LambdaFunctionStartFailedEventDetails
