{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionFailedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about a lambda function that failed during an execution.
--
--
--
-- /See:/ 'lambdaFunctionFailedEventDetails' smart constructor.
data LambdaFunctionFailedEventDetails = LambdaFunctionFailedEventDetails'
  { _lffedError ::
      !(Maybe (Sensitive Text)),
    _lffedCause ::
      !(Maybe (Sensitive Text))
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaFunctionFailedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lffedError' - The error code of the failure.
--
-- * 'lffedCause' - A more detailed explanation of the cause of the failure.
lambdaFunctionFailedEventDetails ::
  LambdaFunctionFailedEventDetails
lambdaFunctionFailedEventDetails =
  LambdaFunctionFailedEventDetails'
    { _lffedError = Nothing,
      _lffedCause = Nothing
    }

-- | The error code of the failure.
lffedError :: Lens' LambdaFunctionFailedEventDetails (Maybe Text)
lffedError = lens _lffedError (\s a -> s {_lffedError = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the failure.
lffedCause :: Lens' LambdaFunctionFailedEventDetails (Maybe Text)
lffedCause = lens _lffedCause (\s a -> s {_lffedCause = a}) . mapping _Sensitive

instance FromJSON LambdaFunctionFailedEventDetails where
  parseJSON =
    withObject
      "LambdaFunctionFailedEventDetails"
      ( \x ->
          LambdaFunctionFailedEventDetails'
            <$> (x .:? "error") <*> (x .:? "cause")
      )

instance Hashable LambdaFunctionFailedEventDetails

instance NFData LambdaFunctionFailedEventDetails
