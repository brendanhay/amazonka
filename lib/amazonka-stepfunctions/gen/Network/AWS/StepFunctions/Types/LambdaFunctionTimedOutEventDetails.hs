{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionTimedOutEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionTimedOutEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about a lambda function timeout that occurred during an execution.
--
--
--
-- /See:/ 'lambdaFunctionTimedOutEventDetails' smart constructor.
data LambdaFunctionTimedOutEventDetails = LambdaFunctionTimedOutEventDetails'
  { _lftoedError ::
      !( Maybe
           (Sensitive Text)
       ),
    _lftoedCause ::
      !( Maybe
           (Sensitive Text)
       )
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaFunctionTimedOutEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lftoedError' - The error code of the failure.
--
-- * 'lftoedCause' - A more detailed explanation of the cause of the timeout.
lambdaFunctionTimedOutEventDetails ::
  LambdaFunctionTimedOutEventDetails
lambdaFunctionTimedOutEventDetails =
  LambdaFunctionTimedOutEventDetails'
    { _lftoedError = Nothing,
      _lftoedCause = Nothing
    }

-- | The error code of the failure.
lftoedError :: Lens' LambdaFunctionTimedOutEventDetails (Maybe Text)
lftoedError = lens _lftoedError (\s a -> s {_lftoedError = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the timeout.
lftoedCause :: Lens' LambdaFunctionTimedOutEventDetails (Maybe Text)
lftoedCause = lens _lftoedCause (\s a -> s {_lftoedCause = a}) . mapping _Sensitive

instance FromJSON LambdaFunctionTimedOutEventDetails where
  parseJSON =
    withObject
      "LambdaFunctionTimedOutEventDetails"
      ( \x ->
          LambdaFunctionTimedOutEventDetails'
            <$> (x .:? "error") <*> (x .:? "cause")
      )

instance Hashable LambdaFunctionTimedOutEventDetails

instance NFData LambdaFunctionTimedOutEventDetails
