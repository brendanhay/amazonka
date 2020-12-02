{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionSucceededEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionSucceededEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about a lambda function that successfully terminated during an execution.
--
--
--
-- /See:/ 'lambdaFunctionSucceededEventDetails' smart constructor.
data LambdaFunctionSucceededEventDetails = LambdaFunctionSucceededEventDetails'
  { _lfsedOutput ::
      !( Maybe
           ( Sensitive
               Text
           )
       ),
    _lfsedOutputDetails ::
      !( Maybe
           HistoryEventExecutionDataDetails
       )
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaFunctionSucceededEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfsedOutput' - The JSON data output by the lambda function. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- * 'lfsedOutputDetails' - Contains details about the output of an execution history event.
lambdaFunctionSucceededEventDetails ::
  LambdaFunctionSucceededEventDetails
lambdaFunctionSucceededEventDetails =
  LambdaFunctionSucceededEventDetails'
    { _lfsedOutput = Nothing,
      _lfsedOutputDetails = Nothing
    }

-- | The JSON data output by the lambda function. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
lfsedOutput :: Lens' LambdaFunctionSucceededEventDetails (Maybe Text)
lfsedOutput = lens _lfsedOutput (\s a -> s {_lfsedOutput = a}) . mapping _Sensitive

-- | Contains details about the output of an execution history event.
lfsedOutputDetails :: Lens' LambdaFunctionSucceededEventDetails (Maybe HistoryEventExecutionDataDetails)
lfsedOutputDetails = lens _lfsedOutputDetails (\s a -> s {_lfsedOutputDetails = a})

instance FromJSON LambdaFunctionSucceededEventDetails where
  parseJSON =
    withObject
      "LambdaFunctionSucceededEventDetails"
      ( \x ->
          LambdaFunctionSucceededEventDetails'
            <$> (x .:? "output") <*> (x .:? "outputDetails")
      )

instance Hashable LambdaFunctionSucceededEventDetails

instance NFData LambdaFunctionSucceededEventDetails
