{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionStartedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionStartedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about the start of the execution.
--
--
--
-- /See:/ 'executionStartedEventDetails' smart constructor.
data ExecutionStartedEventDetails = ExecutionStartedEventDetails'
  { _esedInputDetails ::
      !( Maybe
           HistoryEventExecutionDataDetails
       ),
    _esedInput ::
      !(Maybe (Sensitive Text)),
    _esedRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecutionStartedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esedInputDetails' - Contains details about the input for an execution history event.
--
-- * 'esedInput' - The JSON data input to the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- * 'esedRoleARN' - The Amazon Resource Name (ARN) of the IAM role used for executing AWS Lambda tasks.
executionStartedEventDetails ::
  ExecutionStartedEventDetails
executionStartedEventDetails =
  ExecutionStartedEventDetails'
    { _esedInputDetails = Nothing,
      _esedInput = Nothing,
      _esedRoleARN = Nothing
    }

-- | Contains details about the input for an execution history event.
esedInputDetails :: Lens' ExecutionStartedEventDetails (Maybe HistoryEventExecutionDataDetails)
esedInputDetails = lens _esedInputDetails (\s a -> s {_esedInputDetails = a})

-- | The JSON data input to the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
esedInput :: Lens' ExecutionStartedEventDetails (Maybe Text)
esedInput = lens _esedInput (\s a -> s {_esedInput = a}) . mapping _Sensitive

-- | The Amazon Resource Name (ARN) of the IAM role used for executing AWS Lambda tasks.
esedRoleARN :: Lens' ExecutionStartedEventDetails (Maybe Text)
esedRoleARN = lens _esedRoleARN (\s a -> s {_esedRoleARN = a})

instance FromJSON ExecutionStartedEventDetails where
  parseJSON =
    withObject
      "ExecutionStartedEventDetails"
      ( \x ->
          ExecutionStartedEventDetails'
            <$> (x .:? "inputDetails") <*> (x .:? "input") <*> (x .:? "roleArn")
      )

instance Hashable ExecutionStartedEventDetails

instance NFData ExecutionStartedEventDetails
