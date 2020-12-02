{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionSucceededEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionSucceededEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about the successful termination of the execution.
--
--
--
-- /See:/ 'executionSucceededEventDetails' smart constructor.
data ExecutionSucceededEventDetails = ExecutionSucceededEventDetails'
  { _esedOutput ::
      !(Maybe (Sensitive Text)),
    _esedOutputDetails ::
      !( Maybe
           HistoryEventExecutionDataDetails
       )
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecutionSucceededEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esedOutput' - The JSON data output by the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- * 'esedOutputDetails' - Contains details about the output of an execution history event.
executionSucceededEventDetails ::
  ExecutionSucceededEventDetails
executionSucceededEventDetails =
  ExecutionSucceededEventDetails'
    { _esedOutput = Nothing,
      _esedOutputDetails = Nothing
    }

-- | The JSON data output by the execution. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
esedOutput :: Lens' ExecutionSucceededEventDetails (Maybe Text)
esedOutput = lens _esedOutput (\s a -> s {_esedOutput = a}) . mapping _Sensitive

-- | Contains details about the output of an execution history event.
esedOutputDetails :: Lens' ExecutionSucceededEventDetails (Maybe HistoryEventExecutionDataDetails)
esedOutputDetails = lens _esedOutputDetails (\s a -> s {_esedOutputDetails = a})

instance FromJSON ExecutionSucceededEventDetails where
  parseJSON =
    withObject
      "ExecutionSucceededEventDetails"
      ( \x ->
          ExecutionSucceededEventDetails'
            <$> (x .:? "output") <*> (x .:? "outputDetails")
      )

instance Hashable ExecutionSucceededEventDetails

instance NFData ExecutionSucceededEventDetails
