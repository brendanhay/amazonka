{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivitySucceededEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivitySucceededEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about an activity that successfully terminated during an execution.
--
--
--
-- /See:/ 'activitySucceededEventDetails' smart constructor.
data ActivitySucceededEventDetails = ActivitySucceededEventDetails'
  { _asedOutput ::
      !(Maybe (Sensitive Text)),
    _asedOutputDetails ::
      !( Maybe
           HistoryEventExecutionDataDetails
       )
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivitySucceededEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asedOutput' - The JSON data output by the activity task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- * 'asedOutputDetails' - Contains details about the output of an execution history event.
activitySucceededEventDetails ::
  ActivitySucceededEventDetails
activitySucceededEventDetails =
  ActivitySucceededEventDetails'
    { _asedOutput = Nothing,
      _asedOutputDetails = Nothing
    }

-- | The JSON data output by the activity task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
asedOutput :: Lens' ActivitySucceededEventDetails (Maybe Text)
asedOutput = lens _asedOutput (\s a -> s {_asedOutput = a}) . mapping _Sensitive

-- | Contains details about the output of an execution history event.
asedOutputDetails :: Lens' ActivitySucceededEventDetails (Maybe HistoryEventExecutionDataDetails)
asedOutputDetails = lens _asedOutputDetails (\s a -> s {_asedOutputDetails = a})

instance FromJSON ActivitySucceededEventDetails where
  parseJSON =
    withObject
      "ActivitySucceededEventDetails"
      ( \x ->
          ActivitySucceededEventDetails'
            <$> (x .:? "output") <*> (x .:? "outputDetails")
      )

instance Hashable ActivitySucceededEventDetails

instance NFData ActivitySucceededEventDetails
