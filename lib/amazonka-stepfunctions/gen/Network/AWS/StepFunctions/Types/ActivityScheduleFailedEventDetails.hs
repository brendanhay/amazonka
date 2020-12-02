{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityScheduleFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityScheduleFailedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about an activity schedule failure that occurred during an execution.
--
--
--
-- /See:/ 'activityScheduleFailedEventDetails' smart constructor.
data ActivityScheduleFailedEventDetails = ActivityScheduleFailedEventDetails'
  { _asfedError ::
      !( Maybe
           (Sensitive Text)
       ),
    _asfedCause ::
      !( Maybe
           (Sensitive Text)
       )
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivityScheduleFailedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asfedError' - The error code of the failure.
--
-- * 'asfedCause' - A more detailed explanation of the cause of the failure.
activityScheduleFailedEventDetails ::
  ActivityScheduleFailedEventDetails
activityScheduleFailedEventDetails =
  ActivityScheduleFailedEventDetails'
    { _asfedError = Nothing,
      _asfedCause = Nothing
    }

-- | The error code of the failure.
asfedError :: Lens' ActivityScheduleFailedEventDetails (Maybe Text)
asfedError = lens _asfedError (\s a -> s {_asfedError = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the failure.
asfedCause :: Lens' ActivityScheduleFailedEventDetails (Maybe Text)
asfedCause = lens _asfedCause (\s a -> s {_asfedCause = a}) . mapping _Sensitive

instance FromJSON ActivityScheduleFailedEventDetails where
  parseJSON =
    withObject
      "ActivityScheduleFailedEventDetails"
      ( \x ->
          ActivityScheduleFailedEventDetails'
            <$> (x .:? "error") <*> (x .:? "cause")
      )

instance Hashable ActivityScheduleFailedEventDetails

instance NFData ActivityScheduleFailedEventDetails
