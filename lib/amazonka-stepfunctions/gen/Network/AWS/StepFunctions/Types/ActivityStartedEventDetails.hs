{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityStartedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityStartedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about the start of an activity during an execution.
--
--
--
-- /See:/ 'activityStartedEventDetails' smart constructor.
newtype ActivityStartedEventDetails = ActivityStartedEventDetails'
  { _asedWorkerName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivityStartedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asedWorkerName' - The name of the worker that the task is assigned to. These names are provided by the workers when calling 'GetActivityTask' .
activityStartedEventDetails ::
  ActivityStartedEventDetails
activityStartedEventDetails =
  ActivityStartedEventDetails' {_asedWorkerName = Nothing}

-- | The name of the worker that the task is assigned to. These names are provided by the workers when calling 'GetActivityTask' .
asedWorkerName :: Lens' ActivityStartedEventDetails (Maybe Text)
asedWorkerName = lens _asedWorkerName (\s a -> s {_asedWorkerName = a})

instance FromJSON ActivityStartedEventDetails where
  parseJSON =
    withObject
      "ActivityStartedEventDetails"
      (\x -> ActivityStartedEventDetails' <$> (x .:? "workerName"))

instance Hashable ActivityStartedEventDetails

instance NFData ActivityStartedEventDetails
