{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityFailedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about an activity that failed during an execution.
--
--
--
-- /See:/ 'activityFailedEventDetails' smart constructor.
data ActivityFailedEventDetails = ActivityFailedEventDetails'
  { _afedError ::
      !(Maybe (Sensitive Text)),
    _afedCause ::
      !(Maybe (Sensitive Text))
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivityFailedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afedError' - The error code of the failure.
--
-- * 'afedCause' - A more detailed explanation of the cause of the failure.
activityFailedEventDetails ::
  ActivityFailedEventDetails
activityFailedEventDetails =
  ActivityFailedEventDetails'
    { _afedError = Nothing,
      _afedCause = Nothing
    }

-- | The error code of the failure.
afedError :: Lens' ActivityFailedEventDetails (Maybe Text)
afedError = lens _afedError (\s a -> s {_afedError = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the failure.
afedCause :: Lens' ActivityFailedEventDetails (Maybe Text)
afedCause = lens _afedCause (\s a -> s {_afedCause = a}) . mapping _Sensitive

instance FromJSON ActivityFailedEventDetails where
  parseJSON =
    withObject
      "ActivityFailedEventDetails"
      ( \x ->
          ActivityFailedEventDetails'
            <$> (x .:? "error") <*> (x .:? "cause")
      )

instance Hashable ActivityFailedEventDetails

instance NFData ActivityFailedEventDetails
