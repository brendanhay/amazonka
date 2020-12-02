{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityTimedOutEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityTimedOutEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about an activity timeout that occurred during an execution.
--
--
--
-- /See:/ 'activityTimedOutEventDetails' smart constructor.
data ActivityTimedOutEventDetails = ActivityTimedOutEventDetails'
  { _atoedError ::
      !(Maybe (Sensitive Text)),
    _atoedCause ::
      !(Maybe (Sensitive Text))
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivityTimedOutEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atoedError' - The error code of the failure.
--
-- * 'atoedCause' - A more detailed explanation of the cause of the timeout.
activityTimedOutEventDetails ::
  ActivityTimedOutEventDetails
activityTimedOutEventDetails =
  ActivityTimedOutEventDetails'
    { _atoedError = Nothing,
      _atoedCause = Nothing
    }

-- | The error code of the failure.
atoedError :: Lens' ActivityTimedOutEventDetails (Maybe Text)
atoedError = lens _atoedError (\s a -> s {_atoedError = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the timeout.
atoedCause :: Lens' ActivityTimedOutEventDetails (Maybe Text)
atoedCause = lens _atoedCause (\s a -> s {_atoedCause = a}) . mapping _Sensitive

instance FromJSON ActivityTimedOutEventDetails where
  parseJSON =
    withObject
      "ActivityTimedOutEventDetails"
      ( \x ->
          ActivityTimedOutEventDetails'
            <$> (x .:? "error") <*> (x .:? "cause")
      )

instance Hashable ActivityTimedOutEventDetails

instance NFData ActivityTimedOutEventDetails
