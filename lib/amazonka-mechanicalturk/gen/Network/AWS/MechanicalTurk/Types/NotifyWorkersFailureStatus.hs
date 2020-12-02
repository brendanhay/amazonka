{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureStatus where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureCode
import Network.AWS.Prelude

-- | When MTurk encounters an issue with notifying the Workers you specified, it returns back this object with failure details.
--
--
--
-- /See:/ 'notifyWorkersFailureStatus' smart constructor.
data NotifyWorkersFailureStatus = NotifyWorkersFailureStatus'
  { _nwfsNotifyWorkersFailureMessage ::
      !(Maybe Text),
    _nwfsNotifyWorkersFailureCode ::
      !(Maybe NotifyWorkersFailureCode),
    _nwfsWorkerId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotifyWorkersFailureStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nwfsNotifyWorkersFailureMessage' - A message detailing the reason the Worker could not be notified.
--
-- * 'nwfsNotifyWorkersFailureCode' - Encoded value for the failure type.
--
-- * 'nwfsWorkerId' - The ID of the Worker.
notifyWorkersFailureStatus ::
  NotifyWorkersFailureStatus
notifyWorkersFailureStatus =
  NotifyWorkersFailureStatus'
    { _nwfsNotifyWorkersFailureMessage =
        Nothing,
      _nwfsNotifyWorkersFailureCode = Nothing,
      _nwfsWorkerId = Nothing
    }

-- | A message detailing the reason the Worker could not be notified.
nwfsNotifyWorkersFailureMessage :: Lens' NotifyWorkersFailureStatus (Maybe Text)
nwfsNotifyWorkersFailureMessage = lens _nwfsNotifyWorkersFailureMessage (\s a -> s {_nwfsNotifyWorkersFailureMessage = a})

-- | Encoded value for the failure type.
nwfsNotifyWorkersFailureCode :: Lens' NotifyWorkersFailureStatus (Maybe NotifyWorkersFailureCode)
nwfsNotifyWorkersFailureCode = lens _nwfsNotifyWorkersFailureCode (\s a -> s {_nwfsNotifyWorkersFailureCode = a})

-- | The ID of the Worker.
nwfsWorkerId :: Lens' NotifyWorkersFailureStatus (Maybe Text)
nwfsWorkerId = lens _nwfsWorkerId (\s a -> s {_nwfsWorkerId = a})

instance FromJSON NotifyWorkersFailureStatus where
  parseJSON =
    withObject
      "NotifyWorkersFailureStatus"
      ( \x ->
          NotifyWorkersFailureStatus'
            <$> (x .:? "NotifyWorkersFailureMessage")
            <*> (x .:? "NotifyWorkersFailureCode")
            <*> (x .:? "WorkerId")
      )

instance Hashable NotifyWorkersFailureStatus

instance NFData NotifyWorkersFailureStatus
