{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.NotificationProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.NotificationProperty where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies configuration properties of a notification.
--
--
--
-- /See:/ 'notificationProperty' smart constructor.
newtype NotificationProperty = NotificationProperty'
  { _npNotifyDelayAfter ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotificationProperty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'npNotifyDelayAfter' - After a job run starts, the number of minutes to wait before sending a job run delay notification.
notificationProperty ::
  NotificationProperty
notificationProperty =
  NotificationProperty' {_npNotifyDelayAfter = Nothing}

-- | After a job run starts, the number of minutes to wait before sending a job run delay notification.
npNotifyDelayAfter :: Lens' NotificationProperty (Maybe Natural)
npNotifyDelayAfter = lens _npNotifyDelayAfter (\s a -> s {_npNotifyDelayAfter = a}) . mapping _Nat

instance FromJSON NotificationProperty where
  parseJSON =
    withObject
      "NotificationProperty"
      (\x -> NotificationProperty' <$> (x .:? "NotifyDelayAfter"))

instance Hashable NotificationProperty

instance NFData NotificationProperty

instance ToJSON NotificationProperty where
  toJSON NotificationProperty' {..} =
    object
      (catMaybes [("NotifyDelayAfter" .=) <$> _npNotifyDelayAfter])
