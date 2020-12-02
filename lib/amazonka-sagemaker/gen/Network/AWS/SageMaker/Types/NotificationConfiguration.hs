{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotificationConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configures SNS notifications of available or expiring work items for work teams.
--
--
--
-- /See:/ 'notificationConfiguration' smart constructor.
newtype NotificationConfiguration = NotificationConfiguration'
  { _ncNotificationTopicARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncNotificationTopicARN' - The ARN for the SNS topic to which notifications should be published.
notificationConfiguration ::
  NotificationConfiguration
notificationConfiguration =
  NotificationConfiguration' {_ncNotificationTopicARN = Nothing}

-- | The ARN for the SNS topic to which notifications should be published.
ncNotificationTopicARN :: Lens' NotificationConfiguration (Maybe Text)
ncNotificationTopicARN = lens _ncNotificationTopicARN (\s a -> s {_ncNotificationTopicARN = a})

instance FromJSON NotificationConfiguration where
  parseJSON =
    withObject
      "NotificationConfiguration"
      ( \x ->
          NotificationConfiguration' <$> (x .:? "NotificationTopicArn")
      )

instance Hashable NotificationConfiguration

instance NFData NotificationConfiguration

instance ToJSON NotificationConfiguration where
  toJSON NotificationConfiguration' {..} =
    object
      ( catMaybes
          [("NotificationTopicArn" .=) <$> _ncNotificationTopicARN]
      )
