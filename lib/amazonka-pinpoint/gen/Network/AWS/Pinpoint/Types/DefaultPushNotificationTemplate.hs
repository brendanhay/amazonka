{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DefaultPushNotificationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.DefaultPushNotificationTemplate where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Action
import Network.AWS.Prelude

-- | Specifies the default settings and content for a message template that can be used in messages that are sent through a push notification channel.
--
--
--
-- /See:/ 'defaultPushNotificationTemplate' smart constructor.
data DefaultPushNotificationTemplate = DefaultPushNotificationTemplate'
  { _dpntBody ::
      !(Maybe Text),
    _dpntURL :: !(Maybe Text),
    _dpntSound :: !(Maybe Text),
    _dpntAction ::
      !(Maybe Action),
    _dpntTitle :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DefaultPushNotificationTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpntBody' - The message body to use in push notifications that are based on the message template.
--
-- * 'dpntURL' - The URL to open in a recipient's default mobile browser, if a recipient taps a push notification that's based on the message template and the value of the Action property is URL.
--
-- * 'dpntSound' - The sound to play when a recipient receives a push notification that's based on the message template. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/. For an iOS platform, this value is the key for the name of a sound file in your app's main bundle or the Library/Sounds folder in your app's data container. If the sound file can't be found or you specify default for the value, the system plays the default alert sound.
--
-- * 'dpntAction' - The action to occur if a recipient taps a push notification that's based on the message template. Valid values are:     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This setting uses the deep-linking features of the iOS and Android platforms.     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
-- * 'dpntTitle' - The title to use in push notifications that are based on the message template. This title appears above the notification message on a recipient's device.
defaultPushNotificationTemplate ::
  DefaultPushNotificationTemplate
defaultPushNotificationTemplate =
  DefaultPushNotificationTemplate'
    { _dpntBody = Nothing,
      _dpntURL = Nothing,
      _dpntSound = Nothing,
      _dpntAction = Nothing,
      _dpntTitle = Nothing
    }

-- | The message body to use in push notifications that are based on the message template.
dpntBody :: Lens' DefaultPushNotificationTemplate (Maybe Text)
dpntBody = lens _dpntBody (\s a -> s {_dpntBody = a})

-- | The URL to open in a recipient's default mobile browser, if a recipient taps a push notification that's based on the message template and the value of the Action property is URL.
dpntURL :: Lens' DefaultPushNotificationTemplate (Maybe Text)
dpntURL = lens _dpntURL (\s a -> s {_dpntURL = a})

-- | The sound to play when a recipient receives a push notification that's based on the message template. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/. For an iOS platform, this value is the key for the name of a sound file in your app's main bundle or the Library/Sounds folder in your app's data container. If the sound file can't be found or you specify default for the value, the system plays the default alert sound.
dpntSound :: Lens' DefaultPushNotificationTemplate (Maybe Text)
dpntSound = lens _dpntSound (\s a -> s {_dpntSound = a})

-- | The action to occur if a recipient taps a push notification that's based on the message template. Valid values are:     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This setting uses the deep-linking features of the iOS and Android platforms.     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
dpntAction :: Lens' DefaultPushNotificationTemplate (Maybe Action)
dpntAction = lens _dpntAction (\s a -> s {_dpntAction = a})

-- | The title to use in push notifications that are based on the message template. This title appears above the notification message on a recipient's device.
dpntTitle :: Lens' DefaultPushNotificationTemplate (Maybe Text)
dpntTitle = lens _dpntTitle (\s a -> s {_dpntTitle = a})

instance FromJSON DefaultPushNotificationTemplate where
  parseJSON =
    withObject
      "DefaultPushNotificationTemplate"
      ( \x ->
          DefaultPushNotificationTemplate'
            <$> (x .:? "Body")
            <*> (x .:? "Url")
            <*> (x .:? "Sound")
            <*> (x .:? "Action")
            <*> (x .:? "Title")
      )

instance Hashable DefaultPushNotificationTemplate

instance NFData DefaultPushNotificationTemplate

instance ToJSON DefaultPushNotificationTemplate where
  toJSON DefaultPushNotificationTemplate' {..} =
    object
      ( catMaybes
          [ ("Body" .=) <$> _dpntBody,
            ("Url" .=) <$> _dpntURL,
            ("Sound" .=) <$> _dpntSound,
            ("Action" .=) <$> _dpntAction,
            ("Title" .=) <$> _dpntTitle
          ]
      )
