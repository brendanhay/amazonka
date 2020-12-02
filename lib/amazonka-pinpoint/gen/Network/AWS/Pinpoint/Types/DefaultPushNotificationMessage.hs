{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DefaultPushNotificationMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.DefaultPushNotificationMessage where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Action
import Network.AWS.Prelude

-- | Specifies the default settings and content for a push notification that's sent directly to an endpoint.
--
--
--
-- /See:/ 'defaultPushNotificationMessage' smart constructor.
data DefaultPushNotificationMessage = DefaultPushNotificationMessage'
  { _dpnmSubstitutions ::
      !(Maybe (Map Text ([Text]))),
    _dpnmSilentPush ::
      !(Maybe Bool),
    _dpnmData ::
      !(Maybe (Map Text (Text))),
    _dpnmBody :: !(Maybe Text),
    _dpnmURL :: !(Maybe Text),
    _dpnmAction ::
      !(Maybe Action),
    _dpnmTitle :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DefaultPushNotificationMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpnmSubstitutions' - The default message variables to use in the notification message. You can override the default variables with individual address variables.
--
-- * 'dpnmSilentPush' - Specifies whether the default notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or delivering messages to an in-app notification center.
--
-- * 'dpnmData' - The JSON data payload to use for the default push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
--
-- * 'dpnmBody' - The default body of the notification message.
--
-- * 'dpnmURL' - The default URL to open in a recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- * 'dpnmAction' - The default action to occur if a recipient taps the push notification. Valid values are:     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This setting uses the deep-linking features of the iOS and Android platforms.     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
-- * 'dpnmTitle' - The default title to display above the notification message on a recipient's device.
defaultPushNotificationMessage ::
  DefaultPushNotificationMessage
defaultPushNotificationMessage =
  DefaultPushNotificationMessage'
    { _dpnmSubstitutions = Nothing,
      _dpnmSilentPush = Nothing,
      _dpnmData = Nothing,
      _dpnmBody = Nothing,
      _dpnmURL = Nothing,
      _dpnmAction = Nothing,
      _dpnmTitle = Nothing
    }

-- | The default message variables to use in the notification message. You can override the default variables with individual address variables.
dpnmSubstitutions :: Lens' DefaultPushNotificationMessage (HashMap Text ([Text]))
dpnmSubstitutions = lens _dpnmSubstitutions (\s a -> s {_dpnmSubstitutions = a}) . _Default . _Map

-- | Specifies whether the default notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or delivering messages to an in-app notification center.
dpnmSilentPush :: Lens' DefaultPushNotificationMessage (Maybe Bool)
dpnmSilentPush = lens _dpnmSilentPush (\s a -> s {_dpnmSilentPush = a})

-- | The JSON data payload to use for the default push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
dpnmData :: Lens' DefaultPushNotificationMessage (HashMap Text (Text))
dpnmData = lens _dpnmData (\s a -> s {_dpnmData = a}) . _Default . _Map

-- | The default body of the notification message.
dpnmBody :: Lens' DefaultPushNotificationMessage (Maybe Text)
dpnmBody = lens _dpnmBody (\s a -> s {_dpnmBody = a})

-- | The default URL to open in a recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
dpnmURL :: Lens' DefaultPushNotificationMessage (Maybe Text)
dpnmURL = lens _dpnmURL (\s a -> s {_dpnmURL = a})

-- | The default action to occur if a recipient taps the push notification. Valid values are:     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This setting uses the deep-linking features of the iOS and Android platforms.     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
dpnmAction :: Lens' DefaultPushNotificationMessage (Maybe Action)
dpnmAction = lens _dpnmAction (\s a -> s {_dpnmAction = a})

-- | The default title to display above the notification message on a recipient's device.
dpnmTitle :: Lens' DefaultPushNotificationMessage (Maybe Text)
dpnmTitle = lens _dpnmTitle (\s a -> s {_dpnmTitle = a})

instance Hashable DefaultPushNotificationMessage

instance NFData DefaultPushNotificationMessage

instance ToJSON DefaultPushNotificationMessage where
  toJSON DefaultPushNotificationMessage' {..} =
    object
      ( catMaybes
          [ ("Substitutions" .=) <$> _dpnmSubstitutions,
            ("SilentPush" .=) <$> _dpnmSilentPush,
            ("Data" .=) <$> _dpnmData,
            ("Body" .=) <$> _dpnmBody,
            ("Url" .=) <$> _dpnmURL,
            ("Action" .=) <$> _dpnmAction,
            ("Title" .=) <$> _dpnmTitle
          ]
      )
