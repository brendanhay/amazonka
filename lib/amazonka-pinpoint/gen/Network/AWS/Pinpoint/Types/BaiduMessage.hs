{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.BaiduMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.BaiduMessage where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Action
import Network.AWS.Prelude

-- | Specifies the settings for a one-time message that's sent directly to an endpoint through the Baidu (Baidu Cloud Push) channel.
--
--
--
-- /See:/ 'baiduMessage' smart constructor.
data BaiduMessage = BaiduMessage'
  { _bmSubstitutions ::
      !(Maybe (Map Text ([Text]))),
    _bmSilentPush :: !(Maybe Bool),
    _bmImageIconURL :: !(Maybe Text),
    _bmRawContent :: !(Maybe Text),
    _bmData :: !(Maybe (Map Text (Text))),
    _bmSmallImageIconURL :: !(Maybe Text),
    _bmBody :: !(Maybe Text),
    _bmTimeToLive :: !(Maybe Int),
    _bmURL :: !(Maybe Text),
    _bmSound :: !(Maybe Text),
    _bmAction :: !(Maybe Action),
    _bmImageURL :: !(Maybe Text),
    _bmTitle :: !(Maybe Text),
    _bmIconReference :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BaiduMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmSubstitutions' - The default message variables to use in the notification message. You can override the default variables with individual address variables.
--
-- * 'bmSilentPush' - Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
--
-- * 'bmImageIconURL' - The URL of the large icon image to display in the content view of the push notification.
--
-- * 'bmRawContent' - The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
--
-- * 'bmData' - The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
--
-- * 'bmSmallImageIconURL' - The URL of the small icon image to display in the status bar and the content view of the push notification.
--
-- * 'bmBody' - The body of the notification message.
--
-- * 'bmTimeToLive' - The amount of time, in seconds, that the Baidu Cloud Push service should store the message if the recipient's device is offline. The default value and maximum supported time is 604,800 seconds (7 days).
--
-- * 'bmURL' - The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- * 'bmSound' - The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
--
-- * 'bmAction' - The action to occur if the recipient taps the push notification. Valid values are:     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This action uses the deep-linking features of the Android platform.     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
-- * 'bmImageURL' - The URL of an image to display in the push notification.
--
-- * 'bmTitle' - The title to display above the notification message on the recipient's device.
--
-- * 'bmIconReference' - The icon image name of the asset saved in your app.
baiduMessage ::
  BaiduMessage
baiduMessage =
  BaiduMessage'
    { _bmSubstitutions = Nothing,
      _bmSilentPush = Nothing,
      _bmImageIconURL = Nothing,
      _bmRawContent = Nothing,
      _bmData = Nothing,
      _bmSmallImageIconURL = Nothing,
      _bmBody = Nothing,
      _bmTimeToLive = Nothing,
      _bmURL = Nothing,
      _bmSound = Nothing,
      _bmAction = Nothing,
      _bmImageURL = Nothing,
      _bmTitle = Nothing,
      _bmIconReference = Nothing
    }

-- | The default message variables to use in the notification message. You can override the default variables with individual address variables.
bmSubstitutions :: Lens' BaiduMessage (HashMap Text ([Text]))
bmSubstitutions = lens _bmSubstitutions (\s a -> s {_bmSubstitutions = a}) . _Default . _Map

-- | Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
bmSilentPush :: Lens' BaiduMessage (Maybe Bool)
bmSilentPush = lens _bmSilentPush (\s a -> s {_bmSilentPush = a})

-- | The URL of the large icon image to display in the content view of the push notification.
bmImageIconURL :: Lens' BaiduMessage (Maybe Text)
bmImageIconURL = lens _bmImageIconURL (\s a -> s {_bmImageIconURL = a})

-- | The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
bmRawContent :: Lens' BaiduMessage (Maybe Text)
bmRawContent = lens _bmRawContent (\s a -> s {_bmRawContent = a})

-- | The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
bmData :: Lens' BaiduMessage (HashMap Text (Text))
bmData = lens _bmData (\s a -> s {_bmData = a}) . _Default . _Map

-- | The URL of the small icon image to display in the status bar and the content view of the push notification.
bmSmallImageIconURL :: Lens' BaiduMessage (Maybe Text)
bmSmallImageIconURL = lens _bmSmallImageIconURL (\s a -> s {_bmSmallImageIconURL = a})

-- | The body of the notification message.
bmBody :: Lens' BaiduMessage (Maybe Text)
bmBody = lens _bmBody (\s a -> s {_bmBody = a})

-- | The amount of time, in seconds, that the Baidu Cloud Push service should store the message if the recipient's device is offline. The default value and maximum supported time is 604,800 seconds (7 days).
bmTimeToLive :: Lens' BaiduMessage (Maybe Int)
bmTimeToLive = lens _bmTimeToLive (\s a -> s {_bmTimeToLive = a})

-- | The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
bmURL :: Lens' BaiduMessage (Maybe Text)
bmURL = lens _bmURL (\s a -> s {_bmURL = a})

-- | The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
bmSound :: Lens' BaiduMessage (Maybe Text)
bmSound = lens _bmSound (\s a -> s {_bmSound = a})

-- | The action to occur if the recipient taps the push notification. Valid values are:     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This action uses the deep-linking features of the Android platform.     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
bmAction :: Lens' BaiduMessage (Maybe Action)
bmAction = lens _bmAction (\s a -> s {_bmAction = a})

-- | The URL of an image to display in the push notification.
bmImageURL :: Lens' BaiduMessage (Maybe Text)
bmImageURL = lens _bmImageURL (\s a -> s {_bmImageURL = a})

-- | The title to display above the notification message on the recipient's device.
bmTitle :: Lens' BaiduMessage (Maybe Text)
bmTitle = lens _bmTitle (\s a -> s {_bmTitle = a})

-- | The icon image name of the asset saved in your app.
bmIconReference :: Lens' BaiduMessage (Maybe Text)
bmIconReference = lens _bmIconReference (\s a -> s {_bmIconReference = a})

instance Hashable BaiduMessage

instance NFData BaiduMessage

instance ToJSON BaiduMessage where
  toJSON BaiduMessage' {..} =
    object
      ( catMaybes
          [ ("Substitutions" .=) <$> _bmSubstitutions,
            ("SilentPush" .=) <$> _bmSilentPush,
            ("ImageIconUrl" .=) <$> _bmImageIconURL,
            ("RawContent" .=) <$> _bmRawContent,
            ("Data" .=) <$> _bmData,
            ("SmallImageIconUrl" .=) <$> _bmSmallImageIconURL,
            ("Body" .=) <$> _bmBody,
            ("TimeToLive" .=) <$> _bmTimeToLive,
            ("Url" .=) <$> _bmURL,
            ("Sound" .=) <$> _bmSound,
            ("Action" .=) <$> _bmAction,
            ("ImageUrl" .=) <$> _bmImageURL,
            ("Title" .=) <$> _bmTitle,
            ("IconReference" .=) <$> _bmIconReference
          ]
      )
