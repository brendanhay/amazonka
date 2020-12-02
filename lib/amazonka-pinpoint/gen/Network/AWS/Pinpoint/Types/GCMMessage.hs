{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.GCMMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.GCMMessage where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Action
import Network.AWS.Prelude

-- | Specifies the settings for a one-time message that's sent directly to an endpoint through the GCM channel. The GCM channel enables Amazon Pinpoint to send messages to the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
--
--
--
-- /See:/ 'gcmMessage' smart constructor.
data GCMMessage = GCMMessage'
  { _gmSubstitutions ::
      !(Maybe (Map Text ([Text]))),
    _gmSilentPush :: !(Maybe Bool),
    _gmImageIconURL :: !(Maybe Text),
    _gmPriority :: !(Maybe Text),
    _gmRawContent :: !(Maybe Text),
    _gmData :: !(Maybe (Map Text (Text))),
    _gmRestrictedPackageName :: !(Maybe Text),
    _gmSmallImageIconURL :: !(Maybe Text),
    _gmBody :: !(Maybe Text),
    _gmTimeToLive :: !(Maybe Int),
    _gmURL :: !(Maybe Text),
    _gmSound :: !(Maybe Text),
    _gmAction :: !(Maybe Action),
    _gmCollapseKey :: !(Maybe Text),
    _gmImageURL :: !(Maybe Text),
    _gmTitle :: !(Maybe Text),
    _gmIconReference :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GCMMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmSubstitutions' - The default message variables to use in the notification message. You can override the default variables with individual address variables.
--
-- * 'gmSilentPush' - Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
--
-- * 'gmImageIconURL' - The URL of the large icon image to display in the content view of the push notification.
--
-- * 'gmPriority' - para>normal - The notification might be delayed. Delivery is optimized for battery usage on the recipient's device. Use this value unless immediate delivery is required. /listitem>     * high - The notification is sent immediately and might wake a sleeping device. /para> Amazon Pinpoint specifies this value in the FCM priority parameter when it sends the notification message to FCM. The equivalent values for Apple Push Notification service (APNs) are 5, for normal, and 10, for high. If you specify an APNs value for this property, Amazon Pinpoint accepts and converts the value to the corresponding FCM value.
--
-- * 'gmRawContent' - The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
--
-- * 'gmData' - The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
--
-- * 'gmRestrictedPackageName' - The package name of the application where registration tokens must match in order for the recipient to receive the message.
--
-- * 'gmSmallImageIconURL' - The URL of the small icon image to display in the status bar and the content view of the push notification.
--
-- * 'gmBody' - The body of the notification message.
--
-- * 'gmTimeToLive' - The amount of time, in seconds, that FCM should store and attempt to deliver the push notification, if the service is unable to deliver the notification the first time. If you don't specify this value, FCM defaults to the maximum value, which is 2,419,200 seconds (28 days). Amazon Pinpoint specifies this value in the FCM time_to_live parameter when it sends the notification message to FCM.
--
-- * 'gmURL' - The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- * 'gmSound' - The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
--
-- * 'gmAction' - The action to occur if the recipient taps the push notification. Valid values are:     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This action uses the deep-linking features of the Android platform.     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
-- * 'gmCollapseKey' - An arbitrary string that identifies a group of messages that can be collapsed to ensure that only the last message is sent when delivery can resume. This helps avoid sending too many instances of the same messages when the recipient's device comes online again or becomes active. Amazon Pinpoint specifies this value in the Firebase Cloud Messaging (FCM) collapse_key parameter when it sends the notification message to FCM.
--
-- * 'gmImageURL' - The URL of an image to display in the push notification.
--
-- * 'gmTitle' - The title to display above the notification message on the recipient's device.
--
-- * 'gmIconReference' - The icon image name of the asset saved in your app.
gcmMessage ::
  GCMMessage
gcmMessage =
  GCMMessage'
    { _gmSubstitutions = Nothing,
      _gmSilentPush = Nothing,
      _gmImageIconURL = Nothing,
      _gmPriority = Nothing,
      _gmRawContent = Nothing,
      _gmData = Nothing,
      _gmRestrictedPackageName = Nothing,
      _gmSmallImageIconURL = Nothing,
      _gmBody = Nothing,
      _gmTimeToLive = Nothing,
      _gmURL = Nothing,
      _gmSound = Nothing,
      _gmAction = Nothing,
      _gmCollapseKey = Nothing,
      _gmImageURL = Nothing,
      _gmTitle = Nothing,
      _gmIconReference = Nothing
    }

-- | The default message variables to use in the notification message. You can override the default variables with individual address variables.
gmSubstitutions :: Lens' GCMMessage (HashMap Text ([Text]))
gmSubstitutions = lens _gmSubstitutions (\s a -> s {_gmSubstitutions = a}) . _Default . _Map

-- | Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
gmSilentPush :: Lens' GCMMessage (Maybe Bool)
gmSilentPush = lens _gmSilentPush (\s a -> s {_gmSilentPush = a})

-- | The URL of the large icon image to display in the content view of the push notification.
gmImageIconURL :: Lens' GCMMessage (Maybe Text)
gmImageIconURL = lens _gmImageIconURL (\s a -> s {_gmImageIconURL = a})

-- | para>normal - The notification might be delayed. Delivery is optimized for battery usage on the recipient's device. Use this value unless immediate delivery is required. /listitem>     * high - The notification is sent immediately and might wake a sleeping device. /para> Amazon Pinpoint specifies this value in the FCM priority parameter when it sends the notification message to FCM. The equivalent values for Apple Push Notification service (APNs) are 5, for normal, and 10, for high. If you specify an APNs value for this property, Amazon Pinpoint accepts and converts the value to the corresponding FCM value.
gmPriority :: Lens' GCMMessage (Maybe Text)
gmPriority = lens _gmPriority (\s a -> s {_gmPriority = a})

-- | The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
gmRawContent :: Lens' GCMMessage (Maybe Text)
gmRawContent = lens _gmRawContent (\s a -> s {_gmRawContent = a})

-- | The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
gmData :: Lens' GCMMessage (HashMap Text (Text))
gmData = lens _gmData (\s a -> s {_gmData = a}) . _Default . _Map

-- | The package name of the application where registration tokens must match in order for the recipient to receive the message.
gmRestrictedPackageName :: Lens' GCMMessage (Maybe Text)
gmRestrictedPackageName = lens _gmRestrictedPackageName (\s a -> s {_gmRestrictedPackageName = a})

-- | The URL of the small icon image to display in the status bar and the content view of the push notification.
gmSmallImageIconURL :: Lens' GCMMessage (Maybe Text)
gmSmallImageIconURL = lens _gmSmallImageIconURL (\s a -> s {_gmSmallImageIconURL = a})

-- | The body of the notification message.
gmBody :: Lens' GCMMessage (Maybe Text)
gmBody = lens _gmBody (\s a -> s {_gmBody = a})

-- | The amount of time, in seconds, that FCM should store and attempt to deliver the push notification, if the service is unable to deliver the notification the first time. If you don't specify this value, FCM defaults to the maximum value, which is 2,419,200 seconds (28 days). Amazon Pinpoint specifies this value in the FCM time_to_live parameter when it sends the notification message to FCM.
gmTimeToLive :: Lens' GCMMessage (Maybe Int)
gmTimeToLive = lens _gmTimeToLive (\s a -> s {_gmTimeToLive = a})

-- | The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
gmURL :: Lens' GCMMessage (Maybe Text)
gmURL = lens _gmURL (\s a -> s {_gmURL = a})

-- | The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
gmSound :: Lens' GCMMessage (Maybe Text)
gmSound = lens _gmSound (\s a -> s {_gmSound = a})

-- | The action to occur if the recipient taps the push notification. Valid values are:     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This action uses the deep-linking features of the Android platform.     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
gmAction :: Lens' GCMMessage (Maybe Action)
gmAction = lens _gmAction (\s a -> s {_gmAction = a})

-- | An arbitrary string that identifies a group of messages that can be collapsed to ensure that only the last message is sent when delivery can resume. This helps avoid sending too many instances of the same messages when the recipient's device comes online again or becomes active. Amazon Pinpoint specifies this value in the Firebase Cloud Messaging (FCM) collapse_key parameter when it sends the notification message to FCM.
gmCollapseKey :: Lens' GCMMessage (Maybe Text)
gmCollapseKey = lens _gmCollapseKey (\s a -> s {_gmCollapseKey = a})

-- | The URL of an image to display in the push notification.
gmImageURL :: Lens' GCMMessage (Maybe Text)
gmImageURL = lens _gmImageURL (\s a -> s {_gmImageURL = a})

-- | The title to display above the notification message on the recipient's device.
gmTitle :: Lens' GCMMessage (Maybe Text)
gmTitle = lens _gmTitle (\s a -> s {_gmTitle = a})

-- | The icon image name of the asset saved in your app.
gmIconReference :: Lens' GCMMessage (Maybe Text)
gmIconReference = lens _gmIconReference (\s a -> s {_gmIconReference = a})

instance Hashable GCMMessage

instance NFData GCMMessage

instance ToJSON GCMMessage where
  toJSON GCMMessage' {..} =
    object
      ( catMaybes
          [ ("Substitutions" .=) <$> _gmSubstitutions,
            ("SilentPush" .=) <$> _gmSilentPush,
            ("ImageIconUrl" .=) <$> _gmImageIconURL,
            ("Priority" .=) <$> _gmPriority,
            ("RawContent" .=) <$> _gmRawContent,
            ("Data" .=) <$> _gmData,
            ("RestrictedPackageName" .=) <$> _gmRestrictedPackageName,
            ("SmallImageIconUrl" .=) <$> _gmSmallImageIconURL,
            ("Body" .=) <$> _gmBody,
            ("TimeToLive" .=) <$> _gmTimeToLive,
            ("Url" .=) <$> _gmURL,
            ("Sound" .=) <$> _gmSound,
            ("Action" .=) <$> _gmAction,
            ("CollapseKey" .=) <$> _gmCollapseKey,
            ("ImageUrl" .=) <$> _gmImageURL,
            ("Title" .=) <$> _gmTitle,
            ("IconReference" .=) <$> _gmIconReference
          ]
      )
