{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Message
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Message where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Action
import Network.AWS.Prelude

-- | Specifies the content and settings for a push notification that's sent to recipients of a campaign.
--
--
--
-- /See:/ 'message' smart constructor.
data Message = Message'
  { _mSilentPush :: !(Maybe Bool),
    _mImageIconURL :: !(Maybe Text),
    _mRawContent :: !(Maybe Text),
    _mBody :: !(Maybe Text),
    _mTimeToLive :: !(Maybe Int),
    _mImageSmallIconURL :: !(Maybe Text),
    _mJSONBody :: !(Maybe Text),
    _mURL :: !(Maybe Text),
    _mAction :: !(Maybe Action),
    _mImageURL :: !(Maybe Text),
    _mMediaURL :: !(Maybe Text),
    _mTitle :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mSilentPush' - Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration, displaying messages in an in-app message center, or supporting phone home functionality.
--
-- * 'mImageIconURL' - The URL of the image to display as the push-notification icon, such as the icon for the app.
--
-- * 'mRawContent' - The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
--
-- * 'mBody' - The body of the notification message. The maximum number of characters is 200.
--
-- * 'mTimeToLive' - The number of seconds that the push-notification service should keep the message, if the service is unable to deliver the notification the first time. This value is converted to an expiration value when it's sent to a push-notification service. If this value is 0, the service treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again. This value doesn't apply to messages that are sent through the Amazon Device Messaging (ADM) service.
--
-- * 'mImageSmallIconURL' - The URL of the image to display as the small, push-notification icon, such as a small version of the icon for the app.
--
-- * 'mJSONBody' - The JSON payload to use for a silent push notification.
--
-- * 'mURL' - The URL to open in a recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- * 'mAction' - The action to occur if a recipient taps the push notification. Valid values are:     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This setting uses the deep-linking features of iOS and Android.     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
-- * 'mImageURL' - The URL of an image to display in the push notification.
--
-- * 'mMediaURL' - The URL of the image or video to display in the push notification.
--
-- * 'mTitle' - The title to display above the notification message on a recipient's device.
message ::
  Message
message =
  Message'
    { _mSilentPush = Nothing,
      _mImageIconURL = Nothing,
      _mRawContent = Nothing,
      _mBody = Nothing,
      _mTimeToLive = Nothing,
      _mImageSmallIconURL = Nothing,
      _mJSONBody = Nothing,
      _mURL = Nothing,
      _mAction = Nothing,
      _mImageURL = Nothing,
      _mMediaURL = Nothing,
      _mTitle = Nothing
    }

-- | Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration, displaying messages in an in-app message center, or supporting phone home functionality.
mSilentPush :: Lens' Message (Maybe Bool)
mSilentPush = lens _mSilentPush (\s a -> s {_mSilentPush = a})

-- | The URL of the image to display as the push-notification icon, such as the icon for the app.
mImageIconURL :: Lens' Message (Maybe Text)
mImageIconURL = lens _mImageIconURL (\s a -> s {_mImageIconURL = a})

-- | The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
mRawContent :: Lens' Message (Maybe Text)
mRawContent = lens _mRawContent (\s a -> s {_mRawContent = a})

-- | The body of the notification message. The maximum number of characters is 200.
mBody :: Lens' Message (Maybe Text)
mBody = lens _mBody (\s a -> s {_mBody = a})

-- | The number of seconds that the push-notification service should keep the message, if the service is unable to deliver the notification the first time. This value is converted to an expiration value when it's sent to a push-notification service. If this value is 0, the service treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again. This value doesn't apply to messages that are sent through the Amazon Device Messaging (ADM) service.
mTimeToLive :: Lens' Message (Maybe Int)
mTimeToLive = lens _mTimeToLive (\s a -> s {_mTimeToLive = a})

-- | The URL of the image to display as the small, push-notification icon, such as a small version of the icon for the app.
mImageSmallIconURL :: Lens' Message (Maybe Text)
mImageSmallIconURL = lens _mImageSmallIconURL (\s a -> s {_mImageSmallIconURL = a})

-- | The JSON payload to use for a silent push notification.
mJSONBody :: Lens' Message (Maybe Text)
mJSONBody = lens _mJSONBody (\s a -> s {_mJSONBody = a})

-- | The URL to open in a recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
mURL :: Lens' Message (Maybe Text)
mURL = lens _mURL (\s a -> s {_mURL = a})

-- | The action to occur if a recipient taps the push notification. Valid values are:     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This setting uses the deep-linking features of iOS and Android.     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
mAction :: Lens' Message (Maybe Action)
mAction = lens _mAction (\s a -> s {_mAction = a})

-- | The URL of an image to display in the push notification.
mImageURL :: Lens' Message (Maybe Text)
mImageURL = lens _mImageURL (\s a -> s {_mImageURL = a})

-- | The URL of the image or video to display in the push notification.
mMediaURL :: Lens' Message (Maybe Text)
mMediaURL = lens _mMediaURL (\s a -> s {_mMediaURL = a})

-- | The title to display above the notification message on a recipient's device.
mTitle :: Lens' Message (Maybe Text)
mTitle = lens _mTitle (\s a -> s {_mTitle = a})

instance FromJSON Message where
  parseJSON =
    withObject
      "Message"
      ( \x ->
          Message'
            <$> (x .:? "SilentPush")
            <*> (x .:? "ImageIconUrl")
            <*> (x .:? "RawContent")
            <*> (x .:? "Body")
            <*> (x .:? "TimeToLive")
            <*> (x .:? "ImageSmallIconUrl")
            <*> (x .:? "JsonBody")
            <*> (x .:? "Url")
            <*> (x .:? "Action")
            <*> (x .:? "ImageUrl")
            <*> (x .:? "MediaUrl")
            <*> (x .:? "Title")
      )

instance Hashable Message

instance NFData Message

instance ToJSON Message where
  toJSON Message' {..} =
    object
      ( catMaybes
          [ ("SilentPush" .=) <$> _mSilentPush,
            ("ImageIconUrl" .=) <$> _mImageIconURL,
            ("RawContent" .=) <$> _mRawContent,
            ("Body" .=) <$> _mBody,
            ("TimeToLive" .=) <$> _mTimeToLive,
            ("ImageSmallIconUrl" .=) <$> _mImageSmallIconURL,
            ("JsonBody" .=) <$> _mJSONBody,
            ("Url" .=) <$> _mURL,
            ("Action" .=) <$> _mAction,
            ("ImageUrl" .=) <$> _mImageURL,
            ("MediaUrl" .=) <$> _mMediaURL,
            ("Title" .=) <$> _mTitle
          ]
      )
