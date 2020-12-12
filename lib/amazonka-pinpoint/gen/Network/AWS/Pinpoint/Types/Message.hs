{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Message
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Message
  ( Message (..),

    -- * Smart constructor
    mkMessage,

    -- * Lenses
    mSilentPush,
    mImageIconURL,
    mRawContent,
    mBody,
    mTimeToLive,
    mImageSmallIconURL,
    mJSONBody,
    mURL,
    mAction,
    mImageURL,
    mMediaURL,
    mTitle,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action
import qualified Network.AWS.Prelude as Lude

-- | Specifies the content and settings for a push notification that's sent to recipients of a campaign.
--
-- /See:/ 'mkMessage' smart constructor.
data Message = Message'
  { silentPush :: Lude.Maybe Lude.Bool,
    imageIconURL :: Lude.Maybe Lude.Text,
    rawContent :: Lude.Maybe Lude.Text,
    body :: Lude.Maybe Lude.Text,
    timeToLive :: Lude.Maybe Lude.Int,
    imageSmallIconURL :: Lude.Maybe Lude.Text,
    jsonBody :: Lude.Maybe Lude.Text,
    url :: Lude.Maybe Lude.Text,
    action :: Lude.Maybe Action,
    imageURL :: Lude.Maybe Lude.Text,
    mediaURL :: Lude.Maybe Lude.Text,
    title :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- * 'action' - The action to occur if a recipient taps the push notification. Valid values are:
--
--
--     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.
--
--
--     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This setting uses the deep-linking features of iOS and Android.
--
--
--     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
--
-- * 'body' - The body of the notification message. The maximum number of characters is 200.
-- * 'imageIconURL' - The URL of the image to display as the push-notification icon, such as the icon for the app.
-- * 'imageSmallIconURL' - The URL of the image to display as the small, push-notification icon, such as a small version of the icon for the app.
-- * 'imageURL' - The URL of an image to display in the push notification.
-- * 'jsonBody' - The JSON payload to use for a silent push notification.
-- * 'mediaURL' - The URL of the image or video to display in the push notification.
-- * 'rawContent' - The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
-- * 'silentPush' - Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration, displaying messages in an in-app message center, or supporting phone home functionality.
-- * 'timeToLive' - The number of seconds that the push-notification service should keep the message, if the service is unable to deliver the notification the first time. This value is converted to an expiration value when it's sent to a push-notification service. If this value is 0, the service treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again.
--
-- This value doesn't apply to messages that are sent through the Amazon Device Messaging (ADM) service.
-- * 'title' - The title to display above the notification message on a recipient's device.
-- * 'url' - The URL to open in a recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
mkMessage ::
  Message
mkMessage =
  Message'
    { silentPush = Lude.Nothing,
      imageIconURL = Lude.Nothing,
      rawContent = Lude.Nothing,
      body = Lude.Nothing,
      timeToLive = Lude.Nothing,
      imageSmallIconURL = Lude.Nothing,
      jsonBody = Lude.Nothing,
      url = Lude.Nothing,
      action = Lude.Nothing,
      imageURL = Lude.Nothing,
      mediaURL = Lude.Nothing,
      title = Lude.Nothing
    }

-- | Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration, displaying messages in an in-app message center, or supporting phone home functionality.
--
-- /Note:/ Consider using 'silentPush' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSilentPush :: Lens.Lens' Message (Lude.Maybe Lude.Bool)
mSilentPush = Lens.lens (silentPush :: Message -> Lude.Maybe Lude.Bool) (\s a -> s {silentPush = a} :: Message)
{-# DEPRECATED mSilentPush "Use generic-lens or generic-optics with 'silentPush' instead." #-}

-- | The URL of the image to display as the push-notification icon, such as the icon for the app.
--
-- /Note:/ Consider using 'imageIconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mImageIconURL :: Lens.Lens' Message (Lude.Maybe Lude.Text)
mImageIconURL = Lens.lens (imageIconURL :: Message -> Lude.Maybe Lude.Text) (\s a -> s {imageIconURL = a} :: Message)
{-# DEPRECATED mImageIconURL "Use generic-lens or generic-optics with 'imageIconURL' instead." #-}

-- | The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mRawContent :: Lens.Lens' Message (Lude.Maybe Lude.Text)
mRawContent = Lens.lens (rawContent :: Message -> Lude.Maybe Lude.Text) (\s a -> s {rawContent = a} :: Message)
{-# DEPRECATED mRawContent "Use generic-lens or generic-optics with 'rawContent' instead." #-}

-- | The body of the notification message. The maximum number of characters is 200.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mBody :: Lens.Lens' Message (Lude.Maybe Lude.Text)
mBody = Lens.lens (body :: Message -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: Message)
{-# DEPRECATED mBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The number of seconds that the push-notification service should keep the message, if the service is unable to deliver the notification the first time. This value is converted to an expiration value when it's sent to a push-notification service. If this value is 0, the service treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again.
--
-- This value doesn't apply to messages that are sent through the Amazon Device Messaging (ADM) service.
--
-- /Note:/ Consider using 'timeToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTimeToLive :: Lens.Lens' Message (Lude.Maybe Lude.Int)
mTimeToLive = Lens.lens (timeToLive :: Message -> Lude.Maybe Lude.Int) (\s a -> s {timeToLive = a} :: Message)
{-# DEPRECATED mTimeToLive "Use generic-lens or generic-optics with 'timeToLive' instead." #-}

-- | The URL of the image to display as the small, push-notification icon, such as a small version of the icon for the app.
--
-- /Note:/ Consider using 'imageSmallIconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mImageSmallIconURL :: Lens.Lens' Message (Lude.Maybe Lude.Text)
mImageSmallIconURL = Lens.lens (imageSmallIconURL :: Message -> Lude.Maybe Lude.Text) (\s a -> s {imageSmallIconURL = a} :: Message)
{-# DEPRECATED mImageSmallIconURL "Use generic-lens or generic-optics with 'imageSmallIconURL' instead." #-}

-- | The JSON payload to use for a silent push notification.
--
-- /Note:/ Consider using 'jsonBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mJSONBody :: Lens.Lens' Message (Lude.Maybe Lude.Text)
mJSONBody = Lens.lens (jsonBody :: Message -> Lude.Maybe Lude.Text) (\s a -> s {jsonBody = a} :: Message)
{-# DEPRECATED mJSONBody "Use generic-lens or generic-optics with 'jsonBody' instead." #-}

-- | The URL to open in a recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mURL :: Lens.Lens' Message (Lude.Maybe Lude.Text)
mURL = Lens.lens (url :: Message -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: Message)
{-# DEPRECATED mURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The action to occur if a recipient taps the push notification. Valid values are:
--
--
--     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.
--
--
--     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This setting uses the deep-linking features of iOS and Android.
--
--
--     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
--
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAction :: Lens.Lens' Message (Lude.Maybe Action)
mAction = Lens.lens (action :: Message -> Lude.Maybe Action) (\s a -> s {action = a} :: Message)
{-# DEPRECATED mAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The URL of an image to display in the push notification.
--
-- /Note:/ Consider using 'imageURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mImageURL :: Lens.Lens' Message (Lude.Maybe Lude.Text)
mImageURL = Lens.lens (imageURL :: Message -> Lude.Maybe Lude.Text) (\s a -> s {imageURL = a} :: Message)
{-# DEPRECATED mImageURL "Use generic-lens or generic-optics with 'imageURL' instead." #-}

-- | The URL of the image or video to display in the push notification.
--
-- /Note:/ Consider using 'mediaURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMediaURL :: Lens.Lens' Message (Lude.Maybe Lude.Text)
mMediaURL = Lens.lens (mediaURL :: Message -> Lude.Maybe Lude.Text) (\s a -> s {mediaURL = a} :: Message)
{-# DEPRECATED mMediaURL "Use generic-lens or generic-optics with 'mediaURL' instead." #-}

-- | The title to display above the notification message on a recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTitle :: Lens.Lens' Message (Lude.Maybe Lude.Text)
mTitle = Lens.lens (title :: Message -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: Message)
{-# DEPRECATED mTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Lude.FromJSON Message where
  parseJSON =
    Lude.withObject
      "Message"
      ( \x ->
          Message'
            Lude.<$> (x Lude..:? "SilentPush")
            Lude.<*> (x Lude..:? "ImageIconUrl")
            Lude.<*> (x Lude..:? "RawContent")
            Lude.<*> (x Lude..:? "Body")
            Lude.<*> (x Lude..:? "TimeToLive")
            Lude.<*> (x Lude..:? "ImageSmallIconUrl")
            Lude.<*> (x Lude..:? "JsonBody")
            Lude.<*> (x Lude..:? "Url")
            Lude.<*> (x Lude..:? "Action")
            Lude.<*> (x Lude..:? "ImageUrl")
            Lude.<*> (x Lude..:? "MediaUrl")
            Lude.<*> (x Lude..:? "Title")
      )

instance Lude.ToJSON Message where
  toJSON Message' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SilentPush" Lude..=) Lude.<$> silentPush,
            ("ImageIconUrl" Lude..=) Lude.<$> imageIconURL,
            ("RawContent" Lude..=) Lude.<$> rawContent,
            ("Body" Lude..=) Lude.<$> body,
            ("TimeToLive" Lude..=) Lude.<$> timeToLive,
            ("ImageSmallIconUrl" Lude..=) Lude.<$> imageSmallIconURL,
            ("JsonBody" Lude..=) Lude.<$> jsonBody,
            ("Url" Lude..=) Lude.<$> url,
            ("Action" Lude..=) Lude.<$> action,
            ("ImageUrl" Lude..=) Lude.<$> imageURL,
            ("MediaUrl" Lude..=) Lude.<$> mediaURL,
            ("Title" Lude..=) Lude.<$> title
          ]
      )
