{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Message
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Message where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the content and settings for a push notification that\'s sent
-- to recipients of a campaign.
--
-- /See:/ 'newMessage' smart constructor.
data Message = Message'
  { -- | Specifies whether the notification is a silent push notification, which
    -- is a push notification that doesn\'t display on a recipient\'s device.
    -- Silent push notifications can be used for cases such as updating an
    -- app\'s configuration, displaying messages in an in-app message center,
    -- or supporting phone home functionality.
    silentPush :: Prelude.Maybe Prelude.Bool,
    -- | The URL of the image to display as the push-notification icon, such as
    -- the icon for the app.
    imageIconUrl :: Prelude.Maybe Prelude.Text,
    -- | The title to display above the notification message on a recipient\'s
    -- device.
    title :: Prelude.Maybe Prelude.Text,
    -- | The JSON payload to use for a silent push notification.
    jsonBody :: Prelude.Maybe Prelude.Text,
    -- | The body of the notification message. The maximum number of characters
    -- is 200.
    body :: Prelude.Maybe Prelude.Text,
    -- | The number of seconds that the push-notification service should keep the
    -- message, if the service is unable to deliver the notification the first
    -- time. This value is converted to an expiration value when it\'s sent to
    -- a push-notification service. If this value is 0, the service treats the
    -- notification as if it expires immediately and the service doesn\'t store
    -- or try to deliver the notification again.
    --
    -- This value doesn\'t apply to messages that are sent through the Amazon
    -- Device Messaging (ADM) service.
    timeToLive :: Prelude.Maybe Prelude.Int,
    -- | The URL of the image or video to display in the push notification.
    mediaUrl :: Prelude.Maybe Prelude.Text,
    -- | The URL of an image to display in the push notification.
    imageUrl :: Prelude.Maybe Prelude.Text,
    -- | The action to occur if a recipient taps the push notification. Valid
    -- values are:
    --
    -- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
    --     sent to the background. This is the default action.
    --
    -- -   DEEP_LINK - Your app opens and displays a designated user interface
    --     in the app. This setting uses the deep-linking features of iOS and
    --     Android.
    --
    -- -   URL - The default mobile browser on the recipient\'s device opens
    --     and loads the web page at a URL that you specify.
    action :: Prelude.Maybe Action,
    -- | The URL to open in a recipient\'s default mobile browser, if a recipient
    -- taps the push notification and the value of the Action property is URL.
    url :: Prelude.Maybe Prelude.Text,
    -- | The URL of the image to display as the small, push-notification icon,
    -- such as a small version of the icon for the app.
    imageSmallIconUrl :: Prelude.Maybe Prelude.Text,
    -- | The raw, JSON-formatted string to use as the payload for the
    -- notification message. If specified, this value overrides all other
    -- content for the message.
    rawContent :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Message' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'silentPush', 'message_silentPush' - Specifies whether the notification is a silent push notification, which
-- is a push notification that doesn\'t display on a recipient\'s device.
-- Silent push notifications can be used for cases such as updating an
-- app\'s configuration, displaying messages in an in-app message center,
-- or supporting phone home functionality.
--
-- 'imageIconUrl', 'message_imageIconUrl' - The URL of the image to display as the push-notification icon, such as
-- the icon for the app.
--
-- 'title', 'message_title' - The title to display above the notification message on a recipient\'s
-- device.
--
-- 'jsonBody', 'message_jsonBody' - The JSON payload to use for a silent push notification.
--
-- 'body', 'message_body' - The body of the notification message. The maximum number of characters
-- is 200.
--
-- 'timeToLive', 'message_timeToLive' - The number of seconds that the push-notification service should keep the
-- message, if the service is unable to deliver the notification the first
-- time. This value is converted to an expiration value when it\'s sent to
-- a push-notification service. If this value is 0, the service treats the
-- notification as if it expires immediately and the service doesn\'t store
-- or try to deliver the notification again.
--
-- This value doesn\'t apply to messages that are sent through the Amazon
-- Device Messaging (ADM) service.
--
-- 'mediaUrl', 'message_mediaUrl' - The URL of the image or video to display in the push notification.
--
-- 'imageUrl', 'message_imageUrl' - The URL of an image to display in the push notification.
--
-- 'action', 'message_action' - The action to occur if a recipient taps the push notification. Valid
-- values are:
--
-- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
--     sent to the background. This is the default action.
--
-- -   DEEP_LINK - Your app opens and displays a designated user interface
--     in the app. This setting uses the deep-linking features of iOS and
--     Android.
--
-- -   URL - The default mobile browser on the recipient\'s device opens
--     and loads the web page at a URL that you specify.
--
-- 'url', 'message_url' - The URL to open in a recipient\'s default mobile browser, if a recipient
-- taps the push notification and the value of the Action property is URL.
--
-- 'imageSmallIconUrl', 'message_imageSmallIconUrl' - The URL of the image to display as the small, push-notification icon,
-- such as a small version of the icon for the app.
--
-- 'rawContent', 'message_rawContent' - The raw, JSON-formatted string to use as the payload for the
-- notification message. If specified, this value overrides all other
-- content for the message.
newMessage ::
  Message
newMessage =
  Message'
    { silentPush = Prelude.Nothing,
      imageIconUrl = Prelude.Nothing,
      title = Prelude.Nothing,
      jsonBody = Prelude.Nothing,
      body = Prelude.Nothing,
      timeToLive = Prelude.Nothing,
      mediaUrl = Prelude.Nothing,
      imageUrl = Prelude.Nothing,
      action = Prelude.Nothing,
      url = Prelude.Nothing,
      imageSmallIconUrl = Prelude.Nothing,
      rawContent = Prelude.Nothing
    }

-- | Specifies whether the notification is a silent push notification, which
-- is a push notification that doesn\'t display on a recipient\'s device.
-- Silent push notifications can be used for cases such as updating an
-- app\'s configuration, displaying messages in an in-app message center,
-- or supporting phone home functionality.
message_silentPush :: Lens.Lens' Message (Prelude.Maybe Prelude.Bool)
message_silentPush = Lens.lens (\Message' {silentPush} -> silentPush) (\s@Message' {} a -> s {silentPush = a} :: Message)

-- | The URL of the image to display as the push-notification icon, such as
-- the icon for the app.
message_imageIconUrl :: Lens.Lens' Message (Prelude.Maybe Prelude.Text)
message_imageIconUrl = Lens.lens (\Message' {imageIconUrl} -> imageIconUrl) (\s@Message' {} a -> s {imageIconUrl = a} :: Message)

-- | The title to display above the notification message on a recipient\'s
-- device.
message_title :: Lens.Lens' Message (Prelude.Maybe Prelude.Text)
message_title = Lens.lens (\Message' {title} -> title) (\s@Message' {} a -> s {title = a} :: Message)

-- | The JSON payload to use for a silent push notification.
message_jsonBody :: Lens.Lens' Message (Prelude.Maybe Prelude.Text)
message_jsonBody = Lens.lens (\Message' {jsonBody} -> jsonBody) (\s@Message' {} a -> s {jsonBody = a} :: Message)

-- | The body of the notification message. The maximum number of characters
-- is 200.
message_body :: Lens.Lens' Message (Prelude.Maybe Prelude.Text)
message_body = Lens.lens (\Message' {body} -> body) (\s@Message' {} a -> s {body = a} :: Message)

-- | The number of seconds that the push-notification service should keep the
-- message, if the service is unable to deliver the notification the first
-- time. This value is converted to an expiration value when it\'s sent to
-- a push-notification service. If this value is 0, the service treats the
-- notification as if it expires immediately and the service doesn\'t store
-- or try to deliver the notification again.
--
-- This value doesn\'t apply to messages that are sent through the Amazon
-- Device Messaging (ADM) service.
message_timeToLive :: Lens.Lens' Message (Prelude.Maybe Prelude.Int)
message_timeToLive = Lens.lens (\Message' {timeToLive} -> timeToLive) (\s@Message' {} a -> s {timeToLive = a} :: Message)

-- | The URL of the image or video to display in the push notification.
message_mediaUrl :: Lens.Lens' Message (Prelude.Maybe Prelude.Text)
message_mediaUrl = Lens.lens (\Message' {mediaUrl} -> mediaUrl) (\s@Message' {} a -> s {mediaUrl = a} :: Message)

-- | The URL of an image to display in the push notification.
message_imageUrl :: Lens.Lens' Message (Prelude.Maybe Prelude.Text)
message_imageUrl = Lens.lens (\Message' {imageUrl} -> imageUrl) (\s@Message' {} a -> s {imageUrl = a} :: Message)

-- | The action to occur if a recipient taps the push notification. Valid
-- values are:
--
-- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
--     sent to the background. This is the default action.
--
-- -   DEEP_LINK - Your app opens and displays a designated user interface
--     in the app. This setting uses the deep-linking features of iOS and
--     Android.
--
-- -   URL - The default mobile browser on the recipient\'s device opens
--     and loads the web page at a URL that you specify.
message_action :: Lens.Lens' Message (Prelude.Maybe Action)
message_action = Lens.lens (\Message' {action} -> action) (\s@Message' {} a -> s {action = a} :: Message)

-- | The URL to open in a recipient\'s default mobile browser, if a recipient
-- taps the push notification and the value of the Action property is URL.
message_url :: Lens.Lens' Message (Prelude.Maybe Prelude.Text)
message_url = Lens.lens (\Message' {url} -> url) (\s@Message' {} a -> s {url = a} :: Message)

-- | The URL of the image to display as the small, push-notification icon,
-- such as a small version of the icon for the app.
message_imageSmallIconUrl :: Lens.Lens' Message (Prelude.Maybe Prelude.Text)
message_imageSmallIconUrl = Lens.lens (\Message' {imageSmallIconUrl} -> imageSmallIconUrl) (\s@Message' {} a -> s {imageSmallIconUrl = a} :: Message)

-- | The raw, JSON-formatted string to use as the payload for the
-- notification message. If specified, this value overrides all other
-- content for the message.
message_rawContent :: Lens.Lens' Message (Prelude.Maybe Prelude.Text)
message_rawContent = Lens.lens (\Message' {rawContent} -> rawContent) (\s@Message' {} a -> s {rawContent = a} :: Message)

instance Prelude.FromJSON Message where
  parseJSON =
    Prelude.withObject
      "Message"
      ( \x ->
          Message'
            Prelude.<$> (x Prelude..:? "SilentPush")
            Prelude.<*> (x Prelude..:? "ImageIconUrl")
            Prelude.<*> (x Prelude..:? "Title")
            Prelude.<*> (x Prelude..:? "JsonBody")
            Prelude.<*> (x Prelude..:? "Body")
            Prelude.<*> (x Prelude..:? "TimeToLive")
            Prelude.<*> (x Prelude..:? "MediaUrl")
            Prelude.<*> (x Prelude..:? "ImageUrl")
            Prelude.<*> (x Prelude..:? "Action")
            Prelude.<*> (x Prelude..:? "Url")
            Prelude.<*> (x Prelude..:? "ImageSmallIconUrl")
            Prelude.<*> (x Prelude..:? "RawContent")
      )

instance Prelude.Hashable Message

instance Prelude.NFData Message

instance Prelude.ToJSON Message where
  toJSON Message' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SilentPush" Prelude..=) Prelude.<$> silentPush,
            ("ImageIconUrl" Prelude..=) Prelude.<$> imageIconUrl,
            ("Title" Prelude..=) Prelude.<$> title,
            ("JsonBody" Prelude..=) Prelude.<$> jsonBody,
            ("Body" Prelude..=) Prelude.<$> body,
            ("TimeToLive" Prelude..=) Prelude.<$> timeToLive,
            ("MediaUrl" Prelude..=) Prelude.<$> mediaUrl,
            ("ImageUrl" Prelude..=) Prelude.<$> imageUrl,
            ("Action" Prelude..=) Prelude.<$> action,
            ("Url" Prelude..=) Prelude.<$> url,
            ("ImageSmallIconUrl" Prelude..=)
              Prelude.<$> imageSmallIconUrl,
            ("RawContent" Prelude..=) Prelude.<$> rawContent
          ]
      )
