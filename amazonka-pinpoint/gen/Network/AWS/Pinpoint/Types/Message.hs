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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action

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
    silentPush :: Core.Maybe Core.Bool,
    -- | The URL of the image to display as the push-notification icon, such as
    -- the icon for the app.
    imageIconUrl :: Core.Maybe Core.Text,
    -- | The title to display above the notification message on a recipient\'s
    -- device.
    title :: Core.Maybe Core.Text,
    -- | The JSON payload to use for a silent push notification.
    jsonBody :: Core.Maybe Core.Text,
    -- | The body of the notification message. The maximum number of characters
    -- is 200.
    body :: Core.Maybe Core.Text,
    -- | The number of seconds that the push-notification service should keep the
    -- message, if the service is unable to deliver the notification the first
    -- time. This value is converted to an expiration value when it\'s sent to
    -- a push-notification service. If this value is 0, the service treats the
    -- notification as if it expires immediately and the service doesn\'t store
    -- or try to deliver the notification again.
    --
    -- This value doesn\'t apply to messages that are sent through the Amazon
    -- Device Messaging (ADM) service.
    timeToLive :: Core.Maybe Core.Int,
    -- | The URL of the image or video to display in the push notification.
    mediaUrl :: Core.Maybe Core.Text,
    -- | The URL of an image to display in the push notification.
    imageUrl :: Core.Maybe Core.Text,
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
    action :: Core.Maybe Action,
    -- | The URL to open in a recipient\'s default mobile browser, if a recipient
    -- taps the push notification and the value of the Action property is URL.
    url :: Core.Maybe Core.Text,
    -- | The URL of the image to display as the small, push-notification icon,
    -- such as a small version of the icon for the app.
    imageSmallIconUrl :: Core.Maybe Core.Text,
    -- | The raw, JSON-formatted string to use as the payload for the
    -- notification message. If specified, this value overrides all other
    -- content for the message.
    rawContent :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { silentPush = Core.Nothing,
      imageIconUrl = Core.Nothing,
      title = Core.Nothing,
      jsonBody = Core.Nothing,
      body = Core.Nothing,
      timeToLive = Core.Nothing,
      mediaUrl = Core.Nothing,
      imageUrl = Core.Nothing,
      action = Core.Nothing,
      url = Core.Nothing,
      imageSmallIconUrl = Core.Nothing,
      rawContent = Core.Nothing
    }

-- | Specifies whether the notification is a silent push notification, which
-- is a push notification that doesn\'t display on a recipient\'s device.
-- Silent push notifications can be used for cases such as updating an
-- app\'s configuration, displaying messages in an in-app message center,
-- or supporting phone home functionality.
message_silentPush :: Lens.Lens' Message (Core.Maybe Core.Bool)
message_silentPush = Lens.lens (\Message' {silentPush} -> silentPush) (\s@Message' {} a -> s {silentPush = a} :: Message)

-- | The URL of the image to display as the push-notification icon, such as
-- the icon for the app.
message_imageIconUrl :: Lens.Lens' Message (Core.Maybe Core.Text)
message_imageIconUrl = Lens.lens (\Message' {imageIconUrl} -> imageIconUrl) (\s@Message' {} a -> s {imageIconUrl = a} :: Message)

-- | The title to display above the notification message on a recipient\'s
-- device.
message_title :: Lens.Lens' Message (Core.Maybe Core.Text)
message_title = Lens.lens (\Message' {title} -> title) (\s@Message' {} a -> s {title = a} :: Message)

-- | The JSON payload to use for a silent push notification.
message_jsonBody :: Lens.Lens' Message (Core.Maybe Core.Text)
message_jsonBody = Lens.lens (\Message' {jsonBody} -> jsonBody) (\s@Message' {} a -> s {jsonBody = a} :: Message)

-- | The body of the notification message. The maximum number of characters
-- is 200.
message_body :: Lens.Lens' Message (Core.Maybe Core.Text)
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
message_timeToLive :: Lens.Lens' Message (Core.Maybe Core.Int)
message_timeToLive = Lens.lens (\Message' {timeToLive} -> timeToLive) (\s@Message' {} a -> s {timeToLive = a} :: Message)

-- | The URL of the image or video to display in the push notification.
message_mediaUrl :: Lens.Lens' Message (Core.Maybe Core.Text)
message_mediaUrl = Lens.lens (\Message' {mediaUrl} -> mediaUrl) (\s@Message' {} a -> s {mediaUrl = a} :: Message)

-- | The URL of an image to display in the push notification.
message_imageUrl :: Lens.Lens' Message (Core.Maybe Core.Text)
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
message_action :: Lens.Lens' Message (Core.Maybe Action)
message_action = Lens.lens (\Message' {action} -> action) (\s@Message' {} a -> s {action = a} :: Message)

-- | The URL to open in a recipient\'s default mobile browser, if a recipient
-- taps the push notification and the value of the Action property is URL.
message_url :: Lens.Lens' Message (Core.Maybe Core.Text)
message_url = Lens.lens (\Message' {url} -> url) (\s@Message' {} a -> s {url = a} :: Message)

-- | The URL of the image to display as the small, push-notification icon,
-- such as a small version of the icon for the app.
message_imageSmallIconUrl :: Lens.Lens' Message (Core.Maybe Core.Text)
message_imageSmallIconUrl = Lens.lens (\Message' {imageSmallIconUrl} -> imageSmallIconUrl) (\s@Message' {} a -> s {imageSmallIconUrl = a} :: Message)

-- | The raw, JSON-formatted string to use as the payload for the
-- notification message. If specified, this value overrides all other
-- content for the message.
message_rawContent :: Lens.Lens' Message (Core.Maybe Core.Text)
message_rawContent = Lens.lens (\Message' {rawContent} -> rawContent) (\s@Message' {} a -> s {rawContent = a} :: Message)

instance Core.FromJSON Message where
  parseJSON =
    Core.withObject
      "Message"
      ( \x ->
          Message'
            Core.<$> (x Core..:? "SilentPush")
            Core.<*> (x Core..:? "ImageIconUrl")
            Core.<*> (x Core..:? "Title")
            Core.<*> (x Core..:? "JsonBody")
            Core.<*> (x Core..:? "Body")
            Core.<*> (x Core..:? "TimeToLive")
            Core.<*> (x Core..:? "MediaUrl")
            Core.<*> (x Core..:? "ImageUrl")
            Core.<*> (x Core..:? "Action")
            Core.<*> (x Core..:? "Url")
            Core.<*> (x Core..:? "ImageSmallIconUrl")
            Core.<*> (x Core..:? "RawContent")
      )

instance Core.Hashable Message

instance Core.NFData Message

instance Core.ToJSON Message where
  toJSON Message' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SilentPush" Core..=) Core.<$> silentPush,
            ("ImageIconUrl" Core..=) Core.<$> imageIconUrl,
            ("Title" Core..=) Core.<$> title,
            ("JsonBody" Core..=) Core.<$> jsonBody,
            ("Body" Core..=) Core.<$> body,
            ("TimeToLive" Core..=) Core.<$> timeToLive,
            ("MediaUrl" Core..=) Core.<$> mediaUrl,
            ("ImageUrl" Core..=) Core.<$> imageUrl,
            ("Action" Core..=) Core.<$> action,
            ("Url" Core..=) Core.<$> url,
            ("ImageSmallIconUrl" Core..=)
              Core.<$> imageSmallIconUrl,
            ("RawContent" Core..=) Core.<$> rawContent
          ]
      )
