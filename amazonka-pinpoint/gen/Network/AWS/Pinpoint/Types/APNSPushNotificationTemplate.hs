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
-- Module      : Network.AWS.Pinpoint.Types.APNSPushNotificationTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSPushNotificationTemplate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action

-- | Specifies channel-specific content and settings for a message template
-- that can be used in push notifications that are sent through the APNs
-- (Apple Push Notification service) channel.
--
-- /See:/ 'newAPNSPushNotificationTemplate' smart constructor.
data APNSPushNotificationTemplate = APNSPushNotificationTemplate'
  { -- | The title to use in push notifications that are based on the message
    -- template. This title appears above the notification message on a
    -- recipient\'s device.
    title :: Core.Maybe Core.Text,
    -- | The message body to use in push notifications that are based on the
    -- message template.
    body :: Core.Maybe Core.Text,
    -- | The URL of an image or video to display in push notifications that are
    -- based on the message template.
    mediaUrl :: Core.Maybe Core.Text,
    -- | The action to occur if a recipient taps a push notification that\'s
    -- based on the message template. Valid values are:
    --
    -- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
    --     sent to the background. This is the default action.
    --
    -- -   DEEP_LINK - Your app opens and displays a designated user interface
    --     in the app. This setting uses the deep-linking features of the iOS
    --     platform.
    --
    -- -   URL - The default mobile browser on the recipient\'s device opens
    --     and loads the web page at a URL that you specify.
    action :: Core.Maybe Action,
    -- | The key for the sound to play when the recipient receives a push
    -- notification that\'s based on the message template. The value for this
    -- key is the name of a sound file in your app\'s main bundle or the
    -- Library\/Sounds folder in your app\'s data container. If the sound file
    -- can\'t be found or you specify default for the value, the system plays
    -- the default alert sound.
    sound :: Core.Maybe Core.Text,
    -- | The URL to open in the recipient\'s default mobile browser, if a
    -- recipient taps a push notification that\'s based on the message template
    -- and the value of the Action property is URL.
    url :: Core.Maybe Core.Text,
    -- | The raw, JSON-formatted string to use as the payload for push
    -- notifications that are based on the message template. If specified, this
    -- value overrides all other content for the message template.
    rawContent :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'APNSPushNotificationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'title', 'aPNSPushNotificationTemplate_title' - The title to use in push notifications that are based on the message
-- template. This title appears above the notification message on a
-- recipient\'s device.
--
-- 'body', 'aPNSPushNotificationTemplate_body' - The message body to use in push notifications that are based on the
-- message template.
--
-- 'mediaUrl', 'aPNSPushNotificationTemplate_mediaUrl' - The URL of an image or video to display in push notifications that are
-- based on the message template.
--
-- 'action', 'aPNSPushNotificationTemplate_action' - The action to occur if a recipient taps a push notification that\'s
-- based on the message template. Valid values are:
--
-- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
--     sent to the background. This is the default action.
--
-- -   DEEP_LINK - Your app opens and displays a designated user interface
--     in the app. This setting uses the deep-linking features of the iOS
--     platform.
--
-- -   URL - The default mobile browser on the recipient\'s device opens
--     and loads the web page at a URL that you specify.
--
-- 'sound', 'aPNSPushNotificationTemplate_sound' - The key for the sound to play when the recipient receives a push
-- notification that\'s based on the message template. The value for this
-- key is the name of a sound file in your app\'s main bundle or the
-- Library\/Sounds folder in your app\'s data container. If the sound file
-- can\'t be found or you specify default for the value, the system plays
-- the default alert sound.
--
-- 'url', 'aPNSPushNotificationTemplate_url' - The URL to open in the recipient\'s default mobile browser, if a
-- recipient taps a push notification that\'s based on the message template
-- and the value of the Action property is URL.
--
-- 'rawContent', 'aPNSPushNotificationTemplate_rawContent' - The raw, JSON-formatted string to use as the payload for push
-- notifications that are based on the message template. If specified, this
-- value overrides all other content for the message template.
newAPNSPushNotificationTemplate ::
  APNSPushNotificationTemplate
newAPNSPushNotificationTemplate =
  APNSPushNotificationTemplate'
    { title = Core.Nothing,
      body = Core.Nothing,
      mediaUrl = Core.Nothing,
      action = Core.Nothing,
      sound = Core.Nothing,
      url = Core.Nothing,
      rawContent = Core.Nothing
    }

-- | The title to use in push notifications that are based on the message
-- template. This title appears above the notification message on a
-- recipient\'s device.
aPNSPushNotificationTemplate_title :: Lens.Lens' APNSPushNotificationTemplate (Core.Maybe Core.Text)
aPNSPushNotificationTemplate_title = Lens.lens (\APNSPushNotificationTemplate' {title} -> title) (\s@APNSPushNotificationTemplate' {} a -> s {title = a} :: APNSPushNotificationTemplate)

-- | The message body to use in push notifications that are based on the
-- message template.
aPNSPushNotificationTemplate_body :: Lens.Lens' APNSPushNotificationTemplate (Core.Maybe Core.Text)
aPNSPushNotificationTemplate_body = Lens.lens (\APNSPushNotificationTemplate' {body} -> body) (\s@APNSPushNotificationTemplate' {} a -> s {body = a} :: APNSPushNotificationTemplate)

-- | The URL of an image or video to display in push notifications that are
-- based on the message template.
aPNSPushNotificationTemplate_mediaUrl :: Lens.Lens' APNSPushNotificationTemplate (Core.Maybe Core.Text)
aPNSPushNotificationTemplate_mediaUrl = Lens.lens (\APNSPushNotificationTemplate' {mediaUrl} -> mediaUrl) (\s@APNSPushNotificationTemplate' {} a -> s {mediaUrl = a} :: APNSPushNotificationTemplate)

-- | The action to occur if a recipient taps a push notification that\'s
-- based on the message template. Valid values are:
--
-- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
--     sent to the background. This is the default action.
--
-- -   DEEP_LINK - Your app opens and displays a designated user interface
--     in the app. This setting uses the deep-linking features of the iOS
--     platform.
--
-- -   URL - The default mobile browser on the recipient\'s device opens
--     and loads the web page at a URL that you specify.
aPNSPushNotificationTemplate_action :: Lens.Lens' APNSPushNotificationTemplate (Core.Maybe Action)
aPNSPushNotificationTemplate_action = Lens.lens (\APNSPushNotificationTemplate' {action} -> action) (\s@APNSPushNotificationTemplate' {} a -> s {action = a} :: APNSPushNotificationTemplate)

-- | The key for the sound to play when the recipient receives a push
-- notification that\'s based on the message template. The value for this
-- key is the name of a sound file in your app\'s main bundle or the
-- Library\/Sounds folder in your app\'s data container. If the sound file
-- can\'t be found or you specify default for the value, the system plays
-- the default alert sound.
aPNSPushNotificationTemplate_sound :: Lens.Lens' APNSPushNotificationTemplate (Core.Maybe Core.Text)
aPNSPushNotificationTemplate_sound = Lens.lens (\APNSPushNotificationTemplate' {sound} -> sound) (\s@APNSPushNotificationTemplate' {} a -> s {sound = a} :: APNSPushNotificationTemplate)

-- | The URL to open in the recipient\'s default mobile browser, if a
-- recipient taps a push notification that\'s based on the message template
-- and the value of the Action property is URL.
aPNSPushNotificationTemplate_url :: Lens.Lens' APNSPushNotificationTemplate (Core.Maybe Core.Text)
aPNSPushNotificationTemplate_url = Lens.lens (\APNSPushNotificationTemplate' {url} -> url) (\s@APNSPushNotificationTemplate' {} a -> s {url = a} :: APNSPushNotificationTemplate)

-- | The raw, JSON-formatted string to use as the payload for push
-- notifications that are based on the message template. If specified, this
-- value overrides all other content for the message template.
aPNSPushNotificationTemplate_rawContent :: Lens.Lens' APNSPushNotificationTemplate (Core.Maybe Core.Text)
aPNSPushNotificationTemplate_rawContent = Lens.lens (\APNSPushNotificationTemplate' {rawContent} -> rawContent) (\s@APNSPushNotificationTemplate' {} a -> s {rawContent = a} :: APNSPushNotificationTemplate)

instance Core.FromJSON APNSPushNotificationTemplate where
  parseJSON =
    Core.withObject
      "APNSPushNotificationTemplate"
      ( \x ->
          APNSPushNotificationTemplate'
            Core.<$> (x Core..:? "Title")
            Core.<*> (x Core..:? "Body")
            Core.<*> (x Core..:? "MediaUrl")
            Core.<*> (x Core..:? "Action")
            Core.<*> (x Core..:? "Sound")
            Core.<*> (x Core..:? "Url")
            Core.<*> (x Core..:? "RawContent")
      )

instance Core.Hashable APNSPushNotificationTemplate

instance Core.NFData APNSPushNotificationTemplate

instance Core.ToJSON APNSPushNotificationTemplate where
  toJSON APNSPushNotificationTemplate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Title" Core..=) Core.<$> title,
            ("Body" Core..=) Core.<$> body,
            ("MediaUrl" Core..=) Core.<$> mediaUrl,
            ("Action" Core..=) Core.<$> action,
            ("Sound" Core..=) Core.<$> sound,
            ("Url" Core..=) Core.<$> url,
            ("RawContent" Core..=) Core.<$> rawContent
          ]
      )
