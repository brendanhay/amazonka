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
-- Module      : Amazonka.Pinpoint.Types.GCMMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.GCMMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.Action
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings for a one-time message that\'s sent directly to
-- an endpoint through the GCM channel. The GCM channel enables Amazon
-- Pinpoint to send messages to the Firebase Cloud Messaging (FCM),
-- formerly Google Cloud Messaging (GCM), service.
--
-- /See:/ 'newGCMMessage' smart constructor.
data GCMMessage = GCMMessage'
  { -- | The amount of time, in seconds, that FCM should store and attempt to
    -- deliver the push notification, if the service is unable to deliver the
    -- notification the first time. If you don\'t specify this value, FCM
    -- defaults to the maximum value, which is 2,419,200 seconds (28 days).
    --
    -- Amazon Pinpoint specifies this value in the FCM time_to_live parameter
    -- when it sends the notification message to FCM.
    timeToLive :: Prelude.Maybe Prelude.Int,
    -- | An arbitrary string that identifies a group of messages that can be
    -- collapsed to ensure that only the last message is sent when delivery can
    -- resume. This helps avoid sending too many instances of the same messages
    -- when the recipient\'s device comes online again or becomes active.
    --
    -- Amazon Pinpoint specifies this value in the Firebase Cloud Messaging
    -- (FCM) collapse_key parameter when it sends the notification message to
    -- FCM.
    collapseKey :: Prelude.Maybe Prelude.Text,
    -- | The icon image name of the asset saved in your app.
    iconReference :: Prelude.Maybe Prelude.Text,
    -- | The body of the notification message.
    body :: Prelude.Maybe Prelude.Text,
    -- | The URL of an image to display in the push notification.
    imageUrl :: Prelude.Maybe Prelude.Text,
    -- | The URL to open in the recipient\'s default mobile browser, if a
    -- recipient taps the push notification and the value of the Action
    -- property is URL.
    url :: Prelude.Maybe Prelude.Text,
    -- | The default message variables to use in the notification message. You
    -- can override the default variables with individual address variables.
    substitutions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The raw, JSON-formatted string to use as the payload for the
    -- notification message. If specified, this value overrides all other
    -- content for the message.
    rawContent :: Prelude.Maybe Prelude.Text,
    -- | The sound to play when the recipient receives the push notification. You
    -- can use the default stream or specify the file name of a sound resource
    -- that\'s bundled in your app. On an Android platform, the sound file must
    -- reside in \/res\/raw\/.
    sound :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the notification is a silent push notification, which
    -- is a push notification that doesn\'t display on a recipient\'s device.
    -- Silent push notifications can be used for cases such as updating an
    -- app\'s configuration or supporting phone home functionality.
    silentPush :: Prelude.Maybe Prelude.Bool,
    -- | The URL of the large icon image to display in the content view of the
    -- push notification.
    imageIconUrl :: Prelude.Maybe Prelude.Text,
    -- | The title to display above the notification message on the recipient\'s
    -- device.
    title :: Prelude.Maybe Prelude.Text,
    -- | para>normal - The notification might be delayed. Delivery is optimized
    -- for battery usage on the recipient\'s device. Use this value unless
    -- immediate delivery is required.
    --
    -- \/listitem>
    --
    -- high - The notification is sent immediately and might wake a sleeping
    -- device.
    --
    -- \/para>
    --
    -- Amazon Pinpoint specifies this value in the FCM priority parameter when
    -- it sends the notification message to FCM.
    --
    -- The equivalent values for Apple Push Notification service (APNs) are 5,
    -- for normal, and 10, for high. If you specify an APNs value for this
    -- property, Amazon Pinpoint accepts and converts the value to the
    -- corresponding FCM value.
    priority :: Prelude.Maybe Prelude.Text,
    -- | The action to occur if the recipient taps the push notification. Valid
    -- values are:
    --
    -- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
    --     sent to the background. This is the default action.
    --
    -- -   DEEP_LINK - Your app opens and displays a designated user interface
    --     in the app. This action uses the deep-linking features of the
    --     Android platform.
    --
    -- -   URL - The default mobile browser on the recipient\'s device opens
    --     and loads the web page at a URL that you specify.
    action :: Prelude.Maybe Action,
    -- | The package name of the application where registration tokens must match
    -- in order for the recipient to receive the message.
    restrictedPackageName :: Prelude.Maybe Prelude.Text,
    -- | The JSON data payload to use for the push notification, if the
    -- notification is a silent push notification. This payload is added to the
    -- data.pinpoint.jsonBody object of the notification.
    data' :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The URL of the small icon image to display in the status bar and the
    -- content view of the push notification.
    smallImageIconUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GCMMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeToLive', 'gCMMessage_timeToLive' - The amount of time, in seconds, that FCM should store and attempt to
-- deliver the push notification, if the service is unable to deliver the
-- notification the first time. If you don\'t specify this value, FCM
-- defaults to the maximum value, which is 2,419,200 seconds (28 days).
--
-- Amazon Pinpoint specifies this value in the FCM time_to_live parameter
-- when it sends the notification message to FCM.
--
-- 'collapseKey', 'gCMMessage_collapseKey' - An arbitrary string that identifies a group of messages that can be
-- collapsed to ensure that only the last message is sent when delivery can
-- resume. This helps avoid sending too many instances of the same messages
-- when the recipient\'s device comes online again or becomes active.
--
-- Amazon Pinpoint specifies this value in the Firebase Cloud Messaging
-- (FCM) collapse_key parameter when it sends the notification message to
-- FCM.
--
-- 'iconReference', 'gCMMessage_iconReference' - The icon image name of the asset saved in your app.
--
-- 'body', 'gCMMessage_body' - The body of the notification message.
--
-- 'imageUrl', 'gCMMessage_imageUrl' - The URL of an image to display in the push notification.
--
-- 'url', 'gCMMessage_url' - The URL to open in the recipient\'s default mobile browser, if a
-- recipient taps the push notification and the value of the Action
-- property is URL.
--
-- 'substitutions', 'gCMMessage_substitutions' - The default message variables to use in the notification message. You
-- can override the default variables with individual address variables.
--
-- 'rawContent', 'gCMMessage_rawContent' - The raw, JSON-formatted string to use as the payload for the
-- notification message. If specified, this value overrides all other
-- content for the message.
--
-- 'sound', 'gCMMessage_sound' - The sound to play when the recipient receives the push notification. You
-- can use the default stream or specify the file name of a sound resource
-- that\'s bundled in your app. On an Android platform, the sound file must
-- reside in \/res\/raw\/.
--
-- 'silentPush', 'gCMMessage_silentPush' - Specifies whether the notification is a silent push notification, which
-- is a push notification that doesn\'t display on a recipient\'s device.
-- Silent push notifications can be used for cases such as updating an
-- app\'s configuration or supporting phone home functionality.
--
-- 'imageIconUrl', 'gCMMessage_imageIconUrl' - The URL of the large icon image to display in the content view of the
-- push notification.
--
-- 'title', 'gCMMessage_title' - The title to display above the notification message on the recipient\'s
-- device.
--
-- 'priority', 'gCMMessage_priority' - para>normal - The notification might be delayed. Delivery is optimized
-- for battery usage on the recipient\'s device. Use this value unless
-- immediate delivery is required.
--
-- \/listitem>
--
-- high - The notification is sent immediately and might wake a sleeping
-- device.
--
-- \/para>
--
-- Amazon Pinpoint specifies this value in the FCM priority parameter when
-- it sends the notification message to FCM.
--
-- The equivalent values for Apple Push Notification service (APNs) are 5,
-- for normal, and 10, for high. If you specify an APNs value for this
-- property, Amazon Pinpoint accepts and converts the value to the
-- corresponding FCM value.
--
-- 'action', 'gCMMessage_action' - The action to occur if the recipient taps the push notification. Valid
-- values are:
--
-- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
--     sent to the background. This is the default action.
--
-- -   DEEP_LINK - Your app opens and displays a designated user interface
--     in the app. This action uses the deep-linking features of the
--     Android platform.
--
-- -   URL - The default mobile browser on the recipient\'s device opens
--     and loads the web page at a URL that you specify.
--
-- 'restrictedPackageName', 'gCMMessage_restrictedPackageName' - The package name of the application where registration tokens must match
-- in order for the recipient to receive the message.
--
-- 'data'', 'gCMMessage_data' - The JSON data payload to use for the push notification, if the
-- notification is a silent push notification. This payload is added to the
-- data.pinpoint.jsonBody object of the notification.
--
-- 'smallImageIconUrl', 'gCMMessage_smallImageIconUrl' - The URL of the small icon image to display in the status bar and the
-- content view of the push notification.
newGCMMessage ::
  GCMMessage
newGCMMessage =
  GCMMessage'
    { timeToLive = Prelude.Nothing,
      collapseKey = Prelude.Nothing,
      iconReference = Prelude.Nothing,
      body = Prelude.Nothing,
      imageUrl = Prelude.Nothing,
      url = Prelude.Nothing,
      substitutions = Prelude.Nothing,
      rawContent = Prelude.Nothing,
      sound = Prelude.Nothing,
      silentPush = Prelude.Nothing,
      imageIconUrl = Prelude.Nothing,
      title = Prelude.Nothing,
      priority = Prelude.Nothing,
      action = Prelude.Nothing,
      restrictedPackageName = Prelude.Nothing,
      data' = Prelude.Nothing,
      smallImageIconUrl = Prelude.Nothing
    }

-- | The amount of time, in seconds, that FCM should store and attempt to
-- deliver the push notification, if the service is unable to deliver the
-- notification the first time. If you don\'t specify this value, FCM
-- defaults to the maximum value, which is 2,419,200 seconds (28 days).
--
-- Amazon Pinpoint specifies this value in the FCM time_to_live parameter
-- when it sends the notification message to FCM.
gCMMessage_timeToLive :: Lens.Lens' GCMMessage (Prelude.Maybe Prelude.Int)
gCMMessage_timeToLive = Lens.lens (\GCMMessage' {timeToLive} -> timeToLive) (\s@GCMMessage' {} a -> s {timeToLive = a} :: GCMMessage)

-- | An arbitrary string that identifies a group of messages that can be
-- collapsed to ensure that only the last message is sent when delivery can
-- resume. This helps avoid sending too many instances of the same messages
-- when the recipient\'s device comes online again or becomes active.
--
-- Amazon Pinpoint specifies this value in the Firebase Cloud Messaging
-- (FCM) collapse_key parameter when it sends the notification message to
-- FCM.
gCMMessage_collapseKey :: Lens.Lens' GCMMessage (Prelude.Maybe Prelude.Text)
gCMMessage_collapseKey = Lens.lens (\GCMMessage' {collapseKey} -> collapseKey) (\s@GCMMessage' {} a -> s {collapseKey = a} :: GCMMessage)

-- | The icon image name of the asset saved in your app.
gCMMessage_iconReference :: Lens.Lens' GCMMessage (Prelude.Maybe Prelude.Text)
gCMMessage_iconReference = Lens.lens (\GCMMessage' {iconReference} -> iconReference) (\s@GCMMessage' {} a -> s {iconReference = a} :: GCMMessage)

-- | The body of the notification message.
gCMMessage_body :: Lens.Lens' GCMMessage (Prelude.Maybe Prelude.Text)
gCMMessage_body = Lens.lens (\GCMMessage' {body} -> body) (\s@GCMMessage' {} a -> s {body = a} :: GCMMessage)

-- | The URL of an image to display in the push notification.
gCMMessage_imageUrl :: Lens.Lens' GCMMessage (Prelude.Maybe Prelude.Text)
gCMMessage_imageUrl = Lens.lens (\GCMMessage' {imageUrl} -> imageUrl) (\s@GCMMessage' {} a -> s {imageUrl = a} :: GCMMessage)

-- | The URL to open in the recipient\'s default mobile browser, if a
-- recipient taps the push notification and the value of the Action
-- property is URL.
gCMMessage_url :: Lens.Lens' GCMMessage (Prelude.Maybe Prelude.Text)
gCMMessage_url = Lens.lens (\GCMMessage' {url} -> url) (\s@GCMMessage' {} a -> s {url = a} :: GCMMessage)

-- | The default message variables to use in the notification message. You
-- can override the default variables with individual address variables.
gCMMessage_substitutions :: Lens.Lens' GCMMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
gCMMessage_substitutions = Lens.lens (\GCMMessage' {substitutions} -> substitutions) (\s@GCMMessage' {} a -> s {substitutions = a} :: GCMMessage) Prelude.. Lens.mapping Lens.coerced

-- | The raw, JSON-formatted string to use as the payload for the
-- notification message. If specified, this value overrides all other
-- content for the message.
gCMMessage_rawContent :: Lens.Lens' GCMMessage (Prelude.Maybe Prelude.Text)
gCMMessage_rawContent = Lens.lens (\GCMMessage' {rawContent} -> rawContent) (\s@GCMMessage' {} a -> s {rawContent = a} :: GCMMessage)

-- | The sound to play when the recipient receives the push notification. You
-- can use the default stream or specify the file name of a sound resource
-- that\'s bundled in your app. On an Android platform, the sound file must
-- reside in \/res\/raw\/.
gCMMessage_sound :: Lens.Lens' GCMMessage (Prelude.Maybe Prelude.Text)
gCMMessage_sound = Lens.lens (\GCMMessage' {sound} -> sound) (\s@GCMMessage' {} a -> s {sound = a} :: GCMMessage)

-- | Specifies whether the notification is a silent push notification, which
-- is a push notification that doesn\'t display on a recipient\'s device.
-- Silent push notifications can be used for cases such as updating an
-- app\'s configuration or supporting phone home functionality.
gCMMessage_silentPush :: Lens.Lens' GCMMessage (Prelude.Maybe Prelude.Bool)
gCMMessage_silentPush = Lens.lens (\GCMMessage' {silentPush} -> silentPush) (\s@GCMMessage' {} a -> s {silentPush = a} :: GCMMessage)

-- | The URL of the large icon image to display in the content view of the
-- push notification.
gCMMessage_imageIconUrl :: Lens.Lens' GCMMessage (Prelude.Maybe Prelude.Text)
gCMMessage_imageIconUrl = Lens.lens (\GCMMessage' {imageIconUrl} -> imageIconUrl) (\s@GCMMessage' {} a -> s {imageIconUrl = a} :: GCMMessage)

-- | The title to display above the notification message on the recipient\'s
-- device.
gCMMessage_title :: Lens.Lens' GCMMessage (Prelude.Maybe Prelude.Text)
gCMMessage_title = Lens.lens (\GCMMessage' {title} -> title) (\s@GCMMessage' {} a -> s {title = a} :: GCMMessage)

-- | para>normal - The notification might be delayed. Delivery is optimized
-- for battery usage on the recipient\'s device. Use this value unless
-- immediate delivery is required.
--
-- \/listitem>
--
-- high - The notification is sent immediately and might wake a sleeping
-- device.
--
-- \/para>
--
-- Amazon Pinpoint specifies this value in the FCM priority parameter when
-- it sends the notification message to FCM.
--
-- The equivalent values for Apple Push Notification service (APNs) are 5,
-- for normal, and 10, for high. If you specify an APNs value for this
-- property, Amazon Pinpoint accepts and converts the value to the
-- corresponding FCM value.
gCMMessage_priority :: Lens.Lens' GCMMessage (Prelude.Maybe Prelude.Text)
gCMMessage_priority = Lens.lens (\GCMMessage' {priority} -> priority) (\s@GCMMessage' {} a -> s {priority = a} :: GCMMessage)

-- | The action to occur if the recipient taps the push notification. Valid
-- values are:
--
-- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
--     sent to the background. This is the default action.
--
-- -   DEEP_LINK - Your app opens and displays a designated user interface
--     in the app. This action uses the deep-linking features of the
--     Android platform.
--
-- -   URL - The default mobile browser on the recipient\'s device opens
--     and loads the web page at a URL that you specify.
gCMMessage_action :: Lens.Lens' GCMMessage (Prelude.Maybe Action)
gCMMessage_action = Lens.lens (\GCMMessage' {action} -> action) (\s@GCMMessage' {} a -> s {action = a} :: GCMMessage)

-- | The package name of the application where registration tokens must match
-- in order for the recipient to receive the message.
gCMMessage_restrictedPackageName :: Lens.Lens' GCMMessage (Prelude.Maybe Prelude.Text)
gCMMessage_restrictedPackageName = Lens.lens (\GCMMessage' {restrictedPackageName} -> restrictedPackageName) (\s@GCMMessage' {} a -> s {restrictedPackageName = a} :: GCMMessage)

-- | The JSON data payload to use for the push notification, if the
-- notification is a silent push notification. This payload is added to the
-- data.pinpoint.jsonBody object of the notification.
gCMMessage_data :: Lens.Lens' GCMMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
gCMMessage_data = Lens.lens (\GCMMessage' {data'} -> data') (\s@GCMMessage' {} a -> s {data' = a} :: GCMMessage) Prelude.. Lens.mapping Lens.coerced

-- | The URL of the small icon image to display in the status bar and the
-- content view of the push notification.
gCMMessage_smallImageIconUrl :: Lens.Lens' GCMMessage (Prelude.Maybe Prelude.Text)
gCMMessage_smallImageIconUrl = Lens.lens (\GCMMessage' {smallImageIconUrl} -> smallImageIconUrl) (\s@GCMMessage' {} a -> s {smallImageIconUrl = a} :: GCMMessage)

instance Prelude.Hashable GCMMessage where
  hashWithSalt _salt GCMMessage' {..} =
    _salt `Prelude.hashWithSalt` timeToLive
      `Prelude.hashWithSalt` collapseKey
      `Prelude.hashWithSalt` iconReference
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` imageUrl
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` substitutions
      `Prelude.hashWithSalt` rawContent
      `Prelude.hashWithSalt` sound
      `Prelude.hashWithSalt` silentPush
      `Prelude.hashWithSalt` imageIconUrl
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` restrictedPackageName
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` smallImageIconUrl

instance Prelude.NFData GCMMessage where
  rnf GCMMessage' {..} =
    Prelude.rnf timeToLive
      `Prelude.seq` Prelude.rnf collapseKey
      `Prelude.seq` Prelude.rnf iconReference
      `Prelude.seq` Prelude.rnf body
      `Prelude.seq` Prelude.rnf imageUrl
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf substitutions
      `Prelude.seq` Prelude.rnf rawContent
      `Prelude.seq` Prelude.rnf sound
      `Prelude.seq` Prelude.rnf silentPush
      `Prelude.seq` Prelude.rnf imageIconUrl
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf restrictedPackageName
      `Prelude.seq` Prelude.rnf data'
      `Prelude.seq` Prelude.rnf smallImageIconUrl

instance Core.ToJSON GCMMessage where
  toJSON GCMMessage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TimeToLive" Core..=) Prelude.<$> timeToLive,
            ("CollapseKey" Core..=) Prelude.<$> collapseKey,
            ("IconReference" Core..=) Prelude.<$> iconReference,
            ("Body" Core..=) Prelude.<$> body,
            ("ImageUrl" Core..=) Prelude.<$> imageUrl,
            ("Url" Core..=) Prelude.<$> url,
            ("Substitutions" Core..=) Prelude.<$> substitutions,
            ("RawContent" Core..=) Prelude.<$> rawContent,
            ("Sound" Core..=) Prelude.<$> sound,
            ("SilentPush" Core..=) Prelude.<$> silentPush,
            ("ImageIconUrl" Core..=) Prelude.<$> imageIconUrl,
            ("Title" Core..=) Prelude.<$> title,
            ("Priority" Core..=) Prelude.<$> priority,
            ("Action" Core..=) Prelude.<$> action,
            ("RestrictedPackageName" Core..=)
              Prelude.<$> restrictedPackageName,
            ("Data" Core..=) Prelude.<$> data',
            ("SmallImageIconUrl" Core..=)
              Prelude.<$> smallImageIconUrl
          ]
      )
