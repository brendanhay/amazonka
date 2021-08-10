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
-- Module      : Network.AWS.Pinpoint.Types.BaiduMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.BaiduMessage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the settings for a one-time message that\'s sent directly to
-- an endpoint through the Baidu (Baidu Cloud Push) channel.
--
-- /See:/ 'newBaiduMessage' smart constructor.
data BaiduMessage = BaiduMessage'
  { -- | Specifies whether the notification is a silent push notification, which
    -- is a push notification that doesn\'t display on a recipient\'s device.
    -- Silent push notifications can be used for cases such as updating an
    -- app\'s configuration or supporting phone home functionality.
    silentPush :: Prelude.Maybe Prelude.Bool,
    -- | The URL of the large icon image to display in the content view of the
    -- push notification.
    imageIconUrl :: Prelude.Maybe Prelude.Text,
    -- | The JSON data payload to use for the push notification, if the
    -- notification is a silent push notification. This payload is added to the
    -- data.pinpoint.jsonBody object of the notification.
    data' :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The title to display above the notification message on the recipient\'s
    -- device.
    title :: Prelude.Maybe Prelude.Text,
    -- | The icon image name of the asset saved in your app.
    iconReference :: Prelude.Maybe Prelude.Text,
    -- | The body of the notification message.
    body :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, that the Baidu Cloud Push service should
    -- store the message if the recipient\'s device is offline. The default
    -- value and maximum supported time is 604,800 seconds (7 days).
    timeToLive :: Prelude.Maybe Prelude.Int,
    -- | The default message variables to use in the notification message. You
    -- can override the default variables with individual address variables.
    substitutions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The URL of an image to display in the push notification.
    imageUrl :: Prelude.Maybe Prelude.Text,
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
    -- | The sound to play when the recipient receives the push notification. You
    -- can use the default stream or specify the file name of a sound resource
    -- that\'s bundled in your app. On an Android platform, the sound file must
    -- reside in \/res\/raw\/.
    sound :: Prelude.Maybe Prelude.Text,
    -- | The URL to open in the recipient\'s default mobile browser, if a
    -- recipient taps the push notification and the value of the Action
    -- property is URL.
    url :: Prelude.Maybe Prelude.Text,
    -- | The URL of the small icon image to display in the status bar and the
    -- content view of the push notification.
    smallImageIconUrl :: Prelude.Maybe Prelude.Text,
    -- | The raw, JSON-formatted string to use as the payload for the
    -- notification message. If specified, this value overrides all other
    -- content for the message.
    rawContent :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BaiduMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'silentPush', 'baiduMessage_silentPush' - Specifies whether the notification is a silent push notification, which
-- is a push notification that doesn\'t display on a recipient\'s device.
-- Silent push notifications can be used for cases such as updating an
-- app\'s configuration or supporting phone home functionality.
--
-- 'imageIconUrl', 'baiduMessage_imageIconUrl' - The URL of the large icon image to display in the content view of the
-- push notification.
--
-- 'data'', 'baiduMessage_data' - The JSON data payload to use for the push notification, if the
-- notification is a silent push notification. This payload is added to the
-- data.pinpoint.jsonBody object of the notification.
--
-- 'title', 'baiduMessage_title' - The title to display above the notification message on the recipient\'s
-- device.
--
-- 'iconReference', 'baiduMessage_iconReference' - The icon image name of the asset saved in your app.
--
-- 'body', 'baiduMessage_body' - The body of the notification message.
--
-- 'timeToLive', 'baiduMessage_timeToLive' - The amount of time, in seconds, that the Baidu Cloud Push service should
-- store the message if the recipient\'s device is offline. The default
-- value and maximum supported time is 604,800 seconds (7 days).
--
-- 'substitutions', 'baiduMessage_substitutions' - The default message variables to use in the notification message. You
-- can override the default variables with individual address variables.
--
-- 'imageUrl', 'baiduMessage_imageUrl' - The URL of an image to display in the push notification.
--
-- 'action', 'baiduMessage_action' - The action to occur if the recipient taps the push notification. Valid
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
-- 'sound', 'baiduMessage_sound' - The sound to play when the recipient receives the push notification. You
-- can use the default stream or specify the file name of a sound resource
-- that\'s bundled in your app. On an Android platform, the sound file must
-- reside in \/res\/raw\/.
--
-- 'url', 'baiduMessage_url' - The URL to open in the recipient\'s default mobile browser, if a
-- recipient taps the push notification and the value of the Action
-- property is URL.
--
-- 'smallImageIconUrl', 'baiduMessage_smallImageIconUrl' - The URL of the small icon image to display in the status bar and the
-- content view of the push notification.
--
-- 'rawContent', 'baiduMessage_rawContent' - The raw, JSON-formatted string to use as the payload for the
-- notification message. If specified, this value overrides all other
-- content for the message.
newBaiduMessage ::
  BaiduMessage
newBaiduMessage =
  BaiduMessage'
    { silentPush = Prelude.Nothing,
      imageIconUrl = Prelude.Nothing,
      data' = Prelude.Nothing,
      title = Prelude.Nothing,
      iconReference = Prelude.Nothing,
      body = Prelude.Nothing,
      timeToLive = Prelude.Nothing,
      substitutions = Prelude.Nothing,
      imageUrl = Prelude.Nothing,
      action = Prelude.Nothing,
      sound = Prelude.Nothing,
      url = Prelude.Nothing,
      smallImageIconUrl = Prelude.Nothing,
      rawContent = Prelude.Nothing
    }

-- | Specifies whether the notification is a silent push notification, which
-- is a push notification that doesn\'t display on a recipient\'s device.
-- Silent push notifications can be used for cases such as updating an
-- app\'s configuration or supporting phone home functionality.
baiduMessage_silentPush :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Bool)
baiduMessage_silentPush = Lens.lens (\BaiduMessage' {silentPush} -> silentPush) (\s@BaiduMessage' {} a -> s {silentPush = a} :: BaiduMessage)

-- | The URL of the large icon image to display in the content view of the
-- push notification.
baiduMessage_imageIconUrl :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_imageIconUrl = Lens.lens (\BaiduMessage' {imageIconUrl} -> imageIconUrl) (\s@BaiduMessage' {} a -> s {imageIconUrl = a} :: BaiduMessage)

-- | The JSON data payload to use for the push notification, if the
-- notification is a silent push notification. This payload is added to the
-- data.pinpoint.jsonBody object of the notification.
baiduMessage_data :: Lens.Lens' BaiduMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
baiduMessage_data = Lens.lens (\BaiduMessage' {data'} -> data') (\s@BaiduMessage' {} a -> s {data' = a} :: BaiduMessage) Prelude.. Lens.mapping Lens._Coerce

-- | The title to display above the notification message on the recipient\'s
-- device.
baiduMessage_title :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_title = Lens.lens (\BaiduMessage' {title} -> title) (\s@BaiduMessage' {} a -> s {title = a} :: BaiduMessage)

-- | The icon image name of the asset saved in your app.
baiduMessage_iconReference :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_iconReference = Lens.lens (\BaiduMessage' {iconReference} -> iconReference) (\s@BaiduMessage' {} a -> s {iconReference = a} :: BaiduMessage)

-- | The body of the notification message.
baiduMessage_body :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_body = Lens.lens (\BaiduMessage' {body} -> body) (\s@BaiduMessage' {} a -> s {body = a} :: BaiduMessage)

-- | The amount of time, in seconds, that the Baidu Cloud Push service should
-- store the message if the recipient\'s device is offline. The default
-- value and maximum supported time is 604,800 seconds (7 days).
baiduMessage_timeToLive :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Int)
baiduMessage_timeToLive = Lens.lens (\BaiduMessage' {timeToLive} -> timeToLive) (\s@BaiduMessage' {} a -> s {timeToLive = a} :: BaiduMessage)

-- | The default message variables to use in the notification message. You
-- can override the default variables with individual address variables.
baiduMessage_substitutions :: Lens.Lens' BaiduMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
baiduMessage_substitutions = Lens.lens (\BaiduMessage' {substitutions} -> substitutions) (\s@BaiduMessage' {} a -> s {substitutions = a} :: BaiduMessage) Prelude.. Lens.mapping Lens._Coerce

-- | The URL of an image to display in the push notification.
baiduMessage_imageUrl :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_imageUrl = Lens.lens (\BaiduMessage' {imageUrl} -> imageUrl) (\s@BaiduMessage' {} a -> s {imageUrl = a} :: BaiduMessage)

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
baiduMessage_action :: Lens.Lens' BaiduMessage (Prelude.Maybe Action)
baiduMessage_action = Lens.lens (\BaiduMessage' {action} -> action) (\s@BaiduMessage' {} a -> s {action = a} :: BaiduMessage)

-- | The sound to play when the recipient receives the push notification. You
-- can use the default stream or specify the file name of a sound resource
-- that\'s bundled in your app. On an Android platform, the sound file must
-- reside in \/res\/raw\/.
baiduMessage_sound :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_sound = Lens.lens (\BaiduMessage' {sound} -> sound) (\s@BaiduMessage' {} a -> s {sound = a} :: BaiduMessage)

-- | The URL to open in the recipient\'s default mobile browser, if a
-- recipient taps the push notification and the value of the Action
-- property is URL.
baiduMessage_url :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_url = Lens.lens (\BaiduMessage' {url} -> url) (\s@BaiduMessage' {} a -> s {url = a} :: BaiduMessage)

-- | The URL of the small icon image to display in the status bar and the
-- content view of the push notification.
baiduMessage_smallImageIconUrl :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_smallImageIconUrl = Lens.lens (\BaiduMessage' {smallImageIconUrl} -> smallImageIconUrl) (\s@BaiduMessage' {} a -> s {smallImageIconUrl = a} :: BaiduMessage)

-- | The raw, JSON-formatted string to use as the payload for the
-- notification message. If specified, this value overrides all other
-- content for the message.
baiduMessage_rawContent :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_rawContent = Lens.lens (\BaiduMessage' {rawContent} -> rawContent) (\s@BaiduMessage' {} a -> s {rawContent = a} :: BaiduMessage)

instance Prelude.Hashable BaiduMessage

instance Prelude.NFData BaiduMessage

instance Core.ToJSON BaiduMessage where
  toJSON BaiduMessage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SilentPush" Core..=) Prelude.<$> silentPush,
            ("ImageIconUrl" Core..=) Prelude.<$> imageIconUrl,
            ("Data" Core..=) Prelude.<$> data',
            ("Title" Core..=) Prelude.<$> title,
            ("IconReference" Core..=) Prelude.<$> iconReference,
            ("Body" Core..=) Prelude.<$> body,
            ("TimeToLive" Core..=) Prelude.<$> timeToLive,
            ("Substitutions" Core..=) Prelude.<$> substitutions,
            ("ImageUrl" Core..=) Prelude.<$> imageUrl,
            ("Action" Core..=) Prelude.<$> action,
            ("Sound" Core..=) Prelude.<$> sound,
            ("Url" Core..=) Prelude.<$> url,
            ("SmallImageIconUrl" Core..=)
              Prelude.<$> smallImageIconUrl,
            ("RawContent" Core..=) Prelude.<$> rawContent
          ]
      )
