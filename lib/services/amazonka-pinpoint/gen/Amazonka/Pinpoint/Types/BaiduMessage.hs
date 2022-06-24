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
-- Module      : Amazonka.Pinpoint.Types.BaiduMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.BaiduMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.Action
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings for a one-time message that\'s sent directly to
-- an endpoint through the Baidu (Baidu Cloud Push) channel.
--
-- /See:/ 'newBaiduMessage' smart constructor.
data BaiduMessage = BaiduMessage'
  { -- | The amount of time, in seconds, that the Baidu Cloud Push service should
    -- store the message if the recipient\'s device is offline. The default
    -- value and maximum supported time is 604,800 seconds (7 days).
    timeToLive :: Prelude.Maybe Prelude.Int,
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
-- Create a value of 'BaiduMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeToLive', 'baiduMessage_timeToLive' - The amount of time, in seconds, that the Baidu Cloud Push service should
-- store the message if the recipient\'s device is offline. The default
-- value and maximum supported time is 604,800 seconds (7 days).
--
-- 'iconReference', 'baiduMessage_iconReference' - The icon image name of the asset saved in your app.
--
-- 'body', 'baiduMessage_body' - The body of the notification message.
--
-- 'imageUrl', 'baiduMessage_imageUrl' - The URL of an image to display in the push notification.
--
-- 'url', 'baiduMessage_url' - The URL to open in the recipient\'s default mobile browser, if a
-- recipient taps the push notification and the value of the Action
-- property is URL.
--
-- 'substitutions', 'baiduMessage_substitutions' - The default message variables to use in the notification message. You
-- can override the default variables with individual address variables.
--
-- 'rawContent', 'baiduMessage_rawContent' - The raw, JSON-formatted string to use as the payload for the
-- notification message. If specified, this value overrides all other
-- content for the message.
--
-- 'sound', 'baiduMessage_sound' - The sound to play when the recipient receives the push notification. You
-- can use the default stream or specify the file name of a sound resource
-- that\'s bundled in your app. On an Android platform, the sound file must
-- reside in \/res\/raw\/.
--
-- 'silentPush', 'baiduMessage_silentPush' - Specifies whether the notification is a silent push notification, which
-- is a push notification that doesn\'t display on a recipient\'s device.
-- Silent push notifications can be used for cases such as updating an
-- app\'s configuration or supporting phone home functionality.
--
-- 'imageIconUrl', 'baiduMessage_imageIconUrl' - The URL of the large icon image to display in the content view of the
-- push notification.
--
-- 'title', 'baiduMessage_title' - The title to display above the notification message on the recipient\'s
-- device.
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
-- 'data'', 'baiduMessage_data' - The JSON data payload to use for the push notification, if the
-- notification is a silent push notification. This payload is added to the
-- data.pinpoint.jsonBody object of the notification.
--
-- 'smallImageIconUrl', 'baiduMessage_smallImageIconUrl' - The URL of the small icon image to display in the status bar and the
-- content view of the push notification.
newBaiduMessage ::
  BaiduMessage
newBaiduMessage =
  BaiduMessage'
    { timeToLive = Prelude.Nothing,
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
      action = Prelude.Nothing,
      data' = Prelude.Nothing,
      smallImageIconUrl = Prelude.Nothing
    }

-- | The amount of time, in seconds, that the Baidu Cloud Push service should
-- store the message if the recipient\'s device is offline. The default
-- value and maximum supported time is 604,800 seconds (7 days).
baiduMessage_timeToLive :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Int)
baiduMessage_timeToLive = Lens.lens (\BaiduMessage' {timeToLive} -> timeToLive) (\s@BaiduMessage' {} a -> s {timeToLive = a} :: BaiduMessage)

-- | The icon image name of the asset saved in your app.
baiduMessage_iconReference :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_iconReference = Lens.lens (\BaiduMessage' {iconReference} -> iconReference) (\s@BaiduMessage' {} a -> s {iconReference = a} :: BaiduMessage)

-- | The body of the notification message.
baiduMessage_body :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_body = Lens.lens (\BaiduMessage' {body} -> body) (\s@BaiduMessage' {} a -> s {body = a} :: BaiduMessage)

-- | The URL of an image to display in the push notification.
baiduMessage_imageUrl :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_imageUrl = Lens.lens (\BaiduMessage' {imageUrl} -> imageUrl) (\s@BaiduMessage' {} a -> s {imageUrl = a} :: BaiduMessage)

-- | The URL to open in the recipient\'s default mobile browser, if a
-- recipient taps the push notification and the value of the Action
-- property is URL.
baiduMessage_url :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_url = Lens.lens (\BaiduMessage' {url} -> url) (\s@BaiduMessage' {} a -> s {url = a} :: BaiduMessage)

-- | The default message variables to use in the notification message. You
-- can override the default variables with individual address variables.
baiduMessage_substitutions :: Lens.Lens' BaiduMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
baiduMessage_substitutions = Lens.lens (\BaiduMessage' {substitutions} -> substitutions) (\s@BaiduMessage' {} a -> s {substitutions = a} :: BaiduMessage) Prelude.. Lens.mapping Lens.coerced

-- | The raw, JSON-formatted string to use as the payload for the
-- notification message. If specified, this value overrides all other
-- content for the message.
baiduMessage_rawContent :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_rawContent = Lens.lens (\BaiduMessage' {rawContent} -> rawContent) (\s@BaiduMessage' {} a -> s {rawContent = a} :: BaiduMessage)

-- | The sound to play when the recipient receives the push notification. You
-- can use the default stream or specify the file name of a sound resource
-- that\'s bundled in your app. On an Android platform, the sound file must
-- reside in \/res\/raw\/.
baiduMessage_sound :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_sound = Lens.lens (\BaiduMessage' {sound} -> sound) (\s@BaiduMessage' {} a -> s {sound = a} :: BaiduMessage)

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

-- | The title to display above the notification message on the recipient\'s
-- device.
baiduMessage_title :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_title = Lens.lens (\BaiduMessage' {title} -> title) (\s@BaiduMessage' {} a -> s {title = a} :: BaiduMessage)

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

-- | The JSON data payload to use for the push notification, if the
-- notification is a silent push notification. This payload is added to the
-- data.pinpoint.jsonBody object of the notification.
baiduMessage_data :: Lens.Lens' BaiduMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
baiduMessage_data = Lens.lens (\BaiduMessage' {data'} -> data') (\s@BaiduMessage' {} a -> s {data' = a} :: BaiduMessage) Prelude.. Lens.mapping Lens.coerced

-- | The URL of the small icon image to display in the status bar and the
-- content view of the push notification.
baiduMessage_smallImageIconUrl :: Lens.Lens' BaiduMessage (Prelude.Maybe Prelude.Text)
baiduMessage_smallImageIconUrl = Lens.lens (\BaiduMessage' {smallImageIconUrl} -> smallImageIconUrl) (\s@BaiduMessage' {} a -> s {smallImageIconUrl = a} :: BaiduMessage)

instance Prelude.Hashable BaiduMessage where
  hashWithSalt _salt BaiduMessage' {..} =
    _salt `Prelude.hashWithSalt` timeToLive
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
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` smallImageIconUrl

instance Prelude.NFData BaiduMessage where
  rnf BaiduMessage' {..} =
    Prelude.rnf timeToLive
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
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf data'
      `Prelude.seq` Prelude.rnf smallImageIconUrl

instance Core.ToJSON BaiduMessage where
  toJSON BaiduMessage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TimeToLive" Core..=) Prelude.<$> timeToLive,
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
            ("Action" Core..=) Prelude.<$> action,
            ("Data" Core..=) Prelude.<$> data',
            ("SmallImageIconUrl" Core..=)
              Prelude.<$> smallImageIconUrl
          ]
      )
