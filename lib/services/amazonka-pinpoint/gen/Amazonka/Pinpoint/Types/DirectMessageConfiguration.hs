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
-- Module      : Amazonka.Pinpoint.Types.DirectMessageConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.DirectMessageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.ADMMessage
import Amazonka.Pinpoint.Types.APNSMessage
import Amazonka.Pinpoint.Types.BaiduMessage
import Amazonka.Pinpoint.Types.DefaultMessage
import Amazonka.Pinpoint.Types.DefaultPushNotificationMessage
import Amazonka.Pinpoint.Types.EmailMessage
import Amazonka.Pinpoint.Types.GCMMessage
import Amazonka.Pinpoint.Types.SMSMessage
import Amazonka.Pinpoint.Types.VoiceMessage
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings and content for the default message and any
-- default messages that you tailored for specific channels.
--
-- /See:/ 'newDirectMessageConfiguration' smart constructor.
data DirectMessageConfiguration = DirectMessageConfiguration'
  { -- | The default push notification message for the ADM (Amazon Device
    -- Messaging) channel. This message overrides the default push notification
    -- message (DefaultPushNotificationMessage).
    aDMMessage :: Prelude.Maybe ADMMessage,
    -- | The default message for the SMS channel. This message overrides the
    -- default message (DefaultMessage).
    sMSMessage :: Prelude.Maybe SMSMessage,
    -- | The default message for the voice channel. This message overrides the
    -- default message (DefaultMessage).
    voiceMessage :: Prelude.Maybe VoiceMessage,
    -- | The default message for all channels.
    defaultMessage :: Prelude.Maybe DefaultMessage,
    -- | The default push notification message for the APNs (Apple Push
    -- Notification service) channel. This message overrides the default push
    -- notification message (DefaultPushNotificationMessage).
    aPNSMessage :: Prelude.Maybe APNSMessage,
    -- | The default push notification message for the GCM channel, which is used
    -- to send notifications through the Firebase Cloud Messaging (FCM),
    -- formerly Google Cloud Messaging (GCM), service. This message overrides
    -- the default push notification message (DefaultPushNotificationMessage).
    gCMMessage :: Prelude.Maybe GCMMessage,
    -- | The default push notification message for all push notification
    -- channels.
    defaultPushNotificationMessage :: Prelude.Maybe DefaultPushNotificationMessage,
    -- | The default message for the email channel. This message overrides the
    -- default message (DefaultMessage).
    emailMessage :: Prelude.Maybe EmailMessage,
    -- | The default push notification message for the Baidu (Baidu Cloud Push)
    -- channel. This message overrides the default push notification message
    -- (DefaultPushNotificationMessage).
    baiduMessage :: Prelude.Maybe BaiduMessage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DirectMessageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aDMMessage', 'directMessageConfiguration_aDMMessage' - The default push notification message for the ADM (Amazon Device
-- Messaging) channel. This message overrides the default push notification
-- message (DefaultPushNotificationMessage).
--
-- 'sMSMessage', 'directMessageConfiguration_sMSMessage' - The default message for the SMS channel. This message overrides the
-- default message (DefaultMessage).
--
-- 'voiceMessage', 'directMessageConfiguration_voiceMessage' - The default message for the voice channel. This message overrides the
-- default message (DefaultMessage).
--
-- 'defaultMessage', 'directMessageConfiguration_defaultMessage' - The default message for all channels.
--
-- 'aPNSMessage', 'directMessageConfiguration_aPNSMessage' - The default push notification message for the APNs (Apple Push
-- Notification service) channel. This message overrides the default push
-- notification message (DefaultPushNotificationMessage).
--
-- 'gCMMessage', 'directMessageConfiguration_gCMMessage' - The default push notification message for the GCM channel, which is used
-- to send notifications through the Firebase Cloud Messaging (FCM),
-- formerly Google Cloud Messaging (GCM), service. This message overrides
-- the default push notification message (DefaultPushNotificationMessage).
--
-- 'defaultPushNotificationMessage', 'directMessageConfiguration_defaultPushNotificationMessage' - The default push notification message for all push notification
-- channels.
--
-- 'emailMessage', 'directMessageConfiguration_emailMessage' - The default message for the email channel. This message overrides the
-- default message (DefaultMessage).
--
-- 'baiduMessage', 'directMessageConfiguration_baiduMessage' - The default push notification message for the Baidu (Baidu Cloud Push)
-- channel. This message overrides the default push notification message
-- (DefaultPushNotificationMessage).
newDirectMessageConfiguration ::
  DirectMessageConfiguration
newDirectMessageConfiguration =
  DirectMessageConfiguration'
    { aDMMessage =
        Prelude.Nothing,
      sMSMessage = Prelude.Nothing,
      voiceMessage = Prelude.Nothing,
      defaultMessage = Prelude.Nothing,
      aPNSMessage = Prelude.Nothing,
      gCMMessage = Prelude.Nothing,
      defaultPushNotificationMessage =
        Prelude.Nothing,
      emailMessage = Prelude.Nothing,
      baiduMessage = Prelude.Nothing
    }

-- | The default push notification message for the ADM (Amazon Device
-- Messaging) channel. This message overrides the default push notification
-- message (DefaultPushNotificationMessage).
directMessageConfiguration_aDMMessage :: Lens.Lens' DirectMessageConfiguration (Prelude.Maybe ADMMessage)
directMessageConfiguration_aDMMessage = Lens.lens (\DirectMessageConfiguration' {aDMMessage} -> aDMMessage) (\s@DirectMessageConfiguration' {} a -> s {aDMMessage = a} :: DirectMessageConfiguration)

-- | The default message for the SMS channel. This message overrides the
-- default message (DefaultMessage).
directMessageConfiguration_sMSMessage :: Lens.Lens' DirectMessageConfiguration (Prelude.Maybe SMSMessage)
directMessageConfiguration_sMSMessage = Lens.lens (\DirectMessageConfiguration' {sMSMessage} -> sMSMessage) (\s@DirectMessageConfiguration' {} a -> s {sMSMessage = a} :: DirectMessageConfiguration)

-- | The default message for the voice channel. This message overrides the
-- default message (DefaultMessage).
directMessageConfiguration_voiceMessage :: Lens.Lens' DirectMessageConfiguration (Prelude.Maybe VoiceMessage)
directMessageConfiguration_voiceMessage = Lens.lens (\DirectMessageConfiguration' {voiceMessage} -> voiceMessage) (\s@DirectMessageConfiguration' {} a -> s {voiceMessage = a} :: DirectMessageConfiguration)

-- | The default message for all channels.
directMessageConfiguration_defaultMessage :: Lens.Lens' DirectMessageConfiguration (Prelude.Maybe DefaultMessage)
directMessageConfiguration_defaultMessage = Lens.lens (\DirectMessageConfiguration' {defaultMessage} -> defaultMessage) (\s@DirectMessageConfiguration' {} a -> s {defaultMessage = a} :: DirectMessageConfiguration)

-- | The default push notification message for the APNs (Apple Push
-- Notification service) channel. This message overrides the default push
-- notification message (DefaultPushNotificationMessage).
directMessageConfiguration_aPNSMessage :: Lens.Lens' DirectMessageConfiguration (Prelude.Maybe APNSMessage)
directMessageConfiguration_aPNSMessage = Lens.lens (\DirectMessageConfiguration' {aPNSMessage} -> aPNSMessage) (\s@DirectMessageConfiguration' {} a -> s {aPNSMessage = a} :: DirectMessageConfiguration)

-- | The default push notification message for the GCM channel, which is used
-- to send notifications through the Firebase Cloud Messaging (FCM),
-- formerly Google Cloud Messaging (GCM), service. This message overrides
-- the default push notification message (DefaultPushNotificationMessage).
directMessageConfiguration_gCMMessage :: Lens.Lens' DirectMessageConfiguration (Prelude.Maybe GCMMessage)
directMessageConfiguration_gCMMessage = Lens.lens (\DirectMessageConfiguration' {gCMMessage} -> gCMMessage) (\s@DirectMessageConfiguration' {} a -> s {gCMMessage = a} :: DirectMessageConfiguration)

-- | The default push notification message for all push notification
-- channels.
directMessageConfiguration_defaultPushNotificationMessage :: Lens.Lens' DirectMessageConfiguration (Prelude.Maybe DefaultPushNotificationMessage)
directMessageConfiguration_defaultPushNotificationMessage = Lens.lens (\DirectMessageConfiguration' {defaultPushNotificationMessage} -> defaultPushNotificationMessage) (\s@DirectMessageConfiguration' {} a -> s {defaultPushNotificationMessage = a} :: DirectMessageConfiguration)

-- | The default message for the email channel. This message overrides the
-- default message (DefaultMessage).
directMessageConfiguration_emailMessage :: Lens.Lens' DirectMessageConfiguration (Prelude.Maybe EmailMessage)
directMessageConfiguration_emailMessage = Lens.lens (\DirectMessageConfiguration' {emailMessage} -> emailMessage) (\s@DirectMessageConfiguration' {} a -> s {emailMessage = a} :: DirectMessageConfiguration)

-- | The default push notification message for the Baidu (Baidu Cloud Push)
-- channel. This message overrides the default push notification message
-- (DefaultPushNotificationMessage).
directMessageConfiguration_baiduMessage :: Lens.Lens' DirectMessageConfiguration (Prelude.Maybe BaiduMessage)
directMessageConfiguration_baiduMessage = Lens.lens (\DirectMessageConfiguration' {baiduMessage} -> baiduMessage) (\s@DirectMessageConfiguration' {} a -> s {baiduMessage = a} :: DirectMessageConfiguration)

instance Prelude.Hashable DirectMessageConfiguration where
  hashWithSalt _salt DirectMessageConfiguration' {..} =
    _salt `Prelude.hashWithSalt` aDMMessage
      `Prelude.hashWithSalt` sMSMessage
      `Prelude.hashWithSalt` voiceMessage
      `Prelude.hashWithSalt` defaultMessage
      `Prelude.hashWithSalt` aPNSMessage
      `Prelude.hashWithSalt` gCMMessage
      `Prelude.hashWithSalt` defaultPushNotificationMessage
      `Prelude.hashWithSalt` emailMessage
      `Prelude.hashWithSalt` baiduMessage

instance Prelude.NFData DirectMessageConfiguration where
  rnf DirectMessageConfiguration' {..} =
    Prelude.rnf aDMMessage
      `Prelude.seq` Prelude.rnf sMSMessage
      `Prelude.seq` Prelude.rnf voiceMessage
      `Prelude.seq` Prelude.rnf defaultMessage
      `Prelude.seq` Prelude.rnf aPNSMessage
      `Prelude.seq` Prelude.rnf gCMMessage
      `Prelude.seq` Prelude.rnf defaultPushNotificationMessage
      `Prelude.seq` Prelude.rnf emailMessage
      `Prelude.seq` Prelude.rnf baiduMessage

instance Core.ToJSON DirectMessageConfiguration where
  toJSON DirectMessageConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ADMMessage" Core..=) Prelude.<$> aDMMessage,
            ("SMSMessage" Core..=) Prelude.<$> sMSMessage,
            ("VoiceMessage" Core..=) Prelude.<$> voiceMessage,
            ("DefaultMessage" Core..=)
              Prelude.<$> defaultMessage,
            ("APNSMessage" Core..=) Prelude.<$> aPNSMessage,
            ("GCMMessage" Core..=) Prelude.<$> gCMMessage,
            ("DefaultPushNotificationMessage" Core..=)
              Prelude.<$> defaultPushNotificationMessage,
            ("EmailMessage" Core..=) Prelude.<$> emailMessage,
            ("BaiduMessage" Core..=) Prelude.<$> baiduMessage
          ]
      )
