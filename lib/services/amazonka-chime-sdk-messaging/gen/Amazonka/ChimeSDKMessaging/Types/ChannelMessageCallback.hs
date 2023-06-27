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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ChannelMessageCallback
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ChannelMessageCallback where

import Amazonka.ChimeSDKMessaging.Types.MessageAttributeValue
import Amazonka.ChimeSDKMessaging.Types.PushNotificationConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Stores information about a callback.
--
-- /See:/ 'newChannelMessageCallback' smart constructor.
data ChannelMessageCallback = ChannelMessageCallback'
  { -- | The message content. For Amazon Lex V2 bot responses, this field holds a
    -- list of messages originating from the bot. For more information, refer
    -- to
    -- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
    -- in the /Amazon Chime SDK Messaging Developer Guide/.
    content :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The content type of the call-back message. For Amazon Lex V2 bot
    -- responses, the content type is @application\/amz-chime-lex-msgs@ for
    -- success responses and @application\/amz-chime-lex-error@ for failure
    -- responses. For more information, refer to
    -- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
    -- in the /Amazon Chime SDK Messaging Developer Guide/.
    contentType :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The attributes for the channel message. For Amazon Lex V2 bot responses,
    -- the attributes are mapped to specific fields from the bot. For more
    -- information, refer to
    -- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
    -- in the /Amazon Chime SDK Messaging Developer Guide/.
    messageAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text MessageAttributeValue),
    -- | The message metadata.
    metadata :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The push notification configuration of the message.
    pushNotification :: Prelude.Maybe PushNotificationConfiguration,
    -- | The ID of the SubChannel.
    subChannelId :: Prelude.Maybe Prelude.Text,
    -- | The message ID.
    messageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelMessageCallback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'channelMessageCallback_content' - The message content. For Amazon Lex V2 bot responses, this field holds a
-- list of messages originating from the bot. For more information, refer
-- to
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
-- in the /Amazon Chime SDK Messaging Developer Guide/.
--
-- 'contentType', 'channelMessageCallback_contentType' - The content type of the call-back message. For Amazon Lex V2 bot
-- responses, the content type is @application\/amz-chime-lex-msgs@ for
-- success responses and @application\/amz-chime-lex-error@ for failure
-- responses. For more information, refer to
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
-- in the /Amazon Chime SDK Messaging Developer Guide/.
--
-- 'messageAttributes', 'channelMessageCallback_messageAttributes' - The attributes for the channel message. For Amazon Lex V2 bot responses,
-- the attributes are mapped to specific fields from the bot. For more
-- information, refer to
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
-- in the /Amazon Chime SDK Messaging Developer Guide/.
--
-- 'metadata', 'channelMessageCallback_metadata' - The message metadata.
--
-- 'pushNotification', 'channelMessageCallback_pushNotification' - The push notification configuration of the message.
--
-- 'subChannelId', 'channelMessageCallback_subChannelId' - The ID of the SubChannel.
--
-- 'messageId', 'channelMessageCallback_messageId' - The message ID.
newChannelMessageCallback ::
  -- | 'messageId'
  Prelude.Text ->
  ChannelMessageCallback
newChannelMessageCallback pMessageId_ =
  ChannelMessageCallback'
    { content = Prelude.Nothing,
      contentType = Prelude.Nothing,
      messageAttributes = Prelude.Nothing,
      metadata = Prelude.Nothing,
      pushNotification = Prelude.Nothing,
      subChannelId = Prelude.Nothing,
      messageId = pMessageId_
    }

-- | The message content. For Amazon Lex V2 bot responses, this field holds a
-- list of messages originating from the bot. For more information, refer
-- to
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
-- in the /Amazon Chime SDK Messaging Developer Guide/.
channelMessageCallback_content :: Lens.Lens' ChannelMessageCallback (Prelude.Maybe Prelude.Text)
channelMessageCallback_content = Lens.lens (\ChannelMessageCallback' {content} -> content) (\s@ChannelMessageCallback' {} a -> s {content = a} :: ChannelMessageCallback) Prelude.. Lens.mapping Data._Sensitive

-- | The content type of the call-back message. For Amazon Lex V2 bot
-- responses, the content type is @application\/amz-chime-lex-msgs@ for
-- success responses and @application\/amz-chime-lex-error@ for failure
-- responses. For more information, refer to
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
-- in the /Amazon Chime SDK Messaging Developer Guide/.
channelMessageCallback_contentType :: Lens.Lens' ChannelMessageCallback (Prelude.Maybe Prelude.Text)
channelMessageCallback_contentType = Lens.lens (\ChannelMessageCallback' {contentType} -> contentType) (\s@ChannelMessageCallback' {} a -> s {contentType = a} :: ChannelMessageCallback) Prelude.. Lens.mapping Data._Sensitive

-- | The attributes for the channel message. For Amazon Lex V2 bot responses,
-- the attributes are mapped to specific fields from the bot. For more
-- information, refer to
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
-- in the /Amazon Chime SDK Messaging Developer Guide/.
channelMessageCallback_messageAttributes :: Lens.Lens' ChannelMessageCallback (Prelude.Maybe (Prelude.HashMap Prelude.Text MessageAttributeValue))
channelMessageCallback_messageAttributes = Lens.lens (\ChannelMessageCallback' {messageAttributes} -> messageAttributes) (\s@ChannelMessageCallback' {} a -> s {messageAttributes = a} :: ChannelMessageCallback) Prelude.. Lens.mapping Lens.coerced

-- | The message metadata.
channelMessageCallback_metadata :: Lens.Lens' ChannelMessageCallback (Prelude.Maybe Prelude.Text)
channelMessageCallback_metadata = Lens.lens (\ChannelMessageCallback' {metadata} -> metadata) (\s@ChannelMessageCallback' {} a -> s {metadata = a} :: ChannelMessageCallback) Prelude.. Lens.mapping Data._Sensitive

-- | The push notification configuration of the message.
channelMessageCallback_pushNotification :: Lens.Lens' ChannelMessageCallback (Prelude.Maybe PushNotificationConfiguration)
channelMessageCallback_pushNotification = Lens.lens (\ChannelMessageCallback' {pushNotification} -> pushNotification) (\s@ChannelMessageCallback' {} a -> s {pushNotification = a} :: ChannelMessageCallback)

-- | The ID of the SubChannel.
channelMessageCallback_subChannelId :: Lens.Lens' ChannelMessageCallback (Prelude.Maybe Prelude.Text)
channelMessageCallback_subChannelId = Lens.lens (\ChannelMessageCallback' {subChannelId} -> subChannelId) (\s@ChannelMessageCallback' {} a -> s {subChannelId = a} :: ChannelMessageCallback)

-- | The message ID.
channelMessageCallback_messageId :: Lens.Lens' ChannelMessageCallback Prelude.Text
channelMessageCallback_messageId = Lens.lens (\ChannelMessageCallback' {messageId} -> messageId) (\s@ChannelMessageCallback' {} a -> s {messageId = a} :: ChannelMessageCallback)

instance Prelude.Hashable ChannelMessageCallback where
  hashWithSalt _salt ChannelMessageCallback' {..} =
    _salt
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` messageAttributes
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` pushNotification
      `Prelude.hashWithSalt` subChannelId
      `Prelude.hashWithSalt` messageId

instance Prelude.NFData ChannelMessageCallback where
  rnf ChannelMessageCallback' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf messageAttributes
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf pushNotification
      `Prelude.seq` Prelude.rnf subChannelId
      `Prelude.seq` Prelude.rnf messageId

instance Data.ToJSON ChannelMessageCallback where
  toJSON ChannelMessageCallback' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Content" Data..=) Prelude.<$> content,
            ("ContentType" Data..=) Prelude.<$> contentType,
            ("MessageAttributes" Data..=)
              Prelude.<$> messageAttributes,
            ("Metadata" Data..=) Prelude.<$> metadata,
            ("PushNotification" Data..=)
              Prelude.<$> pushNotification,
            ("SubChannelId" Data..=) Prelude.<$> subChannelId,
            Prelude.Just ("MessageId" Data..= messageId)
          ]
      )
