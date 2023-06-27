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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ChannelMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ChannelMessage where

import Amazonka.ChimeSDKMessaging.Types.ChannelMessagePersistenceType
import Amazonka.ChimeSDKMessaging.Types.ChannelMessageStatusStructure
import Amazonka.ChimeSDKMessaging.Types.ChannelMessageType
import Amazonka.ChimeSDKMessaging.Types.Identity
import Amazonka.ChimeSDKMessaging.Types.MessageAttributeValue
import Amazonka.ChimeSDKMessaging.Types.Target
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a message in a channel.
--
-- /See:/ 'newChannelMessage' smart constructor.
data ChannelMessage = ChannelMessage'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The content of the channel message. For Amazon Lex V2 bot responses,
    -- this field holds a list of messages originating from the bot. For more
    -- information, refer to
    -- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
    -- in the /Amazon Chime SDK Messaging Developer Guide/.
    content :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The content type of the channel message. For Amazon Lex V2 bot
    -- responses, the content type is @application\/amz-chime-lex-msgs@ for
    -- success responses and @application\/amz-chime-lex-error@ for failure
    -- responses. For more information, refer to
    -- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
    -- in the /Amazon Chime SDK Messaging Developer Guide/.
    contentType :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The time at which the message was created.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The time at which a message was edited.
    lastEditedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The time at which a message was updated.
    lastUpdatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The attributes for the channel message. For Amazon Lex V2 bot responses,
    -- the attributes are mapped to specific fields from the bot. For more
    -- information, refer to
    -- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
    -- in the /Amazon Chime SDK Messaging Developer Guide/.
    messageAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text MessageAttributeValue),
    -- | The ID of a message.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The message metadata.
    metadata :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The persistence setting for a channel message.
    persistence :: Prelude.Maybe ChannelMessagePersistenceType,
    -- | Hides the content of a message.
    redacted :: Prelude.Maybe Prelude.Bool,
    -- | The message sender.
    sender :: Prelude.Maybe Identity,
    -- | The status of the channel message.
    status :: Prelude.Maybe ChannelMessageStatusStructure,
    -- | The ID of the SubChannel.
    subChannelId :: Prelude.Maybe Prelude.Text,
    -- | The target of a message, a sender, a user, or a bot. Only the target and
    -- the sender can view targeted messages. Only users who can see targeted
    -- messages can take actions on them. However, administrators can delete
    -- targeted messages that they can’t see.
    target :: Prelude.Maybe (Prelude.NonEmpty Target),
    -- | The message type.
    type' :: Prelude.Maybe ChannelMessageType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'channelMessage_channelArn' - The ARN of the channel.
--
-- 'content', 'channelMessage_content' - The content of the channel message. For Amazon Lex V2 bot responses,
-- this field holds a list of messages originating from the bot. For more
-- information, refer to
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
-- in the /Amazon Chime SDK Messaging Developer Guide/.
--
-- 'contentType', 'channelMessage_contentType' - The content type of the channel message. For Amazon Lex V2 bot
-- responses, the content type is @application\/amz-chime-lex-msgs@ for
-- success responses and @application\/amz-chime-lex-error@ for failure
-- responses. For more information, refer to
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
-- in the /Amazon Chime SDK Messaging Developer Guide/.
--
-- 'createdTimestamp', 'channelMessage_createdTimestamp' - The time at which the message was created.
--
-- 'lastEditedTimestamp', 'channelMessage_lastEditedTimestamp' - The time at which a message was edited.
--
-- 'lastUpdatedTimestamp', 'channelMessage_lastUpdatedTimestamp' - The time at which a message was updated.
--
-- 'messageAttributes', 'channelMessage_messageAttributes' - The attributes for the channel message. For Amazon Lex V2 bot responses,
-- the attributes are mapped to specific fields from the bot. For more
-- information, refer to
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
-- in the /Amazon Chime SDK Messaging Developer Guide/.
--
-- 'messageId', 'channelMessage_messageId' - The ID of a message.
--
-- 'metadata', 'channelMessage_metadata' - The message metadata.
--
-- 'persistence', 'channelMessage_persistence' - The persistence setting for a channel message.
--
-- 'redacted', 'channelMessage_redacted' - Hides the content of a message.
--
-- 'sender', 'channelMessage_sender' - The message sender.
--
-- 'status', 'channelMessage_status' - The status of the channel message.
--
-- 'subChannelId', 'channelMessage_subChannelId' - The ID of the SubChannel.
--
-- 'target', 'channelMessage_target' - The target of a message, a sender, a user, or a bot. Only the target and
-- the sender can view targeted messages. Only users who can see targeted
-- messages can take actions on them. However, administrators can delete
-- targeted messages that they can’t see.
--
-- 'type'', 'channelMessage_type' - The message type.
newChannelMessage ::
  ChannelMessage
newChannelMessage =
  ChannelMessage'
    { channelArn = Prelude.Nothing,
      content = Prelude.Nothing,
      contentType = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      lastEditedTimestamp = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      messageAttributes = Prelude.Nothing,
      messageId = Prelude.Nothing,
      metadata = Prelude.Nothing,
      persistence = Prelude.Nothing,
      redacted = Prelude.Nothing,
      sender = Prelude.Nothing,
      status = Prelude.Nothing,
      subChannelId = Prelude.Nothing,
      target = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ARN of the channel.
channelMessage_channelArn :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.Text)
channelMessage_channelArn = Lens.lens (\ChannelMessage' {channelArn} -> channelArn) (\s@ChannelMessage' {} a -> s {channelArn = a} :: ChannelMessage)

-- | The content of the channel message. For Amazon Lex V2 bot responses,
-- this field holds a list of messages originating from the bot. For more
-- information, refer to
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
-- in the /Amazon Chime SDK Messaging Developer Guide/.
channelMessage_content :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.Text)
channelMessage_content = Lens.lens (\ChannelMessage' {content} -> content) (\s@ChannelMessage' {} a -> s {content = a} :: ChannelMessage) Prelude.. Lens.mapping Data._Sensitive

-- | The content type of the channel message. For Amazon Lex V2 bot
-- responses, the content type is @application\/amz-chime-lex-msgs@ for
-- success responses and @application\/amz-chime-lex-error@ for failure
-- responses. For more information, refer to
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
-- in the /Amazon Chime SDK Messaging Developer Guide/.
channelMessage_contentType :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.Text)
channelMessage_contentType = Lens.lens (\ChannelMessage' {contentType} -> contentType) (\s@ChannelMessage' {} a -> s {contentType = a} :: ChannelMessage) Prelude.. Lens.mapping Data._Sensitive

-- | The time at which the message was created.
channelMessage_createdTimestamp :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.UTCTime)
channelMessage_createdTimestamp = Lens.lens (\ChannelMessage' {createdTimestamp} -> createdTimestamp) (\s@ChannelMessage' {} a -> s {createdTimestamp = a} :: ChannelMessage) Prelude.. Lens.mapping Data._Time

-- | The time at which a message was edited.
channelMessage_lastEditedTimestamp :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.UTCTime)
channelMessage_lastEditedTimestamp = Lens.lens (\ChannelMessage' {lastEditedTimestamp} -> lastEditedTimestamp) (\s@ChannelMessage' {} a -> s {lastEditedTimestamp = a} :: ChannelMessage) Prelude.. Lens.mapping Data._Time

-- | The time at which a message was updated.
channelMessage_lastUpdatedTimestamp :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.UTCTime)
channelMessage_lastUpdatedTimestamp = Lens.lens (\ChannelMessage' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@ChannelMessage' {} a -> s {lastUpdatedTimestamp = a} :: ChannelMessage) Prelude.. Lens.mapping Data._Time

-- | The attributes for the channel message. For Amazon Lex V2 bot responses,
-- the attributes are mapped to specific fields from the bot. For more
-- information, refer to
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/appinstance-bots#process-response.html Processing responses from an AppInstanceBot>
-- in the /Amazon Chime SDK Messaging Developer Guide/.
channelMessage_messageAttributes :: Lens.Lens' ChannelMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text MessageAttributeValue))
channelMessage_messageAttributes = Lens.lens (\ChannelMessage' {messageAttributes} -> messageAttributes) (\s@ChannelMessage' {} a -> s {messageAttributes = a} :: ChannelMessage) Prelude.. Lens.mapping Lens.coerced

-- | The ID of a message.
channelMessage_messageId :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.Text)
channelMessage_messageId = Lens.lens (\ChannelMessage' {messageId} -> messageId) (\s@ChannelMessage' {} a -> s {messageId = a} :: ChannelMessage)

-- | The message metadata.
channelMessage_metadata :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.Text)
channelMessage_metadata = Lens.lens (\ChannelMessage' {metadata} -> metadata) (\s@ChannelMessage' {} a -> s {metadata = a} :: ChannelMessage) Prelude.. Lens.mapping Data._Sensitive

-- | The persistence setting for a channel message.
channelMessage_persistence :: Lens.Lens' ChannelMessage (Prelude.Maybe ChannelMessagePersistenceType)
channelMessage_persistence = Lens.lens (\ChannelMessage' {persistence} -> persistence) (\s@ChannelMessage' {} a -> s {persistence = a} :: ChannelMessage)

-- | Hides the content of a message.
channelMessage_redacted :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.Bool)
channelMessage_redacted = Lens.lens (\ChannelMessage' {redacted} -> redacted) (\s@ChannelMessage' {} a -> s {redacted = a} :: ChannelMessage)

-- | The message sender.
channelMessage_sender :: Lens.Lens' ChannelMessage (Prelude.Maybe Identity)
channelMessage_sender = Lens.lens (\ChannelMessage' {sender} -> sender) (\s@ChannelMessage' {} a -> s {sender = a} :: ChannelMessage)

-- | The status of the channel message.
channelMessage_status :: Lens.Lens' ChannelMessage (Prelude.Maybe ChannelMessageStatusStructure)
channelMessage_status = Lens.lens (\ChannelMessage' {status} -> status) (\s@ChannelMessage' {} a -> s {status = a} :: ChannelMessage)

-- | The ID of the SubChannel.
channelMessage_subChannelId :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.Text)
channelMessage_subChannelId = Lens.lens (\ChannelMessage' {subChannelId} -> subChannelId) (\s@ChannelMessage' {} a -> s {subChannelId = a} :: ChannelMessage)

-- | The target of a message, a sender, a user, or a bot. Only the target and
-- the sender can view targeted messages. Only users who can see targeted
-- messages can take actions on them. However, administrators can delete
-- targeted messages that they can’t see.
channelMessage_target :: Lens.Lens' ChannelMessage (Prelude.Maybe (Prelude.NonEmpty Target))
channelMessage_target = Lens.lens (\ChannelMessage' {target} -> target) (\s@ChannelMessage' {} a -> s {target = a} :: ChannelMessage) Prelude.. Lens.mapping Lens.coerced

-- | The message type.
channelMessage_type :: Lens.Lens' ChannelMessage (Prelude.Maybe ChannelMessageType)
channelMessage_type = Lens.lens (\ChannelMessage' {type'} -> type') (\s@ChannelMessage' {} a -> s {type' = a} :: ChannelMessage)

instance Data.FromJSON ChannelMessage where
  parseJSON =
    Data.withObject
      "ChannelMessage"
      ( \x ->
          ChannelMessage'
            Prelude.<$> (x Data..:? "ChannelArn")
            Prelude.<*> (x Data..:? "Content")
            Prelude.<*> (x Data..:? "ContentType")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "LastEditedTimestamp")
            Prelude.<*> (x Data..:? "LastUpdatedTimestamp")
            Prelude.<*> ( x
                            Data..:? "MessageAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "MessageId")
            Prelude.<*> (x Data..:? "Metadata")
            Prelude.<*> (x Data..:? "Persistence")
            Prelude.<*> (x Data..:? "Redacted")
            Prelude.<*> (x Data..:? "Sender")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "SubChannelId")
            Prelude.<*> (x Data..:? "Target")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable ChannelMessage where
  hashWithSalt _salt ChannelMessage' {..} =
    _salt
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` lastEditedTimestamp
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` messageAttributes
      `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` persistence
      `Prelude.hashWithSalt` redacted
      `Prelude.hashWithSalt` sender
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subChannelId
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ChannelMessage where
  rnf ChannelMessage' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf lastEditedTimestamp
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf messageAttributes
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf persistence
      `Prelude.seq` Prelude.rnf redacted
      `Prelude.seq` Prelude.rnf sender
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf subChannelId
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf type'
