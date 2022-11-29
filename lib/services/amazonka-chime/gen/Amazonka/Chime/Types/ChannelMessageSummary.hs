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
-- Module      : Amazonka.Chime.Types.ChannelMessageSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.ChannelMessageSummary where

import Amazonka.Chime.Types.ChannelMessageType
import Amazonka.Chime.Types.Identity
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Summary of the messages in a @Channel@.
--
-- /See:/ 'newChannelMessageSummary' smart constructor.
data ChannelMessageSummary = ChannelMessageSummary'
  { -- | The time at which a message was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The type of message.
    type' :: Prelude.Maybe ChannelMessageType,
    -- | The metadata of the message.
    metadata :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The time at which the message summary was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Indicates whether a message was redacted.
    redacted :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the message.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The time at which a message was last edited.
    lastEditedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The message sender.
    sender :: Prelude.Maybe Identity,
    -- | The content of the message.
    content :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelMessageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTimestamp', 'channelMessageSummary_lastUpdatedTimestamp' - The time at which a message was last updated.
--
-- 'type'', 'channelMessageSummary_type' - The type of message.
--
-- 'metadata', 'channelMessageSummary_metadata' - The metadata of the message.
--
-- 'createdTimestamp', 'channelMessageSummary_createdTimestamp' - The time at which the message summary was created.
--
-- 'redacted', 'channelMessageSummary_redacted' - Indicates whether a message was redacted.
--
-- 'messageId', 'channelMessageSummary_messageId' - The ID of the message.
--
-- 'lastEditedTimestamp', 'channelMessageSummary_lastEditedTimestamp' - The time at which a message was last edited.
--
-- 'sender', 'channelMessageSummary_sender' - The message sender.
--
-- 'content', 'channelMessageSummary_content' - The content of the message.
newChannelMessageSummary ::
  ChannelMessageSummary
newChannelMessageSummary =
  ChannelMessageSummary'
    { lastUpdatedTimestamp =
        Prelude.Nothing,
      type' = Prelude.Nothing,
      metadata = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      redacted = Prelude.Nothing,
      messageId = Prelude.Nothing,
      lastEditedTimestamp = Prelude.Nothing,
      sender = Prelude.Nothing,
      content = Prelude.Nothing
    }

-- | The time at which a message was last updated.
channelMessageSummary_lastUpdatedTimestamp :: Lens.Lens' ChannelMessageSummary (Prelude.Maybe Prelude.UTCTime)
channelMessageSummary_lastUpdatedTimestamp = Lens.lens (\ChannelMessageSummary' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@ChannelMessageSummary' {} a -> s {lastUpdatedTimestamp = a} :: ChannelMessageSummary) Prelude.. Lens.mapping Core._Time

-- | The type of message.
channelMessageSummary_type :: Lens.Lens' ChannelMessageSummary (Prelude.Maybe ChannelMessageType)
channelMessageSummary_type = Lens.lens (\ChannelMessageSummary' {type'} -> type') (\s@ChannelMessageSummary' {} a -> s {type' = a} :: ChannelMessageSummary)

-- | The metadata of the message.
channelMessageSummary_metadata :: Lens.Lens' ChannelMessageSummary (Prelude.Maybe Prelude.Text)
channelMessageSummary_metadata = Lens.lens (\ChannelMessageSummary' {metadata} -> metadata) (\s@ChannelMessageSummary' {} a -> s {metadata = a} :: ChannelMessageSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The time at which the message summary was created.
channelMessageSummary_createdTimestamp :: Lens.Lens' ChannelMessageSummary (Prelude.Maybe Prelude.UTCTime)
channelMessageSummary_createdTimestamp = Lens.lens (\ChannelMessageSummary' {createdTimestamp} -> createdTimestamp) (\s@ChannelMessageSummary' {} a -> s {createdTimestamp = a} :: ChannelMessageSummary) Prelude.. Lens.mapping Core._Time

-- | Indicates whether a message was redacted.
channelMessageSummary_redacted :: Lens.Lens' ChannelMessageSummary (Prelude.Maybe Prelude.Bool)
channelMessageSummary_redacted = Lens.lens (\ChannelMessageSummary' {redacted} -> redacted) (\s@ChannelMessageSummary' {} a -> s {redacted = a} :: ChannelMessageSummary)

-- | The ID of the message.
channelMessageSummary_messageId :: Lens.Lens' ChannelMessageSummary (Prelude.Maybe Prelude.Text)
channelMessageSummary_messageId = Lens.lens (\ChannelMessageSummary' {messageId} -> messageId) (\s@ChannelMessageSummary' {} a -> s {messageId = a} :: ChannelMessageSummary)

-- | The time at which a message was last edited.
channelMessageSummary_lastEditedTimestamp :: Lens.Lens' ChannelMessageSummary (Prelude.Maybe Prelude.UTCTime)
channelMessageSummary_lastEditedTimestamp = Lens.lens (\ChannelMessageSummary' {lastEditedTimestamp} -> lastEditedTimestamp) (\s@ChannelMessageSummary' {} a -> s {lastEditedTimestamp = a} :: ChannelMessageSummary) Prelude.. Lens.mapping Core._Time

-- | The message sender.
channelMessageSummary_sender :: Lens.Lens' ChannelMessageSummary (Prelude.Maybe Identity)
channelMessageSummary_sender = Lens.lens (\ChannelMessageSummary' {sender} -> sender) (\s@ChannelMessageSummary' {} a -> s {sender = a} :: ChannelMessageSummary)

-- | The content of the message.
channelMessageSummary_content :: Lens.Lens' ChannelMessageSummary (Prelude.Maybe Prelude.Text)
channelMessageSummary_content = Lens.lens (\ChannelMessageSummary' {content} -> content) (\s@ChannelMessageSummary' {} a -> s {content = a} :: ChannelMessageSummary) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON ChannelMessageSummary where
  parseJSON =
    Core.withObject
      "ChannelMessageSummary"
      ( \x ->
          ChannelMessageSummary'
            Prelude.<$> (x Core..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Metadata")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "Redacted")
            Prelude.<*> (x Core..:? "MessageId")
            Prelude.<*> (x Core..:? "LastEditedTimestamp")
            Prelude.<*> (x Core..:? "Sender")
            Prelude.<*> (x Core..:? "Content")
      )

instance Prelude.Hashable ChannelMessageSummary where
  hashWithSalt _salt ChannelMessageSummary' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` redacted
      `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` lastEditedTimestamp
      `Prelude.hashWithSalt` sender
      `Prelude.hashWithSalt` content

instance Prelude.NFData ChannelMessageSummary where
  rnf ChannelMessageSummary' {..} =
    Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf redacted
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf lastEditedTimestamp
      `Prelude.seq` Prelude.rnf sender
      `Prelude.seq` Prelude.rnf content
