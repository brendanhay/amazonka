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
-- Module      : Amazonka.Connect.Types.Contact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.Contact where

import Amazonka.Connect.Types.AgentInfo
import Amazonka.Connect.Types.Channel
import Amazonka.Connect.Types.ContactInitiationMethod
import Amazonka.Connect.Types.QueueInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a contact.
--
-- /See:/ 'newContact' smart constructor.
data Contact = Contact'
  { -- | Information about the agent who accepted the contact.
    agentInfo :: Prelude.Maybe AgentInfo,
    -- | The Amazon Resource Name (ARN) for the contact.
    arn :: Prelude.Maybe Prelude.Text,
    -- | How the contact reached your contact center.
    channel :: Prelude.Maybe Channel,
    -- | The description of the contact.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the customer endpoint disconnected from Amazon
    -- Connect.
    disconnectTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The identifier for the contact.
    id :: Prelude.Maybe Prelude.Text,
    -- | If this contact is related to other contacts, this is the ID of the
    -- initial contact.
    initialContactId :: Prelude.Maybe Prelude.Text,
    -- | Indicates how the contact was initiated.
    initiationMethod :: Prelude.Maybe ContactInitiationMethod,
    -- | The date and time this contact was initiated, in UTC time. For
    -- @INBOUND@, this is when the contact arrived. For @OUTBOUND@, this is
    -- when the agent began dialing. For @CALLBACK@, this is when the callback
    -- contact was created. For @TRANSFER@ and @QUEUE_TRANSFER@, this is when
    -- the transfer was initiated. For @API@, this is when the request arrived.
    initiationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The timestamp when contact was last updated.
    lastUpdateTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The name of the contact.
    name :: Prelude.Maybe Prelude.Text,
    -- | If this contact is not the first contact, this is the ID of the previous
    -- contact.
    previousContactId :: Prelude.Maybe Prelude.Text,
    -- | If this contact was queued, this contains information about the queue.
    queueInfo :: Prelude.Maybe QueueInfo,
    -- | The timestamp, in Unix epoch time format, at which to start running the
    -- inbound flow.
    scheduledTimestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Contact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentInfo', 'contact_agentInfo' - Information about the agent who accepted the contact.
--
-- 'arn', 'contact_arn' - The Amazon Resource Name (ARN) for the contact.
--
-- 'channel', 'contact_channel' - How the contact reached your contact center.
--
-- 'description', 'contact_description' - The description of the contact.
--
-- 'disconnectTimestamp', 'contact_disconnectTimestamp' - The timestamp when the customer endpoint disconnected from Amazon
-- Connect.
--
-- 'id', 'contact_id' - The identifier for the contact.
--
-- 'initialContactId', 'contact_initialContactId' - If this contact is related to other contacts, this is the ID of the
-- initial contact.
--
-- 'initiationMethod', 'contact_initiationMethod' - Indicates how the contact was initiated.
--
-- 'initiationTimestamp', 'contact_initiationTimestamp' - The date and time this contact was initiated, in UTC time. For
-- @INBOUND@, this is when the contact arrived. For @OUTBOUND@, this is
-- when the agent began dialing. For @CALLBACK@, this is when the callback
-- contact was created. For @TRANSFER@ and @QUEUE_TRANSFER@, this is when
-- the transfer was initiated. For @API@, this is when the request arrived.
--
-- 'lastUpdateTimestamp', 'contact_lastUpdateTimestamp' - The timestamp when contact was last updated.
--
-- 'name', 'contact_name' - The name of the contact.
--
-- 'previousContactId', 'contact_previousContactId' - If this contact is not the first contact, this is the ID of the previous
-- contact.
--
-- 'queueInfo', 'contact_queueInfo' - If this contact was queued, this contains information about the queue.
--
-- 'scheduledTimestamp', 'contact_scheduledTimestamp' - The timestamp, in Unix epoch time format, at which to start running the
-- inbound flow.
newContact ::
  Contact
newContact =
  Contact'
    { agentInfo = Prelude.Nothing,
      arn = Prelude.Nothing,
      channel = Prelude.Nothing,
      description = Prelude.Nothing,
      disconnectTimestamp = Prelude.Nothing,
      id = Prelude.Nothing,
      initialContactId = Prelude.Nothing,
      initiationMethod = Prelude.Nothing,
      initiationTimestamp = Prelude.Nothing,
      lastUpdateTimestamp = Prelude.Nothing,
      name = Prelude.Nothing,
      previousContactId = Prelude.Nothing,
      queueInfo = Prelude.Nothing,
      scheduledTimestamp = Prelude.Nothing
    }

-- | Information about the agent who accepted the contact.
contact_agentInfo :: Lens.Lens' Contact (Prelude.Maybe AgentInfo)
contact_agentInfo = Lens.lens (\Contact' {agentInfo} -> agentInfo) (\s@Contact' {} a -> s {agentInfo = a} :: Contact)

-- | The Amazon Resource Name (ARN) for the contact.
contact_arn :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_arn = Lens.lens (\Contact' {arn} -> arn) (\s@Contact' {} a -> s {arn = a} :: Contact)

-- | How the contact reached your contact center.
contact_channel :: Lens.Lens' Contact (Prelude.Maybe Channel)
contact_channel = Lens.lens (\Contact' {channel} -> channel) (\s@Contact' {} a -> s {channel = a} :: Contact)

-- | The description of the contact.
contact_description :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_description = Lens.lens (\Contact' {description} -> description) (\s@Contact' {} a -> s {description = a} :: Contact)

-- | The timestamp when the customer endpoint disconnected from Amazon
-- Connect.
contact_disconnectTimestamp :: Lens.Lens' Contact (Prelude.Maybe Prelude.UTCTime)
contact_disconnectTimestamp = Lens.lens (\Contact' {disconnectTimestamp} -> disconnectTimestamp) (\s@Contact' {} a -> s {disconnectTimestamp = a} :: Contact) Prelude.. Lens.mapping Data._Time

-- | The identifier for the contact.
contact_id :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_id = Lens.lens (\Contact' {id} -> id) (\s@Contact' {} a -> s {id = a} :: Contact)

-- | If this contact is related to other contacts, this is the ID of the
-- initial contact.
contact_initialContactId :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_initialContactId = Lens.lens (\Contact' {initialContactId} -> initialContactId) (\s@Contact' {} a -> s {initialContactId = a} :: Contact)

-- | Indicates how the contact was initiated.
contact_initiationMethod :: Lens.Lens' Contact (Prelude.Maybe ContactInitiationMethod)
contact_initiationMethod = Lens.lens (\Contact' {initiationMethod} -> initiationMethod) (\s@Contact' {} a -> s {initiationMethod = a} :: Contact)

-- | The date and time this contact was initiated, in UTC time. For
-- @INBOUND@, this is when the contact arrived. For @OUTBOUND@, this is
-- when the agent began dialing. For @CALLBACK@, this is when the callback
-- contact was created. For @TRANSFER@ and @QUEUE_TRANSFER@, this is when
-- the transfer was initiated. For @API@, this is when the request arrived.
contact_initiationTimestamp :: Lens.Lens' Contact (Prelude.Maybe Prelude.UTCTime)
contact_initiationTimestamp = Lens.lens (\Contact' {initiationTimestamp} -> initiationTimestamp) (\s@Contact' {} a -> s {initiationTimestamp = a} :: Contact) Prelude.. Lens.mapping Data._Time

-- | The timestamp when contact was last updated.
contact_lastUpdateTimestamp :: Lens.Lens' Contact (Prelude.Maybe Prelude.UTCTime)
contact_lastUpdateTimestamp = Lens.lens (\Contact' {lastUpdateTimestamp} -> lastUpdateTimestamp) (\s@Contact' {} a -> s {lastUpdateTimestamp = a} :: Contact) Prelude.. Lens.mapping Data._Time

-- | The name of the contact.
contact_name :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_name = Lens.lens (\Contact' {name} -> name) (\s@Contact' {} a -> s {name = a} :: Contact)

-- | If this contact is not the first contact, this is the ID of the previous
-- contact.
contact_previousContactId :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_previousContactId = Lens.lens (\Contact' {previousContactId} -> previousContactId) (\s@Contact' {} a -> s {previousContactId = a} :: Contact)

-- | If this contact was queued, this contains information about the queue.
contact_queueInfo :: Lens.Lens' Contact (Prelude.Maybe QueueInfo)
contact_queueInfo = Lens.lens (\Contact' {queueInfo} -> queueInfo) (\s@Contact' {} a -> s {queueInfo = a} :: Contact)

-- | The timestamp, in Unix epoch time format, at which to start running the
-- inbound flow.
contact_scheduledTimestamp :: Lens.Lens' Contact (Prelude.Maybe Prelude.UTCTime)
contact_scheduledTimestamp = Lens.lens (\Contact' {scheduledTimestamp} -> scheduledTimestamp) (\s@Contact' {} a -> s {scheduledTimestamp = a} :: Contact) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Contact where
  parseJSON =
    Data.withObject
      "Contact"
      ( \x ->
          Contact'
            Prelude.<$> (x Data..:? "AgentInfo")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Channel")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DisconnectTimestamp")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "InitialContactId")
            Prelude.<*> (x Data..:? "InitiationMethod")
            Prelude.<*> (x Data..:? "InitiationTimestamp")
            Prelude.<*> (x Data..:? "LastUpdateTimestamp")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PreviousContactId")
            Prelude.<*> (x Data..:? "QueueInfo")
            Prelude.<*> (x Data..:? "ScheduledTimestamp")
      )

instance Prelude.Hashable Contact where
  hashWithSalt _salt Contact' {..} =
    _salt
      `Prelude.hashWithSalt` agentInfo
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` channel
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` disconnectTimestamp
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` initialContactId
      `Prelude.hashWithSalt` initiationMethod
      `Prelude.hashWithSalt` initiationTimestamp
      `Prelude.hashWithSalt` lastUpdateTimestamp
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` previousContactId
      `Prelude.hashWithSalt` queueInfo
      `Prelude.hashWithSalt` scheduledTimestamp

instance Prelude.NFData Contact where
  rnf Contact' {..} =
    Prelude.rnf agentInfo
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf channel
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf disconnectTimestamp
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf initialContactId
      `Prelude.seq` Prelude.rnf initiationMethod
      `Prelude.seq` Prelude.rnf initiationTimestamp
      `Prelude.seq` Prelude.rnf lastUpdateTimestamp
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf previousContactId
      `Prelude.seq` Prelude.rnf queueInfo
      `Prelude.seq` Prelude.rnf scheduledTimestamp
