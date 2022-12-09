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
-- Module      : Amazonka.Connect.Types.AgentContactReference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.AgentContactReference where

import Amazonka.Connect.Types.Channel
import Amazonka.Connect.Types.ContactInitiationMethod
import Amazonka.Connect.Types.ContactState
import Amazonka.Connect.Types.QueueReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the
-- <https://docs.aws.amazon.com/connect/latest/APIReference/API_Contact.html contact>
-- associated to the user.
--
-- /See:/ 'newAgentContactReference' smart constructor.
data AgentContactReference = AgentContactReference'
  { -- | The
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/about-contact-states.html state of the contact>.
    agentContactState :: Prelude.Maybe ContactState,
    -- | The channel of the contact.
    channel :: Prelude.Maybe Channel,
    -- | The time at which the contact was connected to an agent.
    connectedToAgentTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the contact in this instance of Amazon Connect.
    contactId :: Prelude.Maybe Prelude.Text,
    -- | How the contact was initiated.
    initiationMethod :: Prelude.Maybe ContactInitiationMethod,
    queue :: Prelude.Maybe QueueReference,
    -- | The epoch timestamp when the contact state started.
    stateStartTimestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentContactReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentContactState', 'agentContactReference_agentContactState' - The
-- <https://docs.aws.amazon.com/connect/latest/adminguide/about-contact-states.html state of the contact>.
--
-- 'channel', 'agentContactReference_channel' - The channel of the contact.
--
-- 'connectedToAgentTimestamp', 'agentContactReference_connectedToAgentTimestamp' - The time at which the contact was connected to an agent.
--
-- 'contactId', 'agentContactReference_contactId' - The identifier of the contact in this instance of Amazon Connect.
--
-- 'initiationMethod', 'agentContactReference_initiationMethod' - How the contact was initiated.
--
-- 'queue', 'agentContactReference_queue' - Undocumented member.
--
-- 'stateStartTimestamp', 'agentContactReference_stateStartTimestamp' - The epoch timestamp when the contact state started.
newAgentContactReference ::
  AgentContactReference
newAgentContactReference =
  AgentContactReference'
    { agentContactState =
        Prelude.Nothing,
      channel = Prelude.Nothing,
      connectedToAgentTimestamp = Prelude.Nothing,
      contactId = Prelude.Nothing,
      initiationMethod = Prelude.Nothing,
      queue = Prelude.Nothing,
      stateStartTimestamp = Prelude.Nothing
    }

-- | The
-- <https://docs.aws.amazon.com/connect/latest/adminguide/about-contact-states.html state of the contact>.
agentContactReference_agentContactState :: Lens.Lens' AgentContactReference (Prelude.Maybe ContactState)
agentContactReference_agentContactState = Lens.lens (\AgentContactReference' {agentContactState} -> agentContactState) (\s@AgentContactReference' {} a -> s {agentContactState = a} :: AgentContactReference)

-- | The channel of the contact.
agentContactReference_channel :: Lens.Lens' AgentContactReference (Prelude.Maybe Channel)
agentContactReference_channel = Lens.lens (\AgentContactReference' {channel} -> channel) (\s@AgentContactReference' {} a -> s {channel = a} :: AgentContactReference)

-- | The time at which the contact was connected to an agent.
agentContactReference_connectedToAgentTimestamp :: Lens.Lens' AgentContactReference (Prelude.Maybe Prelude.UTCTime)
agentContactReference_connectedToAgentTimestamp = Lens.lens (\AgentContactReference' {connectedToAgentTimestamp} -> connectedToAgentTimestamp) (\s@AgentContactReference' {} a -> s {connectedToAgentTimestamp = a} :: AgentContactReference) Prelude.. Lens.mapping Data._Time

-- | The identifier of the contact in this instance of Amazon Connect.
agentContactReference_contactId :: Lens.Lens' AgentContactReference (Prelude.Maybe Prelude.Text)
agentContactReference_contactId = Lens.lens (\AgentContactReference' {contactId} -> contactId) (\s@AgentContactReference' {} a -> s {contactId = a} :: AgentContactReference)

-- | How the contact was initiated.
agentContactReference_initiationMethod :: Lens.Lens' AgentContactReference (Prelude.Maybe ContactInitiationMethod)
agentContactReference_initiationMethod = Lens.lens (\AgentContactReference' {initiationMethod} -> initiationMethod) (\s@AgentContactReference' {} a -> s {initiationMethod = a} :: AgentContactReference)

-- | Undocumented member.
agentContactReference_queue :: Lens.Lens' AgentContactReference (Prelude.Maybe QueueReference)
agentContactReference_queue = Lens.lens (\AgentContactReference' {queue} -> queue) (\s@AgentContactReference' {} a -> s {queue = a} :: AgentContactReference)

-- | The epoch timestamp when the contact state started.
agentContactReference_stateStartTimestamp :: Lens.Lens' AgentContactReference (Prelude.Maybe Prelude.UTCTime)
agentContactReference_stateStartTimestamp = Lens.lens (\AgentContactReference' {stateStartTimestamp} -> stateStartTimestamp) (\s@AgentContactReference' {} a -> s {stateStartTimestamp = a} :: AgentContactReference) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON AgentContactReference where
  parseJSON =
    Data.withObject
      "AgentContactReference"
      ( \x ->
          AgentContactReference'
            Prelude.<$> (x Data..:? "AgentContactState")
            Prelude.<*> (x Data..:? "Channel")
            Prelude.<*> (x Data..:? "ConnectedToAgentTimestamp")
            Prelude.<*> (x Data..:? "ContactId")
            Prelude.<*> (x Data..:? "InitiationMethod")
            Prelude.<*> (x Data..:? "Queue")
            Prelude.<*> (x Data..:? "StateStartTimestamp")
      )

instance Prelude.Hashable AgentContactReference where
  hashWithSalt _salt AgentContactReference' {..} =
    _salt `Prelude.hashWithSalt` agentContactState
      `Prelude.hashWithSalt` channel
      `Prelude.hashWithSalt` connectedToAgentTimestamp
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` initiationMethod
      `Prelude.hashWithSalt` queue
      `Prelude.hashWithSalt` stateStartTimestamp

instance Prelude.NFData AgentContactReference where
  rnf AgentContactReference' {..} =
    Prelude.rnf agentContactState
      `Prelude.seq` Prelude.rnf channel
      `Prelude.seq` Prelude.rnf connectedToAgentTimestamp
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf initiationMethod
      `Prelude.seq` Prelude.rnf queue
      `Prelude.seq` Prelude.rnf stateStartTimestamp
