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
-- Module      : Amazonka.CloudWatchEvents.Types.PartnerEventSourceAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.PartnerEventSourceAccount where

import Amazonka.CloudWatchEvents.Types.EventSourceState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Web Services account that a partner event source has been
-- offered to.
--
-- /See:/ 'newPartnerEventSourceAccount' smart constructor.
data PartnerEventSourceAccount = PartnerEventSourceAccount'
  { -- | The Amazon Web Services account ID that the partner event source was
    -- offered to.
    account :: Prelude.Maybe Prelude.Text,
    -- | The date and time the event source was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time that the event source will expire, if the Amazon Web
    -- Services account doesn\'t create a matching event bus for it.
    expirationTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the event source. If it is ACTIVE, you have already created
    -- a matching event bus for this event source, and that event bus is
    -- active. If it is PENDING, either you haven\'t yet created a matching
    -- event bus, or that event bus is deactivated. If it is DELETED, you have
    -- created a matching event bus, but the event source has since been
    -- deleted.
    state :: Prelude.Maybe EventSourceState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PartnerEventSourceAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'account', 'partnerEventSourceAccount_account' - The Amazon Web Services account ID that the partner event source was
-- offered to.
--
-- 'creationTime', 'partnerEventSourceAccount_creationTime' - The date and time the event source was created.
--
-- 'expirationTime', 'partnerEventSourceAccount_expirationTime' - The date and time that the event source will expire, if the Amazon Web
-- Services account doesn\'t create a matching event bus for it.
--
-- 'state', 'partnerEventSourceAccount_state' - The state of the event source. If it is ACTIVE, you have already created
-- a matching event bus for this event source, and that event bus is
-- active. If it is PENDING, either you haven\'t yet created a matching
-- event bus, or that event bus is deactivated. If it is DELETED, you have
-- created a matching event bus, but the event source has since been
-- deleted.
newPartnerEventSourceAccount ::
  PartnerEventSourceAccount
newPartnerEventSourceAccount =
  PartnerEventSourceAccount'
    { account =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The Amazon Web Services account ID that the partner event source was
-- offered to.
partnerEventSourceAccount_account :: Lens.Lens' PartnerEventSourceAccount (Prelude.Maybe Prelude.Text)
partnerEventSourceAccount_account = Lens.lens (\PartnerEventSourceAccount' {account} -> account) (\s@PartnerEventSourceAccount' {} a -> s {account = a} :: PartnerEventSourceAccount)

-- | The date and time the event source was created.
partnerEventSourceAccount_creationTime :: Lens.Lens' PartnerEventSourceAccount (Prelude.Maybe Prelude.UTCTime)
partnerEventSourceAccount_creationTime = Lens.lens (\PartnerEventSourceAccount' {creationTime} -> creationTime) (\s@PartnerEventSourceAccount' {} a -> s {creationTime = a} :: PartnerEventSourceAccount) Prelude.. Lens.mapping Data._Time

-- | The date and time that the event source will expire, if the Amazon Web
-- Services account doesn\'t create a matching event bus for it.
partnerEventSourceAccount_expirationTime :: Lens.Lens' PartnerEventSourceAccount (Prelude.Maybe Prelude.UTCTime)
partnerEventSourceAccount_expirationTime = Lens.lens (\PartnerEventSourceAccount' {expirationTime} -> expirationTime) (\s@PartnerEventSourceAccount' {} a -> s {expirationTime = a} :: PartnerEventSourceAccount) Prelude.. Lens.mapping Data._Time

-- | The state of the event source. If it is ACTIVE, you have already created
-- a matching event bus for this event source, and that event bus is
-- active. If it is PENDING, either you haven\'t yet created a matching
-- event bus, or that event bus is deactivated. If it is DELETED, you have
-- created a matching event bus, but the event source has since been
-- deleted.
partnerEventSourceAccount_state :: Lens.Lens' PartnerEventSourceAccount (Prelude.Maybe EventSourceState)
partnerEventSourceAccount_state = Lens.lens (\PartnerEventSourceAccount' {state} -> state) (\s@PartnerEventSourceAccount' {} a -> s {state = a} :: PartnerEventSourceAccount)

instance Data.FromJSON PartnerEventSourceAccount where
  parseJSON =
    Data.withObject
      "PartnerEventSourceAccount"
      ( \x ->
          PartnerEventSourceAccount'
            Prelude.<$> (x Data..:? "Account")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "ExpirationTime")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable PartnerEventSourceAccount where
  hashWithSalt _salt PartnerEventSourceAccount' {..} =
    _salt
      `Prelude.hashWithSalt` account
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` expirationTime
      `Prelude.hashWithSalt` state

instance Prelude.NFData PartnerEventSourceAccount where
  rnf PartnerEventSourceAccount' {..} =
    Prelude.rnf account
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf expirationTime
      `Prelude.seq` Prelude.rnf state
