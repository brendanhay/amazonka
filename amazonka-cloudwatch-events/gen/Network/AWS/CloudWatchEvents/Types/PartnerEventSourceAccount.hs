{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudWatchEvents.Types.PartnerEventSourceAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PartnerEventSourceAccount where

import Network.AWS.CloudWatchEvents.Types.EventSourceState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The AWS account that a partner event source has been offered to.
--
-- /See:/ 'newPartnerEventSourceAccount' smart constructor.
data PartnerEventSourceAccount = PartnerEventSourceAccount'
  { -- | The date and time the event source was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time that the event source will expire, if the AWS account
    -- doesn\'t create a matching event bus for it.
    expirationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The state of the event source. If it is ACTIVE, you have already created
    -- a matching event bus for this event source, and that event bus is
    -- active. If it is PENDING, either you haven\'t yet created a matching
    -- event bus, or that event bus is deactivated. If it is DELETED, you have
    -- created a matching event bus, but the event source has since been
    -- deleted.
    state :: Prelude.Maybe EventSourceState,
    -- | The AWS account ID that the partner event source was offered to.
    account :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PartnerEventSourceAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'partnerEventSourceAccount_creationTime' - The date and time the event source was created.
--
-- 'expirationTime', 'partnerEventSourceAccount_expirationTime' - The date and time that the event source will expire, if the AWS account
-- doesn\'t create a matching event bus for it.
--
-- 'state', 'partnerEventSourceAccount_state' - The state of the event source. If it is ACTIVE, you have already created
-- a matching event bus for this event source, and that event bus is
-- active. If it is PENDING, either you haven\'t yet created a matching
-- event bus, or that event bus is deactivated. If it is DELETED, you have
-- created a matching event bus, but the event source has since been
-- deleted.
--
-- 'account', 'partnerEventSourceAccount_account' - The AWS account ID that the partner event source was offered to.
newPartnerEventSourceAccount ::
  PartnerEventSourceAccount
newPartnerEventSourceAccount =
  PartnerEventSourceAccount'
    { creationTime =
        Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      state = Prelude.Nothing,
      account = Prelude.Nothing
    }

-- | The date and time the event source was created.
partnerEventSourceAccount_creationTime :: Lens.Lens' PartnerEventSourceAccount (Prelude.Maybe Prelude.UTCTime)
partnerEventSourceAccount_creationTime = Lens.lens (\PartnerEventSourceAccount' {creationTime} -> creationTime) (\s@PartnerEventSourceAccount' {} a -> s {creationTime = a} :: PartnerEventSourceAccount) Prelude.. Lens.mapping Prelude._Time

-- | The date and time that the event source will expire, if the AWS account
-- doesn\'t create a matching event bus for it.
partnerEventSourceAccount_expirationTime :: Lens.Lens' PartnerEventSourceAccount (Prelude.Maybe Prelude.UTCTime)
partnerEventSourceAccount_expirationTime = Lens.lens (\PartnerEventSourceAccount' {expirationTime} -> expirationTime) (\s@PartnerEventSourceAccount' {} a -> s {expirationTime = a} :: PartnerEventSourceAccount) Prelude.. Lens.mapping Prelude._Time

-- | The state of the event source. If it is ACTIVE, you have already created
-- a matching event bus for this event source, and that event bus is
-- active. If it is PENDING, either you haven\'t yet created a matching
-- event bus, or that event bus is deactivated. If it is DELETED, you have
-- created a matching event bus, but the event source has since been
-- deleted.
partnerEventSourceAccount_state :: Lens.Lens' PartnerEventSourceAccount (Prelude.Maybe EventSourceState)
partnerEventSourceAccount_state = Lens.lens (\PartnerEventSourceAccount' {state} -> state) (\s@PartnerEventSourceAccount' {} a -> s {state = a} :: PartnerEventSourceAccount)

-- | The AWS account ID that the partner event source was offered to.
partnerEventSourceAccount_account :: Lens.Lens' PartnerEventSourceAccount (Prelude.Maybe Prelude.Text)
partnerEventSourceAccount_account = Lens.lens (\PartnerEventSourceAccount' {account} -> account) (\s@PartnerEventSourceAccount' {} a -> s {account = a} :: PartnerEventSourceAccount)

instance Prelude.FromJSON PartnerEventSourceAccount where
  parseJSON =
    Prelude.withObject
      "PartnerEventSourceAccount"
      ( \x ->
          PartnerEventSourceAccount'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "ExpirationTime")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "Account")
      )

instance Prelude.Hashable PartnerEventSourceAccount

instance Prelude.NFData PartnerEventSourceAccount
