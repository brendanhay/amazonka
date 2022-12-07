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
-- Module      : Amazonka.GlobalAccelerator.Types.ByoipCidr
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.ByoipCidr where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types.ByoipCidrEvent
import Amazonka.GlobalAccelerator.Types.ByoipCidrState
import qualified Amazonka.Prelude as Prelude

-- | Information about an IP address range that is provisioned for use with
-- your Amazon Web Services resources through bring your own IP address
-- (BYOIP).
--
-- The following describes each BYOIP @State@ that your IP address range
-- can be in.
--
-- -   __PENDING_PROVISIONING__ — You’ve submitted a request to provision
--     an IP address range but it is not yet provisioned with Global
--     Accelerator.
--
-- -   __READY__ — The address range is provisioned with Global Accelerator
--     and can be advertised.
--
-- -   __PENDING_ADVERTISING__ — You’ve submitted a request for Global
--     Accelerator to advertise an address range but it is not yet being
--     advertised.
--
-- -   __ADVERTISING__ — The address range is being advertised by Global
--     Accelerator.
--
-- -   __PENDING_WITHDRAWING__ — You’ve submitted a request to withdraw an
--     address range from being advertised but it is still being advertised
--     by Global Accelerator.
--
-- -   __PENDING_DEPROVISIONING__ — You’ve submitted a request to
--     deprovision an address range from Global Accelerator but it is still
--     provisioned.
--
-- -   __DEPROVISIONED__ — The address range is deprovisioned from Global
--     Accelerator.
--
-- -   __FAILED_PROVISION__ — The request to provision the address range
--     from Global Accelerator was not successful. Please make sure that
--     you provide all of the correct information, and try again. If the
--     request fails a second time, contact Amazon Web Services support.
--
-- -   __FAILED_ADVERTISING__ — The request for Global Accelerator to
--     advertise the address range was not successful. Please make sure
--     that you provide all of the correct information, and try again. If
--     the request fails a second time, contact Amazon Web Services
--     support.
--
-- -   __FAILED_WITHDRAW__ — The request to withdraw the address range from
--     advertising by Global Accelerator was not successful. Please make
--     sure that you provide all of the correct information, and try again.
--     If the request fails a second time, contact Amazon Web Services
--     support.
--
-- -   __FAILED_DEPROVISION__ — The request to deprovision the address
--     range from Global Accelerator was not successful. Please make sure
--     that you provide all of the correct information, and try again. If
--     the request fails a second time, contact Amazon Web Services
--     support.
--
-- /See:/ 'newByoipCidr' smart constructor.
data ByoipCidr = ByoipCidr'
  { -- | The address range, in CIDR notation.
    cidr :: Prelude.Maybe Prelude.Text,
    -- | The state of the address pool.
    state :: Prelude.Maybe ByoipCidrState,
    -- | A history of status changes for an IP address range that you bring to
    -- Global Accelerator through bring your own IP address (BYOIP).
    events :: Prelude.Maybe [ByoipCidrEvent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ByoipCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'byoipCidr_cidr' - The address range, in CIDR notation.
--
-- 'state', 'byoipCidr_state' - The state of the address pool.
--
-- 'events', 'byoipCidr_events' - A history of status changes for an IP address range that you bring to
-- Global Accelerator through bring your own IP address (BYOIP).
newByoipCidr ::
  ByoipCidr
newByoipCidr =
  ByoipCidr'
    { cidr = Prelude.Nothing,
      state = Prelude.Nothing,
      events = Prelude.Nothing
    }

-- | The address range, in CIDR notation.
byoipCidr_cidr :: Lens.Lens' ByoipCidr (Prelude.Maybe Prelude.Text)
byoipCidr_cidr = Lens.lens (\ByoipCidr' {cidr} -> cidr) (\s@ByoipCidr' {} a -> s {cidr = a} :: ByoipCidr)

-- | The state of the address pool.
byoipCidr_state :: Lens.Lens' ByoipCidr (Prelude.Maybe ByoipCidrState)
byoipCidr_state = Lens.lens (\ByoipCidr' {state} -> state) (\s@ByoipCidr' {} a -> s {state = a} :: ByoipCidr)

-- | A history of status changes for an IP address range that you bring to
-- Global Accelerator through bring your own IP address (BYOIP).
byoipCidr_events :: Lens.Lens' ByoipCidr (Prelude.Maybe [ByoipCidrEvent])
byoipCidr_events = Lens.lens (\ByoipCidr' {events} -> events) (\s@ByoipCidr' {} a -> s {events = a} :: ByoipCidr) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ByoipCidr where
  parseJSON =
    Data.withObject
      "ByoipCidr"
      ( \x ->
          ByoipCidr'
            Prelude.<$> (x Data..:? "Cidr")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Events" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ByoipCidr where
  hashWithSalt _salt ByoipCidr' {..} =
    _salt `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` events

instance Prelude.NFData ByoipCidr where
  rnf ByoipCidr' {..} =
    Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf events
