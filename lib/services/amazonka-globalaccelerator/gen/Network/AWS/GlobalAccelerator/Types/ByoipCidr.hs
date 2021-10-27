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
-- Module      : Network.AWS.GlobalAccelerator.Types.ByoipCidr
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GlobalAccelerator.Types.ByoipCidr where

import qualified Network.AWS.Core as Core
import Network.AWS.GlobalAccelerator.Types.ByoipCidrEvent
import Network.AWS.GlobalAccelerator.Types.ByoipCidrState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an IP address range that is provisioned for use with
-- your AWS resources through bring your own IP address (BYOIP).
--
-- The following describes each BYOIP @State@ that your IP address range
-- can be in.
--
-- -   __PENDING_PROVISIONING__ — You’ve submitted a request to provision
--     an IP address range but it is not yet provisioned with AWS Global
--     Accelerator.
--
-- -   __READY__ — The address range is provisioned with AWS Global
--     Accelerator and can be advertised.
--
-- -   __PENDING_ADVERTISING__ — You’ve submitted a request for AWS Global
--     Accelerator to advertise an address range but it is not yet being
--     advertised.
--
-- -   __ADVERTISING__ — The address range is being advertised by AWS
--     Global Accelerator.
--
-- -   __PENDING_WITHDRAWING__ — You’ve submitted a request to withdraw an
--     address range from being advertised but it is still being advertised
--     by AWS Global Accelerator.
--
-- -   __PENDING_DEPROVISIONING__ — You’ve submitted a request to
--     deprovision an address range from AWS Global Accelerator but it is
--     still provisioned.
--
-- -   __DEPROVISIONED__ — The address range is deprovisioned from AWS
--     Global Accelerator.
--
-- -   __FAILED_PROVISION__ — The request to provision the address range
--     from AWS Global Accelerator was not successful. Please make sure
--     that you provide all of the correct information, and try again. If
--     the request fails a second time, contact AWS support.
--
-- -   __FAILED_ADVERTISING__ — The request for AWS Global Accelerator to
--     advertise the address range was not successful. Please make sure
--     that you provide all of the correct information, and try again. If
--     the request fails a second time, contact AWS support.
--
-- -   __FAILED_WITHDRAW__ — The request to withdraw the address range from
--     advertising by AWS Global Accelerator was not successful. Please
--     make sure that you provide all of the correct information, and try
--     again. If the request fails a second time, contact AWS support.
--
-- -   __FAILED_DEPROVISION__ — The request to deprovision the address
--     range from AWS Global Accelerator was not successful. Please make
--     sure that you provide all of the correct information, and try again.
--     If the request fails a second time, contact AWS support.
--
-- /See:/ 'newByoipCidr' smart constructor.
data ByoipCidr = ByoipCidr'
  { -- | The state of the address pool.
    state :: Prelude.Maybe ByoipCidrState,
    -- | The address range, in CIDR notation.
    cidr :: Prelude.Maybe Prelude.Text,
    -- | A history of status changes for an IP address range that you bring to
    -- AWS Global Accelerator through bring your own IP address (BYOIP).
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
-- 'state', 'byoipCidr_state' - The state of the address pool.
--
-- 'cidr', 'byoipCidr_cidr' - The address range, in CIDR notation.
--
-- 'events', 'byoipCidr_events' - A history of status changes for an IP address range that you bring to
-- AWS Global Accelerator through bring your own IP address (BYOIP).
newByoipCidr ::
  ByoipCidr
newByoipCidr =
  ByoipCidr'
    { state = Prelude.Nothing,
      cidr = Prelude.Nothing,
      events = Prelude.Nothing
    }

-- | The state of the address pool.
byoipCidr_state :: Lens.Lens' ByoipCidr (Prelude.Maybe ByoipCidrState)
byoipCidr_state = Lens.lens (\ByoipCidr' {state} -> state) (\s@ByoipCidr' {} a -> s {state = a} :: ByoipCidr)

-- | The address range, in CIDR notation.
byoipCidr_cidr :: Lens.Lens' ByoipCidr (Prelude.Maybe Prelude.Text)
byoipCidr_cidr = Lens.lens (\ByoipCidr' {cidr} -> cidr) (\s@ByoipCidr' {} a -> s {cidr = a} :: ByoipCidr)

-- | A history of status changes for an IP address range that you bring to
-- AWS Global Accelerator through bring your own IP address (BYOIP).
byoipCidr_events :: Lens.Lens' ByoipCidr (Prelude.Maybe [ByoipCidrEvent])
byoipCidr_events = Lens.lens (\ByoipCidr' {events} -> events) (\s@ByoipCidr' {} a -> s {events = a} :: ByoipCidr) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ByoipCidr where
  parseJSON =
    Core.withObject
      "ByoipCidr"
      ( \x ->
          ByoipCidr'
            Prelude.<$> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Cidr")
            Prelude.<*> (x Core..:? "Events" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ByoipCidr

instance Prelude.NFData ByoipCidr
