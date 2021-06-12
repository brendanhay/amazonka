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
-- Module      : Network.AWS.EC2.Types.ByoipCidr
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ByoipCidr where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ByoipCidrState
import qualified Network.AWS.Lens as Lens

-- | Information about an address range that is provisioned for use with your
-- AWS resources through bring your own IP addresses (BYOIP).
--
-- /See:/ 'newByoipCidr' smart constructor.
data ByoipCidr = ByoipCidr'
  { -- | Upon success, contains the ID of the address pool. Otherwise, contains
    -- an error message.
    statusMessage :: Core.Maybe Core.Text,
    -- | The state of the address pool.
    state :: Core.Maybe ByoipCidrState,
    -- | The address range, in CIDR notation.
    cidr :: Core.Maybe Core.Text,
    -- | The description of the address range.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ByoipCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'byoipCidr_statusMessage' - Upon success, contains the ID of the address pool. Otherwise, contains
-- an error message.
--
-- 'state', 'byoipCidr_state' - The state of the address pool.
--
-- 'cidr', 'byoipCidr_cidr' - The address range, in CIDR notation.
--
-- 'description', 'byoipCidr_description' - The description of the address range.
newByoipCidr ::
  ByoipCidr
newByoipCidr =
  ByoipCidr'
    { statusMessage = Core.Nothing,
      state = Core.Nothing,
      cidr = Core.Nothing,
      description = Core.Nothing
    }

-- | Upon success, contains the ID of the address pool. Otherwise, contains
-- an error message.
byoipCidr_statusMessage :: Lens.Lens' ByoipCidr (Core.Maybe Core.Text)
byoipCidr_statusMessage = Lens.lens (\ByoipCidr' {statusMessage} -> statusMessage) (\s@ByoipCidr' {} a -> s {statusMessage = a} :: ByoipCidr)

-- | The state of the address pool.
byoipCidr_state :: Lens.Lens' ByoipCidr (Core.Maybe ByoipCidrState)
byoipCidr_state = Lens.lens (\ByoipCidr' {state} -> state) (\s@ByoipCidr' {} a -> s {state = a} :: ByoipCidr)

-- | The address range, in CIDR notation.
byoipCidr_cidr :: Lens.Lens' ByoipCidr (Core.Maybe Core.Text)
byoipCidr_cidr = Lens.lens (\ByoipCidr' {cidr} -> cidr) (\s@ByoipCidr' {} a -> s {cidr = a} :: ByoipCidr)

-- | The description of the address range.
byoipCidr_description :: Lens.Lens' ByoipCidr (Core.Maybe Core.Text)
byoipCidr_description = Lens.lens (\ByoipCidr' {description} -> description) (\s@ByoipCidr' {} a -> s {description = a} :: ByoipCidr)

instance Core.FromXML ByoipCidr where
  parseXML x =
    ByoipCidr'
      Core.<$> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "cidr")
      Core.<*> (x Core..@? "description")

instance Core.Hashable ByoipCidr

instance Core.NFData ByoipCidr
