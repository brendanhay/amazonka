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
-- Module      : Network.AWS.RDS.Types.TargetHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.TargetHealth where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.TargetHealthReason
import Network.AWS.RDS.Types.TargetState

-- | Information about the connection health of an RDS Proxy target.
--
-- /See:/ 'newTargetHealth' smart constructor.
data TargetHealth = TargetHealth'
  { -- | The current state of the connection health lifecycle for the RDS Proxy
    -- target. The following is a typical lifecycle example for the states of
    -- an RDS Proxy target:
    --
    -- @registering@ > @unavailable@ > @available@ > @unavailable@ >
    -- @available@
    state :: Core.Maybe TargetState,
    -- | The reason for the current health @State@ of the RDS Proxy target.
    reason :: Core.Maybe TargetHealthReason,
    -- | A description of the health of the RDS Proxy target. If the @State@ is
    -- @AVAILABLE@, a description is not included.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TargetHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'targetHealth_state' - The current state of the connection health lifecycle for the RDS Proxy
-- target. The following is a typical lifecycle example for the states of
-- an RDS Proxy target:
--
-- @registering@ > @unavailable@ > @available@ > @unavailable@ >
-- @available@
--
-- 'reason', 'targetHealth_reason' - The reason for the current health @State@ of the RDS Proxy target.
--
-- 'description', 'targetHealth_description' - A description of the health of the RDS Proxy target. If the @State@ is
-- @AVAILABLE@, a description is not included.
newTargetHealth ::
  TargetHealth
newTargetHealth =
  TargetHealth'
    { state = Core.Nothing,
      reason = Core.Nothing,
      description = Core.Nothing
    }

-- | The current state of the connection health lifecycle for the RDS Proxy
-- target. The following is a typical lifecycle example for the states of
-- an RDS Proxy target:
--
-- @registering@ > @unavailable@ > @available@ > @unavailable@ >
-- @available@
targetHealth_state :: Lens.Lens' TargetHealth (Core.Maybe TargetState)
targetHealth_state = Lens.lens (\TargetHealth' {state} -> state) (\s@TargetHealth' {} a -> s {state = a} :: TargetHealth)

-- | The reason for the current health @State@ of the RDS Proxy target.
targetHealth_reason :: Lens.Lens' TargetHealth (Core.Maybe TargetHealthReason)
targetHealth_reason = Lens.lens (\TargetHealth' {reason} -> reason) (\s@TargetHealth' {} a -> s {reason = a} :: TargetHealth)

-- | A description of the health of the RDS Proxy target. If the @State@ is
-- @AVAILABLE@, a description is not included.
targetHealth_description :: Lens.Lens' TargetHealth (Core.Maybe Core.Text)
targetHealth_description = Lens.lens (\TargetHealth' {description} -> description) (\s@TargetHealth' {} a -> s {description = a} :: TargetHealth)

instance Core.FromXML TargetHealth where
  parseXML x =
    TargetHealth'
      Core.<$> (x Core..@? "State")
      Core.<*> (x Core..@? "Reason")
      Core.<*> (x Core..@? "Description")

instance Core.Hashable TargetHealth

instance Core.NFData TargetHealth
