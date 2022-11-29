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
-- Module      : Amazonka.RDS.Types.TargetHealth
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.TargetHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.TargetHealthReason
import Amazonka.RDS.Types.TargetState

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
    state :: Prelude.Maybe TargetState,
    -- | A description of the health of the RDS Proxy target. If the @State@ is
    -- @AVAILABLE@, a description is not included.
    description :: Prelude.Maybe Prelude.Text,
    -- | The reason for the current health @State@ of the RDS Proxy target.
    reason :: Prelude.Maybe TargetHealthReason
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'description', 'targetHealth_description' - A description of the health of the RDS Proxy target. If the @State@ is
-- @AVAILABLE@, a description is not included.
--
-- 'reason', 'targetHealth_reason' - The reason for the current health @State@ of the RDS Proxy target.
newTargetHealth ::
  TargetHealth
newTargetHealth =
  TargetHealth'
    { state = Prelude.Nothing,
      description = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The current state of the connection health lifecycle for the RDS Proxy
-- target. The following is a typical lifecycle example for the states of
-- an RDS Proxy target:
--
-- @registering@ > @unavailable@ > @available@ > @unavailable@ >
-- @available@
targetHealth_state :: Lens.Lens' TargetHealth (Prelude.Maybe TargetState)
targetHealth_state = Lens.lens (\TargetHealth' {state} -> state) (\s@TargetHealth' {} a -> s {state = a} :: TargetHealth)

-- | A description of the health of the RDS Proxy target. If the @State@ is
-- @AVAILABLE@, a description is not included.
targetHealth_description :: Lens.Lens' TargetHealth (Prelude.Maybe Prelude.Text)
targetHealth_description = Lens.lens (\TargetHealth' {description} -> description) (\s@TargetHealth' {} a -> s {description = a} :: TargetHealth)

-- | The reason for the current health @State@ of the RDS Proxy target.
targetHealth_reason :: Lens.Lens' TargetHealth (Prelude.Maybe TargetHealthReason)
targetHealth_reason = Lens.lens (\TargetHealth' {reason} -> reason) (\s@TargetHealth' {} a -> s {reason = a} :: TargetHealth)

instance Core.FromXML TargetHealth where
  parseXML x =
    TargetHealth'
      Prelude.<$> (x Core..@? "State")
      Prelude.<*> (x Core..@? "Description")
      Prelude.<*> (x Core..@? "Reason")

instance Prelude.Hashable TargetHealth where
  hashWithSalt _salt TargetHealth' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` reason

instance Prelude.NFData TargetHealth where
  rnf TargetHealth' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf reason
