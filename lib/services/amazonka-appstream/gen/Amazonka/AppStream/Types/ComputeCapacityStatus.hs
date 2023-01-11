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
-- Module      : Amazonka.AppStream.Types.ComputeCapacityStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.ComputeCapacityStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the capacity status for a fleet.
--
-- /See:/ 'newComputeCapacityStatus' smart constructor.
data ComputeCapacityStatus = ComputeCapacityStatus'
  { -- | The number of currently available instances that can be used to stream
    -- sessions.
    available :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in use for streaming.
    inUse :: Prelude.Maybe Prelude.Int,
    -- | The total number of simultaneous streaming instances that are running.
    running :: Prelude.Maybe Prelude.Int,
    -- | The desired number of streaming instances.
    desired :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComputeCapacityStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'available', 'computeCapacityStatus_available' - The number of currently available instances that can be used to stream
-- sessions.
--
-- 'inUse', 'computeCapacityStatus_inUse' - The number of instances in use for streaming.
--
-- 'running', 'computeCapacityStatus_running' - The total number of simultaneous streaming instances that are running.
--
-- 'desired', 'computeCapacityStatus_desired' - The desired number of streaming instances.
newComputeCapacityStatus ::
  -- | 'desired'
  Prelude.Int ->
  ComputeCapacityStatus
newComputeCapacityStatus pDesired_ =
  ComputeCapacityStatus'
    { available = Prelude.Nothing,
      inUse = Prelude.Nothing,
      running = Prelude.Nothing,
      desired = pDesired_
    }

-- | The number of currently available instances that can be used to stream
-- sessions.
computeCapacityStatus_available :: Lens.Lens' ComputeCapacityStatus (Prelude.Maybe Prelude.Int)
computeCapacityStatus_available = Lens.lens (\ComputeCapacityStatus' {available} -> available) (\s@ComputeCapacityStatus' {} a -> s {available = a} :: ComputeCapacityStatus)

-- | The number of instances in use for streaming.
computeCapacityStatus_inUse :: Lens.Lens' ComputeCapacityStatus (Prelude.Maybe Prelude.Int)
computeCapacityStatus_inUse = Lens.lens (\ComputeCapacityStatus' {inUse} -> inUse) (\s@ComputeCapacityStatus' {} a -> s {inUse = a} :: ComputeCapacityStatus)

-- | The total number of simultaneous streaming instances that are running.
computeCapacityStatus_running :: Lens.Lens' ComputeCapacityStatus (Prelude.Maybe Prelude.Int)
computeCapacityStatus_running = Lens.lens (\ComputeCapacityStatus' {running} -> running) (\s@ComputeCapacityStatus' {} a -> s {running = a} :: ComputeCapacityStatus)

-- | The desired number of streaming instances.
computeCapacityStatus_desired :: Lens.Lens' ComputeCapacityStatus Prelude.Int
computeCapacityStatus_desired = Lens.lens (\ComputeCapacityStatus' {desired} -> desired) (\s@ComputeCapacityStatus' {} a -> s {desired = a} :: ComputeCapacityStatus)

instance Data.FromJSON ComputeCapacityStatus where
  parseJSON =
    Data.withObject
      "ComputeCapacityStatus"
      ( \x ->
          ComputeCapacityStatus'
            Prelude.<$> (x Data..:? "Available")
            Prelude.<*> (x Data..:? "InUse")
            Prelude.<*> (x Data..:? "Running")
            Prelude.<*> (x Data..: "Desired")
      )

instance Prelude.Hashable ComputeCapacityStatus where
  hashWithSalt _salt ComputeCapacityStatus' {..} =
    _salt `Prelude.hashWithSalt` available
      `Prelude.hashWithSalt` inUse
      `Prelude.hashWithSalt` running
      `Prelude.hashWithSalt` desired

instance Prelude.NFData ComputeCapacityStatus where
  rnf ComputeCapacityStatus' {..} =
    Prelude.rnf available
      `Prelude.seq` Prelude.rnf inUse
      `Prelude.seq` Prelude.rnf running
      `Prelude.seq` Prelude.rnf desired
