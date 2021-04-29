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
-- Module      : Network.AWS.AppStream.Types.ComputeCapacityStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ComputeCapacityStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the capacity status for a fleet.
--
-- /See:/ 'newComputeCapacityStatus' smart constructor.
data ComputeCapacityStatus = ComputeCapacityStatus'
  { -- | The total number of simultaneous streaming instances that are running.
    running :: Prelude.Maybe Prelude.Int,
    -- | The number of currently available instances that can be used to stream
    -- sessions.
    available :: Prelude.Maybe Prelude.Int,
    -- | The number of instances in use for streaming.
    inUse :: Prelude.Maybe Prelude.Int,
    -- | The desired number of streaming instances.
    desired :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ComputeCapacityStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'running', 'computeCapacityStatus_running' - The total number of simultaneous streaming instances that are running.
--
-- 'available', 'computeCapacityStatus_available' - The number of currently available instances that can be used to stream
-- sessions.
--
-- 'inUse', 'computeCapacityStatus_inUse' - The number of instances in use for streaming.
--
-- 'desired', 'computeCapacityStatus_desired' - The desired number of streaming instances.
newComputeCapacityStatus ::
  -- | 'desired'
  Prelude.Int ->
  ComputeCapacityStatus
newComputeCapacityStatus pDesired_ =
  ComputeCapacityStatus'
    { running = Prelude.Nothing,
      available = Prelude.Nothing,
      inUse = Prelude.Nothing,
      desired = pDesired_
    }

-- | The total number of simultaneous streaming instances that are running.
computeCapacityStatus_running :: Lens.Lens' ComputeCapacityStatus (Prelude.Maybe Prelude.Int)
computeCapacityStatus_running = Lens.lens (\ComputeCapacityStatus' {running} -> running) (\s@ComputeCapacityStatus' {} a -> s {running = a} :: ComputeCapacityStatus)

-- | The number of currently available instances that can be used to stream
-- sessions.
computeCapacityStatus_available :: Lens.Lens' ComputeCapacityStatus (Prelude.Maybe Prelude.Int)
computeCapacityStatus_available = Lens.lens (\ComputeCapacityStatus' {available} -> available) (\s@ComputeCapacityStatus' {} a -> s {available = a} :: ComputeCapacityStatus)

-- | The number of instances in use for streaming.
computeCapacityStatus_inUse :: Lens.Lens' ComputeCapacityStatus (Prelude.Maybe Prelude.Int)
computeCapacityStatus_inUse = Lens.lens (\ComputeCapacityStatus' {inUse} -> inUse) (\s@ComputeCapacityStatus' {} a -> s {inUse = a} :: ComputeCapacityStatus)

-- | The desired number of streaming instances.
computeCapacityStatus_desired :: Lens.Lens' ComputeCapacityStatus Prelude.Int
computeCapacityStatus_desired = Lens.lens (\ComputeCapacityStatus' {desired} -> desired) (\s@ComputeCapacityStatus' {} a -> s {desired = a} :: ComputeCapacityStatus)

instance Prelude.FromJSON ComputeCapacityStatus where
  parseJSON =
    Prelude.withObject
      "ComputeCapacityStatus"
      ( \x ->
          ComputeCapacityStatus'
            Prelude.<$> (x Prelude..:? "Running")
            Prelude.<*> (x Prelude..:? "Available")
            Prelude.<*> (x Prelude..:? "InUse")
            Prelude.<*> (x Prelude..: "Desired")
      )

instance Prelude.Hashable ComputeCapacityStatus

instance Prelude.NFData ComputeCapacityStatus
