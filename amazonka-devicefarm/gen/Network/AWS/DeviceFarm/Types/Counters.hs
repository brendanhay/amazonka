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
-- Module      : Network.AWS.DeviceFarm.Types.Counters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Counters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents entity counters.
--
-- /See:/ 'newCounters' smart constructor.
data Counters = Counters'
  { -- | The number of errored entities.
    errored :: Core.Maybe Core.Int,
    -- | The number of warned entities.
    warned :: Core.Maybe Core.Int,
    -- | The number of passed entities.
    passed :: Core.Maybe Core.Int,
    -- | The total number of entities.
    total :: Core.Maybe Core.Int,
    -- | The number of stopped entities.
    stopped :: Core.Maybe Core.Int,
    -- | The number of failed entities.
    failed :: Core.Maybe Core.Int,
    -- | The number of skipped entities.
    skipped :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Counters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errored', 'counters_errored' - The number of errored entities.
--
-- 'warned', 'counters_warned' - The number of warned entities.
--
-- 'passed', 'counters_passed' - The number of passed entities.
--
-- 'total', 'counters_total' - The total number of entities.
--
-- 'stopped', 'counters_stopped' - The number of stopped entities.
--
-- 'failed', 'counters_failed' - The number of failed entities.
--
-- 'skipped', 'counters_skipped' - The number of skipped entities.
newCounters ::
  Counters
newCounters =
  Counters'
    { errored = Core.Nothing,
      warned = Core.Nothing,
      passed = Core.Nothing,
      total = Core.Nothing,
      stopped = Core.Nothing,
      failed = Core.Nothing,
      skipped = Core.Nothing
    }

-- | The number of errored entities.
counters_errored :: Lens.Lens' Counters (Core.Maybe Core.Int)
counters_errored = Lens.lens (\Counters' {errored} -> errored) (\s@Counters' {} a -> s {errored = a} :: Counters)

-- | The number of warned entities.
counters_warned :: Lens.Lens' Counters (Core.Maybe Core.Int)
counters_warned = Lens.lens (\Counters' {warned} -> warned) (\s@Counters' {} a -> s {warned = a} :: Counters)

-- | The number of passed entities.
counters_passed :: Lens.Lens' Counters (Core.Maybe Core.Int)
counters_passed = Lens.lens (\Counters' {passed} -> passed) (\s@Counters' {} a -> s {passed = a} :: Counters)

-- | The total number of entities.
counters_total :: Lens.Lens' Counters (Core.Maybe Core.Int)
counters_total = Lens.lens (\Counters' {total} -> total) (\s@Counters' {} a -> s {total = a} :: Counters)

-- | The number of stopped entities.
counters_stopped :: Lens.Lens' Counters (Core.Maybe Core.Int)
counters_stopped = Lens.lens (\Counters' {stopped} -> stopped) (\s@Counters' {} a -> s {stopped = a} :: Counters)

-- | The number of failed entities.
counters_failed :: Lens.Lens' Counters (Core.Maybe Core.Int)
counters_failed = Lens.lens (\Counters' {failed} -> failed) (\s@Counters' {} a -> s {failed = a} :: Counters)

-- | The number of skipped entities.
counters_skipped :: Lens.Lens' Counters (Core.Maybe Core.Int)
counters_skipped = Lens.lens (\Counters' {skipped} -> skipped) (\s@Counters' {} a -> s {skipped = a} :: Counters)

instance Core.FromJSON Counters where
  parseJSON =
    Core.withObject
      "Counters"
      ( \x ->
          Counters'
            Core.<$> (x Core..:? "errored")
            Core.<*> (x Core..:? "warned")
            Core.<*> (x Core..:? "passed")
            Core.<*> (x Core..:? "total")
            Core.<*> (x Core..:? "stopped")
            Core.<*> (x Core..:? "failed")
            Core.<*> (x Core..:? "skipped")
      )

instance Core.Hashable Counters

instance Core.NFData Counters
