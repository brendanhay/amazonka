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
-- Module      : Amazonka.DeviceFarm.Types.Counters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.Counters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents entity counters.
--
-- /See:/ 'newCounters' smart constructor.
data Counters = Counters'
  { -- | The number of errored entities.
    errored :: Prelude.Maybe Prelude.Int,
    -- | The number of failed entities.
    failed :: Prelude.Maybe Prelude.Int,
    -- | The number of passed entities.
    passed :: Prelude.Maybe Prelude.Int,
    -- | The number of skipped entities.
    skipped :: Prelude.Maybe Prelude.Int,
    -- | The number of stopped entities.
    stopped :: Prelude.Maybe Prelude.Int,
    -- | The total number of entities.
    total :: Prelude.Maybe Prelude.Int,
    -- | The number of warned entities.
    warned :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'failed', 'counters_failed' - The number of failed entities.
--
-- 'passed', 'counters_passed' - The number of passed entities.
--
-- 'skipped', 'counters_skipped' - The number of skipped entities.
--
-- 'stopped', 'counters_stopped' - The number of stopped entities.
--
-- 'total', 'counters_total' - The total number of entities.
--
-- 'warned', 'counters_warned' - The number of warned entities.
newCounters ::
  Counters
newCounters =
  Counters'
    { errored = Prelude.Nothing,
      failed = Prelude.Nothing,
      passed = Prelude.Nothing,
      skipped = Prelude.Nothing,
      stopped = Prelude.Nothing,
      total = Prelude.Nothing,
      warned = Prelude.Nothing
    }

-- | The number of errored entities.
counters_errored :: Lens.Lens' Counters (Prelude.Maybe Prelude.Int)
counters_errored = Lens.lens (\Counters' {errored} -> errored) (\s@Counters' {} a -> s {errored = a} :: Counters)

-- | The number of failed entities.
counters_failed :: Lens.Lens' Counters (Prelude.Maybe Prelude.Int)
counters_failed = Lens.lens (\Counters' {failed} -> failed) (\s@Counters' {} a -> s {failed = a} :: Counters)

-- | The number of passed entities.
counters_passed :: Lens.Lens' Counters (Prelude.Maybe Prelude.Int)
counters_passed = Lens.lens (\Counters' {passed} -> passed) (\s@Counters' {} a -> s {passed = a} :: Counters)

-- | The number of skipped entities.
counters_skipped :: Lens.Lens' Counters (Prelude.Maybe Prelude.Int)
counters_skipped = Lens.lens (\Counters' {skipped} -> skipped) (\s@Counters' {} a -> s {skipped = a} :: Counters)

-- | The number of stopped entities.
counters_stopped :: Lens.Lens' Counters (Prelude.Maybe Prelude.Int)
counters_stopped = Lens.lens (\Counters' {stopped} -> stopped) (\s@Counters' {} a -> s {stopped = a} :: Counters)

-- | The total number of entities.
counters_total :: Lens.Lens' Counters (Prelude.Maybe Prelude.Int)
counters_total = Lens.lens (\Counters' {total} -> total) (\s@Counters' {} a -> s {total = a} :: Counters)

-- | The number of warned entities.
counters_warned :: Lens.Lens' Counters (Prelude.Maybe Prelude.Int)
counters_warned = Lens.lens (\Counters' {warned} -> warned) (\s@Counters' {} a -> s {warned = a} :: Counters)

instance Data.FromJSON Counters where
  parseJSON =
    Data.withObject
      "Counters"
      ( \x ->
          Counters'
            Prelude.<$> (x Data..:? "errored")
            Prelude.<*> (x Data..:? "failed")
            Prelude.<*> (x Data..:? "passed")
            Prelude.<*> (x Data..:? "skipped")
            Prelude.<*> (x Data..:? "stopped")
            Prelude.<*> (x Data..:? "total")
            Prelude.<*> (x Data..:? "warned")
      )

instance Prelude.Hashable Counters where
  hashWithSalt _salt Counters' {..} =
    _salt
      `Prelude.hashWithSalt` errored
      `Prelude.hashWithSalt` failed
      `Prelude.hashWithSalt` passed
      `Prelude.hashWithSalt` skipped
      `Prelude.hashWithSalt` stopped
      `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` warned

instance Prelude.NFData Counters where
  rnf Counters' {..} =
    Prelude.rnf errored
      `Prelude.seq` Prelude.rnf failed
      `Prelude.seq` Prelude.rnf passed
      `Prelude.seq` Prelude.rnf skipped
      `Prelude.seq` Prelude.rnf stopped
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf warned
