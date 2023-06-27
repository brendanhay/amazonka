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
-- Module      : Amazonka.DataSync.Types.Throughput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.Throughput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The throughput peaks for an on-premises storage system volume. Each data
-- point represents the 95th percentile peak value during a 1-hour
-- interval.
--
-- /See:/ 'newThroughput' smart constructor.
data Throughput = Throughput'
  { -- | Peak throughput unrelated to read and write operations.
    other :: Prelude.Maybe Prelude.Double,
    -- | Peak throughput related to read operations.
    read :: Prelude.Maybe Prelude.Double,
    -- | Peak total throughput on your on-premises storage system resource.
    total :: Prelude.Maybe Prelude.Double,
    -- | Peak throughput related to write operations.
    write :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Throughput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'other', 'throughput_other' - Peak throughput unrelated to read and write operations.
--
-- 'read', 'throughput_read' - Peak throughput related to read operations.
--
-- 'total', 'throughput_total' - Peak total throughput on your on-premises storage system resource.
--
-- 'write', 'throughput_write' - Peak throughput related to write operations.
newThroughput ::
  Throughput
newThroughput =
  Throughput'
    { other = Prelude.Nothing,
      read = Prelude.Nothing,
      total = Prelude.Nothing,
      write = Prelude.Nothing
    }

-- | Peak throughput unrelated to read and write operations.
throughput_other :: Lens.Lens' Throughput (Prelude.Maybe Prelude.Double)
throughput_other = Lens.lens (\Throughput' {other} -> other) (\s@Throughput' {} a -> s {other = a} :: Throughput)

-- | Peak throughput related to read operations.
throughput_read :: Lens.Lens' Throughput (Prelude.Maybe Prelude.Double)
throughput_read = Lens.lens (\Throughput' {read} -> read) (\s@Throughput' {} a -> s {read = a} :: Throughput)

-- | Peak total throughput on your on-premises storage system resource.
throughput_total :: Lens.Lens' Throughput (Prelude.Maybe Prelude.Double)
throughput_total = Lens.lens (\Throughput' {total} -> total) (\s@Throughput' {} a -> s {total = a} :: Throughput)

-- | Peak throughput related to write operations.
throughput_write :: Lens.Lens' Throughput (Prelude.Maybe Prelude.Double)
throughput_write = Lens.lens (\Throughput' {write} -> write) (\s@Throughput' {} a -> s {write = a} :: Throughput)

instance Data.FromJSON Throughput where
  parseJSON =
    Data.withObject
      "Throughput"
      ( \x ->
          Throughput'
            Prelude.<$> (x Data..:? "Other")
            Prelude.<*> (x Data..:? "Read")
            Prelude.<*> (x Data..:? "Total")
            Prelude.<*> (x Data..:? "Write")
      )

instance Prelude.Hashable Throughput where
  hashWithSalt _salt Throughput' {..} =
    _salt
      `Prelude.hashWithSalt` other
      `Prelude.hashWithSalt` read
      `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` write

instance Prelude.NFData Throughput where
  rnf Throughput' {..} =
    Prelude.rnf other
      `Prelude.seq` Prelude.rnf read
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf write
