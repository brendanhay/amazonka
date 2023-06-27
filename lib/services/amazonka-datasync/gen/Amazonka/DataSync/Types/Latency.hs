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
-- Module      : Amazonka.DataSync.Types.Latency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.Latency where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The latency peaks for an on-premises storage system resource. Each data
-- point represents the 95th percentile peak value during a 1-hour
-- interval.
--
-- /See:/ 'newLatency' smart constructor.
data Latency = Latency'
  { -- | Peak latency for operations unrelated to read and write operations.
    other :: Prelude.Maybe Prelude.Double,
    -- | Peak latency for read operations.
    read :: Prelude.Maybe Prelude.Double,
    -- | Peak latency for write operations.
    write :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Latency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'other', 'latency_other' - Peak latency for operations unrelated to read and write operations.
--
-- 'read', 'latency_read' - Peak latency for read operations.
--
-- 'write', 'latency_write' - Peak latency for write operations.
newLatency ::
  Latency
newLatency =
  Latency'
    { other = Prelude.Nothing,
      read = Prelude.Nothing,
      write = Prelude.Nothing
    }

-- | Peak latency for operations unrelated to read and write operations.
latency_other :: Lens.Lens' Latency (Prelude.Maybe Prelude.Double)
latency_other = Lens.lens (\Latency' {other} -> other) (\s@Latency' {} a -> s {other = a} :: Latency)

-- | Peak latency for read operations.
latency_read :: Lens.Lens' Latency (Prelude.Maybe Prelude.Double)
latency_read = Lens.lens (\Latency' {read} -> read) (\s@Latency' {} a -> s {read = a} :: Latency)

-- | Peak latency for write operations.
latency_write :: Lens.Lens' Latency (Prelude.Maybe Prelude.Double)
latency_write = Lens.lens (\Latency' {write} -> write) (\s@Latency' {} a -> s {write = a} :: Latency)

instance Data.FromJSON Latency where
  parseJSON =
    Data.withObject
      "Latency"
      ( \x ->
          Latency'
            Prelude.<$> (x Data..:? "Other")
            Prelude.<*> (x Data..:? "Read")
            Prelude.<*> (x Data..:? "Write")
      )

instance Prelude.Hashable Latency where
  hashWithSalt _salt Latency' {..} =
    _salt
      `Prelude.hashWithSalt` other
      `Prelude.hashWithSalt` read
      `Prelude.hashWithSalt` write

instance Prelude.NFData Latency where
  rnf Latency' {..} =
    Prelude.rnf other
      `Prelude.seq` Prelude.rnf read
      `Prelude.seq` Prelude.rnf write
