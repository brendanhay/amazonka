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
-- Module      : Amazonka.DataSync.Types.IOPS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.IOPS where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The IOPS peaks for an on-premises storage system resource. Each data
-- point represents the 95th percentile peak value during a 1-hour
-- interval.
--
-- /See:/ 'newIOPS' smart constructor.
data IOPS = IOPS'
  { -- | Peak IOPS unrelated to read and write operations.
    other :: Prelude.Maybe Prelude.Double,
    -- | Peak IOPS related to read operations.
    read :: Prelude.Maybe Prelude.Double,
    -- | Peak total IOPS on your on-premises storage system resource.
    total :: Prelude.Maybe Prelude.Double,
    -- | Peak IOPS related to write operations.
    write :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IOPS' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'other', 'iops_other' - Peak IOPS unrelated to read and write operations.
--
-- 'read', 'iops_read' - Peak IOPS related to read operations.
--
-- 'total', 'iops_total' - Peak total IOPS on your on-premises storage system resource.
--
-- 'write', 'iops_write' - Peak IOPS related to write operations.
newIOPS ::
  IOPS
newIOPS =
  IOPS'
    { other = Prelude.Nothing,
      read = Prelude.Nothing,
      total = Prelude.Nothing,
      write = Prelude.Nothing
    }

-- | Peak IOPS unrelated to read and write operations.
iops_other :: Lens.Lens' IOPS (Prelude.Maybe Prelude.Double)
iops_other = Lens.lens (\IOPS' {other} -> other) (\s@IOPS' {} a -> s {other = a} :: IOPS)

-- | Peak IOPS related to read operations.
iops_read :: Lens.Lens' IOPS (Prelude.Maybe Prelude.Double)
iops_read = Lens.lens (\IOPS' {read} -> read) (\s@IOPS' {} a -> s {read = a} :: IOPS)

-- | Peak total IOPS on your on-premises storage system resource.
iops_total :: Lens.Lens' IOPS (Prelude.Maybe Prelude.Double)
iops_total = Lens.lens (\IOPS' {total} -> total) (\s@IOPS' {} a -> s {total = a} :: IOPS)

-- | Peak IOPS related to write operations.
iops_write :: Lens.Lens' IOPS (Prelude.Maybe Prelude.Double)
iops_write = Lens.lens (\IOPS' {write} -> write) (\s@IOPS' {} a -> s {write = a} :: IOPS)

instance Data.FromJSON IOPS where
  parseJSON =
    Data.withObject
      "IOPS"
      ( \x ->
          IOPS'
            Prelude.<$> (x Data..:? "Other")
            Prelude.<*> (x Data..:? "Read")
            Prelude.<*> (x Data..:? "Total")
            Prelude.<*> (x Data..:? "Write")
      )

instance Prelude.Hashable IOPS where
  hashWithSalt _salt IOPS' {..} =
    _salt
      `Prelude.hashWithSalt` other
      `Prelude.hashWithSalt` read
      `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` write

instance Prelude.NFData IOPS where
  rnf IOPS' {..} =
    Prelude.rnf other
      `Prelude.seq` Prelude.rnf read
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf write
