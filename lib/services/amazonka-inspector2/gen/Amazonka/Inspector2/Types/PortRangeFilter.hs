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
-- Module      : Amazonka.Inspector2.Types.PortRangeFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.PortRangeFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that describes the details of a port range filter.
--
-- /See:/ 'newPortRangeFilter' smart constructor.
data PortRangeFilter = PortRangeFilter'
  { -- | The port number the port range begins at.
    beginInclusive :: Prelude.Maybe Prelude.Natural,
    -- | The port number the port range ends at.
    endInclusive :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortRangeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginInclusive', 'portRangeFilter_beginInclusive' - The port number the port range begins at.
--
-- 'endInclusive', 'portRangeFilter_endInclusive' - The port number the port range ends at.
newPortRangeFilter ::
  PortRangeFilter
newPortRangeFilter =
  PortRangeFilter'
    { beginInclusive = Prelude.Nothing,
      endInclusive = Prelude.Nothing
    }

-- | The port number the port range begins at.
portRangeFilter_beginInclusive :: Lens.Lens' PortRangeFilter (Prelude.Maybe Prelude.Natural)
portRangeFilter_beginInclusive = Lens.lens (\PortRangeFilter' {beginInclusive} -> beginInclusive) (\s@PortRangeFilter' {} a -> s {beginInclusive = a} :: PortRangeFilter)

-- | The port number the port range ends at.
portRangeFilter_endInclusive :: Lens.Lens' PortRangeFilter (Prelude.Maybe Prelude.Natural)
portRangeFilter_endInclusive = Lens.lens (\PortRangeFilter' {endInclusive} -> endInclusive) (\s@PortRangeFilter' {} a -> s {endInclusive = a} :: PortRangeFilter)

instance Data.FromJSON PortRangeFilter where
  parseJSON =
    Data.withObject
      "PortRangeFilter"
      ( \x ->
          PortRangeFilter'
            Prelude.<$> (x Data..:? "beginInclusive")
            Prelude.<*> (x Data..:? "endInclusive")
      )

instance Prelude.Hashable PortRangeFilter where
  hashWithSalt _salt PortRangeFilter' {..} =
    _salt `Prelude.hashWithSalt` beginInclusive
      `Prelude.hashWithSalt` endInclusive

instance Prelude.NFData PortRangeFilter where
  rnf PortRangeFilter' {..} =
    Prelude.rnf beginInclusive
      `Prelude.seq` Prelude.rnf endInclusive

instance Data.ToJSON PortRangeFilter where
  toJSON PortRangeFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("beginInclusive" Data..=)
              Prelude.<$> beginInclusive,
            ("endInclusive" Data..=) Prelude.<$> endInclusive
          ]
      )
