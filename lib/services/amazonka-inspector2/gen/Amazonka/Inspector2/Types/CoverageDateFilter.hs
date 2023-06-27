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
-- Module      : Amazonka.Inspector2.Types.CoverageDateFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.CoverageDateFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details of a coverage date filter.
--
-- /See:/ 'newCoverageDateFilter' smart constructor.
data CoverageDateFilter = CoverageDateFilter'
  { -- | A timestamp representing the end of the time period to filter results
    -- by.
    endInclusive :: Prelude.Maybe Data.POSIX,
    -- | A timestamp representing the start of the time period to filter results
    -- by.
    startInclusive :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageDateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endInclusive', 'coverageDateFilter_endInclusive' - A timestamp representing the end of the time period to filter results
-- by.
--
-- 'startInclusive', 'coverageDateFilter_startInclusive' - A timestamp representing the start of the time period to filter results
-- by.
newCoverageDateFilter ::
  CoverageDateFilter
newCoverageDateFilter =
  CoverageDateFilter'
    { endInclusive = Prelude.Nothing,
      startInclusive = Prelude.Nothing
    }

-- | A timestamp representing the end of the time period to filter results
-- by.
coverageDateFilter_endInclusive :: Lens.Lens' CoverageDateFilter (Prelude.Maybe Prelude.UTCTime)
coverageDateFilter_endInclusive = Lens.lens (\CoverageDateFilter' {endInclusive} -> endInclusive) (\s@CoverageDateFilter' {} a -> s {endInclusive = a} :: CoverageDateFilter) Prelude.. Lens.mapping Data._Time

-- | A timestamp representing the start of the time period to filter results
-- by.
coverageDateFilter_startInclusive :: Lens.Lens' CoverageDateFilter (Prelude.Maybe Prelude.UTCTime)
coverageDateFilter_startInclusive = Lens.lens (\CoverageDateFilter' {startInclusive} -> startInclusive) (\s@CoverageDateFilter' {} a -> s {startInclusive = a} :: CoverageDateFilter) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable CoverageDateFilter where
  hashWithSalt _salt CoverageDateFilter' {..} =
    _salt
      `Prelude.hashWithSalt` endInclusive
      `Prelude.hashWithSalt` startInclusive

instance Prelude.NFData CoverageDateFilter where
  rnf CoverageDateFilter' {..} =
    Prelude.rnf endInclusive
      `Prelude.seq` Prelude.rnf startInclusive

instance Data.ToJSON CoverageDateFilter where
  toJSON CoverageDateFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("endInclusive" Data..=) Prelude.<$> endInclusive,
            ("startInclusive" Data..=)
              Prelude.<$> startInclusive
          ]
      )
