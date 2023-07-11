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
-- Module      : Amazonka.Inspector2.Types.DateFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.DateFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details on the time range used to filter findings.
--
-- /See:/ 'newDateFilter' smart constructor.
data DateFilter = DateFilter'
  { -- | A timestamp representing the end of the time period filtered on.
    endInclusive :: Prelude.Maybe Data.POSIX,
    -- | A timestamp representing the start of the time period filtered on.
    startInclusive :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endInclusive', 'dateFilter_endInclusive' - A timestamp representing the end of the time period filtered on.
--
-- 'startInclusive', 'dateFilter_startInclusive' - A timestamp representing the start of the time period filtered on.
newDateFilter ::
  DateFilter
newDateFilter =
  DateFilter'
    { endInclusive = Prelude.Nothing,
      startInclusive = Prelude.Nothing
    }

-- | A timestamp representing the end of the time period filtered on.
dateFilter_endInclusive :: Lens.Lens' DateFilter (Prelude.Maybe Prelude.UTCTime)
dateFilter_endInclusive = Lens.lens (\DateFilter' {endInclusive} -> endInclusive) (\s@DateFilter' {} a -> s {endInclusive = a} :: DateFilter) Prelude.. Lens.mapping Data._Time

-- | A timestamp representing the start of the time period filtered on.
dateFilter_startInclusive :: Lens.Lens' DateFilter (Prelude.Maybe Prelude.UTCTime)
dateFilter_startInclusive = Lens.lens (\DateFilter' {startInclusive} -> startInclusive) (\s@DateFilter' {} a -> s {startInclusive = a} :: DateFilter) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON DateFilter where
  parseJSON =
    Data.withObject
      "DateFilter"
      ( \x ->
          DateFilter'
            Prelude.<$> (x Data..:? "endInclusive")
            Prelude.<*> (x Data..:? "startInclusive")
      )

instance Prelude.Hashable DateFilter where
  hashWithSalt _salt DateFilter' {..} =
    _salt
      `Prelude.hashWithSalt` endInclusive
      `Prelude.hashWithSalt` startInclusive

instance Prelude.NFData DateFilter where
  rnf DateFilter' {..} =
    Prelude.rnf endInclusive
      `Prelude.seq` Prelude.rnf startInclusive

instance Data.ToJSON DateFilter where
  toJSON DateFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("endInclusive" Data..=) Prelude.<$> endInclusive,
            ("startInclusive" Data..=)
              Prelude.<$> startInclusive
          ]
      )
