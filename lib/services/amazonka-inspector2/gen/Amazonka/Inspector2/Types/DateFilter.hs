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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.DateFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details on the time range used to filter findings.
--
-- /See:/ 'newDateFilter' smart constructor.
data DateFilter = DateFilter'
  { -- | A timestamp representing the start of the time period filtered on.
    startInclusive :: Prelude.Maybe Core.POSIX,
    -- | A timestamp representing the end of the time period filtered on.
    endInclusive :: Prelude.Maybe Core.POSIX
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
-- 'startInclusive', 'dateFilter_startInclusive' - A timestamp representing the start of the time period filtered on.
--
-- 'endInclusive', 'dateFilter_endInclusive' - A timestamp representing the end of the time period filtered on.
newDateFilter ::
  DateFilter
newDateFilter =
  DateFilter'
    { startInclusive = Prelude.Nothing,
      endInclusive = Prelude.Nothing
    }

-- | A timestamp representing the start of the time period filtered on.
dateFilter_startInclusive :: Lens.Lens' DateFilter (Prelude.Maybe Prelude.UTCTime)
dateFilter_startInclusive = Lens.lens (\DateFilter' {startInclusive} -> startInclusive) (\s@DateFilter' {} a -> s {startInclusive = a} :: DateFilter) Prelude.. Lens.mapping Core._Time

-- | A timestamp representing the end of the time period filtered on.
dateFilter_endInclusive :: Lens.Lens' DateFilter (Prelude.Maybe Prelude.UTCTime)
dateFilter_endInclusive = Lens.lens (\DateFilter' {endInclusive} -> endInclusive) (\s@DateFilter' {} a -> s {endInclusive = a} :: DateFilter) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DateFilter where
  parseJSON =
    Core.withObject
      "DateFilter"
      ( \x ->
          DateFilter'
            Prelude.<$> (x Core..:? "startInclusive")
            Prelude.<*> (x Core..:? "endInclusive")
      )

instance Prelude.Hashable DateFilter where
  hashWithSalt _salt DateFilter' {..} =
    _salt `Prelude.hashWithSalt` startInclusive
      `Prelude.hashWithSalt` endInclusive

instance Prelude.NFData DateFilter where
  rnf DateFilter' {..} =
    Prelude.rnf startInclusive
      `Prelude.seq` Prelude.rnf endInclusive

instance Core.ToJSON DateFilter where
  toJSON DateFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("startInclusive" Core..=)
              Prelude.<$> startInclusive,
            ("endInclusive" Core..=) Prelude.<$> endInclusive
          ]
      )
