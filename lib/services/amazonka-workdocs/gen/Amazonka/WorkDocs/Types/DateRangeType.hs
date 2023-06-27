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
-- Module      : Amazonka.WorkDocs.Types.DateRangeType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.DateRangeType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters results based on timestamp range (in epochs).
--
-- /See:/ 'newDateRangeType' smart constructor.
data DateRangeType = DateRangeType'
  { -- | Timestamp range end value (in epochs).
    endValue :: Prelude.Maybe Data.POSIX,
    -- | Timestamp range start value (in epochs)
    startValue :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateRangeType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endValue', 'dateRangeType_endValue' - Timestamp range end value (in epochs).
--
-- 'startValue', 'dateRangeType_startValue' - Timestamp range start value (in epochs)
newDateRangeType ::
  DateRangeType
newDateRangeType =
  DateRangeType'
    { endValue = Prelude.Nothing,
      startValue = Prelude.Nothing
    }

-- | Timestamp range end value (in epochs).
dateRangeType_endValue :: Lens.Lens' DateRangeType (Prelude.Maybe Prelude.UTCTime)
dateRangeType_endValue = Lens.lens (\DateRangeType' {endValue} -> endValue) (\s@DateRangeType' {} a -> s {endValue = a} :: DateRangeType) Prelude.. Lens.mapping Data._Time

-- | Timestamp range start value (in epochs)
dateRangeType_startValue :: Lens.Lens' DateRangeType (Prelude.Maybe Prelude.UTCTime)
dateRangeType_startValue = Lens.lens (\DateRangeType' {startValue} -> startValue) (\s@DateRangeType' {} a -> s {startValue = a} :: DateRangeType) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable DateRangeType where
  hashWithSalt _salt DateRangeType' {..} =
    _salt
      `Prelude.hashWithSalt` endValue
      `Prelude.hashWithSalt` startValue

instance Prelude.NFData DateRangeType where
  rnf DateRangeType' {..} =
    Prelude.rnf endValue
      `Prelude.seq` Prelude.rnf startValue

instance Data.ToJSON DateRangeType where
  toJSON DateRangeType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndValue" Data..=) Prelude.<$> endValue,
            ("StartValue" Data..=) Prelude.<$> startValue
          ]
      )
