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
-- Module      : Amazonka.DataSync.Types.FilterRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.FilterRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.FilterType
import qualified Amazonka.Prelude as Prelude

-- | Specifies which files, folders, and objects to include or exclude when
-- transferring files from source to destination.
--
-- /See:/ 'newFilterRule' smart constructor.
data FilterRule = FilterRule'
  { -- | The type of filter rule to apply. DataSync only supports the
    -- SIMPLE_PATTERN rule type.
    filterType :: Prelude.Maybe FilterType,
    -- | A single filter string that consists of the patterns to include or
    -- exclude. The patterns are delimited by \"|\" (that is, a pipe), for
    -- example: @\/folder1|\/folder2@
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterType', 'filterRule_filterType' - The type of filter rule to apply. DataSync only supports the
-- SIMPLE_PATTERN rule type.
--
-- 'value', 'filterRule_value' - A single filter string that consists of the patterns to include or
-- exclude. The patterns are delimited by \"|\" (that is, a pipe), for
-- example: @\/folder1|\/folder2@
newFilterRule ::
  FilterRule
newFilterRule =
  FilterRule'
    { filterType = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The type of filter rule to apply. DataSync only supports the
-- SIMPLE_PATTERN rule type.
filterRule_filterType :: Lens.Lens' FilterRule (Prelude.Maybe FilterType)
filterRule_filterType = Lens.lens (\FilterRule' {filterType} -> filterType) (\s@FilterRule' {} a -> s {filterType = a} :: FilterRule)

-- | A single filter string that consists of the patterns to include or
-- exclude. The patterns are delimited by \"|\" (that is, a pipe), for
-- example: @\/folder1|\/folder2@
filterRule_value :: Lens.Lens' FilterRule (Prelude.Maybe Prelude.Text)
filterRule_value = Lens.lens (\FilterRule' {value} -> value) (\s@FilterRule' {} a -> s {value = a} :: FilterRule)

instance Data.FromJSON FilterRule where
  parseJSON =
    Data.withObject
      "FilterRule"
      ( \x ->
          FilterRule'
            Prelude.<$> (x Data..:? "FilterType")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable FilterRule where
  hashWithSalt _salt FilterRule' {..} =
    _salt
      `Prelude.hashWithSalt` filterType
      `Prelude.hashWithSalt` value

instance Prelude.NFData FilterRule where
  rnf FilterRule' {..} =
    Prelude.rnf filterType
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON FilterRule where
  toJSON FilterRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FilterType" Data..=) Prelude.<$> filterType,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
