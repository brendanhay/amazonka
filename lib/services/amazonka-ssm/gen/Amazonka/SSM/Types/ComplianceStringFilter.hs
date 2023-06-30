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
-- Module      : Amazonka.SSM.Types.ComplianceStringFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ComplianceStringFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.ComplianceQueryOperatorType

-- | One or more filters. Use a filter to return a more specific list of
-- results.
--
-- /See:/ 'newComplianceStringFilter' smart constructor.
data ComplianceStringFilter = ComplianceStringFilter'
  { -- | The name of the filter.
    key :: Prelude.Maybe Prelude.Text,
    -- | The type of comparison that should be performed for the value: Equal,
    -- NotEqual, BeginWith, LessThan, or GreaterThan.
    type' :: Prelude.Maybe ComplianceQueryOperatorType,
    -- | The value for which to search.
    values :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComplianceStringFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'complianceStringFilter_key' - The name of the filter.
--
-- 'type'', 'complianceStringFilter_type' - The type of comparison that should be performed for the value: Equal,
-- NotEqual, BeginWith, LessThan, or GreaterThan.
--
-- 'values', 'complianceStringFilter_values' - The value for which to search.
newComplianceStringFilter ::
  ComplianceStringFilter
newComplianceStringFilter =
  ComplianceStringFilter'
    { key = Prelude.Nothing,
      type' = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the filter.
complianceStringFilter_key :: Lens.Lens' ComplianceStringFilter (Prelude.Maybe Prelude.Text)
complianceStringFilter_key = Lens.lens (\ComplianceStringFilter' {key} -> key) (\s@ComplianceStringFilter' {} a -> s {key = a} :: ComplianceStringFilter)

-- | The type of comparison that should be performed for the value: Equal,
-- NotEqual, BeginWith, LessThan, or GreaterThan.
complianceStringFilter_type :: Lens.Lens' ComplianceStringFilter (Prelude.Maybe ComplianceQueryOperatorType)
complianceStringFilter_type = Lens.lens (\ComplianceStringFilter' {type'} -> type') (\s@ComplianceStringFilter' {} a -> s {type' = a} :: ComplianceStringFilter)

-- | The value for which to search.
complianceStringFilter_values :: Lens.Lens' ComplianceStringFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
complianceStringFilter_values = Lens.lens (\ComplianceStringFilter' {values} -> values) (\s@ComplianceStringFilter' {} a -> s {values = a} :: ComplianceStringFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ComplianceStringFilter where
  hashWithSalt _salt ComplianceStringFilter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` values

instance Prelude.NFData ComplianceStringFilter where
  rnf ComplianceStringFilter' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON ComplianceStringFilter where
  toJSON ComplianceStringFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("Type" Data..=) Prelude.<$> type',
            ("Values" Data..=) Prelude.<$> values
          ]
      )
