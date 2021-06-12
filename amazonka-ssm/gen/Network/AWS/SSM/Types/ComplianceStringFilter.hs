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
-- Module      : Network.AWS.SSM.Types.ComplianceStringFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceStringFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.ComplianceQueryOperatorType

-- | One or more filters. Use a filter to return a more specific list of
-- results.
--
-- /See:/ 'newComplianceStringFilter' smart constructor.
data ComplianceStringFilter = ComplianceStringFilter'
  { -- | The name of the filter.
    key :: Core.Maybe Core.Text,
    -- | The value for which to search.
    values :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The type of comparison that should be performed for the value: Equal,
    -- NotEqual, BeginWith, LessThan, or GreaterThan.
    type' :: Core.Maybe ComplianceQueryOperatorType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'values', 'complianceStringFilter_values' - The value for which to search.
--
-- 'type'', 'complianceStringFilter_type' - The type of comparison that should be performed for the value: Equal,
-- NotEqual, BeginWith, LessThan, or GreaterThan.
newComplianceStringFilter ::
  ComplianceStringFilter
newComplianceStringFilter =
  ComplianceStringFilter'
    { key = Core.Nothing,
      values = Core.Nothing,
      type' = Core.Nothing
    }

-- | The name of the filter.
complianceStringFilter_key :: Lens.Lens' ComplianceStringFilter (Core.Maybe Core.Text)
complianceStringFilter_key = Lens.lens (\ComplianceStringFilter' {key} -> key) (\s@ComplianceStringFilter' {} a -> s {key = a} :: ComplianceStringFilter)

-- | The value for which to search.
complianceStringFilter_values :: Lens.Lens' ComplianceStringFilter (Core.Maybe (Core.NonEmpty Core.Text))
complianceStringFilter_values = Lens.lens (\ComplianceStringFilter' {values} -> values) (\s@ComplianceStringFilter' {} a -> s {values = a} :: ComplianceStringFilter) Core.. Lens.mapping Lens._Coerce

-- | The type of comparison that should be performed for the value: Equal,
-- NotEqual, BeginWith, LessThan, or GreaterThan.
complianceStringFilter_type :: Lens.Lens' ComplianceStringFilter (Core.Maybe ComplianceQueryOperatorType)
complianceStringFilter_type = Lens.lens (\ComplianceStringFilter' {type'} -> type') (\s@ComplianceStringFilter' {} a -> s {type' = a} :: ComplianceStringFilter)

instance Core.Hashable ComplianceStringFilter

instance Core.NFData ComplianceStringFilter

instance Core.ToJSON ComplianceStringFilter where
  toJSON ComplianceStringFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Values" Core..=) Core.<$> values,
            ("Type" Core..=) Core.<$> type'
          ]
      )
