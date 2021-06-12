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
-- Module      : Network.AWS.SSM.Types.OpsFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.OpsFilterOperatorType

-- | A filter for viewing OpsItem summaries.
--
-- /See:/ 'newOpsFilter' smart constructor.
data OpsFilter = OpsFilter'
  { -- | The type of filter.
    type' :: Core.Maybe OpsFilterOperatorType,
    -- | The name of the filter.
    key :: Core.Text,
    -- | The filter value.
    values :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OpsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'opsFilter_type' - The type of filter.
--
-- 'key', 'opsFilter_key' - The name of the filter.
--
-- 'values', 'opsFilter_values' - The filter value.
newOpsFilter ::
  -- | 'key'
  Core.Text ->
  -- | 'values'
  Core.NonEmpty Core.Text ->
  OpsFilter
newOpsFilter pKey_ pValues_ =
  OpsFilter'
    { type' = Core.Nothing,
      key = pKey_,
      values = Lens._Coerce Lens.# pValues_
    }

-- | The type of filter.
opsFilter_type :: Lens.Lens' OpsFilter (Core.Maybe OpsFilterOperatorType)
opsFilter_type = Lens.lens (\OpsFilter' {type'} -> type') (\s@OpsFilter' {} a -> s {type' = a} :: OpsFilter)

-- | The name of the filter.
opsFilter_key :: Lens.Lens' OpsFilter Core.Text
opsFilter_key = Lens.lens (\OpsFilter' {key} -> key) (\s@OpsFilter' {} a -> s {key = a} :: OpsFilter)

-- | The filter value.
opsFilter_values :: Lens.Lens' OpsFilter (Core.NonEmpty Core.Text)
opsFilter_values = Lens.lens (\OpsFilter' {values} -> values) (\s@OpsFilter' {} a -> s {values = a} :: OpsFilter) Core.. Lens._Coerce

instance Core.Hashable OpsFilter

instance Core.NFData OpsFilter

instance Core.ToJSON OpsFilter where
  toJSON OpsFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Type" Core..=) Core.<$> type',
            Core.Just ("Key" Core..= key),
            Core.Just ("Values" Core..= values)
          ]
      )
