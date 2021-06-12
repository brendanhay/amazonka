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
-- Module      : Network.AWS.SSM.Types.InstancePatchStateFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstancePatchStateFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.InstancePatchStateOperatorType

-- | Defines a filter used in DescribeInstancePatchStatesForPatchGroup used
-- to scope down the information returned by the API.
--
-- /See:/ 'newInstancePatchStateFilter' smart constructor.
data InstancePatchStateFilter = InstancePatchStateFilter'
  { -- | The key for the filter. Supported values are FailedCount,
    -- InstalledCount, InstalledOtherCount, MissingCount and
    -- NotApplicableCount.
    key :: Core.Text,
    -- | The value for the filter, must be an integer greater than or equal to 0.
    values :: Core.NonEmpty Core.Text,
    -- | The type of comparison that should be performed for the value: Equal,
    -- NotEqual, LessThan or GreaterThan.
    type' :: InstancePatchStateOperatorType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstancePatchStateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'instancePatchStateFilter_key' - The key for the filter. Supported values are FailedCount,
-- InstalledCount, InstalledOtherCount, MissingCount and
-- NotApplicableCount.
--
-- 'values', 'instancePatchStateFilter_values' - The value for the filter, must be an integer greater than or equal to 0.
--
-- 'type'', 'instancePatchStateFilter_type' - The type of comparison that should be performed for the value: Equal,
-- NotEqual, LessThan or GreaterThan.
newInstancePatchStateFilter ::
  -- | 'key'
  Core.Text ->
  -- | 'values'
  Core.NonEmpty Core.Text ->
  -- | 'type''
  InstancePatchStateOperatorType ->
  InstancePatchStateFilter
newInstancePatchStateFilter pKey_ pValues_ pType_ =
  InstancePatchStateFilter'
    { key = pKey_,
      values = Lens._Coerce Lens.# pValues_,
      type' = pType_
    }

-- | The key for the filter. Supported values are FailedCount,
-- InstalledCount, InstalledOtherCount, MissingCount and
-- NotApplicableCount.
instancePatchStateFilter_key :: Lens.Lens' InstancePatchStateFilter Core.Text
instancePatchStateFilter_key = Lens.lens (\InstancePatchStateFilter' {key} -> key) (\s@InstancePatchStateFilter' {} a -> s {key = a} :: InstancePatchStateFilter)

-- | The value for the filter, must be an integer greater than or equal to 0.
instancePatchStateFilter_values :: Lens.Lens' InstancePatchStateFilter (Core.NonEmpty Core.Text)
instancePatchStateFilter_values = Lens.lens (\InstancePatchStateFilter' {values} -> values) (\s@InstancePatchStateFilter' {} a -> s {values = a} :: InstancePatchStateFilter) Core.. Lens._Coerce

-- | The type of comparison that should be performed for the value: Equal,
-- NotEqual, LessThan or GreaterThan.
instancePatchStateFilter_type :: Lens.Lens' InstancePatchStateFilter InstancePatchStateOperatorType
instancePatchStateFilter_type = Lens.lens (\InstancePatchStateFilter' {type'} -> type') (\s@InstancePatchStateFilter' {} a -> s {type' = a} :: InstancePatchStateFilter)

instance Core.Hashable InstancePatchStateFilter

instance Core.NFData InstancePatchStateFilter

instance Core.ToJSON InstancePatchStateFilter where
  toJSON InstancePatchStateFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            Core.Just ("Values" Core..= values),
            Core.Just ("Type" Core..= type')
          ]
      )
