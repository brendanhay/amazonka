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
-- Module      : Network.AWS.SSM.Types.InstanceInformationFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceInformationFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.InstanceInformationFilterKey

-- | Describes a filter for a specific list of instances. You can filter
-- instances information by using tags. You specify tags by using a
-- key-value mapping.
--
-- Use this action instead of the
-- DescribeInstanceInformationRequest$InstanceInformationFilterList method.
-- The @InstanceInformationFilterList@ method is a legacy method and does
-- not support tags.
--
-- /See:/ 'newInstanceInformationFilter' smart constructor.
data InstanceInformationFilter = InstanceInformationFilter'
  { -- | The name of the filter.
    key :: InstanceInformationFilterKey,
    -- | The filter values.
    valueSet :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceInformationFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'instanceInformationFilter_key' - The name of the filter.
--
-- 'valueSet', 'instanceInformationFilter_valueSet' - The filter values.
newInstanceInformationFilter ::
  -- | 'key'
  InstanceInformationFilterKey ->
  -- | 'valueSet'
  Core.NonEmpty Core.Text ->
  InstanceInformationFilter
newInstanceInformationFilter pKey_ pValueSet_ =
  InstanceInformationFilter'
    { key = pKey_,
      valueSet = Lens._Coerce Lens.# pValueSet_
    }

-- | The name of the filter.
instanceInformationFilter_key :: Lens.Lens' InstanceInformationFilter InstanceInformationFilterKey
instanceInformationFilter_key = Lens.lens (\InstanceInformationFilter' {key} -> key) (\s@InstanceInformationFilter' {} a -> s {key = a} :: InstanceInformationFilter)

-- | The filter values.
instanceInformationFilter_valueSet :: Lens.Lens' InstanceInformationFilter (Core.NonEmpty Core.Text)
instanceInformationFilter_valueSet = Lens.lens (\InstanceInformationFilter' {valueSet} -> valueSet) (\s@InstanceInformationFilter' {} a -> s {valueSet = a} :: InstanceInformationFilter) Core.. Lens._Coerce

instance Core.Hashable InstanceInformationFilter

instance Core.NFData InstanceInformationFilter

instance Core.ToJSON InstanceInformationFilter where
  toJSON InstanceInformationFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("key" Core..= key),
            Core.Just ("valueSet" Core..= valueSet)
          ]
      )
