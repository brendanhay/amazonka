{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceInformationFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceInformationFilter
  ( InstanceInformationFilter (..),

    -- * Smart constructor
    mkInstanceInformationFilter,

    -- * Lenses
    iifKey,
    iifValueSet,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.InstanceInformationFilterKey as Types
import qualified Network.AWS.SSM.Types.InstanceInformationFilterValue as Types

-- | Describes a filter for a specific list of instances. You can filter instances information by using tags. You specify tags by using a key-value mapping.
--
-- Use this action instead of the 'DescribeInstanceInformationRequest$InstanceInformationFilterList' method. The @InstanceInformationFilterList@ method is a legacy method and does not support tags.
--
-- /See:/ 'mkInstanceInformationFilter' smart constructor.
data InstanceInformationFilter = InstanceInformationFilter'
  { -- | The name of the filter.
    key :: Types.InstanceInformationFilterKey,
    -- | The filter values.
    valueSet :: Core.NonEmpty Types.InstanceInformationFilterValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceInformationFilter' value with any optional fields omitted.
mkInstanceInformationFilter ::
  -- | 'key'
  Types.InstanceInformationFilterKey ->
  -- | 'valueSet'
  Core.NonEmpty Types.InstanceInformationFilterValue ->
  InstanceInformationFilter
mkInstanceInformationFilter key valueSet =
  InstanceInformationFilter' {key, valueSet}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifKey :: Lens.Lens' InstanceInformationFilter Types.InstanceInformationFilterKey
iifKey = Lens.field @"key"
{-# DEPRECATED iifKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The filter values.
--
-- /Note:/ Consider using 'valueSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifValueSet :: Lens.Lens' InstanceInformationFilter (Core.NonEmpty Types.InstanceInformationFilterValue)
iifValueSet = Lens.field @"valueSet"
{-# DEPRECATED iifValueSet "Use generic-lens or generic-optics with 'valueSet' instead." #-}

instance Core.FromJSON InstanceInformationFilter where
  toJSON InstanceInformationFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("key" Core..= key),
            Core.Just ("valueSet" Core..= valueSet)
          ]
      )
