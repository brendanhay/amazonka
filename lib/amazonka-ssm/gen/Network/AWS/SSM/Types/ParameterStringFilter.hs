{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterStringFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterStringFilter
  ( ParameterStringFilter (..),

    -- * Smart constructor
    mkParameterStringFilter,

    -- * Lenses
    psfKey,
    psfOption,
    psfValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.Key as Types
import qualified Network.AWS.SSM.Types.Option as Types
import qualified Network.AWS.SSM.Types.ParameterStringFilterValue as Types

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- /See:/ 'mkParameterStringFilter' smart constructor.
data ParameterStringFilter = ParameterStringFilter'
  { -- | The name of the filter.
    key :: Types.Key,
    -- | For all filters used with 'DescribeParameters' , valid options include @Equals@ and @BeginsWith@ . The @Name@ filter additionally supports the @Contains@ option. (Exception: For filters using the key @Path@ , valid options include @Recursive@ and @OneLevel@ .)
    --
    -- For filters used with 'GetParametersByPath' , valid options include @Equals@ and @BeginsWith@ . (Exception: For filters using @Label@ as the Key name, the only valid option is @Equals@ .)
    option :: Core.Maybe Types.Option,
    -- | The value you want to search for.
    values :: Core.Maybe (Core.NonEmpty Types.ParameterStringFilterValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterStringFilter' value with any optional fields omitted.
mkParameterStringFilter ::
  -- | 'key'
  Types.Key ->
  ParameterStringFilter
mkParameterStringFilter key =
  ParameterStringFilter'
    { key,
      option = Core.Nothing,
      values = Core.Nothing
    }

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfKey :: Lens.Lens' ParameterStringFilter Types.Key
psfKey = Lens.field @"key"
{-# DEPRECATED psfKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | For all filters used with 'DescribeParameters' , valid options include @Equals@ and @BeginsWith@ . The @Name@ filter additionally supports the @Contains@ option. (Exception: For filters using the key @Path@ , valid options include @Recursive@ and @OneLevel@ .)
--
-- For filters used with 'GetParametersByPath' , valid options include @Equals@ and @BeginsWith@ . (Exception: For filters using @Label@ as the Key name, the only valid option is @Equals@ .)
--
-- /Note:/ Consider using 'option' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfOption :: Lens.Lens' ParameterStringFilter (Core.Maybe Types.Option)
psfOption = Lens.field @"option"
{-# DEPRECATED psfOption "Use generic-lens or generic-optics with 'option' instead." #-}

-- | The value you want to search for.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfValues :: Lens.Lens' ParameterStringFilter (Core.Maybe (Core.NonEmpty Types.ParameterStringFilterValue))
psfValues = Lens.field @"values"
{-# DEPRECATED psfValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON ParameterStringFilter where
  toJSON ParameterStringFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            ("Option" Core..=) Core.<$> option,
            ("Values" Core..=) Core.<$> values
          ]
      )
