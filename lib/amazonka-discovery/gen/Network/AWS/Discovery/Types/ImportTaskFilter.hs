{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ImportTaskFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ImportTaskFilter
  ( ImportTaskFilter (..),

    -- * Smart constructor
    mkImportTaskFilter,

    -- * Lenses
    itfName,
    itfValues,
  )
where

import qualified Network.AWS.Discovery.Types.ImportTaskFilterName as Types
import qualified Network.AWS.Discovery.Types.ImportTaskFilterValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A name-values pair of elements you can use to filter the results when querying your import tasks. Currently, wildcards are not supported for filters.
--
-- /See:/ 'mkImportTaskFilter' smart constructor.
data ImportTaskFilter = ImportTaskFilter'
  { -- | The name, status, or import task ID for a specific import task.
    name :: Core.Maybe Types.ImportTaskFilterName,
    -- | An array of strings that you can provide to match against a specific name, status, or import task ID to filter the results for your import task queries.
    values :: Core.Maybe (Core.NonEmpty Types.ImportTaskFilterValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportTaskFilter' value with any optional fields omitted.
mkImportTaskFilter ::
  ImportTaskFilter
mkImportTaskFilter =
  ImportTaskFilter' {name = Core.Nothing, values = Core.Nothing}

-- | The name, status, or import task ID for a specific import task.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itfName :: Lens.Lens' ImportTaskFilter (Core.Maybe Types.ImportTaskFilterName)
itfName = Lens.field @"name"
{-# DEPRECATED itfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An array of strings that you can provide to match against a specific name, status, or import task ID to filter the results for your import task queries.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itfValues :: Lens.Lens' ImportTaskFilter (Core.Maybe (Core.NonEmpty Types.ImportTaskFilterValue))
itfValues = Lens.field @"values"
{-# DEPRECATED itfValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON ImportTaskFilter where
  toJSON ImportTaskFilter {..} =
    Core.object
      ( Core.catMaybes
          [ ("name" Core..=) Core.<$> name,
            ("values" Core..=) Core.<$> values
          ]
      )
