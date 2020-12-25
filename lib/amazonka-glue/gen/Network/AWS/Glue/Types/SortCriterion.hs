{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SortCriterion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SortCriterion
  ( SortCriterion (..),

    -- * Smart constructor
    mkSortCriterion,

    -- * Lenses
    scFieldName,
    scSort,
  )
where

import qualified Network.AWS.Glue.Types.Sort as Types
import qualified Network.AWS.Glue.Types.ValueString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a field to sort by and a sort order.
--
-- /See:/ 'mkSortCriterion' smart constructor.
data SortCriterion = SortCriterion'
  { -- | The name of the field on which to sort.
    fieldName :: Core.Maybe Types.ValueString,
    -- | An ascending or descending sort.
    sort :: Core.Maybe Types.Sort
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SortCriterion' value with any optional fields omitted.
mkSortCriterion ::
  SortCriterion
mkSortCriterion =
  SortCriterion' {fieldName = Core.Nothing, sort = Core.Nothing}

-- | The name of the field on which to sort.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scFieldName :: Lens.Lens' SortCriterion (Core.Maybe Types.ValueString)
scFieldName = Lens.field @"fieldName"
{-# DEPRECATED scFieldName "Use generic-lens or generic-optics with 'fieldName' instead." #-}

-- | An ascending or descending sort.
--
-- /Note:/ Consider using 'sort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSort :: Lens.Lens' SortCriterion (Core.Maybe Types.Sort)
scSort = Lens.field @"sort"
{-# DEPRECATED scSort "Use generic-lens or generic-optics with 'sort' instead." #-}

instance Core.FromJSON SortCriterion where
  toJSON SortCriterion {..} =
    Core.object
      ( Core.catMaybes
          [ ("FieldName" Core..=) Core.<$> fieldName,
            ("Sort" Core..=) Core.<$> sort
          ]
      )
