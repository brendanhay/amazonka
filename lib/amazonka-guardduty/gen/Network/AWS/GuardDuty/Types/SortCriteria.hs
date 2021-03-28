{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.SortCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.SortCriteria
  ( SortCriteria (..)
  -- * Smart constructor
  , mkSortCriteria
  -- * Lenses
  , scAttributeName
  , scOrderBy
  ) where

import qualified Network.AWS.GuardDuty.Types.OrderBy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the criteria used for sorting findings.
--
-- /See:/ 'mkSortCriteria' smart constructor.
data SortCriteria = SortCriteria'
  { attributeName :: Core.Maybe Core.Text
    -- ^ Represents the finding attribute (for example, accountId) to sort findings by.
  , orderBy :: Core.Maybe Types.OrderBy
    -- ^ The order by which the sorted findings are to be displayed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SortCriteria' value with any optional fields omitted.
mkSortCriteria
    :: SortCriteria
mkSortCriteria
  = SortCriteria'{attributeName = Core.Nothing,
                  orderBy = Core.Nothing}

-- | Represents the finding attribute (for example, accountId) to sort findings by.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scAttributeName :: Lens.Lens' SortCriteria (Core.Maybe Core.Text)
scAttributeName = Lens.field @"attributeName"
{-# INLINEABLE scAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

-- | The order by which the sorted findings are to be displayed.
--
-- /Note:/ Consider using 'orderBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scOrderBy :: Lens.Lens' SortCriteria (Core.Maybe Types.OrderBy)
scOrderBy = Lens.field @"orderBy"
{-# INLINEABLE scOrderBy #-}
{-# DEPRECATED orderBy "Use generic-lens or generic-optics with 'orderBy' instead"  #-}

instance Core.FromJSON SortCriteria where
        toJSON SortCriteria{..}
          = Core.object
              (Core.catMaybes
                 [("attributeName" Core..=) Core.<$> attributeName,
                  ("orderBy" Core..=) Core.<$> orderBy])
