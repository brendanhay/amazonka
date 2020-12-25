{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledActionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledActionFilter
  ( ScheduledActionFilter (..),

    -- * Smart constructor
    mkScheduledActionFilter,

    -- * Lenses
    safName,
    safValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.ScheduledActionFilterName as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | A set of elements to filter the returned scheduled actions.
--
-- /See:/ 'mkScheduledActionFilter' smart constructor.
data ScheduledActionFilter = ScheduledActionFilter'
  { -- | The type of element to filter.
    name :: Types.ScheduledActionFilterName,
    -- | List of values. Compare if the value (of type defined by @Name@ ) equals an item in the list of scheduled actions.
    values :: [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledActionFilter' value with any optional fields omitted.
mkScheduledActionFilter ::
  -- | 'name'
  Types.ScheduledActionFilterName ->
  ScheduledActionFilter
mkScheduledActionFilter name =
  ScheduledActionFilter' {name, values = Core.mempty}

-- | The type of element to filter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
safName :: Lens.Lens' ScheduledActionFilter Types.ScheduledActionFilterName
safName = Lens.field @"name"
{-# DEPRECATED safName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | List of values. Compare if the value (of type defined by @Name@ ) equals an item in the list of scheduled actions.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
safValues :: Lens.Lens' ScheduledActionFilter [Types.String]
safValues = Lens.field @"values"
{-# DEPRECATED safValues "Use generic-lens or generic-optics with 'values' instead." #-}
