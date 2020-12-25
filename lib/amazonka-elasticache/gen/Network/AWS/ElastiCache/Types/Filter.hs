{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Filter
  ( Filter (..),

    -- * Smart constructor
    mkFilter,

    -- * Lenses
    fName,
    fValues,
  )
where

import qualified Network.AWS.ElastiCache.Types.FilterName as Types
import qualified Network.AWS.ElastiCache.Types.FilterValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used to streamline results of a search based on the property being filtered.
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { -- | The property being filtered. For example, UserId.
    name :: Types.FilterName,
    -- | The property values to filter on. For example, "user-123".
    values :: Core.NonEmpty Types.FilterValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Filter' value with any optional fields omitted.
mkFilter ::
  -- | 'name'
  Types.FilterName ->
  -- | 'values'
  Core.NonEmpty Types.FilterValue ->
  Filter
mkFilter name values = Filter' {name, values}

-- | The property being filtered. For example, UserId.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fName :: Lens.Lens' Filter Types.FilterName
fName = Lens.field @"name"
{-# DEPRECATED fName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The property values to filter on. For example, "user-123".
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValues :: Lens.Lens' Filter (Core.NonEmpty Types.FilterValue)
fValues = Lens.field @"values"
{-# DEPRECATED fValues "Use generic-lens or generic-optics with 'values' instead." #-}
