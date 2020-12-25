{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyLevelUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyLevelUpdate
  ( HierarchyLevelUpdate (..),

    -- * Smart constructor
    mkHierarchyLevelUpdate,

    -- * Lenses
    hluName,
  )
where

import qualified Network.AWS.Connect.Types.HierarchyLevelName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the hierarchy level to update.
--
-- /See:/ 'mkHierarchyLevelUpdate' smart constructor.
newtype HierarchyLevelUpdate = HierarchyLevelUpdate'
  { -- | The name of the user hierarchy level. Must not be more than 50 characters.
    name :: Types.HierarchyLevelName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HierarchyLevelUpdate' value with any optional fields omitted.
mkHierarchyLevelUpdate ::
  -- | 'name'
  Types.HierarchyLevelName ->
  HierarchyLevelUpdate
mkHierarchyLevelUpdate name = HierarchyLevelUpdate' {name}

-- | The name of the user hierarchy level. Must not be more than 50 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hluName :: Lens.Lens' HierarchyLevelUpdate Types.HierarchyLevelName
hluName = Lens.field @"name"
{-# DEPRECATED hluName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON HierarchyLevelUpdate where
  toJSON HierarchyLevelUpdate {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])
