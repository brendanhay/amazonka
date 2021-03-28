{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.GroupDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.GroupDefinition
  ( GroupDefinition (..)
  -- * Smart constructor
  , mkGroupDefinition
  -- * Lenses
  , gdKey
  , gdType
  ) where

import qualified Network.AWS.CostExplorer.Types.GroupDefinitionType as Types
import qualified Network.AWS.CostExplorer.Types.Key as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a group when you specify a group by criteria or in the response to a query with a specific grouping.
--
-- /See:/ 'mkGroupDefinition' smart constructor.
data GroupDefinition = GroupDefinition'
  { key :: Core.Maybe Types.Key
    -- ^ The string that represents a key for a specified group.
  , type' :: Core.Maybe Types.GroupDefinitionType
    -- ^ The string that represents the type of group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupDefinition' value with any optional fields omitted.
mkGroupDefinition
    :: GroupDefinition
mkGroupDefinition
  = GroupDefinition'{key = Core.Nothing, type' = Core.Nothing}

-- | The string that represents a key for a specified group.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdKey :: Lens.Lens' GroupDefinition (Core.Maybe Types.Key)
gdKey = Lens.field @"key"
{-# INLINEABLE gdKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The string that represents the type of group.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdType :: Lens.Lens' GroupDefinition (Core.Maybe Types.GroupDefinitionType)
gdType = Lens.field @"type'"
{-# INLINEABLE gdType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON GroupDefinition where
        toJSON GroupDefinition{..}
          = Core.object
              (Core.catMaybes
                 [("Key" Core..=) Core.<$> key, ("Type" Core..=) Core.<$> type'])

instance Core.FromJSON GroupDefinition where
        parseJSON
          = Core.withObject "GroupDefinition" Core.$
              \ x ->
                GroupDefinition' Core.<$>
                  (x Core..:? "Key") Core.<*> x Core..:? "Type"
