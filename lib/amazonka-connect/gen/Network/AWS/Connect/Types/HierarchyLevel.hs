{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.HierarchyLevel
  ( HierarchyLevel (..)
  -- * Smart constructor
  , mkHierarchyLevel
  -- * Lenses
  , hlArn
  , hlId
  , hlName
  ) where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Connect.Types.HierarchyLevelName as Types
import qualified Network.AWS.Connect.Types.Id as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a hierarchy level.
--
-- /See:/ 'mkHierarchyLevel' smart constructor.
data HierarchyLevel = HierarchyLevel'
  { arn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the hierarchy level.
  , id :: Core.Maybe Types.Id
    -- ^ The identifier of the hierarchy level.
  , name :: Core.Maybe Types.HierarchyLevelName
    -- ^ The name of the hierarchy level.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HierarchyLevel' value with any optional fields omitted.
mkHierarchyLevel
    :: HierarchyLevel
mkHierarchyLevel
  = HierarchyLevel'{arn = Core.Nothing, id = Core.Nothing,
                    name = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the hierarchy level.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlArn :: Lens.Lens' HierarchyLevel (Core.Maybe Types.ARN)
hlArn = Lens.field @"arn"
{-# INLINEABLE hlArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The identifier of the hierarchy level.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlId :: Lens.Lens' HierarchyLevel (Core.Maybe Types.Id)
hlId = Lens.field @"id"
{-# INLINEABLE hlId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the hierarchy level.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlName :: Lens.Lens' HierarchyLevel (Core.Maybe Types.HierarchyLevelName)
hlName = Lens.field @"name"
{-# INLINEABLE hlName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON HierarchyLevel where
        parseJSON
          = Core.withObject "HierarchyLevel" Core.$
              \ x ->
                HierarchyLevel' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Id" Core.<*>
                    x Core..:? "Name"
