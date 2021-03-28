{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.HierarchyGroupSummary
  ( HierarchyGroupSummary (..)
  -- * Smart constructor
  , mkHierarchyGroupSummary
  -- * Lenses
  , hgsArn
  , hgsId
  , hgsName
  ) where

import qualified Network.AWS.Connect.Types.Arn as Types
import qualified Network.AWS.Connect.Types.Id as Types
import qualified Network.AWS.Connect.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains summary information about a hierarchy group.
--
-- /See:/ 'mkHierarchyGroupSummary' smart constructor.
data HierarchyGroupSummary = HierarchyGroupSummary'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the hierarchy group.
  , id :: Core.Maybe Types.Id
    -- ^ The identifier of the hierarchy group.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the hierarchy group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HierarchyGroupSummary' value with any optional fields omitted.
mkHierarchyGroupSummary
    :: HierarchyGroupSummary
mkHierarchyGroupSummary
  = HierarchyGroupSummary'{arn = Core.Nothing, id = Core.Nothing,
                           name = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the hierarchy group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsArn :: Lens.Lens' HierarchyGroupSummary (Core.Maybe Types.Arn)
hgsArn = Lens.field @"arn"
{-# INLINEABLE hgsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The identifier of the hierarchy group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsId :: Lens.Lens' HierarchyGroupSummary (Core.Maybe Types.Id)
hgsId = Lens.field @"id"
{-# INLINEABLE hgsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the hierarchy group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsName :: Lens.Lens' HierarchyGroupSummary (Core.Maybe Types.Name)
hgsName = Lens.field @"name"
{-# INLINEABLE hgsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON HierarchyGroupSummary where
        parseJSON
          = Core.withObject "HierarchyGroupSummary" Core.$
              \ x ->
                HierarchyGroupSummary' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Id" Core.<*>
                    x Core..:? "Name"
