{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.GroupedResourceCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.GroupedResourceCount
  ( GroupedResourceCount (..),

    -- * Smart constructor
    mkGroupedResourceCount,

    -- * Lenses
    grcGroupName,
    grcResourceCount,
  )
where

import qualified Network.AWS.Config.Types.GroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The count of resources that are grouped by the group name.
--
-- /See:/ 'mkGroupedResourceCount' smart constructor.
data GroupedResourceCount = GroupedResourceCount'
  { -- | The name of the group that can be region, account ID, or resource type. For example, region1, region2 if the region was chosen as @GroupByKey@ .
    groupName :: Types.GroupName,
    -- | The number of resources in the group.
    resourceCount :: Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupedResourceCount' value with any optional fields omitted.
mkGroupedResourceCount ::
  -- | 'groupName'
  Types.GroupName ->
  -- | 'resourceCount'
  Core.Integer ->
  GroupedResourceCount
mkGroupedResourceCount groupName resourceCount =
  GroupedResourceCount' {groupName, resourceCount}

-- | The name of the group that can be region, account ID, or resource type. For example, region1, region2 if the region was chosen as @GroupByKey@ .
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcGroupName :: Lens.Lens' GroupedResourceCount Types.GroupName
grcGroupName = Lens.field @"groupName"
{-# DEPRECATED grcGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The number of resources in the group.
--
-- /Note:/ Consider using 'resourceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcResourceCount :: Lens.Lens' GroupedResourceCount Core.Integer
grcResourceCount = Lens.field @"resourceCount"
{-# DEPRECATED grcResourceCount "Use generic-lens or generic-optics with 'resourceCount' instead." #-}

instance Core.FromJSON GroupedResourceCount where
  parseJSON =
    Core.withObject "GroupedResourceCount" Core.$
      \x ->
        GroupedResourceCount'
          Core.<$> (x Core..: "GroupName") Core.<*> (x Core..: "ResourceCount")
