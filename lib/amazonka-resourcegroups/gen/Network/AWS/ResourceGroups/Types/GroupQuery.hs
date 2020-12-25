{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupQuery
  ( GroupQuery (..),

    -- * Smart constructor
    mkGroupQuery,

    -- * Lenses
    gqGroupName,
    gqResourceQuery,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ResourceGroups.Types.GroupName as Types
import qualified Network.AWS.ResourceGroups.Types.ResourceQuery as Types

-- | A mapping of a query attached to a resource group that determines the AWS resources that are members of the group.
--
-- /See:/ 'mkGroupQuery' smart constructor.
data GroupQuery = GroupQuery'
  { -- | The name of the resource group that is associated with the specified resource query.
    groupName :: Types.GroupName,
    -- | The resource query that determines which AWS resources are members of the associated resource group.
    resourceQuery :: Types.ResourceQuery
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupQuery' value with any optional fields omitted.
mkGroupQuery ::
  -- | 'groupName'
  Types.GroupName ->
  -- | 'resourceQuery'
  Types.ResourceQuery ->
  GroupQuery
mkGroupQuery groupName resourceQuery =
  GroupQuery' {groupName, resourceQuery}

-- | The name of the resource group that is associated with the specified resource query.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqGroupName :: Lens.Lens' GroupQuery Types.GroupName
gqGroupName = Lens.field @"groupName"
{-# DEPRECATED gqGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The resource query that determines which AWS resources are members of the associated resource group.
--
-- /Note:/ Consider using 'resourceQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqResourceQuery :: Lens.Lens' GroupQuery Types.ResourceQuery
gqResourceQuery = Lens.field @"resourceQuery"
{-# DEPRECATED gqResourceQuery "Use generic-lens or generic-optics with 'resourceQuery' instead." #-}

instance Core.FromJSON GroupQuery where
  parseJSON =
    Core.withObject "GroupQuery" Core.$
      \x ->
        GroupQuery'
          Core.<$> (x Core..: "GroupName") Core.<*> (x Core..: "ResourceQuery")
