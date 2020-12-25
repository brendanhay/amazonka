{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.Group
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.Group
  ( Group (..),

    -- * Smart constructor
    mkGroup,

    -- * Lenses
    gGroupArn,
    gName,
    gDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ResourceGroups.Types.Description as Types
import qualified Network.AWS.ResourceGroups.Types.GroupArn as Types
import qualified Network.AWS.ResourceGroups.Types.Name as Types

-- | A resource group that contains AWS resources. You can assign resources to the group by associating either of the following elements with the group:
--
--
--     * 'ResourceQuery' - Use a resource query to specify a set of tag keys and values. All resources in the same AWS Region and AWS account that have those keys with the same values are included in the group. You can add a resource query when you create the group.
--
--
--     * 'GroupConfiguration' - Use a service configuration to associate the group with an AWS service. The configuration specifies which resource types can be included in the group.
--
--
--
-- /See:/ 'mkGroup' smart constructor.
data Group = Group'
  { -- | The ARN of the resource group.
    groupArn :: Types.GroupArn,
    -- | The name of the resource group.
    name :: Types.Name,
    -- | The description of the resource group.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Group' value with any optional fields omitted.
mkGroup ::
  -- | 'groupArn'
  Types.GroupArn ->
  -- | 'name'
  Types.Name ->
  Group
mkGroup groupArn name =
  Group' {groupArn, name, description = Core.Nothing}

-- | The ARN of the resource group.
--
-- /Note:/ Consider using 'groupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gGroupArn :: Lens.Lens' Group Types.GroupArn
gGroupArn = Lens.field @"groupArn"
{-# DEPRECATED gGroupArn "Use generic-lens or generic-optics with 'groupArn' instead." #-}

-- | The name of the resource group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gName :: Lens.Lens' Group Types.Name
gName = Lens.field @"name"
{-# DEPRECATED gName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the resource group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gDescription :: Lens.Lens' Group (Core.Maybe Types.Description)
gDescription = Lens.field @"description"
{-# DEPRECATED gDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON Group where
  parseJSON =
    Core.withObject "Group" Core.$
      \x ->
        Group'
          Core.<$> (x Core..: "GroupArn")
          Core.<*> (x Core..: "Name")
          Core.<*> (x Core..:? "Description")
