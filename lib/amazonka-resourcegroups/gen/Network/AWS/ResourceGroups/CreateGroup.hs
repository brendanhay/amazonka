{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.CreateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource group with the specified name and description. You can optionally include a resource query, or a service configuration.
module Network.AWS.ResourceGroups.CreateGroup
  ( -- * Creating a request
    CreateGroup (..),
    mkCreateGroup,

    -- ** Request lenses
    cgName,
    cgConfiguration,
    cgDescription,
    cgResourceQuery,
    cgTags,

    -- * Destructuring the response
    CreateGroupResponse (..),
    mkCreateGroupResponse,

    -- ** Response lenses
    cgrrsGroup,
    cgrrsGroupConfiguration,
    cgrrsResourceQuery,
    cgrrsTags,
    cgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { -- | The name of the group, which is the identifier of the group in other operations. You can't change the name of a resource group after you create it. A resource group name can consist of letters, numbers, hyphens, periods, and underscores. The name cannot start with @AWS@ or @aws@ ; these are reserved. A resource group name must be unique within each AWS Region in your AWS account.
    name :: Types.Name,
    -- | A configuration associates the resource group with an AWS service and specifies how the service can interact with the resources in the group. A configuration is an array of 'GroupConfigurationItem' elements.
    configuration :: Core.Maybe [Types.GroupConfigurationItem],
    -- | The description of the resource group. Descriptions can consist of letters, numbers, hyphens, underscores, periods, and spaces.
    description :: Core.Maybe Types.Description,
    -- | The resource query that determines which AWS resources are members of this group.
    resourceQuery :: Core.Maybe Types.ResourceQuery,
    -- | The tags to add to the group. A tag is key-value pair string.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGroup' value with any optional fields omitted.
mkCreateGroup ::
  -- | 'name'
  Types.Name ->
  CreateGroup
mkCreateGroup name =
  CreateGroup'
    { name,
      configuration = Core.Nothing,
      description = Core.Nothing,
      resourceQuery = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the group, which is the identifier of the group in other operations. You can't change the name of a resource group after you create it. A resource group name can consist of letters, numbers, hyphens, periods, and underscores. The name cannot start with @AWS@ or @aws@ ; these are reserved. A resource group name must be unique within each AWS Region in your AWS account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgName :: Lens.Lens' CreateGroup Types.Name
cgName = Lens.field @"name"
{-# DEPRECATED cgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A configuration associates the resource group with an AWS service and specifies how the service can interact with the resources in the group. A configuration is an array of 'GroupConfigurationItem' elements.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgConfiguration :: Lens.Lens' CreateGroup (Core.Maybe [Types.GroupConfigurationItem])
cgConfiguration = Lens.field @"configuration"
{-# DEPRECATED cgConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The description of the resource group. Descriptions can consist of letters, numbers, hyphens, underscores, periods, and spaces.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgDescription :: Lens.Lens' CreateGroup (Core.Maybe Types.Description)
cgDescription = Lens.field @"description"
{-# DEPRECATED cgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The resource query that determines which AWS resources are members of this group.
--
-- /Note:/ Consider using 'resourceQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgResourceQuery :: Lens.Lens' CreateGroup (Core.Maybe Types.ResourceQuery)
cgResourceQuery = Lens.field @"resourceQuery"
{-# DEPRECATED cgResourceQuery "Use generic-lens or generic-optics with 'resourceQuery' instead." #-}

-- | The tags to add to the group. A tag is key-value pair string.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgTags :: Lens.Lens' CreateGroup (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cgTags = Lens.field @"tags"
{-# DEPRECATED cgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateGroup where
  toJSON CreateGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("Configuration" Core..=) Core.<$> configuration,
            ("Description" Core..=) Core.<$> description,
            ("ResourceQuery" Core..=) Core.<$> resourceQuery,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateGroup where
  type Rs CreateGroup = CreateGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/groups",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Core.<$> (x Core..:? "Group")
            Core.<*> (x Core..:? "GroupConfiguration")
            Core.<*> (x Core..:? "ResourceQuery")
            Core.<*> (x Core..:? "Tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { -- | The description of the resource group.
    group :: Core.Maybe Types.Group,
    -- | The service configuration associated with the resource group. AWS Resource Groups supports adding service configurations for the following resource group types:
    --
    --
    --     * @AWS::EC2::CapacityReservationPool@ - Amazon EC2 capacity reservation pools. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
    groupConfiguration :: Core.Maybe Types.GroupConfiguration,
    -- | The resource query associated with the group.
    resourceQuery :: Core.Maybe Types.ResourceQuery,
    -- | The tags associated with the group.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGroupResponse' value with any optional fields omitted.
mkCreateGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateGroupResponse
mkCreateGroupResponse responseStatus =
  CreateGroupResponse'
    { group = Core.Nothing,
      groupConfiguration = Core.Nothing,
      resourceQuery = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | The description of the resource group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrrsGroup :: Lens.Lens' CreateGroupResponse (Core.Maybe Types.Group)
cgrrsGroup = Lens.field @"group"
{-# DEPRECATED cgrrsGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The service configuration associated with the resource group. AWS Resource Groups supports adding service configurations for the following resource group types:
--
--
--     * @AWS::EC2::CapacityReservationPool@ - Amazon EC2 capacity reservation pools. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
--
--
--
-- /Note:/ Consider using 'groupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrrsGroupConfiguration :: Lens.Lens' CreateGroupResponse (Core.Maybe Types.GroupConfiguration)
cgrrsGroupConfiguration = Lens.field @"groupConfiguration"
{-# DEPRECATED cgrrsGroupConfiguration "Use generic-lens or generic-optics with 'groupConfiguration' instead." #-}

-- | The resource query associated with the group.
--
-- /Note:/ Consider using 'resourceQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrrsResourceQuery :: Lens.Lens' CreateGroupResponse (Core.Maybe Types.ResourceQuery)
cgrrsResourceQuery = Lens.field @"resourceQuery"
{-# DEPRECATED cgrrsResourceQuery "Use generic-lens or generic-optics with 'resourceQuery' instead." #-}

-- | The tags associated with the group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrrsTags :: Lens.Lens' CreateGroupResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cgrrsTags = Lens.field @"tags"
{-# DEPRECATED cgrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrrsResponseStatus :: Lens.Lens' CreateGroupResponse Core.Int
cgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
