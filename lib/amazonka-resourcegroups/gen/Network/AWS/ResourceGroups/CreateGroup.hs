{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cgResourceQuery,
    cgConfiguration,
    cgDescription,
    cgTags,
    cgName,

    -- * Destructuring the response
    CreateGroupResponse (..),
    mkCreateGroupResponse,

    -- ** Response lenses
    cgrsGroup,
    cgrsGroupConfiguration,
    cgrsResourceQuery,
    cgrsTags,
    cgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { resourceQuery ::
      Lude.Maybe ResourceQuery,
    configuration :: Lude.Maybe [GroupConfigurationItem],
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGroup' with the minimum fields required to make a request.
--
-- * 'configuration' - A configuration associates the resource group with an AWS service and specifies how the service can interact with the resources in the group. A configuration is an array of 'GroupConfigurationItem' elements.
-- * 'description' - The description of the resource group. Descriptions can consist of letters, numbers, hyphens, underscores, periods, and spaces.
-- * 'name' - The name of the group, which is the identifier of the group in other operations. You can't change the name of a resource group after you create it. A resource group name can consist of letters, numbers, hyphens, periods, and underscores. The name cannot start with @AWS@ or @aws@ ; these are reserved. A resource group name must be unique within each AWS Region in your AWS account.
-- * 'resourceQuery' - The resource query that determines which AWS resources are members of this group.
-- * 'tags' - The tags to add to the group. A tag is key-value pair string.
mkCreateGroup ::
  -- | 'name'
  Lude.Text ->
  CreateGroup
mkCreateGroup pName_ =
  CreateGroup'
    { resourceQuery = Lude.Nothing,
      configuration = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_
    }

-- | The resource query that determines which AWS resources are members of this group.
--
-- /Note:/ Consider using 'resourceQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgResourceQuery :: Lens.Lens' CreateGroup (Lude.Maybe ResourceQuery)
cgResourceQuery = Lens.lens (resourceQuery :: CreateGroup -> Lude.Maybe ResourceQuery) (\s a -> s {resourceQuery = a} :: CreateGroup)
{-# DEPRECATED cgResourceQuery "Use generic-lens or generic-optics with 'resourceQuery' instead." #-}

-- | A configuration associates the resource group with an AWS service and specifies how the service can interact with the resources in the group. A configuration is an array of 'GroupConfigurationItem' elements.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgConfiguration :: Lens.Lens' CreateGroup (Lude.Maybe [GroupConfigurationItem])
cgConfiguration = Lens.lens (configuration :: CreateGroup -> Lude.Maybe [GroupConfigurationItem]) (\s a -> s {configuration = a} :: CreateGroup)
{-# DEPRECATED cgConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The description of the resource group. Descriptions can consist of letters, numbers, hyphens, underscores, periods, and spaces.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgDescription :: Lens.Lens' CreateGroup (Lude.Maybe Lude.Text)
cgDescription = Lens.lens (description :: CreateGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateGroup)
{-# DEPRECATED cgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags to add to the group. A tag is key-value pair string.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgTags :: Lens.Lens' CreateGroup (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cgTags = Lens.lens (tags :: CreateGroup -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateGroup)
{-# DEPRECATED cgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the group, which is the identifier of the group in other operations. You can't change the name of a resource group after you create it. A resource group name can consist of letters, numbers, hyphens, periods, and underscores. The name cannot start with @AWS@ or @aws@ ; these are reserved. A resource group name must be unique within each AWS Region in your AWS account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgName :: Lens.Lens' CreateGroup Lude.Text
cgName = Lens.lens (name :: CreateGroup -> Lude.Text) (\s a -> s {name = a} :: CreateGroup)
{-# DEPRECATED cgName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateGroup where
  type Rs CreateGroup = CreateGroupResponse
  request = Req.postJSON resourceGroupsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Lude.<$> (x Lude..?> "Group")
            Lude.<*> (x Lude..?> "GroupConfiguration")
            Lude.<*> (x Lude..?> "ResourceQuery")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceQuery" Lude..=) Lude.<$> resourceQuery,
            ("Configuration" Lude..=) Lude.<$> configuration,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateGroup where
  toPath = Lude.const "/groups"

instance Lude.ToQuery CreateGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { group ::
      Lude.Maybe Group,
    groupConfiguration :: Lude.Maybe GroupConfiguration,
    resourceQuery :: Lude.Maybe ResourceQuery,
    tags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGroupResponse' with the minimum fields required to make a request.
--
-- * 'group' - The description of the resource group.
-- * 'groupConfiguration' - The service configuration associated with the resource group. AWS Resource Groups supports adding service configurations for the following resource group types:
--
--
--     * @AWS::EC2::CapacityReservationPool@ - Amazon EC2 capacity reservation pools. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
--
--
-- * 'resourceQuery' - The resource query associated with the group.
-- * 'responseStatus' - The response status code.
-- * 'tags' - The tags associated with the group.
mkCreateGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGroupResponse
mkCreateGroupResponse pResponseStatus_ =
  CreateGroupResponse'
    { group = Lude.Nothing,
      groupConfiguration = Lude.Nothing,
      resourceQuery = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The description of the resource group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsGroup :: Lens.Lens' CreateGroupResponse (Lude.Maybe Group)
cgrsGroup = Lens.lens (group :: CreateGroupResponse -> Lude.Maybe Group) (\s a -> s {group = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The service configuration associated with the resource group. AWS Resource Groups supports adding service configurations for the following resource group types:
--
--
--     * @AWS::EC2::CapacityReservationPool@ - Amazon EC2 capacity reservation pools. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
--
--
--
-- /Note:/ Consider using 'groupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsGroupConfiguration :: Lens.Lens' CreateGroupResponse (Lude.Maybe GroupConfiguration)
cgrsGroupConfiguration = Lens.lens (groupConfiguration :: CreateGroupResponse -> Lude.Maybe GroupConfiguration) (\s a -> s {groupConfiguration = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsGroupConfiguration "Use generic-lens or generic-optics with 'groupConfiguration' instead." #-}

-- | The resource query associated with the group.
--
-- /Note:/ Consider using 'resourceQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsResourceQuery :: Lens.Lens' CreateGroupResponse (Lude.Maybe ResourceQuery)
cgrsResourceQuery = Lens.lens (resourceQuery :: CreateGroupResponse -> Lude.Maybe ResourceQuery) (\s a -> s {resourceQuery = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsResourceQuery "Use generic-lens or generic-optics with 'resourceQuery' instead." #-}

-- | The tags associated with the group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsTags :: Lens.Lens' CreateGroupResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cgrsTags = Lens.lens (tags :: CreateGroupResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsResponseStatus :: Lens.Lens' CreateGroupResponse Lude.Int
cgrsResponseStatus = Lens.lens (responseStatus :: CreateGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
