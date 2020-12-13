{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.CreateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group resource with a name and a filter expression.
module Network.AWS.XRay.CreateGroup
  ( -- * Creating a request
    CreateGroup (..),
    mkCreateGroup,

    -- ** Request lenses
    cgFilterExpression,
    cgInsightsConfiguration,
    cgGroupName,
    cgTags,

    -- * Destructuring the response
    CreateGroupResponse (..),
    mkCreateGroupResponse,

    -- ** Response lenses
    cgrsGroup,
    cgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { -- | The filter expression defining criteria by which to group traces.
    filterExpression :: Lude.Maybe Lude.Text,
    -- | The structure containing configurations related to insights.
    --
    --
    --     * The InsightsEnabled boolean can be set to true to enable insights for the new group or false to disable insights for the new group.
    --
    --
    --     * The NotifcationsEnabled boolean can be set to true to enable insights notifications for the new group. Notifications may only be enabled on a group with InsightsEnabled set to true.
    insightsConfiguration :: Lude.Maybe InsightsConfiguration,
    -- | The case-sensitive name of the new group. Default is a reserved name and names must be unique.
    groupName :: Lude.Text,
    -- | A map that contains one or more tag keys and tag values to attach to an X-Ray group. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ .
    --
    -- The following restrictions apply to tags:
    --
    --     * Maximum number of user-applied tags per resource: 50
    --
    --
    --     * Maximum tag key length: 128 Unicode characters
    --
    --
    --     * Maximum tag value length: 256 Unicode characters
    --
    --
    --     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @
    --
    --
    --     * Tag keys and values are case sensitive.
    --
    --
    --     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGroup' with the minimum fields required to make a request.
--
-- * 'filterExpression' - The filter expression defining criteria by which to group traces.
-- * 'insightsConfiguration' - The structure containing configurations related to insights.
--
--
--     * The InsightsEnabled boolean can be set to true to enable insights for the new group or false to disable insights for the new group.
--
--
--     * The NotifcationsEnabled boolean can be set to true to enable insights notifications for the new group. Notifications may only be enabled on a group with InsightsEnabled set to true.
--
--
-- * 'groupName' - The case-sensitive name of the new group. Default is a reserved name and names must be unique.
-- * 'tags' - A map that contains one or more tag keys and tag values to attach to an X-Ray group. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ .
--
-- The following restrictions apply to tags:
--
--     * Maximum number of user-applied tags per resource: 50
--
--
--     * Maximum tag key length: 128 Unicode characters
--
--
--     * Maximum tag value length: 256 Unicode characters
--
--
--     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @
--
--
--     * Tag keys and values are case sensitive.
--
--
--     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use.
mkCreateGroup ::
  -- | 'groupName'
  Lude.Text ->
  CreateGroup
mkCreateGroup pGroupName_ =
  CreateGroup'
    { filterExpression = Lude.Nothing,
      insightsConfiguration = Lude.Nothing,
      groupName = pGroupName_,
      tags = Lude.Nothing
    }

-- | The filter expression defining criteria by which to group traces.
--
-- /Note:/ Consider using 'filterExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgFilterExpression :: Lens.Lens' CreateGroup (Lude.Maybe Lude.Text)
cgFilterExpression = Lens.lens (filterExpression :: CreateGroup -> Lude.Maybe Lude.Text) (\s a -> s {filterExpression = a} :: CreateGroup)
{-# DEPRECATED cgFilterExpression "Use generic-lens or generic-optics with 'filterExpression' instead." #-}

-- | The structure containing configurations related to insights.
--
--
--     * The InsightsEnabled boolean can be set to true to enable insights for the new group or false to disable insights for the new group.
--
--
--     * The NotifcationsEnabled boolean can be set to true to enable insights notifications for the new group. Notifications may only be enabled on a group with InsightsEnabled set to true.
--
--
--
-- /Note:/ Consider using 'insightsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgInsightsConfiguration :: Lens.Lens' CreateGroup (Lude.Maybe InsightsConfiguration)
cgInsightsConfiguration = Lens.lens (insightsConfiguration :: CreateGroup -> Lude.Maybe InsightsConfiguration) (\s a -> s {insightsConfiguration = a} :: CreateGroup)
{-# DEPRECATED cgInsightsConfiguration "Use generic-lens or generic-optics with 'insightsConfiguration' instead." #-}

-- | The case-sensitive name of the new group. Default is a reserved name and names must be unique.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgGroupName :: Lens.Lens' CreateGroup Lude.Text
cgGroupName = Lens.lens (groupName :: CreateGroup -> Lude.Text) (\s a -> s {groupName = a} :: CreateGroup)
{-# DEPRECATED cgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | A map that contains one or more tag keys and tag values to attach to an X-Ray group. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ .
--
-- The following restrictions apply to tags:
--
--     * Maximum number of user-applied tags per resource: 50
--
--
--     * Maximum tag key length: 128 Unicode characters
--
--
--     * Maximum tag value length: 256 Unicode characters
--
--
--     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @
--
--
--     * Tag keys and values are case sensitive.
--
--
--     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgTags :: Lens.Lens' CreateGroup (Lude.Maybe [Tag])
cgTags = Lens.lens (tags :: CreateGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateGroup)
{-# DEPRECATED cgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateGroup where
  type Rs CreateGroup = CreateGroupResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Lude.<$> (x Lude..?> "Group") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FilterExpression" Lude..=) Lude.<$> filterExpression,
            ("InsightsConfiguration" Lude..=) Lude.<$> insightsConfiguration,
            Lude.Just ("GroupName" Lude..= groupName),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateGroup where
  toPath = Lude.const "/CreateGroup"

instance Lude.ToQuery CreateGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { -- | The group that was created. Contains the name of the group that was created, the Amazon Resource Name (ARN) of the group that was generated based on the group name, the filter expression, and the insight configuration that was assigned to the group.
    group :: Lude.Maybe Group,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGroupResponse' with the minimum fields required to make a request.
--
-- * 'group' - The group that was created. Contains the name of the group that was created, the Amazon Resource Name (ARN) of the group that was generated based on the group name, the filter expression, and the insight configuration that was assigned to the group.
-- * 'responseStatus' - The response status code.
mkCreateGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGroupResponse
mkCreateGroupResponse pResponseStatus_ =
  CreateGroupResponse'
    { group = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The group that was created. Contains the name of the group that was created, the Amazon Resource Name (ARN) of the group that was generated based on the group name, the filter expression, and the insight configuration that was assigned to the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsGroup :: Lens.Lens' CreateGroupResponse (Lude.Maybe Group)
cgrsGroup = Lens.lens (group :: CreateGroupResponse -> Lude.Maybe Group) (\s a -> s {group = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsResponseStatus :: Lens.Lens' CreateGroupResponse Lude.Int
cgrsResponseStatus = Lens.lens (responseStatus :: CreateGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
