{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.UpdateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a group resource.
module Network.AWS.XRay.UpdateGroup
  ( -- * Creating a request
    UpdateGroup (..),
    mkUpdateGroup,

    -- ** Request lenses
    ugFilterExpression,
    ugInsightsConfiguration,
    ugGroupARN,
    ugGroupName,

    -- * Destructuring the response
    UpdateGroupResponse (..),
    mkUpdateGroupResponse,

    -- ** Response lenses
    ugrsGroup,
    ugrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { -- | The updated filter expression defining criteria by which to group traces.
    filterExpression :: Lude.Maybe Lude.Text,
    -- | The structure containing configurations related to insights.
    --
    --
    --     * The InsightsEnabled boolean can be set to true to enable insights for the group or false to disable insights for the group.
    --
    --
    --     * The NotifcationsEnabled boolean can be set to true to enable insights notifications for the group. Notifications can only be enabled on a group with InsightsEnabled set to true.
    insightsConfiguration :: Lude.Maybe InsightsConfiguration,
    -- | The ARN that was generated upon creation.
    groupARN :: Lude.Maybe Lude.Text,
    -- | The case-sensitive name of the group.
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGroup' with the minimum fields required to make a request.
--
-- * 'filterExpression' - The updated filter expression defining criteria by which to group traces.
-- * 'insightsConfiguration' - The structure containing configurations related to insights.
--
--
--     * The InsightsEnabled boolean can be set to true to enable insights for the group or false to disable insights for the group.
--
--
--     * The NotifcationsEnabled boolean can be set to true to enable insights notifications for the group. Notifications can only be enabled on a group with InsightsEnabled set to true.
--
--
-- * 'groupARN' - The ARN that was generated upon creation.
-- * 'groupName' - The case-sensitive name of the group.
mkUpdateGroup ::
  UpdateGroup
mkUpdateGroup =
  UpdateGroup'
    { filterExpression = Lude.Nothing,
      insightsConfiguration = Lude.Nothing,
      groupARN = Lude.Nothing,
      groupName = Lude.Nothing
    }

-- | The updated filter expression defining criteria by which to group traces.
--
-- /Note:/ Consider using 'filterExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugFilterExpression :: Lens.Lens' UpdateGroup (Lude.Maybe Lude.Text)
ugFilterExpression = Lens.lens (filterExpression :: UpdateGroup -> Lude.Maybe Lude.Text) (\s a -> s {filterExpression = a} :: UpdateGroup)
{-# DEPRECATED ugFilterExpression "Use generic-lens or generic-optics with 'filterExpression' instead." #-}

-- | The structure containing configurations related to insights.
--
--
--     * The InsightsEnabled boolean can be set to true to enable insights for the group or false to disable insights for the group.
--
--
--     * The NotifcationsEnabled boolean can be set to true to enable insights notifications for the group. Notifications can only be enabled on a group with InsightsEnabled set to true.
--
--
--
-- /Note:/ Consider using 'insightsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugInsightsConfiguration :: Lens.Lens' UpdateGroup (Lude.Maybe InsightsConfiguration)
ugInsightsConfiguration = Lens.lens (insightsConfiguration :: UpdateGroup -> Lude.Maybe InsightsConfiguration) (\s a -> s {insightsConfiguration = a} :: UpdateGroup)
{-# DEPRECATED ugInsightsConfiguration "Use generic-lens or generic-optics with 'insightsConfiguration' instead." #-}

-- | The ARN that was generated upon creation.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroupARN :: Lens.Lens' UpdateGroup (Lude.Maybe Lude.Text)
ugGroupARN = Lens.lens (groupARN :: UpdateGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupARN = a} :: UpdateGroup)
{-# DEPRECATED ugGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The case-sensitive name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroupName :: Lens.Lens' UpdateGroup (Lude.Maybe Lude.Text)
ugGroupName = Lens.lens (groupName :: UpdateGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: UpdateGroup)
{-# DEPRECATED ugGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest UpdateGroup where
  type Rs UpdateGroup = UpdateGroupResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGroupResponse'
            Lude.<$> (x Lude..?> "Group") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateGroup where
  toJSON UpdateGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FilterExpression" Lude..=) Lude.<$> filterExpression,
            ("InsightsConfiguration" Lude..=) Lude.<$> insightsConfiguration,
            ("GroupARN" Lude..=) Lude.<$> groupARN,
            ("GroupName" Lude..=) Lude.<$> groupName
          ]
      )

instance Lude.ToPath UpdateGroup where
  toPath = Lude.const "/UpdateGroup"

instance Lude.ToQuery UpdateGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { -- | The group that was updated. Contains the name of the group that was updated, the ARN of the group that was updated, the updated filter expression, and the updated insight configuration assigned to the group.
    group :: Lude.Maybe Group,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGroupResponse' with the minimum fields required to make a request.
--
-- * 'group' - The group that was updated. Contains the name of the group that was updated, the ARN of the group that was updated, the updated filter expression, and the updated insight configuration assigned to the group.
-- * 'responseStatus' - The response status code.
mkUpdateGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGroupResponse
mkUpdateGroupResponse pResponseStatus_ =
  UpdateGroupResponse'
    { group = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The group that was updated. Contains the name of the group that was updated, the ARN of the group that was updated, the updated filter expression, and the updated insight configuration assigned to the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrsGroup :: Lens.Lens' UpdateGroupResponse (Lude.Maybe Group)
ugrsGroup = Lens.lens (group :: UpdateGroupResponse -> Lude.Maybe Group) (\s a -> s {group = a} :: UpdateGroupResponse)
{-# DEPRECATED ugrsGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrsResponseStatus :: Lens.Lens' UpdateGroupResponse Lude.Int
ugrsResponseStatus = Lens.lens (responseStatus :: UpdateGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGroupResponse)
{-# DEPRECATED ugrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
