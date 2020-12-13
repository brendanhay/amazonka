{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.GroupResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified resources to the specified group.
module Network.AWS.ResourceGroups.GroupResources
  ( -- * Creating a request
    GroupResources (..),
    mkGroupResources,

    -- ** Request lenses
    grGroup,
    grResourceARNs,

    -- * Destructuring the response
    GroupResourcesResponse (..),
    mkGroupResourcesResponse,

    -- ** Response lenses
    grrsSucceeded,
    grrsFailed,
    grrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGroupResources' smart constructor.
data GroupResources = GroupResources'
  { -- | The name or the ARN of the resource group to add resources to.
    group :: Lude.Text,
    -- | The list of ARNs for resources to be added to the group.
    resourceARNs :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupResources' with the minimum fields required to make a request.
--
-- * 'group' - The name or the ARN of the resource group to add resources to.
-- * 'resourceARNs' - The list of ARNs for resources to be added to the group.
mkGroupResources ::
  -- | 'group'
  Lude.Text ->
  -- | 'resourceARNs'
  Lude.NonEmpty Lude.Text ->
  GroupResources
mkGroupResources pGroup_ pResourceARNs_ =
  GroupResources' {group = pGroup_, resourceARNs = pResourceARNs_}

-- | The name or the ARN of the resource group to add resources to.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grGroup :: Lens.Lens' GroupResources Lude.Text
grGroup = Lens.lens (group :: GroupResources -> Lude.Text) (\s a -> s {group = a} :: GroupResources)
{-# DEPRECATED grGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The list of ARNs for resources to be added to the group.
--
-- /Note:/ Consider using 'resourceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grResourceARNs :: Lens.Lens' GroupResources (Lude.NonEmpty Lude.Text)
grResourceARNs = Lens.lens (resourceARNs :: GroupResources -> Lude.NonEmpty Lude.Text) (\s a -> s {resourceARNs = a} :: GroupResources)
{-# DEPRECATED grResourceARNs "Use generic-lens or generic-optics with 'resourceARNs' instead." #-}

instance Lude.AWSRequest GroupResources where
  type Rs GroupResources = GroupResourcesResponse
  request = Req.postJSON resourceGroupsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GroupResourcesResponse'
            Lude.<$> (x Lude..?> "Succeeded")
            Lude.<*> (x Lude..?> "Failed" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GroupResources where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GroupResources where
  toJSON GroupResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Group" Lude..= group),
            Lude.Just ("ResourceArns" Lude..= resourceARNs)
          ]
      )

instance Lude.ToPath GroupResources where
  toPath = Lude.const "/group-resources"

instance Lude.ToQuery GroupResources where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGroupResourcesResponse' smart constructor.
data GroupResourcesResponse = GroupResourcesResponse'
  { -- | The ARNs of the resources that were successfully added to the group by this operation.
    succeeded :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The ARNs of the resources that failed to be added to the group by this operation.
    failed :: Lude.Maybe [FailedResource],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupResourcesResponse' with the minimum fields required to make a request.
--
-- * 'succeeded' - The ARNs of the resources that were successfully added to the group by this operation.
-- * 'failed' - The ARNs of the resources that failed to be added to the group by this operation.
-- * 'responseStatus' - The response status code.
mkGroupResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GroupResourcesResponse
mkGroupResourcesResponse pResponseStatus_ =
  GroupResourcesResponse'
    { succeeded = Lude.Nothing,
      failed = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARNs of the resources that were successfully added to the group by this operation.
--
-- /Note:/ Consider using 'succeeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsSucceeded :: Lens.Lens' GroupResourcesResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
grrsSucceeded = Lens.lens (succeeded :: GroupResourcesResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {succeeded = a} :: GroupResourcesResponse)
{-# DEPRECATED grrsSucceeded "Use generic-lens or generic-optics with 'succeeded' instead." #-}

-- | The ARNs of the resources that failed to be added to the group by this operation.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsFailed :: Lens.Lens' GroupResourcesResponse (Lude.Maybe [FailedResource])
grrsFailed = Lens.lens (failed :: GroupResourcesResponse -> Lude.Maybe [FailedResource]) (\s a -> s {failed = a} :: GroupResourcesResponse)
{-# DEPRECATED grrsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResponseStatus :: Lens.Lens' GroupResourcesResponse Lude.Int
grrsResponseStatus = Lens.lens (responseStatus :: GroupResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GroupResourcesResponse)
{-# DEPRECATED grrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
