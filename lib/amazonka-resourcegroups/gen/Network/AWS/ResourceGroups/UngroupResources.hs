{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.UngroupResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified resources from the specified group.
module Network.AWS.ResourceGroups.UngroupResources
  ( -- * Creating a request
    UngroupResources (..),
    mkUngroupResources,

    -- ** Request lenses
    urGroup,
    urResourceARNs,

    -- * Destructuring the response
    UngroupResourcesResponse (..),
    mkUngroupResourcesResponse,

    -- ** Response lenses
    urrsSucceeded,
    urrsFailed,
    urrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUngroupResources' smart constructor.
data UngroupResources = UngroupResources'
  { group :: Lude.Text,
    resourceARNs :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UngroupResources' with the minimum fields required to make a request.
--
-- * 'group' - The name or the ARN of the resource group from which to remove the resources.
-- * 'resourceARNs' - The ARNs of the resources to be removed from the group.
mkUngroupResources ::
  -- | 'group'
  Lude.Text ->
  -- | 'resourceARNs'
  Lude.NonEmpty Lude.Text ->
  UngroupResources
mkUngroupResources pGroup_ pResourceARNs_ =
  UngroupResources' {group = pGroup_, resourceARNs = pResourceARNs_}

-- | The name or the ARN of the resource group from which to remove the resources.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urGroup :: Lens.Lens' UngroupResources Lude.Text
urGroup = Lens.lens (group :: UngroupResources -> Lude.Text) (\s a -> s {group = a} :: UngroupResources)
{-# DEPRECATED urGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The ARNs of the resources to be removed from the group.
--
-- /Note:/ Consider using 'resourceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceARNs :: Lens.Lens' UngroupResources (Lude.NonEmpty Lude.Text)
urResourceARNs = Lens.lens (resourceARNs :: UngroupResources -> Lude.NonEmpty Lude.Text) (\s a -> s {resourceARNs = a} :: UngroupResources)
{-# DEPRECATED urResourceARNs "Use generic-lens or generic-optics with 'resourceARNs' instead." #-}

instance Lude.AWSRequest UngroupResources where
  type Rs UngroupResources = UngroupResourcesResponse
  request = Req.postJSON resourceGroupsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UngroupResourcesResponse'
            Lude.<$> (x Lude..?> "Succeeded")
            Lude.<*> (x Lude..?> "Failed" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UngroupResources where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UngroupResources where
  toJSON UngroupResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Group" Lude..= group),
            Lude.Just ("ResourceArns" Lude..= resourceARNs)
          ]
      )

instance Lude.ToPath UngroupResources where
  toPath = Lude.const "/ungroup-resources"

instance Lude.ToQuery UngroupResources where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUngroupResourcesResponse' smart constructor.
data UngroupResourcesResponse = UngroupResourcesResponse'
  { succeeded ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    failed :: Lude.Maybe [FailedResource],
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

-- | Creates a value of 'UngroupResourcesResponse' with the minimum fields required to make a request.
--
-- * 'failed' - The resources that failed to be removed from the group.
-- * 'responseStatus' - The response status code.
-- * 'succeeded' - The ARNs of the resources that were successfully removed from the group.
mkUngroupResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UngroupResourcesResponse
mkUngroupResourcesResponse pResponseStatus_ =
  UngroupResourcesResponse'
    { succeeded = Lude.Nothing,
      failed = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARNs of the resources that were successfully removed from the group.
--
-- /Note:/ Consider using 'succeeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsSucceeded :: Lens.Lens' UngroupResourcesResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
urrsSucceeded = Lens.lens (succeeded :: UngroupResourcesResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {succeeded = a} :: UngroupResourcesResponse)
{-# DEPRECATED urrsSucceeded "Use generic-lens or generic-optics with 'succeeded' instead." #-}

-- | The resources that failed to be removed from the group.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsFailed :: Lens.Lens' UngroupResourcesResponse (Lude.Maybe [FailedResource])
urrsFailed = Lens.lens (failed :: UngroupResourcesResponse -> Lude.Maybe [FailedResource]) (\s a -> s {failed = a} :: UngroupResourcesResponse)
{-# DEPRECATED urrsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UngroupResourcesResponse Lude.Int
urrsResponseStatus = Lens.lens (responseStatus :: UngroupResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UngroupResourcesResponse)
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
