{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeResourceGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the resource groups that are specified by the ARNs of the resource groups.
module Network.AWS.Inspector.DescribeResourceGroups
  ( -- * Creating a request
    DescribeResourceGroups (..),
    mkDescribeResourceGroups,

    -- ** Request lenses
    drgResourceGroupARNs,

    -- * Destructuring the response
    DescribeResourceGroupsResponse (..),
    mkDescribeResourceGroupsResponse,

    -- ** Response lenses
    drgrsResponseStatus,
    drgrsResourceGroups,
    drgrsFailedItems,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeResourceGroups' smart constructor.
newtype DescribeResourceGroups = DescribeResourceGroups'
  { resourceGroupARNs ::
      Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeResourceGroups' with the minimum fields required to make a request.
--
-- * 'resourceGroupARNs' - The ARN that specifies the resource group that you want to describe.
mkDescribeResourceGroups ::
  -- | 'resourceGroupARNs'
  Lude.NonEmpty Lude.Text ->
  DescribeResourceGroups
mkDescribeResourceGroups pResourceGroupARNs_ =
  DescribeResourceGroups' {resourceGroupARNs = pResourceGroupARNs_}

-- | The ARN that specifies the resource group that you want to describe.
--
-- /Note:/ Consider using 'resourceGroupARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgResourceGroupARNs :: Lens.Lens' DescribeResourceGroups (Lude.NonEmpty Lude.Text)
drgResourceGroupARNs = Lens.lens (resourceGroupARNs :: DescribeResourceGroups -> Lude.NonEmpty Lude.Text) (\s a -> s {resourceGroupARNs = a} :: DescribeResourceGroups)
{-# DEPRECATED drgResourceGroupARNs "Use generic-lens or generic-optics with 'resourceGroupARNs' instead." #-}

instance Lude.AWSRequest DescribeResourceGroups where
  type Rs DescribeResourceGroups = DescribeResourceGroupsResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeResourceGroupsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "resourceGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "failedItems" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders DescribeResourceGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.DescribeResourceGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeResourceGroups where
  toJSON DescribeResourceGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("resourceGroupArns" Lude..= resourceGroupARNs)]
      )

instance Lude.ToPath DescribeResourceGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeResourceGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeResourceGroupsResponse' smart constructor.
data DescribeResourceGroupsResponse = DescribeResourceGroupsResponse'
  { responseStatus ::
      Lude.Int,
    resourceGroups ::
      [ResourceGroup],
    failedItems ::
      Lude.HashMap
        Lude.Text
        (FailedItemDetails)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeResourceGroupsResponse' with the minimum fields required to make a request.
--
-- * 'failedItems' - Resource group details that cannot be described. An error code is provided for each failed item.
-- * 'resourceGroups' - Information about a resource group.
-- * 'responseStatus' - The response status code.
mkDescribeResourceGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeResourceGroupsResponse
mkDescribeResourceGroupsResponse pResponseStatus_ =
  DescribeResourceGroupsResponse'
    { responseStatus =
        pResponseStatus_,
      resourceGroups = Lude.mempty,
      failedItems = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrsResponseStatus :: Lens.Lens' DescribeResourceGroupsResponse Lude.Int
drgrsResponseStatus = Lens.lens (responseStatus :: DescribeResourceGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeResourceGroupsResponse)
{-# DEPRECATED drgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Information about a resource group.
--
-- /Note:/ Consider using 'resourceGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrsResourceGroups :: Lens.Lens' DescribeResourceGroupsResponse [ResourceGroup]
drgrsResourceGroups = Lens.lens (resourceGroups :: DescribeResourceGroupsResponse -> [ResourceGroup]) (\s a -> s {resourceGroups = a} :: DescribeResourceGroupsResponse)
{-# DEPRECATED drgrsResourceGroups "Use generic-lens or generic-optics with 'resourceGroups' instead." #-}

-- | Resource group details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrsFailedItems :: Lens.Lens' DescribeResourceGroupsResponse (Lude.HashMap Lude.Text (FailedItemDetails))
drgrsFailedItems = Lens.lens (failedItems :: DescribeResourceGroupsResponse -> Lude.HashMap Lude.Text (FailedItemDetails)) (\s a -> s {failedItems = a} :: DescribeResourceGroupsResponse)
{-# DEPRECATED drgrsFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}
