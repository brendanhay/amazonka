{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListDeploymentTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of target IDs that are associated a deployment.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeploymentTargets
  ( -- * Creating a request
    ListDeploymentTargets (..),
    mkListDeploymentTargets,

    -- ** Request lenses
    ldtDeploymentId,
    ldtTargetFilters,
    ldtNextToken,

    -- * Destructuring the response
    ListDeploymentTargetsResponse (..),
    mkListDeploymentTargetsResponse,

    -- ** Response lenses
    ldtrsNextToken,
    ldtrsTargetIds,
    ldtrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDeploymentTargets' smart constructor.
data ListDeploymentTargets = ListDeploymentTargets'
  { -- | The unique ID of a deployment.
    deploymentId :: Lude.Maybe Lude.Text,
    -- | A key used to filter the returned targets. The two valid values are:
    --
    --
    --     * @TargetStatus@ - A @TargetStatus@ filter string can be @Failed@ , @InProgress@ , @Pending@ , @Ready@ , @Skipped@ , @Succeeded@ , or @Unknown@ .
    --
    --
    --     * @ServerInstanceLabel@ - A @ServerInstanceLabel@ filter string can be @Blue@ or @Green@ .
    targetFilters :: Lude.Maybe (Lude.HashMap TargetFilterName ([Lude.Text])),
    -- | A token identifier returned from the previous @ListDeploymentTargets@ call. It can be used to return the next set of deployment targets in the list.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeploymentTargets' with the minimum fields required to make a request.
--
-- * 'deploymentId' - The unique ID of a deployment.
-- * 'targetFilters' - A key used to filter the returned targets. The two valid values are:
--
--
--     * @TargetStatus@ - A @TargetStatus@ filter string can be @Failed@ , @InProgress@ , @Pending@ , @Ready@ , @Skipped@ , @Succeeded@ , or @Unknown@ .
--
--
--     * @ServerInstanceLabel@ - A @ServerInstanceLabel@ filter string can be @Blue@ or @Green@ .
--
--
-- * 'nextToken' - A token identifier returned from the previous @ListDeploymentTargets@ call. It can be used to return the next set of deployment targets in the list.
mkListDeploymentTargets ::
  ListDeploymentTargets
mkListDeploymentTargets =
  ListDeploymentTargets'
    { deploymentId = Lude.Nothing,
      targetFilters = Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldtDeploymentId :: Lens.Lens' ListDeploymentTargets (Lude.Maybe Lude.Text)
ldtDeploymentId = Lens.lens (deploymentId :: ListDeploymentTargets -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: ListDeploymentTargets)
{-# DEPRECATED ldtDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | A key used to filter the returned targets. The two valid values are:
--
--
--     * @TargetStatus@ - A @TargetStatus@ filter string can be @Failed@ , @InProgress@ , @Pending@ , @Ready@ , @Skipped@ , @Succeeded@ , or @Unknown@ .
--
--
--     * @ServerInstanceLabel@ - A @ServerInstanceLabel@ filter string can be @Blue@ or @Green@ .
--
--
--
-- /Note:/ Consider using 'targetFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldtTargetFilters :: Lens.Lens' ListDeploymentTargets (Lude.Maybe (Lude.HashMap TargetFilterName ([Lude.Text])))
ldtTargetFilters = Lens.lens (targetFilters :: ListDeploymentTargets -> Lude.Maybe (Lude.HashMap TargetFilterName ([Lude.Text]))) (\s a -> s {targetFilters = a} :: ListDeploymentTargets)
{-# DEPRECATED ldtTargetFilters "Use generic-lens or generic-optics with 'targetFilters' instead." #-}

-- | A token identifier returned from the previous @ListDeploymentTargets@ call. It can be used to return the next set of deployment targets in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldtNextToken :: Lens.Lens' ListDeploymentTargets (Lude.Maybe Lude.Text)
ldtNextToken = Lens.lens (nextToken :: ListDeploymentTargets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeploymentTargets)
{-# DEPRECATED ldtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListDeploymentTargets where
  page rq rs
    | Page.stop (rs Lens.^. ldtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldtrsTargetIds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldtNextToken Lens..~ rs Lens.^. ldtrsNextToken

instance Lude.AWSRequest ListDeploymentTargets where
  type Rs ListDeploymentTargets = ListDeploymentTargetsResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDeploymentTargetsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "targetIds" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDeploymentTargets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.ListDeploymentTargets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDeploymentTargets where
  toJSON ListDeploymentTargets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("deploymentId" Lude..=) Lude.<$> deploymentId,
            ("targetFilters" Lude..=) Lude.<$> targetFilters,
            ("nextToken" Lude..=) Lude.<$> nextToken
          ]
      )

instance Lude.ToPath ListDeploymentTargets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDeploymentTargets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDeploymentTargetsResponse' smart constructor.
data ListDeploymentTargetsResponse = ListDeploymentTargetsResponse'
  { -- | If a large amount of information is returned, a token identifier is also returned. It can be used in a subsequent @ListDeploymentTargets@ call to return the next set of deployment targets in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The unique IDs of deployment targets.
    targetIds :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeploymentTargetsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If a large amount of information is returned, a token identifier is also returned. It can be used in a subsequent @ListDeploymentTargets@ call to return the next set of deployment targets in the list.
-- * 'targetIds' - The unique IDs of deployment targets.
-- * 'responseStatus' - The response status code.
mkListDeploymentTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDeploymentTargetsResponse
mkListDeploymentTargetsResponse pResponseStatus_ =
  ListDeploymentTargetsResponse'
    { nextToken = Lude.Nothing,
      targetIds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If a large amount of information is returned, a token identifier is also returned. It can be used in a subsequent @ListDeploymentTargets@ call to return the next set of deployment targets in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldtrsNextToken :: Lens.Lens' ListDeploymentTargetsResponse (Lude.Maybe Lude.Text)
ldtrsNextToken = Lens.lens (nextToken :: ListDeploymentTargetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeploymentTargetsResponse)
{-# DEPRECATED ldtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The unique IDs of deployment targets.
--
-- /Note:/ Consider using 'targetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldtrsTargetIds :: Lens.Lens' ListDeploymentTargetsResponse (Lude.Maybe [Lude.Text])
ldtrsTargetIds = Lens.lens (targetIds :: ListDeploymentTargetsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {targetIds = a} :: ListDeploymentTargetsResponse)
{-# DEPRECATED ldtrsTargetIds "Use generic-lens or generic-optics with 'targetIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldtrsResponseStatus :: Lens.Lens' ListDeploymentTargetsResponse Lude.Int
ldtrsResponseStatus = Lens.lens (responseStatus :: ListDeploymentTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDeploymentTargetsResponse)
{-# DEPRECATED ldtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
