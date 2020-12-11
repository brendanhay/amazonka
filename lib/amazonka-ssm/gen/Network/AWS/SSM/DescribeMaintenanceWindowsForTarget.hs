{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowsForTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the maintenance window targets or tasks that an instance is associated with.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowsForTarget
  ( -- * Creating a request
    DescribeMaintenanceWindowsForTarget (..),
    mkDescribeMaintenanceWindowsForTarget,

    -- ** Request lenses
    dmwftNextToken,
    dmwftMaxResults,
    dmwftTargets,
    dmwftResourceType,

    -- * Destructuring the response
    DescribeMaintenanceWindowsForTargetResponse (..),
    mkDescribeMaintenanceWindowsForTargetResponse,

    -- ** Response lenses
    dmwftrsWindowIdentities,
    dmwftrsNextToken,
    dmwftrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeMaintenanceWindowsForTarget' smart constructor.
data DescribeMaintenanceWindowsForTarget = DescribeMaintenanceWindowsForTarget'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    maxResults ::
      Lude.Maybe
        Lude.Natural,
    targets :: [Target],
    resourceType ::
      MaintenanceWindowResourceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceWindowsForTarget' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'resourceType' - The type of resource you want to retrieve information about. For example, "INSTANCE".
-- * 'targets' - The instance ID or key/value pair to retrieve information about.
mkDescribeMaintenanceWindowsForTarget ::
  -- | 'resourceType'
  MaintenanceWindowResourceType ->
  DescribeMaintenanceWindowsForTarget
mkDescribeMaintenanceWindowsForTarget pResourceType_ =
  DescribeMaintenanceWindowsForTarget'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      targets = Lude.mempty,
      resourceType = pResourceType_
    }

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwftNextToken :: Lens.Lens' DescribeMaintenanceWindowsForTarget (Lude.Maybe Lude.Text)
dmwftNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindowsForTarget -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindowsForTarget)
{-# DEPRECATED dmwftNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwftMaxResults :: Lens.Lens' DescribeMaintenanceWindowsForTarget (Lude.Maybe Lude.Natural)
dmwftMaxResults = Lens.lens (maxResults :: DescribeMaintenanceWindowsForTarget -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeMaintenanceWindowsForTarget)
{-# DEPRECATED dmwftMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The instance ID or key/value pair to retrieve information about.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwftTargets :: Lens.Lens' DescribeMaintenanceWindowsForTarget [Target]
dmwftTargets = Lens.lens (targets :: DescribeMaintenanceWindowsForTarget -> [Target]) (\s a -> s {targets = a} :: DescribeMaintenanceWindowsForTarget)
{-# DEPRECATED dmwftTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The type of resource you want to retrieve information about. For example, "INSTANCE".
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwftResourceType :: Lens.Lens' DescribeMaintenanceWindowsForTarget MaintenanceWindowResourceType
dmwftResourceType = Lens.lens (resourceType :: DescribeMaintenanceWindowsForTarget -> MaintenanceWindowResourceType) (\s a -> s {resourceType = a} :: DescribeMaintenanceWindowsForTarget)
{-# DEPRECATED dmwftResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Page.AWSPager DescribeMaintenanceWindowsForTarget where
  page rq rs
    | Page.stop (rs Lens.^. dmwftrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dmwftrsWindowIdentities) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dmwftNextToken Lens..~ rs Lens.^. dmwftrsNextToken

instance Lude.AWSRequest DescribeMaintenanceWindowsForTarget where
  type
    Rs DescribeMaintenanceWindowsForTarget =
      DescribeMaintenanceWindowsForTargetResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowsForTargetResponse'
            Lude.<$> (x Lude..?> "WindowIdentities" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMaintenanceWindowsForTarget where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonSSM.DescribeMaintenanceWindowsForTarget" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMaintenanceWindowsForTarget where
  toJSON DescribeMaintenanceWindowsForTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("Targets" Lude..= targets),
            Lude.Just ("ResourceType" Lude..= resourceType)
          ]
      )

instance Lude.ToPath DescribeMaintenanceWindowsForTarget where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMaintenanceWindowsForTarget where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMaintenanceWindowsForTargetResponse' smart constructor.
data DescribeMaintenanceWindowsForTargetResponse = DescribeMaintenanceWindowsForTargetResponse'
  { windowIdentities ::
      Lude.Maybe
        [MaintenanceWindowIdentityForTarget],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceWindowsForTargetResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of items to return. (You use this token in the next call.)
-- * 'responseStatus' - The response status code.
-- * 'windowIdentities' - Information about the maintenance window targets and tasks an instance is associated with.
mkDescribeMaintenanceWindowsForTargetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMaintenanceWindowsForTargetResponse
mkDescribeMaintenanceWindowsForTargetResponse pResponseStatus_ =
  DescribeMaintenanceWindowsForTargetResponse'
    { windowIdentities =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the maintenance window targets and tasks an instance is associated with.
--
-- /Note:/ Consider using 'windowIdentities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwftrsWindowIdentities :: Lens.Lens' DescribeMaintenanceWindowsForTargetResponse (Lude.Maybe [MaintenanceWindowIdentityForTarget])
dmwftrsWindowIdentities = Lens.lens (windowIdentities :: DescribeMaintenanceWindowsForTargetResponse -> Lude.Maybe [MaintenanceWindowIdentityForTarget]) (\s a -> s {windowIdentities = a} :: DescribeMaintenanceWindowsForTargetResponse)
{-# DEPRECATED dmwftrsWindowIdentities "Use generic-lens or generic-optics with 'windowIdentities' instead." #-}

-- | The token for the next set of items to return. (You use this token in the next call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwftrsNextToken :: Lens.Lens' DescribeMaintenanceWindowsForTargetResponse (Lude.Maybe Lude.Text)
dmwftrsNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindowsForTargetResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindowsForTargetResponse)
{-# DEPRECATED dmwftrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwftrsResponseStatus :: Lens.Lens' DescribeMaintenanceWindowsForTargetResponse Lude.Int
dmwftrsResponseStatus = Lens.lens (responseStatus :: DescribeMaintenanceWindowsForTargetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMaintenanceWindowsForTargetResponse)
{-# DEPRECATED dmwftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
