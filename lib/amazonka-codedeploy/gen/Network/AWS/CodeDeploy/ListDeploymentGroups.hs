{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListDeploymentGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployment groups for an application registered with the IAM user or AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeploymentGroups
  ( -- * Creating a request
    ListDeploymentGroups (..),
    mkListDeploymentGroups,

    -- ** Request lenses
    ldgApplicationName,
    ldgNextToken,

    -- * Destructuring the response
    ListDeploymentGroupsResponse (..),
    mkListDeploymentGroupsResponse,

    -- ** Response lenses
    ldgrrsApplicationName,
    ldgrrsDeploymentGroups,
    ldgrrsNextToken,
    ldgrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListDeploymentGroups@ operation.
--
-- /See:/ 'mkListDeploymentGroups' smart constructor.
data ListDeploymentGroups = ListDeploymentGroups'
  { -- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
    applicationName :: Types.ApplicationName,
    -- | An identifier returned from the previous list deployment groups call. It can be used to return the next set of deployment groups in the list.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeploymentGroups' value with any optional fields omitted.
mkListDeploymentGroups ::
  -- | 'applicationName'
  Types.ApplicationName ->
  ListDeploymentGroups
mkListDeploymentGroups applicationName =
  ListDeploymentGroups' {applicationName, nextToken = Core.Nothing}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldgApplicationName :: Lens.Lens' ListDeploymentGroups Types.ApplicationName
ldgApplicationName = Lens.field @"applicationName"
{-# DEPRECATED ldgApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | An identifier returned from the previous list deployment groups call. It can be used to return the next set of deployment groups in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldgNextToken :: Lens.Lens' ListDeploymentGroups (Core.Maybe Types.NextToken)
ldgNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListDeploymentGroups where
  toJSON ListDeploymentGroups {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("applicationName" Core..= applicationName),
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListDeploymentGroups where
  type Rs ListDeploymentGroups = ListDeploymentGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.ListDeploymentGroups")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentGroupsResponse'
            Core.<$> (x Core..:? "applicationName")
            Core.<*> (x Core..:? "deploymentGroups")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDeploymentGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"deploymentGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the output of a @ListDeploymentGroups@ operation.
--
-- /See:/ 'mkListDeploymentGroupsResponse' smart constructor.
data ListDeploymentGroupsResponse = ListDeploymentGroupsResponse'
  { -- | The application name.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | A list of deployment group names.
    deploymentGroups :: Core.Maybe [Types.DeploymentGroupName],
    -- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployment groups call to return the next set of deployment groups in the list.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeploymentGroupsResponse' value with any optional fields omitted.
mkListDeploymentGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDeploymentGroupsResponse
mkListDeploymentGroupsResponse responseStatus =
  ListDeploymentGroupsResponse'
    { applicationName = Core.Nothing,
      deploymentGroups = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldgrrsApplicationName :: Lens.Lens' ListDeploymentGroupsResponse (Core.Maybe Types.ApplicationName)
ldgrrsApplicationName = Lens.field @"applicationName"
{-# DEPRECATED ldgrrsApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | A list of deployment group names.
--
-- /Note:/ Consider using 'deploymentGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldgrrsDeploymentGroups :: Lens.Lens' ListDeploymentGroupsResponse (Core.Maybe [Types.DeploymentGroupName])
ldgrrsDeploymentGroups = Lens.field @"deploymentGroups"
{-# DEPRECATED ldgrrsDeploymentGroups "Use generic-lens or generic-optics with 'deploymentGroups' instead." #-}

-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployment groups call to return the next set of deployment groups in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldgrrsNextToken :: Lens.Lens' ListDeploymentGroupsResponse (Core.Maybe Types.NextToken)
ldgrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldgrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldgrrsResponseStatus :: Lens.Lens' ListDeploymentGroupsResponse Core.Int
ldgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
