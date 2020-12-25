{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListDeploymentConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployment configurations with the IAM user or AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeploymentConfigs
  ( -- * Creating a request
    ListDeploymentConfigs (..),
    mkListDeploymentConfigs,

    -- ** Request lenses
    ldcNextToken,

    -- * Destructuring the response
    ListDeploymentConfigsResponse (..),
    mkListDeploymentConfigsResponse,

    -- ** Response lenses
    ldcrrsDeploymentConfigsList,
    ldcrrsNextToken,
    ldcrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListDeploymentConfigs@ operation.
--
-- /See:/ 'mkListDeploymentConfigs' smart constructor.
newtype ListDeploymentConfigs = ListDeploymentConfigs'
  { -- | An identifier returned from the previous @ListDeploymentConfigs@ call. It can be used to return the next set of deployment configurations in the list.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeploymentConfigs' value with any optional fields omitted.
mkListDeploymentConfigs ::
  ListDeploymentConfigs
mkListDeploymentConfigs =
  ListDeploymentConfigs' {nextToken = Core.Nothing}

-- | An identifier returned from the previous @ListDeploymentConfigs@ call. It can be used to return the next set of deployment configurations in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcNextToken :: Lens.Lens' ListDeploymentConfigs (Core.Maybe Types.NextToken)
ldcNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListDeploymentConfigs where
  toJSON ListDeploymentConfigs {..} =
    Core.object
      (Core.catMaybes [("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListDeploymentConfigs where
  type Rs ListDeploymentConfigs = ListDeploymentConfigsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.ListDeploymentConfigs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentConfigsResponse'
            Core.<$> (x Core..:? "deploymentConfigsList")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDeploymentConfigs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"deploymentConfigsList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the output of a @ListDeploymentConfigs@ operation.
--
-- /See:/ 'mkListDeploymentConfigsResponse' smart constructor.
data ListDeploymentConfigsResponse = ListDeploymentConfigsResponse'
  { -- | A list of deployment configurations, including built-in configurations such as @CodeDeployDefault.OneAtATime@ .
    deploymentConfigsList :: Core.Maybe [Types.DeploymentConfigName],
    -- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployment configurations call to return the next set of deployment configurations in the list.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeploymentConfigsResponse' value with any optional fields omitted.
mkListDeploymentConfigsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDeploymentConfigsResponse
mkListDeploymentConfigsResponse responseStatus =
  ListDeploymentConfigsResponse'
    { deploymentConfigsList =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of deployment configurations, including built-in configurations such as @CodeDeployDefault.OneAtATime@ .
--
-- /Note:/ Consider using 'deploymentConfigsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsDeploymentConfigsList :: Lens.Lens' ListDeploymentConfigsResponse (Core.Maybe [Types.DeploymentConfigName])
ldcrrsDeploymentConfigsList = Lens.field @"deploymentConfigsList"
{-# DEPRECATED ldcrrsDeploymentConfigsList "Use generic-lens or generic-optics with 'deploymentConfigsList' instead." #-}

-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployment configurations call to return the next set of deployment configurations in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsNextToken :: Lens.Lens' ListDeploymentConfigsResponse (Core.Maybe Types.NextToken)
ldcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsResponseStatus :: Lens.Lens' ListDeploymentConfigsResponse Core.Int
ldcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
