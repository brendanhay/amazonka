{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.BatchGetDeploymentGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more deployment groups.
module Network.AWS.CodeDeploy.BatchGetDeploymentGroups
  ( -- * Creating a request
    BatchGetDeploymentGroups (..),
    mkBatchGetDeploymentGroups,

    -- ** Request lenses
    bgdgApplicationName,
    bgdgDeploymentGroupNames,

    -- * Destructuring the response
    BatchGetDeploymentGroupsResponse (..),
    mkBatchGetDeploymentGroupsResponse,

    -- ** Response lenses
    bgdgrrsDeploymentGroupsInfo,
    bgdgrrsErrorMessage,
    bgdgrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @BatchGetDeploymentGroups@ operation.
--
-- /See:/ 'mkBatchGetDeploymentGroups' smart constructor.
data BatchGetDeploymentGroups = BatchGetDeploymentGroups'
  { -- | The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
    applicationName :: Types.ApplicationName,
    -- | The names of the deployment groups.
    deploymentGroupNames :: [Types.DeploymentGroupName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetDeploymentGroups' value with any optional fields omitted.
mkBatchGetDeploymentGroups ::
  -- | 'applicationName'
  Types.ApplicationName ->
  BatchGetDeploymentGroups
mkBatchGetDeploymentGroups applicationName =
  BatchGetDeploymentGroups'
    { applicationName,
      deploymentGroupNames = Core.mempty
    }

-- | The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdgApplicationName :: Lens.Lens' BatchGetDeploymentGroups Types.ApplicationName
bgdgApplicationName = Lens.field @"applicationName"
{-# DEPRECATED bgdgApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The names of the deployment groups.
--
-- /Note:/ Consider using 'deploymentGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdgDeploymentGroupNames :: Lens.Lens' BatchGetDeploymentGroups [Types.DeploymentGroupName]
bgdgDeploymentGroupNames = Lens.field @"deploymentGroupNames"
{-# DEPRECATED bgdgDeploymentGroupNames "Use generic-lens or generic-optics with 'deploymentGroupNames' instead." #-}

instance Core.FromJSON BatchGetDeploymentGroups where
  toJSON BatchGetDeploymentGroups {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("applicationName" Core..= applicationName),
            Core.Just ("deploymentGroupNames" Core..= deploymentGroupNames)
          ]
      )

instance Core.AWSRequest BatchGetDeploymentGroups where
  type Rs BatchGetDeploymentGroups = BatchGetDeploymentGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.BatchGetDeploymentGroups")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetDeploymentGroupsResponse'
            Core.<$> (x Core..:? "deploymentGroupsInfo")
            Core.<*> (x Core..:? "errorMessage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @BatchGetDeploymentGroups@ operation.
--
-- /See:/ 'mkBatchGetDeploymentGroupsResponse' smart constructor.
data BatchGetDeploymentGroupsResponse = BatchGetDeploymentGroupsResponse'
  { -- | Information about the deployment groups.
    deploymentGroupsInfo :: Core.Maybe [Types.DeploymentGroupInfo],
    -- | Information about errors that might have occurred during the API call.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchGetDeploymentGroupsResponse' value with any optional fields omitted.
mkBatchGetDeploymentGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchGetDeploymentGroupsResponse
mkBatchGetDeploymentGroupsResponse responseStatus =
  BatchGetDeploymentGroupsResponse'
    { deploymentGroupsInfo =
        Core.Nothing,
      errorMessage = Core.Nothing,
      responseStatus
    }

-- | Information about the deployment groups.
--
-- /Note:/ Consider using 'deploymentGroupsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdgrrsDeploymentGroupsInfo :: Lens.Lens' BatchGetDeploymentGroupsResponse (Core.Maybe [Types.DeploymentGroupInfo])
bgdgrrsDeploymentGroupsInfo = Lens.field @"deploymentGroupsInfo"
{-# DEPRECATED bgdgrrsDeploymentGroupsInfo "Use generic-lens or generic-optics with 'deploymentGroupsInfo' instead." #-}

-- | Information about errors that might have occurred during the API call.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdgrrsErrorMessage :: Lens.Lens' BatchGetDeploymentGroupsResponse (Core.Maybe Types.ErrorMessage)
bgdgrrsErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED bgdgrrsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdgrrsResponseStatus :: Lens.Lens' BatchGetDeploymentGroupsResponse Core.Int
bgdgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bgdgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
