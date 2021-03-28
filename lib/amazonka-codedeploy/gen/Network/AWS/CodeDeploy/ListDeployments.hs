{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListDeployments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployments in a deployment group for an application registered with the IAM user or AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeployments
    (
    -- * Creating a request
      ListDeployments (..)
    , mkListDeployments
    -- ** Request lenses
    , ldApplicationName
    , ldCreateTimeRange
    , ldDeploymentGroupName
    , ldExternalId
    , ldIncludeOnlyStatuses
    , ldNextToken

    -- * Destructuring the response
    , ListDeploymentsResponse (..)
    , mkListDeploymentsResponse
    -- ** Response lenses
    , ldrrsDeployments
    , ldrrsNextToken
    , ldrrsResponseStatus
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListDeployments@ operation.
--
-- /See:/ 'mkListDeployments' smart constructor.
data ListDeployments = ListDeployments'
  { applicationName :: Core.Maybe Types.ApplicationName
    -- ^ The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
  , createTimeRange :: Core.Maybe Types.TimeRange
    -- ^ A time range (start and end) for returning a subset of the list of deployments.
  , deploymentGroupName :: Core.Maybe Types.DeploymentGroupName
    -- ^ The name of a deployment group for the specified application.
  , externalId :: Core.Maybe Types.ExternalId
    -- ^ The unique ID of an external resource for returning deployments linked to the external resource.
  , includeOnlyStatuses :: Core.Maybe [Types.DeploymentStatus]
    -- ^ A subset of deployments to list by status:
--
--
--     * @Created@ : Include created deployments in the resulting list.
--
--
--     * @Queued@ : Include queued deployments in the resulting list.
--
--
--     * @In Progress@ : Include in-progress deployments in the resulting list.
--
--
--     * @Succeeded@ : Include successful deployments in the resulting list.
--
--
--     * @Failed@ : Include failed deployments in the resulting list.
--
--
--     * @Stopped@ : Include stopped deployments in the resulting list.
--
--
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An identifier returned from the previous list deployments call. It can be used to return the next set of deployments in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListDeployments' value with any optional fields omitted.
mkListDeployments
    :: ListDeployments
mkListDeployments
  = ListDeployments'{applicationName = Core.Nothing,
                     createTimeRange = Core.Nothing, deploymentGroupName = Core.Nothing,
                     externalId = Core.Nothing, includeOnlyStatuses = Core.Nothing,
                     nextToken = Core.Nothing}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldApplicationName :: Lens.Lens' ListDeployments (Core.Maybe Types.ApplicationName)
ldApplicationName = Lens.field @"applicationName"
{-# INLINEABLE ldApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | A time range (start and end) for returning a subset of the list of deployments.
--
-- /Note:/ Consider using 'createTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCreateTimeRange :: Lens.Lens' ListDeployments (Core.Maybe Types.TimeRange)
ldCreateTimeRange = Lens.field @"createTimeRange"
{-# INLINEABLE ldCreateTimeRange #-}
{-# DEPRECATED createTimeRange "Use generic-lens or generic-optics with 'createTimeRange' instead"  #-}

-- | The name of a deployment group for the specified application.
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldDeploymentGroupName :: Lens.Lens' ListDeployments (Core.Maybe Types.DeploymentGroupName)
ldDeploymentGroupName = Lens.field @"deploymentGroupName"
{-# INLINEABLE ldDeploymentGroupName #-}
{-# DEPRECATED deploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead"  #-}

-- | The unique ID of an external resource for returning deployments linked to the external resource.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldExternalId :: Lens.Lens' ListDeployments (Core.Maybe Types.ExternalId)
ldExternalId = Lens.field @"externalId"
{-# INLINEABLE ldExternalId #-}
{-# DEPRECATED externalId "Use generic-lens or generic-optics with 'externalId' instead"  #-}

-- | A subset of deployments to list by status:
--
--
--     * @Created@ : Include created deployments in the resulting list.
--
--
--     * @Queued@ : Include queued deployments in the resulting list.
--
--
--     * @In Progress@ : Include in-progress deployments in the resulting list.
--
--
--     * @Succeeded@ : Include successful deployments in the resulting list.
--
--
--     * @Failed@ : Include failed deployments in the resulting list.
--
--
--     * @Stopped@ : Include stopped deployments in the resulting list.
--
--
--
-- /Note:/ Consider using 'includeOnlyStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldIncludeOnlyStatuses :: Lens.Lens' ListDeployments (Core.Maybe [Types.DeploymentStatus])
ldIncludeOnlyStatuses = Lens.field @"includeOnlyStatuses"
{-# INLINEABLE ldIncludeOnlyStatuses #-}
{-# DEPRECATED includeOnlyStatuses "Use generic-lens or generic-optics with 'includeOnlyStatuses' instead"  #-}

-- | An identifier returned from the previous list deployments call. It can be used to return the next set of deployments in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDeployments (Core.Maybe Types.NextToken)
ldNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListDeployments where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListDeployments where
        toHeaders ListDeployments{..}
          = Core.pure ("X-Amz-Target", "CodeDeploy_20141006.ListDeployments")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListDeployments where
        toJSON ListDeployments{..}
          = Core.object
              (Core.catMaybes
                 [("applicationName" Core..=) Core.<$> applicationName,
                  ("createTimeRange" Core..=) Core.<$> createTimeRange,
                  ("deploymentGroupName" Core..=) Core.<$> deploymentGroupName,
                  ("externalId" Core..=) Core.<$> externalId,
                  ("includeOnlyStatuses" Core..=) Core.<$> includeOnlyStatuses,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListDeployments where
        type Rs ListDeployments = ListDeploymentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDeploymentsResponse' Core.<$>
                   (x Core..:? "deployments") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListDeployments where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"deployments" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the output of a @ListDeployments@ operation.
--
-- /See:/ 'mkListDeploymentsResponse' smart constructor.
data ListDeploymentsResponse = ListDeploymentsResponse'
  { deployments :: Core.Maybe [Types.DeploymentId]
    -- ^ A list of deployment IDs.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployments call to return the next set of deployments in the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeploymentsResponse' value with any optional fields omitted.
mkListDeploymentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDeploymentsResponse
mkListDeploymentsResponse responseStatus
  = ListDeploymentsResponse'{deployments = Core.Nothing,
                             nextToken = Core.Nothing, responseStatus}

-- | A list of deployment IDs.
--
-- /Note:/ Consider using 'deployments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsDeployments :: Lens.Lens' ListDeploymentsResponse (Core.Maybe [Types.DeploymentId])
ldrrsDeployments = Lens.field @"deployments"
{-# INLINEABLE ldrrsDeployments #-}
{-# DEPRECATED deployments "Use generic-lens or generic-optics with 'deployments' instead"  #-}

-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployments call to return the next set of deployments in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsNextToken :: Lens.Lens' ListDeploymentsResponse (Core.Maybe Types.NextToken)
ldrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDeploymentsResponse Core.Int
ldrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
