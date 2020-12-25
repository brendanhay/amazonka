{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment. ''CreateDeployment'' requests are idempotent with respect to the ''X-Amzn-Client-Token'' token and the request parameters.
module Network.AWS.Greengrass.CreateDeployment
  ( -- * Creating a request
    CreateDeployment (..),
    mkCreateDeployment,

    -- ** Request lenses
    cdGroupId,
    cdDeploymentType,
    cdAmznClientToken,
    cdDeploymentId,
    cdGroupVersionId,

    -- * Destructuring the response
    CreateDeploymentResponse (..),
    mkCreateDeploymentResponse,

    -- ** Response lenses
    cdrrsDeploymentArn,
    cdrrsDeploymentId,
    cdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { -- | The ID of the Greengrass group.
    groupId :: Core.Text,
    -- | The type of deployment. When used for ''CreateDeployment'', only ''NewDeployment'' and ''Redeployment'' are valid.
    deploymentType :: Types.DeploymentType,
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | The ID of the deployment if you wish to redeploy a previous deployment.
    deploymentId :: Core.Maybe Core.Text,
    -- | The ID of the group version to be deployed.
    groupVersionId :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDeployment' value with any optional fields omitted.
mkCreateDeployment ::
  -- | 'groupId'
  Core.Text ->
  -- | 'deploymentType'
  Types.DeploymentType ->
  CreateDeployment
mkCreateDeployment groupId deploymentType =
  CreateDeployment'
    { groupId,
      deploymentType,
      amznClientToken = Core.Nothing,
      deploymentId = Core.Nothing,
      groupVersionId = Core.Nothing
    }

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdGroupId :: Lens.Lens' CreateDeployment Core.Text
cdGroupId = Lens.field @"groupId"
{-# DEPRECATED cdGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The type of deployment. When used for ''CreateDeployment'', only ''NewDeployment'' and ''Redeployment'' are valid.
--
-- /Note:/ Consider using 'deploymentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeploymentType :: Lens.Lens' CreateDeployment Types.DeploymentType
cdDeploymentType = Lens.field @"deploymentType"
{-# DEPRECATED cdDeploymentType "Use generic-lens or generic-optics with 'deploymentType' instead." #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAmznClientToken :: Lens.Lens' CreateDeployment (Core.Maybe Core.Text)
cdAmznClientToken = Lens.field @"amznClientToken"
{-# DEPRECATED cdAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | The ID of the deployment if you wish to redeploy a previous deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeploymentId :: Lens.Lens' CreateDeployment (Core.Maybe Core.Text)
cdDeploymentId = Lens.field @"deploymentId"
{-# DEPRECATED cdDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The ID of the group version to be deployed.
--
-- /Note:/ Consider using 'groupVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdGroupVersionId :: Lens.Lens' CreateDeployment (Core.Maybe Core.Text)
cdGroupVersionId = Lens.field @"groupVersionId"
{-# DEPRECATED cdGroupVersionId "Use generic-lens or generic-optics with 'groupVersionId' instead." #-}

instance Core.FromJSON CreateDeployment where
  toJSON CreateDeployment {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DeploymentType" Core..= deploymentType),
            ("DeploymentId" Core..=) Core.<$> deploymentId,
            ("GroupVersionId" Core..=) Core.<$> groupVersionId
          ]
      )

instance Core.AWSRequest CreateDeployment where
  type Rs CreateDeployment = CreateDeploymentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/groups/" Core.<> (Core.toText groupId)
                Core.<> ("/deployments")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "X-Amzn-Client-Token" amznClientToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeploymentResponse'
            Core.<$> (x Core..:? "DeploymentArn")
            Core.<*> (x Core..:? "DeploymentId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { -- | The ARN of the deployment.
    deploymentArn :: Core.Maybe Core.Text,
    -- | The ID of the deployment.
    deploymentId :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDeploymentResponse' value with any optional fields omitted.
mkCreateDeploymentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDeploymentResponse
mkCreateDeploymentResponse responseStatus =
  CreateDeploymentResponse'
    { deploymentArn = Core.Nothing,
      deploymentId = Core.Nothing,
      responseStatus
    }

-- | The ARN of the deployment.
--
-- /Note:/ Consider using 'deploymentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsDeploymentArn :: Lens.Lens' CreateDeploymentResponse (Core.Maybe Core.Text)
cdrrsDeploymentArn = Lens.field @"deploymentArn"
{-# DEPRECATED cdrrsDeploymentArn "Use generic-lens or generic-optics with 'deploymentArn' instead." #-}

-- | The ID of the deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsDeploymentId :: Lens.Lens' CreateDeploymentResponse (Core.Maybe Core.Text)
cdrrsDeploymentId = Lens.field @"deploymentId"
{-# DEPRECATED cdrrsDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' CreateDeploymentResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
