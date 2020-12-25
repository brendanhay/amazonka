{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.BulkDeploymentResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.BulkDeploymentResult
  ( BulkDeploymentResult (..),

    -- * Smart constructor
    mkBulkDeploymentResult,

    -- * Lenses
    bdrCreatedAt,
    bdrDeploymentArn,
    bdrDeploymentId,
    bdrDeploymentStatus,
    bdrDeploymentType,
    bdrErrorDetails,
    bdrErrorMessage,
    bdrGroupArn,
  )
where

import qualified Network.AWS.Greengrass.Types.DeploymentType as Types
import qualified Network.AWS.Greengrass.Types.ErrorDetail as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an individual group deployment in a bulk deployment operation.
--
-- /See:/ 'mkBulkDeploymentResult' smart constructor.
data BulkDeploymentResult = BulkDeploymentResult'
  { -- | The time, in ISO format, when the deployment was created.
    createdAt :: Core.Maybe Core.Text,
    -- | The ARN of the group deployment.
    deploymentArn :: Core.Maybe Core.Text,
    -- | The ID of the group deployment.
    deploymentId :: Core.Maybe Core.Text,
    -- | The current status of the group deployment: ''InProgress'', ''Building'', ''Success'', or ''Failure''.
    deploymentStatus :: Core.Maybe Core.Text,
    -- | The type of the deployment.
    deploymentType :: Core.Maybe Types.DeploymentType,
    -- | Details about the error.
    errorDetails :: Core.Maybe [Types.ErrorDetail],
    -- | The error message for a failed deployment
    errorMessage :: Core.Maybe Core.Text,
    -- | The ARN of the Greengrass group.
    groupArn :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BulkDeploymentResult' value with any optional fields omitted.
mkBulkDeploymentResult ::
  BulkDeploymentResult
mkBulkDeploymentResult =
  BulkDeploymentResult'
    { createdAt = Core.Nothing,
      deploymentArn = Core.Nothing,
      deploymentId = Core.Nothing,
      deploymentStatus = Core.Nothing,
      deploymentType = Core.Nothing,
      errorDetails = Core.Nothing,
      errorMessage = Core.Nothing,
      groupArn = Core.Nothing
    }

-- | The time, in ISO format, when the deployment was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrCreatedAt :: Lens.Lens' BulkDeploymentResult (Core.Maybe Core.Text)
bdrCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED bdrCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The ARN of the group deployment.
--
-- /Note:/ Consider using 'deploymentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrDeploymentArn :: Lens.Lens' BulkDeploymentResult (Core.Maybe Core.Text)
bdrDeploymentArn = Lens.field @"deploymentArn"
{-# DEPRECATED bdrDeploymentArn "Use generic-lens or generic-optics with 'deploymentArn' instead." #-}

-- | The ID of the group deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrDeploymentId :: Lens.Lens' BulkDeploymentResult (Core.Maybe Core.Text)
bdrDeploymentId = Lens.field @"deploymentId"
{-# DEPRECATED bdrDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The current status of the group deployment: ''InProgress'', ''Building'', ''Success'', or ''Failure''.
--
-- /Note:/ Consider using 'deploymentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrDeploymentStatus :: Lens.Lens' BulkDeploymentResult (Core.Maybe Core.Text)
bdrDeploymentStatus = Lens.field @"deploymentStatus"
{-# DEPRECATED bdrDeploymentStatus "Use generic-lens or generic-optics with 'deploymentStatus' instead." #-}

-- | The type of the deployment.
--
-- /Note:/ Consider using 'deploymentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrDeploymentType :: Lens.Lens' BulkDeploymentResult (Core.Maybe Types.DeploymentType)
bdrDeploymentType = Lens.field @"deploymentType"
{-# DEPRECATED bdrDeploymentType "Use generic-lens or generic-optics with 'deploymentType' instead." #-}

-- | Details about the error.
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrErrorDetails :: Lens.Lens' BulkDeploymentResult (Core.Maybe [Types.ErrorDetail])
bdrErrorDetails = Lens.field @"errorDetails"
{-# DEPRECATED bdrErrorDetails "Use generic-lens or generic-optics with 'errorDetails' instead." #-}

-- | The error message for a failed deployment
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrErrorMessage :: Lens.Lens' BulkDeploymentResult (Core.Maybe Core.Text)
bdrErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED bdrErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The ARN of the Greengrass group.
--
-- /Note:/ Consider using 'groupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrGroupArn :: Lens.Lens' BulkDeploymentResult (Core.Maybe Core.Text)
bdrGroupArn = Lens.field @"groupArn"
{-# DEPRECATED bdrGroupArn "Use generic-lens or generic-optics with 'groupArn' instead." #-}

instance Core.FromJSON BulkDeploymentResult where
  parseJSON =
    Core.withObject "BulkDeploymentResult" Core.$
      \x ->
        BulkDeploymentResult'
          Core.<$> (x Core..:? "CreatedAt")
          Core.<*> (x Core..:? "DeploymentArn")
          Core.<*> (x Core..:? "DeploymentId")
          Core.<*> (x Core..:? "DeploymentStatus")
          Core.<*> (x Core..:? "DeploymentType")
          Core.<*> (x Core..:? "ErrorDetails")
          Core.<*> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "GroupArn")
