{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Deployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.Deployment
  ( Deployment (..)
  -- * Smart constructor
  , mkDeployment
  -- * Lenses
  , dCreatedAt
  , dDeploymentArn
  , dDeploymentId
  , dDeploymentType
  , dGroupArn
  ) where

import qualified Network.AWS.Greengrass.Types.DeploymentType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a deployment.
--
-- /See:/ 'mkDeployment' smart constructor.
data Deployment = Deployment'
  { createdAt :: Core.Maybe Core.Text
    -- ^ The time, in milliseconds since the epoch, when the deployment was created.
  , deploymentArn :: Core.Maybe Core.Text
    -- ^ The ARN of the deployment.
  , deploymentId :: Core.Maybe Core.Text
    -- ^ The ID of the deployment.
  , deploymentType :: Core.Maybe Types.DeploymentType
    -- ^ The type of the deployment.
  , groupArn :: Core.Maybe Core.Text
    -- ^ The ARN of the group for this deployment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Deployment' value with any optional fields omitted.
mkDeployment
    :: Deployment
mkDeployment
  = Deployment'{createdAt = Core.Nothing,
                deploymentArn = Core.Nothing, deploymentId = Core.Nothing,
                deploymentType = Core.Nothing, groupArn = Core.Nothing}

-- | The time, in milliseconds since the epoch, when the deployment was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreatedAt :: Lens.Lens' Deployment (Core.Maybe Core.Text)
dCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE dCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The ARN of the deployment.
--
-- /Note:/ Consider using 'deploymentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeploymentArn :: Lens.Lens' Deployment (Core.Maybe Core.Text)
dDeploymentArn = Lens.field @"deploymentArn"
{-# INLINEABLE dDeploymentArn #-}
{-# DEPRECATED deploymentArn "Use generic-lens or generic-optics with 'deploymentArn' instead"  #-}

-- | The ID of the deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeploymentId :: Lens.Lens' Deployment (Core.Maybe Core.Text)
dDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE dDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

-- | The type of the deployment.
--
-- /Note:/ Consider using 'deploymentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeploymentType :: Lens.Lens' Deployment (Core.Maybe Types.DeploymentType)
dDeploymentType = Lens.field @"deploymentType"
{-# INLINEABLE dDeploymentType #-}
{-# DEPRECATED deploymentType "Use generic-lens or generic-optics with 'deploymentType' instead"  #-}

-- | The ARN of the group for this deployment.
--
-- /Note:/ Consider using 'groupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGroupArn :: Lens.Lens' Deployment (Core.Maybe Core.Text)
dGroupArn = Lens.field @"groupArn"
{-# INLINEABLE dGroupArn #-}
{-# DEPRECATED groupArn "Use generic-lens or generic-optics with 'groupArn' instead"  #-}

instance Core.FromJSON Deployment where
        parseJSON
          = Core.withObject "Deployment" Core.$
              \ x ->
                Deployment' Core.<$>
                  (x Core..:? "CreatedAt") Core.<*> x Core..:? "DeploymentArn"
                    Core.<*> x Core..:? "DeploymentId"
                    Core.<*> x Core..:? "DeploymentType"
                    Core.<*> x Core..:? "GroupArn"
