{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.DeploymentController
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DeploymentController
  ( DeploymentController (..),

    -- * Smart constructor
    mkDeploymentController,

    -- * Lenses
    dcType,
  )
where

import qualified Network.AWS.ECS.Types.DeploymentControllerType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The deployment controller to use for the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkDeploymentController' smart constructor.
newtype DeploymentController = DeploymentController'
  { -- | The deployment controller type to use.
    --
    -- There are three deployment controller types available:
    --
    --     * ECS
    --
    --     * The rolling update (@ECS@ ) deployment type involves replacing the current running version of the container with the latest version. The number of containers Amazon ECS adds or removes from the service during a rolling update is controlled by adjusting the minimum and maximum number of healthy tasks allowed during a service deployment, as specified in the 'DeploymentConfiguration' .
    --
    --
    --     * CODE_DEPLOY
    --
    --     * The blue/green (@CODE_DEPLOY@ ) deployment type uses the blue/green deployment model powered by AWS CodeDeploy, which allows you to verify a new deployment of a service before sending production traffic to it.
    --
    --
    --     * EXTERNAL
    --
    --     * The external (@EXTERNAL@ ) deployment type enables you to use any third-party deployment controller for full control over the deployment process for an Amazon ECS service.
    type' :: Types.DeploymentControllerType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeploymentController' value with any optional fields omitted.
mkDeploymentController ::
  -- | 'type\''
  Types.DeploymentControllerType ->
  DeploymentController
mkDeploymentController type' = DeploymentController' {type'}

-- | The deployment controller type to use.
--
-- There are three deployment controller types available:
--
--     * ECS
--
--     * The rolling update (@ECS@ ) deployment type involves replacing the current running version of the container with the latest version. The number of containers Amazon ECS adds or removes from the service during a rolling update is controlled by adjusting the minimum and maximum number of healthy tasks allowed during a service deployment, as specified in the 'DeploymentConfiguration' .
--
--
--     * CODE_DEPLOY
--
--     * The blue/green (@CODE_DEPLOY@ ) deployment type uses the blue/green deployment model powered by AWS CodeDeploy, which allows you to verify a new deployment of a service before sending production traffic to it.
--
--
--     * EXTERNAL
--
--     * The external (@EXTERNAL@ ) deployment type enables you to use any third-party deployment controller for full control over the deployment process for an Amazon ECS service.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcType :: Lens.Lens' DeploymentController Types.DeploymentControllerType
dcType = Lens.field @"type'"
{-# DEPRECATED dcType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON DeploymentController where
  toJSON DeploymentController {..} =
    Core.object (Core.catMaybes [Core.Just ("type" Core..= type')])

instance Core.FromJSON DeploymentController where
  parseJSON =
    Core.withObject "DeploymentController" Core.$
      \x -> DeploymentController' Core.<$> (x Core..: "type")
