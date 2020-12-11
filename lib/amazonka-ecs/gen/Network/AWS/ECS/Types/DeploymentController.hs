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

import Network.AWS.ECS.Types.DeploymentControllerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The deployment controller to use for the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkDeploymentController' smart constructor.
newtype DeploymentController = DeploymentController'
  { type' ::
      DeploymentControllerType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeploymentController' with the minimum fields required to make a request.
--
-- * 'type'' - The deployment controller type to use.
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
mkDeploymentController ::
  -- | 'type''
  DeploymentControllerType ->
  DeploymentController
mkDeploymentController pType_ =
  DeploymentController' {type' = pType_}

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
dcType :: Lens.Lens' DeploymentController DeploymentControllerType
dcType = Lens.lens (type' :: DeploymentController -> DeploymentControllerType) (\s a -> s {type' = a} :: DeploymentController)
{-# DEPRECATED dcType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON DeploymentController where
  parseJSON =
    Lude.withObject
      "DeploymentController"
      (\x -> DeploymentController' Lude.<$> (x Lude..: "type"))

instance Lude.ToJSON DeploymentController where
  toJSON DeploymentController' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("type" Lude..= type')])
