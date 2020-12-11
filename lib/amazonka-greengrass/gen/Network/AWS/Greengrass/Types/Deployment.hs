-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Deployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Deployment
  ( Deployment (..),

    -- * Smart constructor
    mkDeployment,

    -- * Lenses
    dDeploymentId,
    dDeploymentARN,
    dCreatedAt,
    dDeploymentType,
    dGroupARN,
  )
where

import Network.AWS.Greengrass.Types.DeploymentType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a deployment.
--
-- /See:/ 'mkDeployment' smart constructor.
data Deployment = Deployment'
  { deploymentId :: Lude.Maybe Lude.Text,
    deploymentARN :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Text,
    deploymentType :: Lude.Maybe DeploymentType,
    groupARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- * 'createdAt' - The time, in milliseconds since the epoch, when the deployment was created.
-- * 'deploymentARN' - The ARN of the deployment.
-- * 'deploymentId' - The ID of the deployment.
-- * 'deploymentType' - The type of the deployment.
-- * 'groupARN' - The ARN of the group for this deployment.
mkDeployment ::
  Deployment
mkDeployment =
  Deployment'
    { deploymentId = Lude.Nothing,
      deploymentARN = Lude.Nothing,
      createdAt = Lude.Nothing,
      deploymentType = Lude.Nothing,
      groupARN = Lude.Nothing
    }

-- | The ID of the deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeploymentId :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dDeploymentId = Lens.lens (deploymentId :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: Deployment)
{-# DEPRECATED dDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The ARN of the deployment.
--
-- /Note:/ Consider using 'deploymentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeploymentARN :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dDeploymentARN = Lens.lens (deploymentARN :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {deploymentARN = a} :: Deployment)
{-# DEPRECATED dDeploymentARN "Use generic-lens or generic-optics with 'deploymentARN' instead." #-}

-- | The time, in milliseconds since the epoch, when the deployment was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreatedAt :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dCreatedAt = Lens.lens (createdAt :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: Deployment)
{-# DEPRECATED dCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The type of the deployment.
--
-- /Note:/ Consider using 'deploymentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeploymentType :: Lens.Lens' Deployment (Lude.Maybe DeploymentType)
dDeploymentType = Lens.lens (deploymentType :: Deployment -> Lude.Maybe DeploymentType) (\s a -> s {deploymentType = a} :: Deployment)
{-# DEPRECATED dDeploymentType "Use generic-lens or generic-optics with 'deploymentType' instead." #-}

-- | The ARN of the group for this deployment.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGroupARN :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dGroupARN = Lens.lens (groupARN :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {groupARN = a} :: Deployment)
{-# DEPRECATED dGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

instance Lude.FromJSON Deployment where
  parseJSON =
    Lude.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Lude.<$> (x Lude..:? "DeploymentId")
            Lude.<*> (x Lude..:? "DeploymentArn")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "DeploymentType")
            Lude.<*> (x Lude..:? "GroupArn")
      )
