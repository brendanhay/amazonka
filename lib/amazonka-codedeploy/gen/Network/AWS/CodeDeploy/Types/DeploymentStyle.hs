{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentStyle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentStyle
  ( DeploymentStyle (..),

    -- * Smart constructor
    mkDeploymentStyle,

    -- * Lenses
    dsDeploymentOption,
    dsDeploymentType,
  )
where

import Network.AWS.CodeDeploy.Types.DeploymentOption
import Network.AWS.CodeDeploy.Types.DeploymentType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
--
-- /See:/ 'mkDeploymentStyle' smart constructor.
data DeploymentStyle = DeploymentStyle'
  { -- | Indicates whether to route deployment traffic behind a load balancer.
    deploymentOption :: Lude.Maybe DeploymentOption,
    -- | Indicates whether to run an in-place deployment or a blue/green deployment.
    deploymentType :: Lude.Maybe DeploymentType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeploymentStyle' with the minimum fields required to make a request.
--
-- * 'deploymentOption' - Indicates whether to route deployment traffic behind a load balancer.
-- * 'deploymentType' - Indicates whether to run an in-place deployment or a blue/green deployment.
mkDeploymentStyle ::
  DeploymentStyle
mkDeploymentStyle =
  DeploymentStyle'
    { deploymentOption = Lude.Nothing,
      deploymentType = Lude.Nothing
    }

-- | Indicates whether to route deployment traffic behind a load balancer.
--
-- /Note:/ Consider using 'deploymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDeploymentOption :: Lens.Lens' DeploymentStyle (Lude.Maybe DeploymentOption)
dsDeploymentOption = Lens.lens (deploymentOption :: DeploymentStyle -> Lude.Maybe DeploymentOption) (\s a -> s {deploymentOption = a} :: DeploymentStyle)
{-# DEPRECATED dsDeploymentOption "Use generic-lens or generic-optics with 'deploymentOption' instead." #-}

-- | Indicates whether to run an in-place deployment or a blue/green deployment.
--
-- /Note:/ Consider using 'deploymentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDeploymentType :: Lens.Lens' DeploymentStyle (Lude.Maybe DeploymentType)
dsDeploymentType = Lens.lens (deploymentType :: DeploymentStyle -> Lude.Maybe DeploymentType) (\s a -> s {deploymentType = a} :: DeploymentStyle)
{-# DEPRECATED dsDeploymentType "Use generic-lens or generic-optics with 'deploymentType' instead." #-}

instance Lude.FromJSON DeploymentStyle where
  parseJSON =
    Lude.withObject
      "DeploymentStyle"
      ( \x ->
          DeploymentStyle'
            Lude.<$> (x Lude..:? "deploymentOption")
            Lude.<*> (x Lude..:? "deploymentType")
      )

instance Lude.ToJSON DeploymentStyle where
  toJSON DeploymentStyle' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("deploymentOption" Lude..=) Lude.<$> deploymentOption,
            ("deploymentType" Lude..=) Lude.<$> deploymentType
          ]
      )
