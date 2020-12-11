-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceDeployment
  ( ContainerServiceDeployment (..),

    -- * Smart constructor
    mkContainerServiceDeployment,

    -- * Lenses
    csdState,
    csdPublicEndpoint,
    csdCreatedAt,
    csdContainers,
    csdVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.Container
import Network.AWS.Lightsail.Types.ContainerServiceDeploymentState
import Network.AWS.Lightsail.Types.ContainerServiceEndpoint
import qualified Network.AWS.Prelude as Lude

-- | Describes a container deployment configuration of an Amazon Lightsail container service.
--
-- A deployment specifies the settings, such as the ports and launch command, of containers that are deployed to your container service.
--
-- /See:/ 'mkContainerServiceDeployment' smart constructor.
data ContainerServiceDeployment = ContainerServiceDeployment'
  { state ::
      Lude.Maybe
        ContainerServiceDeploymentState,
    publicEndpoint ::
      Lude.Maybe ContainerServiceEndpoint,
    createdAt ::
      Lude.Maybe Lude.Timestamp,
    containers ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Container)
        ),
    version :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerServiceDeployment' with the minimum fields required to make a request.
--
-- * 'containers' - An object that describes the configuration for the containers of the deployment.
-- * 'createdAt' - The timestamp when the deployment was created.
-- * 'publicEndpoint' - An object that describes the endpoint of the deployment.
-- * 'state' - The state of the deployment.
--
-- A deployment can be in one of the following states:
--
--     * @Activating@ - The deployment is being created.
--
--
--     * @Active@ - The deployment was successfully created, and it's currently running on the container service. The container service can have only one deployment in an active state at a time.
--
--
--     * @Inactive@ - The deployment was previously successfully created, but it is not currently running on the container service.
--
--
--     * @Failed@ - The deployment failed. Use the @GetContainerLog@ action to view the log events for the containers in the deployment to try to determine the reason for the failure.
--
--
-- * 'version' - The version number of the deployment.
mkContainerServiceDeployment ::
  ContainerServiceDeployment
mkContainerServiceDeployment =
  ContainerServiceDeployment'
    { state = Lude.Nothing,
      publicEndpoint = Lude.Nothing,
      createdAt = Lude.Nothing,
      containers = Lude.Nothing,
      version = Lude.Nothing
    }

-- | The state of the deployment.
--
-- A deployment can be in one of the following states:
--
--     * @Activating@ - The deployment is being created.
--
--
--     * @Active@ - The deployment was successfully created, and it's currently running on the container service. The container service can have only one deployment in an active state at a time.
--
--
--     * @Inactive@ - The deployment was previously successfully created, but it is not currently running on the container service.
--
--
--     * @Failed@ - The deployment failed. Use the @GetContainerLog@ action to view the log events for the containers in the deployment to try to determine the reason for the failure.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdState :: Lens.Lens' ContainerServiceDeployment (Lude.Maybe ContainerServiceDeploymentState)
csdState = Lens.lens (state :: ContainerServiceDeployment -> Lude.Maybe ContainerServiceDeploymentState) (\s a -> s {state = a} :: ContainerServiceDeployment)
{-# DEPRECATED csdState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | An object that describes the endpoint of the deployment.
--
-- /Note:/ Consider using 'publicEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdPublicEndpoint :: Lens.Lens' ContainerServiceDeployment (Lude.Maybe ContainerServiceEndpoint)
csdPublicEndpoint = Lens.lens (publicEndpoint :: ContainerServiceDeployment -> Lude.Maybe ContainerServiceEndpoint) (\s a -> s {publicEndpoint = a} :: ContainerServiceDeployment)
{-# DEPRECATED csdPublicEndpoint "Use generic-lens or generic-optics with 'publicEndpoint' instead." #-}

-- | The timestamp when the deployment was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdCreatedAt :: Lens.Lens' ContainerServiceDeployment (Lude.Maybe Lude.Timestamp)
csdCreatedAt = Lens.lens (createdAt :: ContainerServiceDeployment -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: ContainerServiceDeployment)
{-# DEPRECATED csdCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | An object that describes the configuration for the containers of the deployment.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdContainers :: Lens.Lens' ContainerServiceDeployment (Lude.Maybe (Lude.HashMap Lude.Text (Container)))
csdContainers = Lens.lens (containers :: ContainerServiceDeployment -> Lude.Maybe (Lude.HashMap Lude.Text (Container))) (\s a -> s {containers = a} :: ContainerServiceDeployment)
{-# DEPRECATED csdContainers "Use generic-lens or generic-optics with 'containers' instead." #-}

-- | The version number of the deployment.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdVersion :: Lens.Lens' ContainerServiceDeployment (Lude.Maybe Lude.Int)
csdVersion = Lens.lens (version :: ContainerServiceDeployment -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: ContainerServiceDeployment)
{-# DEPRECATED csdVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON ContainerServiceDeployment where
  parseJSON =
    Lude.withObject
      "ContainerServiceDeployment"
      ( \x ->
          ContainerServiceDeployment'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "publicEndpoint")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "containers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "version")
      )
