{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceDeployment where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.Container
import Network.AWS.Lightsail.Types.ContainerServiceDeploymentState
import Network.AWS.Lightsail.Types.ContainerServiceEndpoint
import qualified Network.AWS.Prelude as Prelude

-- | Describes a container deployment configuration of an Amazon Lightsail
-- container service.
--
-- A deployment specifies the settings, such as the ports and launch
-- command, of containers that are deployed to your container service.
--
-- /See:/ 'newContainerServiceDeployment' smart constructor.
data ContainerServiceDeployment = ContainerServiceDeployment'
  { -- | The timestamp when the deployment was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The version number of the deployment.
    version :: Prelude.Maybe Prelude.Int,
    -- | An object that describes the endpoint of the deployment.
    publicEndpoint :: Prelude.Maybe ContainerServiceEndpoint,
    -- | The state of the deployment.
    --
    -- A deployment can be in one of the following states:
    --
    -- -   @Activating@ - The deployment is being created.
    --
    -- -   @Active@ - The deployment was successfully created, and it\'s
    --     currently running on the container service. The container service
    --     can have only one deployment in an active state at a time.
    --
    -- -   @Inactive@ - The deployment was previously successfully created, but
    --     it is not currently running on the container service.
    --
    -- -   @Failed@ - The deployment failed. Use the @GetContainerLog@ action
    --     to view the log events for the containers in the deployment to try
    --     to determine the reason for the failure.
    state :: Prelude.Maybe ContainerServiceDeploymentState,
    -- | An object that describes the configuration for the containers of the
    -- deployment.
    containers :: Prelude.Maybe (Prelude.HashMap Prelude.Text Container)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContainerServiceDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'containerServiceDeployment_createdAt' - The timestamp when the deployment was created.
--
-- 'version', 'containerServiceDeployment_version' - The version number of the deployment.
--
-- 'publicEndpoint', 'containerServiceDeployment_publicEndpoint' - An object that describes the endpoint of the deployment.
--
-- 'state', 'containerServiceDeployment_state' - The state of the deployment.
--
-- A deployment can be in one of the following states:
--
-- -   @Activating@ - The deployment is being created.
--
-- -   @Active@ - The deployment was successfully created, and it\'s
--     currently running on the container service. The container service
--     can have only one deployment in an active state at a time.
--
-- -   @Inactive@ - The deployment was previously successfully created, but
--     it is not currently running on the container service.
--
-- -   @Failed@ - The deployment failed. Use the @GetContainerLog@ action
--     to view the log events for the containers in the deployment to try
--     to determine the reason for the failure.
--
-- 'containers', 'containerServiceDeployment_containers' - An object that describes the configuration for the containers of the
-- deployment.
newContainerServiceDeployment ::
  ContainerServiceDeployment
newContainerServiceDeployment =
  ContainerServiceDeployment'
    { createdAt =
        Prelude.Nothing,
      version = Prelude.Nothing,
      publicEndpoint = Prelude.Nothing,
      state = Prelude.Nothing,
      containers = Prelude.Nothing
    }

-- | The timestamp when the deployment was created.
containerServiceDeployment_createdAt :: Lens.Lens' ContainerServiceDeployment (Prelude.Maybe Prelude.UTCTime)
containerServiceDeployment_createdAt = Lens.lens (\ContainerServiceDeployment' {createdAt} -> createdAt) (\s@ContainerServiceDeployment' {} a -> s {createdAt = a} :: ContainerServiceDeployment) Prelude.. Lens.mapping Prelude._Time

-- | The version number of the deployment.
containerServiceDeployment_version :: Lens.Lens' ContainerServiceDeployment (Prelude.Maybe Prelude.Int)
containerServiceDeployment_version = Lens.lens (\ContainerServiceDeployment' {version} -> version) (\s@ContainerServiceDeployment' {} a -> s {version = a} :: ContainerServiceDeployment)

-- | An object that describes the endpoint of the deployment.
containerServiceDeployment_publicEndpoint :: Lens.Lens' ContainerServiceDeployment (Prelude.Maybe ContainerServiceEndpoint)
containerServiceDeployment_publicEndpoint = Lens.lens (\ContainerServiceDeployment' {publicEndpoint} -> publicEndpoint) (\s@ContainerServiceDeployment' {} a -> s {publicEndpoint = a} :: ContainerServiceDeployment)

-- | The state of the deployment.
--
-- A deployment can be in one of the following states:
--
-- -   @Activating@ - The deployment is being created.
--
-- -   @Active@ - The deployment was successfully created, and it\'s
--     currently running on the container service. The container service
--     can have only one deployment in an active state at a time.
--
-- -   @Inactive@ - The deployment was previously successfully created, but
--     it is not currently running on the container service.
--
-- -   @Failed@ - The deployment failed. Use the @GetContainerLog@ action
--     to view the log events for the containers in the deployment to try
--     to determine the reason for the failure.
containerServiceDeployment_state :: Lens.Lens' ContainerServiceDeployment (Prelude.Maybe ContainerServiceDeploymentState)
containerServiceDeployment_state = Lens.lens (\ContainerServiceDeployment' {state} -> state) (\s@ContainerServiceDeployment' {} a -> s {state = a} :: ContainerServiceDeployment)

-- | An object that describes the configuration for the containers of the
-- deployment.
containerServiceDeployment_containers :: Lens.Lens' ContainerServiceDeployment (Prelude.Maybe (Prelude.HashMap Prelude.Text Container))
containerServiceDeployment_containers = Lens.lens (\ContainerServiceDeployment' {containers} -> containers) (\s@ContainerServiceDeployment' {} a -> s {containers = a} :: ContainerServiceDeployment) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ContainerServiceDeployment where
  parseJSON =
    Prelude.withObject
      "ContainerServiceDeployment"
      ( \x ->
          ContainerServiceDeployment'
            Prelude.<$> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "version")
            Prelude.<*> (x Prelude..:? "publicEndpoint")
            Prelude.<*> (x Prelude..:? "state")
            Prelude.<*> ( x Prelude..:? "containers"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ContainerServiceDeployment

instance Prelude.NFData ContainerServiceDeployment
