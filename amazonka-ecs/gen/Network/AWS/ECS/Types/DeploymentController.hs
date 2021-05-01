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
-- Module      : Network.AWS.ECS.Types.DeploymentController
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DeploymentController where

import Network.AWS.ECS.Types.DeploymentControllerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The deployment controller to use for the service. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newDeploymentController' smart constructor.
data DeploymentController = DeploymentController'
  { -- | The deployment controller type to use.
    --
    -- There are three deployment controller types available:
    --
    -- [ECS]
    --     The rolling update (@ECS@) deployment type involves replacing the
    --     current running version of the container with the latest version.
    --     The number of containers Amazon ECS adds or removes from the service
    --     during a rolling update is controlled by adjusting the minimum and
    --     maximum number of healthy tasks allowed during a service deployment,
    --     as specified in the DeploymentConfiguration.
    --
    -- [CODE_DEPLOY]
    --     The blue\/green (@CODE_DEPLOY@) deployment type uses the blue\/green
    --     deployment model powered by AWS CodeDeploy, which allows you to
    --     verify a new deployment of a service before sending production
    --     traffic to it.
    --
    -- [EXTERNAL]
    --     The external (@EXTERNAL@) deployment type enables you to use any
    --     third-party deployment controller for full control over the
    --     deployment process for an Amazon ECS service.
    type' :: DeploymentControllerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeploymentController' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'deploymentController_type' - The deployment controller type to use.
--
-- There are three deployment controller types available:
--
-- [ECS]
--     The rolling update (@ECS@) deployment type involves replacing the
--     current running version of the container with the latest version.
--     The number of containers Amazon ECS adds or removes from the service
--     during a rolling update is controlled by adjusting the minimum and
--     maximum number of healthy tasks allowed during a service deployment,
--     as specified in the DeploymentConfiguration.
--
-- [CODE_DEPLOY]
--     The blue\/green (@CODE_DEPLOY@) deployment type uses the blue\/green
--     deployment model powered by AWS CodeDeploy, which allows you to
--     verify a new deployment of a service before sending production
--     traffic to it.
--
-- [EXTERNAL]
--     The external (@EXTERNAL@) deployment type enables you to use any
--     third-party deployment controller for full control over the
--     deployment process for an Amazon ECS service.
newDeploymentController ::
  -- | 'type''
  DeploymentControllerType ->
  DeploymentController
newDeploymentController pType_ =
  DeploymentController' {type' = pType_}

-- | The deployment controller type to use.
--
-- There are three deployment controller types available:
--
-- [ECS]
--     The rolling update (@ECS@) deployment type involves replacing the
--     current running version of the container with the latest version.
--     The number of containers Amazon ECS adds or removes from the service
--     during a rolling update is controlled by adjusting the minimum and
--     maximum number of healthy tasks allowed during a service deployment,
--     as specified in the DeploymentConfiguration.
--
-- [CODE_DEPLOY]
--     The blue\/green (@CODE_DEPLOY@) deployment type uses the blue\/green
--     deployment model powered by AWS CodeDeploy, which allows you to
--     verify a new deployment of a service before sending production
--     traffic to it.
--
-- [EXTERNAL]
--     The external (@EXTERNAL@) deployment type enables you to use any
--     third-party deployment controller for full control over the
--     deployment process for an Amazon ECS service.
deploymentController_type :: Lens.Lens' DeploymentController DeploymentControllerType
deploymentController_type = Lens.lens (\DeploymentController' {type'} -> type') (\s@DeploymentController' {} a -> s {type' = a} :: DeploymentController)

instance Prelude.FromJSON DeploymentController where
  parseJSON =
    Prelude.withObject
      "DeploymentController"
      ( \x ->
          DeploymentController'
            Prelude.<$> (x Prelude..: "type")
      )

instance Prelude.Hashable DeploymentController

instance Prelude.NFData DeploymentController

instance Prelude.ToJSON DeploymentController where
  toJSON DeploymentController' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("type" Prelude..= type')]
      )
