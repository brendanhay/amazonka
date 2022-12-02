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
-- Module      : Amazonka.ECS.Types.DeploymentController
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.DeploymentController where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.DeploymentControllerType
import qualified Amazonka.Prelude as Prelude

-- | The deployment controller to use for the service. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS deployment types>
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
    --     deployment model powered by CodeDeploy, which allows you to verify a
    --     new deployment of a service before sending production traffic to it.
    --
    -- [EXTERNAL]
    --     The external (@EXTERNAL@) deployment type enables you to use any
    --     third-party deployment controller for full control over the
    --     deployment process for an Amazon ECS service.
    type' :: DeploymentControllerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
--     deployment model powered by CodeDeploy, which allows you to verify a
--     new deployment of a service before sending production traffic to it.
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
--     deployment model powered by CodeDeploy, which allows you to verify a
--     new deployment of a service before sending production traffic to it.
--
-- [EXTERNAL]
--     The external (@EXTERNAL@) deployment type enables you to use any
--     third-party deployment controller for full control over the
--     deployment process for an Amazon ECS service.
deploymentController_type :: Lens.Lens' DeploymentController DeploymentControllerType
deploymentController_type = Lens.lens (\DeploymentController' {type'} -> type') (\s@DeploymentController' {} a -> s {type' = a} :: DeploymentController)

instance Data.FromJSON DeploymentController where
  parseJSON =
    Data.withObject
      "DeploymentController"
      ( \x ->
          DeploymentController' Prelude.<$> (x Data..: "type")
      )

instance Prelude.Hashable DeploymentController where
  hashWithSalt _salt DeploymentController' {..} =
    _salt `Prelude.hashWithSalt` type'

instance Prelude.NFData DeploymentController where
  rnf DeploymentController' {..} = Prelude.rnf type'

instance Data.ToJSON DeploymentController where
  toJSON DeploymentController' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("type" Data..= type')]
      )
