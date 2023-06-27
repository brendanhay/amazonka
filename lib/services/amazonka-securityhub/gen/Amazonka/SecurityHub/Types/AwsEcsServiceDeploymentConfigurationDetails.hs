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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsServiceDeploymentConfigurationDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsServiceDeploymentConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails

-- | Optional deployment parameters for the service.
--
-- /See:/ 'newAwsEcsServiceDeploymentConfigurationDetails' smart constructor.
data AwsEcsServiceDeploymentConfigurationDetails = AwsEcsServiceDeploymentConfigurationDetails'
  { -- | Determines whether a service deployment fails if a service cannot reach
    -- a steady state.
    deploymentCircuitBreaker :: Prelude.Maybe AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails,
    -- | For a service that uses the rolling update (@ECS@) deployment type, the
    -- maximum number of tasks in a service that are allowed in the @RUNNING@
    -- or @PENDING@ state during a deployment, and for tasks that use the EC2
    -- launch type, when any container instances are in the @DRAINING@ state.
    -- Provided as a percentage of the desired number of tasks. The default
    -- value is 200%.
    --
    -- For a service that uses the blue\/green (@CODE_DEPLOY@) or @EXTERNAL@
    -- deployment types, and tasks that use the EC2 launch type, the maximum
    -- number of tasks in the service that remain in the @RUNNING@ state while
    -- the container instances are in the @DRAINING@ state.
    --
    -- For the Fargate launch type, the maximum percent value is not used.
    maximumPercent :: Prelude.Maybe Prelude.Int,
    -- | For a service that uses the rolling update (@ECS@) deployment type, the
    -- minimum number of tasks in a service that must remain in the @RUNNING@
    -- state during a deployment, and while any container instances are in the
    -- @DRAINING@ state if the service contains tasks using the EC2 launch
    -- type. Expressed as a percentage of the desired number of tasks. The
    -- default value is 100%.
    --
    -- For a service that uses the blue\/green (@CODE_DEPLOY@) or @EXTERNAL@
    -- deployment types and tasks that use the EC2 launch type, the minimum
    -- number of the tasks in the service that remain in the @RUNNING@ state
    -- while the container instances are in the @DRAINING@ state.
    --
    -- For the Fargate launch type, the minimum healthy percent value is not
    -- used.
    minimumHealthyPercent :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsServiceDeploymentConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentCircuitBreaker', 'awsEcsServiceDeploymentConfigurationDetails_deploymentCircuitBreaker' - Determines whether a service deployment fails if a service cannot reach
-- a steady state.
--
-- 'maximumPercent', 'awsEcsServiceDeploymentConfigurationDetails_maximumPercent' - For a service that uses the rolling update (@ECS@) deployment type, the
-- maximum number of tasks in a service that are allowed in the @RUNNING@
-- or @PENDING@ state during a deployment, and for tasks that use the EC2
-- launch type, when any container instances are in the @DRAINING@ state.
-- Provided as a percentage of the desired number of tasks. The default
-- value is 200%.
--
-- For a service that uses the blue\/green (@CODE_DEPLOY@) or @EXTERNAL@
-- deployment types, and tasks that use the EC2 launch type, the maximum
-- number of tasks in the service that remain in the @RUNNING@ state while
-- the container instances are in the @DRAINING@ state.
--
-- For the Fargate launch type, the maximum percent value is not used.
--
-- 'minimumHealthyPercent', 'awsEcsServiceDeploymentConfigurationDetails_minimumHealthyPercent' - For a service that uses the rolling update (@ECS@) deployment type, the
-- minimum number of tasks in a service that must remain in the @RUNNING@
-- state during a deployment, and while any container instances are in the
-- @DRAINING@ state if the service contains tasks using the EC2 launch
-- type. Expressed as a percentage of the desired number of tasks. The
-- default value is 100%.
--
-- For a service that uses the blue\/green (@CODE_DEPLOY@) or @EXTERNAL@
-- deployment types and tasks that use the EC2 launch type, the minimum
-- number of the tasks in the service that remain in the @RUNNING@ state
-- while the container instances are in the @DRAINING@ state.
--
-- For the Fargate launch type, the minimum healthy percent value is not
-- used.
newAwsEcsServiceDeploymentConfigurationDetails ::
  AwsEcsServiceDeploymentConfigurationDetails
newAwsEcsServiceDeploymentConfigurationDetails =
  AwsEcsServiceDeploymentConfigurationDetails'
    { deploymentCircuitBreaker =
        Prelude.Nothing,
      maximumPercent =
        Prelude.Nothing,
      minimumHealthyPercent =
        Prelude.Nothing
    }

-- | Determines whether a service deployment fails if a service cannot reach
-- a steady state.
awsEcsServiceDeploymentConfigurationDetails_deploymentCircuitBreaker :: Lens.Lens' AwsEcsServiceDeploymentConfigurationDetails (Prelude.Maybe AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails)
awsEcsServiceDeploymentConfigurationDetails_deploymentCircuitBreaker = Lens.lens (\AwsEcsServiceDeploymentConfigurationDetails' {deploymentCircuitBreaker} -> deploymentCircuitBreaker) (\s@AwsEcsServiceDeploymentConfigurationDetails' {} a -> s {deploymentCircuitBreaker = a} :: AwsEcsServiceDeploymentConfigurationDetails)

-- | For a service that uses the rolling update (@ECS@) deployment type, the
-- maximum number of tasks in a service that are allowed in the @RUNNING@
-- or @PENDING@ state during a deployment, and for tasks that use the EC2
-- launch type, when any container instances are in the @DRAINING@ state.
-- Provided as a percentage of the desired number of tasks. The default
-- value is 200%.
--
-- For a service that uses the blue\/green (@CODE_DEPLOY@) or @EXTERNAL@
-- deployment types, and tasks that use the EC2 launch type, the maximum
-- number of tasks in the service that remain in the @RUNNING@ state while
-- the container instances are in the @DRAINING@ state.
--
-- For the Fargate launch type, the maximum percent value is not used.
awsEcsServiceDeploymentConfigurationDetails_maximumPercent :: Lens.Lens' AwsEcsServiceDeploymentConfigurationDetails (Prelude.Maybe Prelude.Int)
awsEcsServiceDeploymentConfigurationDetails_maximumPercent = Lens.lens (\AwsEcsServiceDeploymentConfigurationDetails' {maximumPercent} -> maximumPercent) (\s@AwsEcsServiceDeploymentConfigurationDetails' {} a -> s {maximumPercent = a} :: AwsEcsServiceDeploymentConfigurationDetails)

-- | For a service that uses the rolling update (@ECS@) deployment type, the
-- minimum number of tasks in a service that must remain in the @RUNNING@
-- state during a deployment, and while any container instances are in the
-- @DRAINING@ state if the service contains tasks using the EC2 launch
-- type. Expressed as a percentage of the desired number of tasks. The
-- default value is 100%.
--
-- For a service that uses the blue\/green (@CODE_DEPLOY@) or @EXTERNAL@
-- deployment types and tasks that use the EC2 launch type, the minimum
-- number of the tasks in the service that remain in the @RUNNING@ state
-- while the container instances are in the @DRAINING@ state.
--
-- For the Fargate launch type, the minimum healthy percent value is not
-- used.
awsEcsServiceDeploymentConfigurationDetails_minimumHealthyPercent :: Lens.Lens' AwsEcsServiceDeploymentConfigurationDetails (Prelude.Maybe Prelude.Int)
awsEcsServiceDeploymentConfigurationDetails_minimumHealthyPercent = Lens.lens (\AwsEcsServiceDeploymentConfigurationDetails' {minimumHealthyPercent} -> minimumHealthyPercent) (\s@AwsEcsServiceDeploymentConfigurationDetails' {} a -> s {minimumHealthyPercent = a} :: AwsEcsServiceDeploymentConfigurationDetails)

instance
  Data.FromJSON
    AwsEcsServiceDeploymentConfigurationDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsServiceDeploymentConfigurationDetails"
      ( \x ->
          AwsEcsServiceDeploymentConfigurationDetails'
            Prelude.<$> (x Data..:? "DeploymentCircuitBreaker")
            Prelude.<*> (x Data..:? "MaximumPercent")
            Prelude.<*> (x Data..:? "MinimumHealthyPercent")
      )

instance
  Prelude.Hashable
    AwsEcsServiceDeploymentConfigurationDetails
  where
  hashWithSalt
    _salt
    AwsEcsServiceDeploymentConfigurationDetails' {..} =
      _salt
        `Prelude.hashWithSalt` deploymentCircuitBreaker
        `Prelude.hashWithSalt` maximumPercent
        `Prelude.hashWithSalt` minimumHealthyPercent

instance
  Prelude.NFData
    AwsEcsServiceDeploymentConfigurationDetails
  where
  rnf AwsEcsServiceDeploymentConfigurationDetails' {..} =
    Prelude.rnf deploymentCircuitBreaker
      `Prelude.seq` Prelude.rnf maximumPercent
      `Prelude.seq` Prelude.rnf minimumHealthyPercent

instance
  Data.ToJSON
    AwsEcsServiceDeploymentConfigurationDetails
  where
  toJSON
    AwsEcsServiceDeploymentConfigurationDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DeploymentCircuitBreaker" Data..=)
                Prelude.<$> deploymentCircuitBreaker,
              ("MaximumPercent" Data..=)
                Prelude.<$> maximumPercent,
              ("MinimumHealthyPercent" Data..=)
                Prelude.<$> minimumHealthyPercent
            ]
        )
