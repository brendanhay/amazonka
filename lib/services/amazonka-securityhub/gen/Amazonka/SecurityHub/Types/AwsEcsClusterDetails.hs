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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsClusterDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsClusterDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsClusterClusterSettingsDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterDefaultCapacityProviderStrategyDetails

-- | Provides details about an Amazon ECS cluster.
--
-- /See:/ 'newAwsEcsClusterDetails' smart constructor.
data AwsEcsClusterDetails = AwsEcsClusterDetails'
  { -- | The number of services that are running on the cluster in an @ACTIVE@
    -- state. You can view these services with the Amazon ECS
    -- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListServices.html ListServices>
    -- API operation.
    activeServicesCount :: Prelude.Maybe Prelude.Int,
    -- | The short name of one or more capacity providers to associate with the
    -- cluster.
    capacityProviders :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) that identifies the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | A name that you use to identify your cluster.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The setting to use to create the cluster. Specifically used to configure
    -- whether to enable CloudWatch Container Insights for the cluster.
    clusterSettings :: Prelude.Maybe [AwsEcsClusterClusterSettingsDetails],
    -- | The run command configuration for the cluster.
    configuration :: Prelude.Maybe AwsEcsClusterConfigurationDetails,
    -- | The default capacity provider strategy for the cluster. The default
    -- capacity provider strategy is used when services or tasks are run
    -- without a specified launch type or capacity provider strategy.
    defaultCapacityProviderStrategy :: Prelude.Maybe [AwsEcsClusterDefaultCapacityProviderStrategyDetails],
    -- | The number of container instances registered into the cluster. This
    -- includes container instances in both @ACTIVE@ and @DRAINING@ status.
    registeredContainerInstancesCount :: Prelude.Maybe Prelude.Int,
    -- | The number of tasks in the cluster that are in the @RUNNING@ state.
    runningTasksCount :: Prelude.Maybe Prelude.Int,
    -- | The status of the cluster.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsClusterDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeServicesCount', 'awsEcsClusterDetails_activeServicesCount' - The number of services that are running on the cluster in an @ACTIVE@
-- state. You can view these services with the Amazon ECS
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListServices.html ListServices>
-- API operation.
--
-- 'capacityProviders', 'awsEcsClusterDetails_capacityProviders' - The short name of one or more capacity providers to associate with the
-- cluster.
--
-- 'clusterArn', 'awsEcsClusterDetails_clusterArn' - The Amazon Resource Name (ARN) that identifies the cluster.
--
-- 'clusterName', 'awsEcsClusterDetails_clusterName' - A name that you use to identify your cluster.
--
-- 'clusterSettings', 'awsEcsClusterDetails_clusterSettings' - The setting to use to create the cluster. Specifically used to configure
-- whether to enable CloudWatch Container Insights for the cluster.
--
-- 'configuration', 'awsEcsClusterDetails_configuration' - The run command configuration for the cluster.
--
-- 'defaultCapacityProviderStrategy', 'awsEcsClusterDetails_defaultCapacityProviderStrategy' - The default capacity provider strategy for the cluster. The default
-- capacity provider strategy is used when services or tasks are run
-- without a specified launch type or capacity provider strategy.
--
-- 'registeredContainerInstancesCount', 'awsEcsClusterDetails_registeredContainerInstancesCount' - The number of container instances registered into the cluster. This
-- includes container instances in both @ACTIVE@ and @DRAINING@ status.
--
-- 'runningTasksCount', 'awsEcsClusterDetails_runningTasksCount' - The number of tasks in the cluster that are in the @RUNNING@ state.
--
-- 'status', 'awsEcsClusterDetails_status' - The status of the cluster.
newAwsEcsClusterDetails ::
  AwsEcsClusterDetails
newAwsEcsClusterDetails =
  AwsEcsClusterDetails'
    { activeServicesCount =
        Prelude.Nothing,
      capacityProviders = Prelude.Nothing,
      clusterArn = Prelude.Nothing,
      clusterName = Prelude.Nothing,
      clusterSettings = Prelude.Nothing,
      configuration = Prelude.Nothing,
      defaultCapacityProviderStrategy = Prelude.Nothing,
      registeredContainerInstancesCount = Prelude.Nothing,
      runningTasksCount = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The number of services that are running on the cluster in an @ACTIVE@
-- state. You can view these services with the Amazon ECS
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListServices.html ListServices>
-- API operation.
awsEcsClusterDetails_activeServicesCount :: Lens.Lens' AwsEcsClusterDetails (Prelude.Maybe Prelude.Int)
awsEcsClusterDetails_activeServicesCount = Lens.lens (\AwsEcsClusterDetails' {activeServicesCount} -> activeServicesCount) (\s@AwsEcsClusterDetails' {} a -> s {activeServicesCount = a} :: AwsEcsClusterDetails)

-- | The short name of one or more capacity providers to associate with the
-- cluster.
awsEcsClusterDetails_capacityProviders :: Lens.Lens' AwsEcsClusterDetails (Prelude.Maybe [Prelude.Text])
awsEcsClusterDetails_capacityProviders = Lens.lens (\AwsEcsClusterDetails' {capacityProviders} -> capacityProviders) (\s@AwsEcsClusterDetails' {} a -> s {capacityProviders = a} :: AwsEcsClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) that identifies the cluster.
awsEcsClusterDetails_clusterArn :: Lens.Lens' AwsEcsClusterDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterDetails_clusterArn = Lens.lens (\AwsEcsClusterDetails' {clusterArn} -> clusterArn) (\s@AwsEcsClusterDetails' {} a -> s {clusterArn = a} :: AwsEcsClusterDetails)

-- | A name that you use to identify your cluster.
awsEcsClusterDetails_clusterName :: Lens.Lens' AwsEcsClusterDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterDetails_clusterName = Lens.lens (\AwsEcsClusterDetails' {clusterName} -> clusterName) (\s@AwsEcsClusterDetails' {} a -> s {clusterName = a} :: AwsEcsClusterDetails)

-- | The setting to use to create the cluster. Specifically used to configure
-- whether to enable CloudWatch Container Insights for the cluster.
awsEcsClusterDetails_clusterSettings :: Lens.Lens' AwsEcsClusterDetails (Prelude.Maybe [AwsEcsClusterClusterSettingsDetails])
awsEcsClusterDetails_clusterSettings = Lens.lens (\AwsEcsClusterDetails' {clusterSettings} -> clusterSettings) (\s@AwsEcsClusterDetails' {} a -> s {clusterSettings = a} :: AwsEcsClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The run command configuration for the cluster.
awsEcsClusterDetails_configuration :: Lens.Lens' AwsEcsClusterDetails (Prelude.Maybe AwsEcsClusterConfigurationDetails)
awsEcsClusterDetails_configuration = Lens.lens (\AwsEcsClusterDetails' {configuration} -> configuration) (\s@AwsEcsClusterDetails' {} a -> s {configuration = a} :: AwsEcsClusterDetails)

-- | The default capacity provider strategy for the cluster. The default
-- capacity provider strategy is used when services or tasks are run
-- without a specified launch type or capacity provider strategy.
awsEcsClusterDetails_defaultCapacityProviderStrategy :: Lens.Lens' AwsEcsClusterDetails (Prelude.Maybe [AwsEcsClusterDefaultCapacityProviderStrategyDetails])
awsEcsClusterDetails_defaultCapacityProviderStrategy = Lens.lens (\AwsEcsClusterDetails' {defaultCapacityProviderStrategy} -> defaultCapacityProviderStrategy) (\s@AwsEcsClusterDetails' {} a -> s {defaultCapacityProviderStrategy = a} :: AwsEcsClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The number of container instances registered into the cluster. This
-- includes container instances in both @ACTIVE@ and @DRAINING@ status.
awsEcsClusterDetails_registeredContainerInstancesCount :: Lens.Lens' AwsEcsClusterDetails (Prelude.Maybe Prelude.Int)
awsEcsClusterDetails_registeredContainerInstancesCount = Lens.lens (\AwsEcsClusterDetails' {registeredContainerInstancesCount} -> registeredContainerInstancesCount) (\s@AwsEcsClusterDetails' {} a -> s {registeredContainerInstancesCount = a} :: AwsEcsClusterDetails)

-- | The number of tasks in the cluster that are in the @RUNNING@ state.
awsEcsClusterDetails_runningTasksCount :: Lens.Lens' AwsEcsClusterDetails (Prelude.Maybe Prelude.Int)
awsEcsClusterDetails_runningTasksCount = Lens.lens (\AwsEcsClusterDetails' {runningTasksCount} -> runningTasksCount) (\s@AwsEcsClusterDetails' {} a -> s {runningTasksCount = a} :: AwsEcsClusterDetails)

-- | The status of the cluster.
awsEcsClusterDetails_status :: Lens.Lens' AwsEcsClusterDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterDetails_status = Lens.lens (\AwsEcsClusterDetails' {status} -> status) (\s@AwsEcsClusterDetails' {} a -> s {status = a} :: AwsEcsClusterDetails)

instance Data.FromJSON AwsEcsClusterDetails where
  parseJSON =
    Data.withObject
      "AwsEcsClusterDetails"
      ( \x ->
          AwsEcsClusterDetails'
            Prelude.<$> (x Data..:? "ActiveServicesCount")
            Prelude.<*> ( x Data..:? "CapacityProviders"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ClusterArn")
            Prelude.<*> (x Data..:? "ClusterName")
            Prelude.<*> ( x Data..:? "ClusterSettings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Configuration")
            Prelude.<*> ( x Data..:? "DefaultCapacityProviderStrategy"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "RegisteredContainerInstancesCount")
            Prelude.<*> (x Data..:? "RunningTasksCount")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable AwsEcsClusterDetails where
  hashWithSalt _salt AwsEcsClusterDetails' {..} =
    _salt `Prelude.hashWithSalt` activeServicesCount
      `Prelude.hashWithSalt` capacityProviders
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` clusterSettings
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` defaultCapacityProviderStrategy
      `Prelude.hashWithSalt` registeredContainerInstancesCount
      `Prelude.hashWithSalt` runningTasksCount
      `Prelude.hashWithSalt` status

instance Prelude.NFData AwsEcsClusterDetails where
  rnf AwsEcsClusterDetails' {..} =
    Prelude.rnf activeServicesCount
      `Prelude.seq` Prelude.rnf capacityProviders
      `Prelude.seq` Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf clusterSettings
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf defaultCapacityProviderStrategy
      `Prelude.seq` Prelude.rnf registeredContainerInstancesCount
      `Prelude.seq` Prelude.rnf runningTasksCount
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON AwsEcsClusterDetails where
  toJSON AwsEcsClusterDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActiveServicesCount" Data..=)
              Prelude.<$> activeServicesCount,
            ("CapacityProviders" Data..=)
              Prelude.<$> capacityProviders,
            ("ClusterArn" Data..=) Prelude.<$> clusterArn,
            ("ClusterName" Data..=) Prelude.<$> clusterName,
            ("ClusterSettings" Data..=)
              Prelude.<$> clusterSettings,
            ("Configuration" Data..=) Prelude.<$> configuration,
            ("DefaultCapacityProviderStrategy" Data..=)
              Prelude.<$> defaultCapacityProviderStrategy,
            ("RegisteredContainerInstancesCount" Data..=)
              Prelude.<$> registeredContainerInstancesCount,
            ("RunningTasksCount" Data..=)
              Prelude.<$> runningTasksCount,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
