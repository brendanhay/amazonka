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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsClusterDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsClusterClusterSettingsDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterDefaultCapacityProviderStrategyDetails

-- | provides details about an ECS cluster.
--
-- /See:/ 'newAwsEcsClusterDetails' smart constructor.
data AwsEcsClusterDetails = AwsEcsClusterDetails'
  { -- | The setting to use to create the cluster. Specifically used to configure
    -- whether to enable CloudWatch Container Insights for the cluster.
    clusterSettings :: Prelude.Maybe [AwsEcsClusterClusterSettingsDetails],
    -- | The run command configuration for the cluster.
    configuration :: Prelude.Maybe AwsEcsClusterConfigurationDetails,
    -- | The short name of one or more capacity providers to associate with the
    -- cluster.
    capacityProviders :: Prelude.Maybe [Prelude.Text],
    -- | The default capacity provider strategy for the cluster. The default
    -- capacity provider strategy is used when services or tasks are run
    -- without a specified launch type or capacity provider strategy.
    defaultCapacityProviderStrategy :: Prelude.Maybe [AwsEcsClusterDefaultCapacityProviderStrategyDetails]
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
-- 'clusterSettings', 'awsEcsClusterDetails_clusterSettings' - The setting to use to create the cluster. Specifically used to configure
-- whether to enable CloudWatch Container Insights for the cluster.
--
-- 'configuration', 'awsEcsClusterDetails_configuration' - The run command configuration for the cluster.
--
-- 'capacityProviders', 'awsEcsClusterDetails_capacityProviders' - The short name of one or more capacity providers to associate with the
-- cluster.
--
-- 'defaultCapacityProviderStrategy', 'awsEcsClusterDetails_defaultCapacityProviderStrategy' - The default capacity provider strategy for the cluster. The default
-- capacity provider strategy is used when services or tasks are run
-- without a specified launch type or capacity provider strategy.
newAwsEcsClusterDetails ::
  AwsEcsClusterDetails
newAwsEcsClusterDetails =
  AwsEcsClusterDetails'
    { clusterSettings =
        Prelude.Nothing,
      configuration = Prelude.Nothing,
      capacityProviders = Prelude.Nothing,
      defaultCapacityProviderStrategy = Prelude.Nothing
    }

-- | The setting to use to create the cluster. Specifically used to configure
-- whether to enable CloudWatch Container Insights for the cluster.
awsEcsClusterDetails_clusterSettings :: Lens.Lens' AwsEcsClusterDetails (Prelude.Maybe [AwsEcsClusterClusterSettingsDetails])
awsEcsClusterDetails_clusterSettings = Lens.lens (\AwsEcsClusterDetails' {clusterSettings} -> clusterSettings) (\s@AwsEcsClusterDetails' {} a -> s {clusterSettings = a} :: AwsEcsClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The run command configuration for the cluster.
awsEcsClusterDetails_configuration :: Lens.Lens' AwsEcsClusterDetails (Prelude.Maybe AwsEcsClusterConfigurationDetails)
awsEcsClusterDetails_configuration = Lens.lens (\AwsEcsClusterDetails' {configuration} -> configuration) (\s@AwsEcsClusterDetails' {} a -> s {configuration = a} :: AwsEcsClusterDetails)

-- | The short name of one or more capacity providers to associate with the
-- cluster.
awsEcsClusterDetails_capacityProviders :: Lens.Lens' AwsEcsClusterDetails (Prelude.Maybe [Prelude.Text])
awsEcsClusterDetails_capacityProviders = Lens.lens (\AwsEcsClusterDetails' {capacityProviders} -> capacityProviders) (\s@AwsEcsClusterDetails' {} a -> s {capacityProviders = a} :: AwsEcsClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The default capacity provider strategy for the cluster. The default
-- capacity provider strategy is used when services or tasks are run
-- without a specified launch type or capacity provider strategy.
awsEcsClusterDetails_defaultCapacityProviderStrategy :: Lens.Lens' AwsEcsClusterDetails (Prelude.Maybe [AwsEcsClusterDefaultCapacityProviderStrategyDetails])
awsEcsClusterDetails_defaultCapacityProviderStrategy = Lens.lens (\AwsEcsClusterDetails' {defaultCapacityProviderStrategy} -> defaultCapacityProviderStrategy) (\s@AwsEcsClusterDetails' {} a -> s {defaultCapacityProviderStrategy = a} :: AwsEcsClusterDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsEcsClusterDetails where
  parseJSON =
    Core.withObject
      "AwsEcsClusterDetails"
      ( \x ->
          AwsEcsClusterDetails'
            Prelude.<$> ( x Core..:? "ClusterSettings"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Configuration")
            Prelude.<*> ( x Core..:? "CapacityProviders"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "DefaultCapacityProviderStrategy"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsEcsClusterDetails where
  hashWithSalt _salt AwsEcsClusterDetails' {..} =
    _salt `Prelude.hashWithSalt` clusterSettings
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` capacityProviders
      `Prelude.hashWithSalt` defaultCapacityProviderStrategy

instance Prelude.NFData AwsEcsClusterDetails where
  rnf AwsEcsClusterDetails' {..} =
    Prelude.rnf clusterSettings
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf capacityProviders
      `Prelude.seq` Prelude.rnf defaultCapacityProviderStrategy

instance Core.ToJSON AwsEcsClusterDetails where
  toJSON AwsEcsClusterDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClusterSettings" Core..=)
              Prelude.<$> clusterSettings,
            ("Configuration" Core..=) Prelude.<$> configuration,
            ("CapacityProviders" Core..=)
              Prelude.<$> capacityProviders,
            ("DefaultCapacityProviderStrategy" Core..=)
              Prelude.<$> defaultCapacityProviderStrategy
          ]
      )
