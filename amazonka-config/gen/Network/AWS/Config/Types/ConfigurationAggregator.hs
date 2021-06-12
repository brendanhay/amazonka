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
-- Module      : Network.AWS.Config.Types.ConfigurationAggregator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigurationAggregator where

import Network.AWS.Config.Types.AccountAggregationSource
import Network.AWS.Config.Types.OrganizationAggregationSource
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The details about the configuration aggregator, including information
-- about source accounts, regions, and metadata of the aggregator.
--
-- /See:/ 'newConfigurationAggregator' smart constructor.
data ConfigurationAggregator = ConfigurationAggregator'
  { -- | The time stamp when the configuration aggregator was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the aggregator.
    configurationAggregatorArn :: Core.Maybe Core.Text,
    -- | The name of the aggregator.
    configurationAggregatorName :: Core.Maybe Core.Text,
    -- | Provides a list of source accounts and regions to be aggregated.
    accountAggregationSources :: Core.Maybe [AccountAggregationSource],
    -- | AWS service that created the configuration aggregator.
    createdBy :: Core.Maybe Core.Text,
    -- | The time of the last update.
    lastUpdatedTime :: Core.Maybe Core.POSIX,
    -- | Provides an organization and list of regions to be aggregated.
    organizationAggregationSource :: Core.Maybe OrganizationAggregationSource
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfigurationAggregator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'configurationAggregator_creationTime' - The time stamp when the configuration aggregator was created.
--
-- 'configurationAggregatorArn', 'configurationAggregator_configurationAggregatorArn' - The Amazon Resource Name (ARN) of the aggregator.
--
-- 'configurationAggregatorName', 'configurationAggregator_configurationAggregatorName' - The name of the aggregator.
--
-- 'accountAggregationSources', 'configurationAggregator_accountAggregationSources' - Provides a list of source accounts and regions to be aggregated.
--
-- 'createdBy', 'configurationAggregator_createdBy' - AWS service that created the configuration aggregator.
--
-- 'lastUpdatedTime', 'configurationAggregator_lastUpdatedTime' - The time of the last update.
--
-- 'organizationAggregationSource', 'configurationAggregator_organizationAggregationSource' - Provides an organization and list of regions to be aggregated.
newConfigurationAggregator ::
  ConfigurationAggregator
newConfigurationAggregator =
  ConfigurationAggregator'
    { creationTime =
        Core.Nothing,
      configurationAggregatorArn = Core.Nothing,
      configurationAggregatorName = Core.Nothing,
      accountAggregationSources = Core.Nothing,
      createdBy = Core.Nothing,
      lastUpdatedTime = Core.Nothing,
      organizationAggregationSource = Core.Nothing
    }

-- | The time stamp when the configuration aggregator was created.
configurationAggregator_creationTime :: Lens.Lens' ConfigurationAggregator (Core.Maybe Core.UTCTime)
configurationAggregator_creationTime = Lens.lens (\ConfigurationAggregator' {creationTime} -> creationTime) (\s@ConfigurationAggregator' {} a -> s {creationTime = a} :: ConfigurationAggregator) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the aggregator.
configurationAggregator_configurationAggregatorArn :: Lens.Lens' ConfigurationAggregator (Core.Maybe Core.Text)
configurationAggregator_configurationAggregatorArn = Lens.lens (\ConfigurationAggregator' {configurationAggregatorArn} -> configurationAggregatorArn) (\s@ConfigurationAggregator' {} a -> s {configurationAggregatorArn = a} :: ConfigurationAggregator)

-- | The name of the aggregator.
configurationAggregator_configurationAggregatorName :: Lens.Lens' ConfigurationAggregator (Core.Maybe Core.Text)
configurationAggregator_configurationAggregatorName = Lens.lens (\ConfigurationAggregator' {configurationAggregatorName} -> configurationAggregatorName) (\s@ConfigurationAggregator' {} a -> s {configurationAggregatorName = a} :: ConfigurationAggregator)

-- | Provides a list of source accounts and regions to be aggregated.
configurationAggregator_accountAggregationSources :: Lens.Lens' ConfigurationAggregator (Core.Maybe [AccountAggregationSource])
configurationAggregator_accountAggregationSources = Lens.lens (\ConfigurationAggregator' {accountAggregationSources} -> accountAggregationSources) (\s@ConfigurationAggregator' {} a -> s {accountAggregationSources = a} :: ConfigurationAggregator) Core.. Lens.mapping Lens._Coerce

-- | AWS service that created the configuration aggregator.
configurationAggregator_createdBy :: Lens.Lens' ConfigurationAggregator (Core.Maybe Core.Text)
configurationAggregator_createdBy = Lens.lens (\ConfigurationAggregator' {createdBy} -> createdBy) (\s@ConfigurationAggregator' {} a -> s {createdBy = a} :: ConfigurationAggregator)

-- | The time of the last update.
configurationAggregator_lastUpdatedTime :: Lens.Lens' ConfigurationAggregator (Core.Maybe Core.UTCTime)
configurationAggregator_lastUpdatedTime = Lens.lens (\ConfigurationAggregator' {lastUpdatedTime} -> lastUpdatedTime) (\s@ConfigurationAggregator' {} a -> s {lastUpdatedTime = a} :: ConfigurationAggregator) Core.. Lens.mapping Core._Time

-- | Provides an organization and list of regions to be aggregated.
configurationAggregator_organizationAggregationSource :: Lens.Lens' ConfigurationAggregator (Core.Maybe OrganizationAggregationSource)
configurationAggregator_organizationAggregationSource = Lens.lens (\ConfigurationAggregator' {organizationAggregationSource} -> organizationAggregationSource) (\s@ConfigurationAggregator' {} a -> s {organizationAggregationSource = a} :: ConfigurationAggregator)

instance Core.FromJSON ConfigurationAggregator where
  parseJSON =
    Core.withObject
      "ConfigurationAggregator"
      ( \x ->
          ConfigurationAggregator'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "ConfigurationAggregatorArn")
            Core.<*> (x Core..:? "ConfigurationAggregatorName")
            Core.<*> ( x Core..:? "AccountAggregationSources"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "CreatedBy")
            Core.<*> (x Core..:? "LastUpdatedTime")
            Core.<*> (x Core..:? "OrganizationAggregationSource")
      )

instance Core.Hashable ConfigurationAggregator

instance Core.NFData ConfigurationAggregator
